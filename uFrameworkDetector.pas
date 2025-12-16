unit uFrameworkDetector;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Generics.Collections,
  Data.DB,
  FireDAC.Comp.Client,
  uDatabaseConnection;

type
  /// <summary>
  /// Validation warnings for framework conflicts
  /// </summary>
  TFrameworkWarning = record
    FilePath: string;
    WarningType: string; // 'comment_vs_uses', 'mapping_vs_package', 'file_vs_package'
    Expected: string;
    Actual: string;
    Message: string;
  end;

  TFrameworkWarningList = TList<TFrameworkWarning>;

  /// <summary>
  /// Multi-tier framework detection with mapping file and package intelligence
  /// </summary>
  TFrameworkDetector = class
  private
    FConnection: TFDConnection;
    FOwnsConnection: Boolean;
    FQuery: TFDQuery;
    FMappingFile: string;
    FMappingData: TJSONObject;
    FWarnings: TFrameworkWarningList;
    FPackageTableChecked: Boolean;
    FPackageTableExists: Boolean;

    /// <summary>Load mapping file (JSON)</summary>
    procedure LoadMappingFile;

    /// <summary>Tier 2: Check mapping file (exact path + glob patterns)</summary>
    function CheckMappingFile(const AFilePath: string): string;

    /// <summary>Tier 5: Folder path fallback</summary>
    function CheckFolderPath(const AFilePath: string): string;

    /// <summary>Extract uses clause from file (interface + implementation)</summary>
    procedure ExtractUsesClause(const AFilePath: string;
      out AInterfaceUses, AImplementationUses: TStringList);

    /// <summary>Detect framework from uses lists</summary>
    function DetectFromUses(AInterfaceUses, AImplementationUses: TStringList): string;

    /// <summary>Normalize path for comparison</summary>
    function NormalizePath(const APath: string): string;

    /// <summary>Add validation warning</summary>
    procedure AddWarning(const AFilePath, AWarningType, AExpected, AActual, AMessage: string);

  public
    /// <summary>Tier 1: Check for comment tag in first 10 lines</summary>
    function CheckCommentTag(const AFilePath: string): string;

    /// <summary>Tier 3: Check package DB</summary>
    function CheckPackageDB(const AFilePath: string): string;

    /// <summary>Tier 4: Analyze uses clauses</summary>
    function AnalyzeUsesClause(const AFilePath: string): string;

    /// <summary>Check if path matches glob pattern</summary>
    function MatchesGlobPattern(const APath, APattern: string): Boolean;
    constructor Create(const ADatabaseFile: string; const AMappingFile: string = ''); overload;
    constructor Create(AExternalConnection: TFDConnection; const AMappingFile: string = ''); overload;
    destructor Destroy; override;

    /// <summary>
    /// Detect framework using multi-tier strategy
    /// Priority: Comment Tag > Mapping File > Package DB > Uses Analysis > Folder Path
    /// </summary>
    function DetectFramework(const AFilePath: string;
      AValidate: Boolean = True): string;

    /// <summary>Get all validation warnings</summary>
    property Warnings: TFrameworkWarningList read FWarnings;

    /// <summary>Print warnings to console</summary>
    procedure PrintWarnings;

    /// <summary>Clear warnings</summary>
    procedure ClearWarnings;
  end;

implementation

uses
  System.Math,
  System.StrUtils,
  System.RegularExpressions,
  FireDAC.Stan.Param;

{ TFrameworkDetector }

constructor TFrameworkDetector.Create(const ADatabaseFile: string; const AMappingFile: string = '');
var
  FullPath: string;
begin
  inherited Create;

  FWarnings := TFrameworkWarningList.Create;
  FMappingFile := AMappingFile;
  FMappingData := nil;

  // Load mapping file if provided
  if (FMappingFile <> '') and FileExists(FMappingFile) then
    LoadMappingFile;

  // Create database connection (owned)
  FConnection := TFDConnection.Create(nil);
  FOwnsConnection := True;
  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;

  // Ensure absolute path
  if not TPath.IsPathRooted(ADatabaseFile) then
    FullPath := TPath.Combine(GetCurrentDir, ADatabaseFile)
  else
    FullPath := ADatabaseFile;

  // Connect (database should already exist)
  if FileExists(FullPath) then
  begin
    TDatabaseConnectionHelper.ConfigureConnection(FConnection, FullPath, False);
    FConnection.Open;
    // Enable WAL mode to allow concurrent access with main connection
    FQuery.SQL.Text := 'PRAGMA journal_mode=WAL';
    FQuery.ExecSQL;
  end;
end;

constructor TFrameworkDetector.Create(AExternalConnection: TFDConnection; const AMappingFile: string = '');
begin
  inherited Create;

  FWarnings := TFrameworkWarningList.Create;
  FMappingFile := AMappingFile;
  FMappingData := nil;

  // Load mapping file if provided
  if (FMappingFile <> '') and FileExists(FMappingFile) then
    LoadMappingFile;

  // Use external connection (not owned)
  FConnection := AExternalConnection;
  FOwnsConnection := False;
  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;
end;

destructor TFrameworkDetector.Destroy;
begin
  if Assigned(FMappingData) then
    FMappingData.Free;

  FWarnings.Free;
  FQuery.Free;
  if FOwnsConnection then
    FConnection.Free;
  inherited;
end;

procedure TFrameworkDetector.LoadMappingFile;
var
  JsonText: string;
begin
  try
    JsonText := TFile.ReadAllText(FMappingFile, TEncoding.UTF8);
    FMappingData := TJSONObject.ParseJSONValue(JsonText) as TJSONObject;

    if not Assigned(FMappingData) then
      WriteLn(Format('Warning: Failed to parse mapping file: %s', [FMappingFile]));

  except
    on E: Exception do
      WriteLn(Format('Warning: Error loading mapping file: %s - %s', [FMappingFile, E.Message]));
  end;
end;

function TFrameworkDetector.NormalizePath(const APath: string): string;
begin
  Result := StringReplace(APath, '\', '/', [rfReplaceAll]);
  Result := LowerCase(Result);

  // Normalize drive letters
  Result := StringReplace(Result, 'w:/', '/mnt/w/', [rfIgnoreCase]);
  Result := StringReplace(Result, 'c:/', '/mnt/c/', [rfIgnoreCase]);
end;

function TFrameworkDetector.CheckCommentTag(const AFilePath: string): string;
var
  Lines: TStringList;
  I: Integer;
  Line, Trimmed: string;
  TagPos: Integer;
begin
  Result := '';

  if not FileExists(AFilePath) then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFilePath, TEncoding.Default);  // Load as ANSI (system code page)

    // Check first 10 lines
    for I := 0 to Min(9, Lines.Count - 1) do
    begin
      Line := Lines[I];
      Trimmed := Trim(Line);

      // Match: "// FRAMEWORK: RTL" (case-insensitive)
      if ContainsText(Trimmed, '// FRAMEWORK:') then
      begin
        TagPos := Pos(':', Trimmed);
        if TagPos > 0 then
        begin
          Result := Trim(Copy(Trimmed, TagPos + 1, Length(Trimmed)));
          Result := UpperCase(Result); // Normalize to uppercase
          Break;
        end;
      end;
    end;

  finally
    Lines.Free;
  end;
end;

function TFrameworkDetector.CheckMappingFile(const AFilePath: string): string;
var
  Overrides: TJSONArray;
  I: Integer;
  Override: TJSONObject;
  MappingPath, MappingPattern: string;
  NormalizedPath: string;
begin
  Result := '';

  if not Assigned(FMappingData) then
    Exit;

  Overrides := FMappingData.GetValue<TJSONArray>('overrides');
  if not Assigned(Overrides) then
    Exit;

  NormalizedPath := NormalizePath(AFilePath);

  // Check each override entry
  for I := 0 to Overrides.Count - 1 do
  begin
    Override := Overrides.Items[I] as TJSONObject;

    // Try exact path match first
    if Override.TryGetValue<string>('file', MappingPath) then
    begin
      MappingPath := NormalizePath(MappingPath);
      if SameText(NormalizedPath, MappingPath) then
      begin
        Override.TryGetValue<string>('framework', Result);
        Exit;
      end;
    end;

    // Try glob pattern match
    if Override.TryGetValue<string>('pattern', MappingPattern) then
    begin
      if MatchesGlobPattern(NormalizedPath, MappingPattern) then
      begin
        Override.TryGetValue<string>('framework', Result);
        Exit;
      end;
    end;
  end;
end;

function TFrameworkDetector.CheckPackageDB(const AFilePath: string): string;
var
  NormalizedPath: string;
begin
  Result := '';

  if not FConnection.Connected then
    Exit;

  // Check once if packages table exists
  if not FPackageTableChecked then
  begin
    FPackageTableChecked := True;
    try
      FQuery.Close;
      FQuery.SQL.Text := 'SELECT 1 FROM packages LIMIT 1';
      FQuery.Open;
      FPackageTableExists := True;
      FQuery.Close;
    except
      FPackageTableExists := False;
      // Silent: table doesn't exist (--scan-packages not run)
    end;
  end;

  if not FPackageTableExists then
    Exit;

  NormalizedPath := NormalizePath(AFilePath);

  try
    FQuery.Close;
    FQuery.SQL.Text := '''
      SELECT p.framework
      FROM packages p
      JOIN package_files pf ON p.package_name = pf.package_name
      WHERE LOWER(pf.file_path) = :file_path
      LIMIT 1
    ''';
    FQuery.ParamByName('file_path').AsString := NormalizedPath;
    FQuery.Open;

    if not FQuery.IsEmpty then
      Result := FQuery.FieldByName('framework').AsString;

    FQuery.Close;
  except
    on E: Exception do
      WriteLn(Format('Warning: Package DB lookup failed: %s', [E.Message]));
  end;
end;

procedure TFrameworkDetector.ExtractUsesClause(const AFilePath: string;
  out AInterfaceUses, AImplementationUses: TStringList);
var
  Lines: TStringList;
  I: Integer;
  Line, Trimmed: string;
  InInterfaceUses, InImplementationUses: Boolean;
begin
  AInterfaceUses := TStringList.Create;
  AImplementationUses := TStringList.Create;

  AInterfaceUses.Duplicates := dupIgnore;
  AImplementationUses.Duplicates := dupIgnore;

  if not FileExists(AFilePath) then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFilePath, TEncoding.Default);  // Load as ANSI (system code page)

    InInterfaceUses := False;
    InImplementationUses := False;

    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];
      Trimmed := Trim(Line);

      // Remove comments
      if Pos('//', Trimmed) > 0 then
        Trimmed := Copy(Trimmed, 1, Pos('//', Trimmed) - 1);

      Trimmed := Trim(Trimmed);

      // Detect sections
      if StartsText('interface', Trimmed) then
      begin
        InInterfaceUses := False;
        InImplementationUses := False;
      end
      else if StartsText('implementation', Trimmed) then
      begin
        InInterfaceUses := False;
        InImplementationUses := False;
      end
      else if StartsText('uses', Trimmed) then
      begin
        // Determine which section we're in (check previous lines for context)
        var PrevLinesText := '';
        for var J := Max(0, I - 5) to I - 1 do
          PrevLinesText := PrevLinesText + Lines[J];

        if ContainsText(PrevLinesText, 'interface') and
           not ContainsText(PrevLinesText, 'implementation') then
          InInterfaceUses := True
        else
          InImplementationUses := True;

        // Remove 'uses' keyword
        Trimmed := StringReplace(Trimmed, 'uses', '', [rfIgnoreCase]);
        Trimmed := Trim(Trimmed);
      end;

      // End of uses clause
      if (InInterfaceUses or InImplementationUses) and
         (Pos(';', Trimmed) > 0) and not ContainsText(Trimmed, ',') then
      begin
        InInterfaceUses := False;
        InImplementationUses := False;
      end;

      // Extract unit names
      if InInterfaceUses or InImplementationUses then
      begin
        // Remove commas, semicolons
        Trimmed := StringReplace(Trimmed, ',', ' ', [rfReplaceAll]);
        Trimmed := StringReplace(Trimmed, ';', '', [rfReplaceAll]);

        // Split by whitespace and extract unit names
        var Units := TStringList.Create;
        try
          Units.Delimiter := ' ';
          Units.StrictDelimiter := True;
          Units.DelimitedText := Trimmed;

          for var J := 0 to Units.Count - 1 do
          begin
            var TrimmedUnit := Trim(Units[J]);
            if (TrimmedUnit <> '') and not StartsText('{', TrimmedUnit) then
            begin
              if InInterfaceUses then
                AInterfaceUses.Add(TrimmedUnit)
              else
                AImplementationUses.Add(TrimmedUnit);
            end;
          end;
        finally
          Units.Free;
        end;
      end;
    end;

  finally
    Lines.Free;
  end;
end;

function TFrameworkDetector.DetectFromUses(AInterfaceUses, AImplementationUses: TStringList): string;
var
  AllUses: TStringList;
  I: Integer;
  UnitName: string;
begin
  AllUses := TStringList.Create;
  try
    AllUses.AddStrings(AInterfaceUses);
    AllUses.AddStrings(AImplementationUses);

    for I := 0 to AllUses.Count - 1 do
    begin
      UnitName := LowerCase(Trim(AllUses[I]));

      // VCL markers (strong signals)
      if StartsText('vcl.', UnitName) or
         (UnitName = 'forms') or (UnitName = 'controls') or
         (UnitName = 'dialogs') or (UnitName = 'stdctrls') or
         (UnitName = 'extctrls') or (UnitName = 'comctrls') then
      begin
        Result := 'VCL';
        Exit; // VCL found, immediate return
      end;

      // FMX markers
      if StartsText('fmx.', UnitName) then
      begin
        Result := 'FMX';
        Exit; // FMX found, immediate return
      end;
    end;

    // Only RTL/Data/WinApi units found
    if AllUses.Count > 0 then
      Result := 'RTL'
    else
      Result := ''; // Unknown

  finally
    AllUses.Free;
  end;
end;

function TFrameworkDetector.AnalyzeUsesClause(const AFilePath: string): string;
var
  InterfaceUses, ImplementationUses: TStringList;
begin
  ExtractUsesClause(AFilePath, InterfaceUses, ImplementationUses);
  try
    Result := DetectFromUses(InterfaceUses, ImplementationUses);
  finally
    InterfaceUses.Free;
    ImplementationUses.Free;
  end;
end;

function TFrameworkDetector.CheckFolderPath(const AFilePath: string): string;
var
  NormalizedPath: string;
begin
  NormalizedPath := LowerCase(AFilePath);

  // VCL patterns
  if ContainsText(NormalizedPath, '\source\vcl\') or
     ContainsText(NormalizedPath, '/source/vcl/') then
    Exit('VCL');

  // FMX patterns
  if ContainsText(NormalizedPath, '\source\fmx\') or
     ContainsText(NormalizedPath, '/source/fmx/') then
    Exit('FMX');

  // RTL patterns
  if ContainsText(NormalizedPath, '\source\rtl\') or
     ContainsText(NormalizedPath, '/source/rtl/') or
     ContainsText(NormalizedPath, '\source\data\') or
     ContainsText(NormalizedPath, '/source/data/') then
    Exit('RTL');

  // Default: unknown
  Result := '';
end;

function TFrameworkDetector.MatchesGlobPattern(const APath, APattern: string): Boolean;
var
  RegexPattern: string;
begin
  // Convert glob pattern to regex
  // ** -> .* (match any characters)
  // * -> [^/]* (match any characters except /)
  // Normalize paths first
  var NormalizedPath := NormalizePath(APath);
  var NormalizedPattern := NormalizePath(APattern);

  RegexPattern := NormalizedPattern;
  RegexPattern := StringReplace(RegexPattern, '.', '\.', [rfReplaceAll]);
  RegexPattern := StringReplace(RegexPattern, '**', '§§', [rfReplaceAll]); // Temp marker
  RegexPattern := StringReplace(RegexPattern, '*', '[^/]*', [rfReplaceAll]);
  RegexPattern := StringReplace(RegexPattern, '§§', '.*', [rfReplaceAll]);

  Result := TRegEx.IsMatch(NormalizedPath, '^' + RegexPattern + '$', [roIgnoreCase]);
end;

procedure TFrameworkDetector.AddWarning(const AFilePath, AWarningType, AExpected, AActual, AMessage: string);
var
  Warning: TFrameworkWarning;
begin
  Warning.FilePath := AFilePath;
  Warning.WarningType := AWarningType;
  Warning.Expected := AExpected;
  Warning.Actual := AActual;
  Warning.Message := AMessage;
  FWarnings.Add(Warning);
end;

function TFrameworkDetector.DetectFramework(const AFilePath: string; AValidate: Boolean = True): string;
var
  CommentTag, MappingResult, PackageResult, UsesResult: string;
begin
  // Tier 1: Comment tag (highest priority)
  CommentTag := CheckCommentTag(AFilePath);
  if CommentTag <> '' then
  begin
    Result := CommentTag;

    // Validate against uses clause if requested
    if AValidate then
    begin
      UsesResult := AnalyzeUsesClause(AFilePath);
      if (UsesResult <> '') and not SameText(CommentTag, UsesResult) then
      begin
        // Only warn if uses is MORE restrictive (e.g., comment says RTL but uses VCL)
        if ((CommentTag = 'RTL') and (UsesResult = 'VCL')) or
           ((CommentTag = 'RTL') and (UsesResult = 'FMX')) then
        begin
          AddWarning(AFilePath, 'comment_vs_uses', CommentTag, UsesResult,
            Format('Comment tag declares %s but uses clause indicates %s', [CommentTag, UsesResult]));
        end;
      end;
    end;

    Exit;
  end;

  // Tier 2: Mapping file
  MappingResult := CheckMappingFile(AFilePath);
  if MappingResult <> '' then
  begin
    Result := MappingResult;

    // Validate against package if requested
    if AValidate then
    begin
      PackageResult := CheckPackageDB(AFilePath);
      if (PackageResult <> '') and not SameText(MappingResult, PackageResult) then
      begin
        AddWarning(AFilePath, 'mapping_vs_package', MappingResult, PackageResult,
          Format('Mapping file declares %s but package is %s', [MappingResult, PackageResult]));
      end;
    end;

    Exit;
  end;

  // Tier 3: Package DB
  PackageResult := CheckPackageDB(AFilePath);
  if PackageResult <> '' then
  begin
    Result := PackageResult;

    // Validate: check if file is MORE restrictive than package
    if AValidate then
    begin
      UsesResult := AnalyzeUsesClause(AFilePath);
      if (UsesResult <> '') and not SameText(PackageResult, UsesResult) then
      begin
        // Warn if file is VCL but package is RTL (possible package error)
        if ((PackageResult = 'RTL') and (UsesResult = 'VCL')) or
           ((PackageResult = 'RTL') and (UsesResult = 'FMX')) then
        begin
          AddWarning(AFilePath, 'file_vs_package', PackageResult, UsesResult,
            Format('Package is %s but file uses %s (possible package .dpk error)', [PackageResult, UsesResult]));
        end;
      end;
    end;

    Exit;
  end;

  // Tier 4: Uses clause analysis
  UsesResult := AnalyzeUsesClause(AFilePath);
  if UsesResult <> '' then
  begin
    Result := UsesResult;

    // Warn: file not in any package (orphaned)
    if AValidate then
    begin
      AddWarning(AFilePath, 'orphaned_file', '', '',
        Format('File not found in any package. Framework detected from uses clause: %s. ' +
                'Consider adding to mapping file or package.', [UsesResult]));
    end;

    Exit;
  end;

  // Tier 5: Folder path fallback
  Result := CheckFolderPath(AFilePath);

  // If still unknown, warn
  if (Result = '') and AValidate then
  begin
    AddWarning(AFilePath, 'unknown_framework', '', '',
      'Unable to detect framework using any method. Manual categorization required.');
  end;
end;

procedure TFrameworkDetector.PrintWarnings;
var
  Warning: TFrameworkWarning;
  WarningCount: Integer;
begin
  WarningCount := 0;

  WriteLn;
  WriteLn('=== Framework Detection Warnings ===');
  WriteLn;

  for Warning in FWarnings do
  begin
    Inc(WarningCount);
    WriteLn(Format('[%d] %s', [WarningCount, Warning.WarningType]));
    WriteLn(Format('    File: %s', [Warning.FilePath]));
    if Warning.Expected <> '' then
      WriteLn(Format('    Expected: %s, Actual: %s', [Warning.Expected, Warning.Actual]));
    WriteLn(Format('    %s', [Warning.Message]));
    WriteLn;
  end;

  if WarningCount = 0 then
    WriteLn('No warnings')
  else
    WriteLn(Format('Total warnings: %d', [WarningCount]));

  WriteLn;
end;

procedure TFrameworkDetector.ClearWarnings;
begin
  FWarnings.Clear;
end;

end.
