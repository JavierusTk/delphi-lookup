unit uPackageScanner;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Hash,
  System.Generics.Collections,
  Data.DB,
  FireDAC.Comp.Client,
  uDatabaseConnection;

type
  /// <summary>
  /// Scans Delphi .dpk package files and populates package intelligence tables
  /// </summary>
  TPackageScanner = class
  private
    FConnection: TFDConnection;
    FQuery: TFDQuery;
    FForceRescan: Boolean;
    FPendingPackages: TDictionary<string, string>; // PackageName -> DpkPath
    FScanningStack: TList<string>; // Currently scanning (circular dependency detection)
    FCurrentCategory: string;

    /// <summary>Parse package name from .dpk file</summary>
    function ExtractPackageName(const ADpkPath: string): string;

    /// <summary>Parse requires clause from .dpk file</summary>
    function ExtractRequires(const ADpkPath: string): TStringList;

    /// <summary>Parse contains clause from .dpk file</summary>
    function ExtractContains(const ADpkPath: string): TStringList;

    /// <summary>Check if package has "assume unknown as RTL" tag</summary>
    function HasAssumeUnknownAsRTL(const ADpkPath: string): Boolean;

    /// <summary>Detect framework from requires clause (with recursive dependency resolution)</summary>
    function DetectFrameworkFromRequires(ARequires: TStringList; AAssumeUnknownAsRTL: Boolean = False): string;

    /// <summary>Lookup package framework from database</summary>
    /// <param name="AScanPending">If True, scan pending packages; if False, only query database</param>
    function LookupPackageFramework(const APackageName: string; AScanPending: Boolean = False): string;

    /// <summary>Calculate MD5 hash of file</summary>
    function CalculateFileHash(const AFilePath: string): string;

    /// <summary>Check if package needs rescanning</summary>
    function NeedsRescan(const APackagePath, AFileHash: string): Boolean;

    /// <summary>Insert or update package record</summary>
    procedure UpsertPackage(const APackageName, APackagePath, AFramework,
      ADetectionMethod, ACategory, AFileHash: string;
      ARequires, AContains: TStringList);

    /// <summary>Delete package and its file records</summary>
    procedure DeletePackage(const APackageName: string);

    /// <summary>Normalize Windows path to Unix path for storage</summary>
    function NormalizePath(const APath: string): string;

    /// <summary>Scan a single package with dependency resolution</summary>
    procedure ScanPackageInternal(const APackageName: string);

  public
    constructor Create(const ADatabaseFile: string);
    destructor Destroy; override;

    /// <summary>Scan a single .dpk file</summary>
    procedure ScanPackage(const ADpkPath: string; const ACategory: string = 'user');

    /// <summary>Scan all .dpk files in a directory recursively</summary>
    procedure ScanDirectory(const ADirPath: string; const ACategory: string = 'user';
      AForceRescan: Boolean = False);

    /// <summary>List all scanned packages</summary>
    procedure ListPackages;
  end;

implementation

uses
  System.Math,
  System.StrUtils,
  FireDAC.Stan.Param;

{ TPackageScanner }

constructor TPackageScanner.Create(const ADatabaseFile: string);
begin
  inherited Create;

  // Create connection
  FConnection := TFDConnection.Create(nil);
  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;

  // Create dependency tracking structures
  FPendingPackages := TDictionary<string, string>.Create;
  FScanningStack := TList<string>.Create;

  // Configure and connect
  TDatabaseConnectionHelper.ConfigureConnectionCreate(FConnection, ADatabaseFile, False);
  FConnection.Open;

  // Enable WAL mode
  FQuery.SQL.Text := 'PRAGMA journal_mode=WAL';
  FQuery.ExecSQL;
end;

destructor TPackageScanner.Destroy;
begin
  FScanningStack.Free;
  FPendingPackages.Free;
  FQuery.Free;
  FConnection.Free;
  inherited;
end;

function TPackageScanner.ExtractPackageName(const ADpkPath: string): string;
var
  Lines: TStringList;
  Line: string;
  I: Integer;
begin
  Result := '';
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(ADpkPath, TEncoding.Default);  // Load as ANSI (system code page)

    // Look for "package PackageName;" in first 20 lines
    for I := 0 to Min(19, Lines.Count - 1) do
    begin
      Line := Trim(Lines[I]);
      if StartsText('package ', Line) then
      begin
        // Extract package name between "package " and ";"
        Result := Trim(Copy(Line, 9, Length(Line)));
        if EndsText(';', Result) then
          Result := Copy(Result, 1, Length(Result) - 1);
        Break;
      end;
    end;

    // Fallback: use filename without extension
    if Result = '' then
      Result := TPath.GetFileNameWithoutExtension(ADpkPath);

  finally
    Lines.Free;
  end;
end;

function TPackageScanner.ExtractRequires(const ADpkPath: string): TStringList;
var
  Lines: TStringList;
  Line, Trimmed: string;
  I: Integer;
  InRequires: Boolean;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := True;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(ADpkPath, TEncoding.Default);  // Load as ANSI (system code page)
    InRequires := False;

    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];
      Trimmed := Trim(Line);

      // Start of requires section
      if StartsText('requires', Trimmed) then
        InRequires := True;

      // End of requires section (contains or end)
      if InRequires and (StartsText('contains', Trimmed) or
                         StartsText('end.', Trimmed)) then
        Break;

      // Extract unit names from requires section
      if InRequires then
      begin
        // Remove comments
        if Pos('//', Trimmed) > 0 then
          Trimmed := Copy(Trimmed, 1, Pos('//', Trimmed) - 1);

        // Remove commas and semicolons
        Trimmed := StringReplace(Trimmed, ',', '', [rfReplaceAll]);
        Trimmed := StringReplace(Trimmed, ';', '', [rfReplaceAll]);
        Trimmed := StringReplace(Trimmed, 'requires', '', [rfIgnoreCase]);
        Trimmed := Trim(Trimmed);

        // Add non-empty unit names
        if (Trimmed <> '') and not ContainsText(Trimmed, '{') then
          Result.Add(Trimmed);
      end;
    end;

  finally
    Lines.Free;
  end;
end;

function TPackageScanner.ExtractContains(const ADpkPath: string): TStringList;
var
  Lines: TStringList;
  Line, Trimmed, FilePath: string;
  I, InPos: Integer;
  InContains: Boolean;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(ADpkPath, TEncoding.Default);  // Load as ANSI (system code page)
    InContains := False;

    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];
      Trimmed := Trim(Line);

      // Start of contains section
      if StartsText('contains', Trimmed) then
        InContains := True;

      // End of contains section
      if InContains and StartsText('end.', Trimmed) then
        Break;

      // Extract file paths from contains section
      if InContains then
      begin
        // Remove comments
        if Pos('//', Trimmed) > 0 then
          Trimmed := Copy(Trimmed, 1, Pos('//', Trimmed) - 1);

        // Look for pattern: UnitName in 'FilePath'
        InPos := Pos(' in ', Trimmed);
        if InPos > 0 then
        begin
          // Extract quoted path
          FilePath := Copy(Trimmed, InPos + 4, Length(Trimmed));
          FilePath := Trim(FilePath);

          // Remove quotes
          if StartsText('''', FilePath) then
            FilePath := Copy(FilePath, 2, Length(FilePath));
          if EndsText('''', FilePath) then
            FilePath := Copy(FilePath, 1, Length(FilePath) - 1);
          if EndsText(',', FilePath) then
            FilePath := Copy(FilePath, 1, Length(FilePath) - 1);
          if EndsText(';', FilePath) then
            FilePath := Copy(FilePath, 1, Length(FilePath) - 1);

          FilePath := Trim(FilePath);

          // Normalize path (W:\ -> /mnt/w/)
          FilePath := NormalizePath(FilePath);

          if FilePath <> '' then
            Result.Add(FilePath);
        end;
      end;
    end;

  finally
    Lines.Free;
  end;
end;

function TPackageScanner.HasAssumeUnknownAsRTL(const ADpkPath: string): Boolean;
var
  Lines: TStringList;
  I: Integer;
  Line, Trimmed: string;
  TagPos: Integer;
  TagValue: string;
begin
  Result := False;

  if not FileExists(ADpkPath) then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(ADpkPath, TEncoding.Default);

    // Check first 10 lines for "// FRAMEWORK: RTL" tag
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
          TagValue := Trim(Copy(Trimmed, TagPos + 1, MaxInt));
          TagValue := UpperCase(TagValue);
          Result := (TagValue = 'RTL');
          Exit;
        end;
      end;
    end;

  finally
    Lines.Free;
  end;
end;

function TPackageScanner.LookupPackageFramework(const APackageName: string; AScanPending: Boolean = False): string;
var
  PackageLower: string;
begin
  Result := '';
  PackageLower := LowerCase(Trim(APackageName));

  if PackageLower = '' then
    Exit;

  // First, try to get framework from database (non-pending packages)
  FQuery.SQL.Text := 'SELECT framework FROM packages WHERE LOWER(package_name) = :package_name';
  FQuery.ParamByName('package_name').AsString := PackageLower;
  FQuery.Open;
  try
    if not FQuery.Eof then
    begin
      Result := FQuery.FieldByName('framework').AsString;
      Exit; // Found in database, return immediately
    end;
  finally
    FQuery.Close;
  end;

  // Not in database - check if it's pending and we're allowed to scan it
  if AScanPending and FPendingPackages.ContainsKey(PackageLower) then
  begin
    // Scan it now (recursive dependency resolution)
    // Note: It might have been removed from pending during recursion, that's ok
    ScanPackageInternal(PackageLower);

    // After scanning, query database again
    FQuery.SQL.Text := 'SELECT framework FROM packages WHERE LOWER(package_name) = :package_name';
    FQuery.ParamByName('package_name').AsString := PackageLower;
    FQuery.Open;
    try
      if not FQuery.Eof then
        Result := FQuery.FieldByName('framework').AsString;
    finally
      FQuery.Close;
    end;
  end;
end;

function TPackageScanner.DetectFrameworkFromRequires(ARequires: TStringList; AAssumeUnknownAsRTL: Boolean = False): string;
var
  I: Integer;
  Req, ReqLower: string;
  RequiredFramework: string;
  AllRTL: Boolean;
begin
  Result := '';

  if ARequires.Count = 0 then
    Exit;

  // ========================================================================
  // PASS 1: Check explicit markers + NON-pending packages only
  // ========================================================================
  for I := 0 to ARequires.Count - 1 do
  begin
    Req := Trim(ARequires[I]);
    ReqLower := LowerCase(Req);

    if ReqLower = '' then
      Continue;

    // Explicit VCL markers
    if (ReqLower = 'vcl') or StartsText('vcl', ReqLower) or
       (ReqLower = 'vcldb') or (ReqLower = 'vclx') or
       (ReqLower = 'vclactnband') or (ReqLower = 'vclimg') or
       (ReqLower = 'vclwinx') or (ReqLower = 'vclsmp') or
       (ReqLower = 'designide') or (ReqLower = 'dclstd') or
       StartsText('dcl', ReqLower) or (ReqLower = 'rtlvcl') then
    begin
      Result := 'VCL';
      Exit;
    end;

    // Explicit FMX markers
    if (ReqLower = 'fmx') or StartsText('fmx', ReqLower) or
       (ReqLower = 'fmxase') then
    begin
      Result := 'FMX';
      Exit;
    end;

    // Check non-pending packages (AScanPending = False)
    RequiredFramework := LookupPackageFramework(Req, False);
    if RequiredFramework = 'VCL' then
    begin
      Result := 'VCL';
      Exit;
    end
    else if RequiredFramework = 'FMX' then
    begin
      Result := 'FMX';
      Exit;
    end;
  end;

  // ========================================================================
  // PASS 2: Check PENDING packages (recursive scan if needed)
  // Only if we didn't find VCL/FMX in pass 1
  // ========================================================================
  for I := 0 to ARequires.Count - 1 do
  begin
    Req := Trim(ARequires[I]);
    ReqLower := LowerCase(Req);

    if ReqLower = '' then
      Continue;

    // Check pending packages (AScanPending = True)
    RequiredFramework := LookupPackageFramework(Req, True);
    if RequiredFramework = 'VCL' then
    begin
      Result := 'VCL';
      Exit;
    end
    else if RequiredFramework = 'FMX' then
    begin
      Result := 'FMX';
      Exit;
    end;
  end;

  // ========================================================================
  // PASS 3: Check if ALL dependencies are RTL
  // Only classify as RTL if EVERY dependency is known to be RTL
  // Unknown dependencies: handled based on AAssumeUnknownAsRTL flag
  // ========================================================================
  AllRTL := True;
  for I := 0 to ARequires.Count - 1 do
  begin
    Req := Trim(ARequires[I]);
    ReqLower := LowerCase(Req);

    if ReqLower = '' then
      Continue;

    // Known RTL-only system packages
    if (ReqLower = 'rtl') or (ReqLower = 'dbrtl') or
       (ReqLower = 'xmlrtl') or (ReqLower = 'soaprtl') or
       (ReqLower = 'inet') or (ReqLower = 'inetdb') or
       (ReqLower = 'indycore') or (ReqLower = 'indysystem') or
       (ReqLower = 'indyprotocols') then
    begin
      Continue; // This is RTL, keep checking
    end;

    // Check package framework (with pending scan)
    RequiredFramework := LookupPackageFramework(Req, True);

    // If RTL, continue
    if RequiredFramework = 'RTL' then
      Continue;

    // Handle unknown dependencies based on flag
    if RequiredFramework = '' then
    begin
      if AAssumeUnknownAsRTL then
        Continue  // Assume RTL, keep checking
      else
      begin
        // Conservative: unknown prevents classification
        AllRTL := False;
        Break;
      end;
    end
    else
    begin
      // Known VCL or FMX dependency found - cannot be RTL
      AllRTL := False;
      Break;
    end;
  end;

  if AllRTL then
    Result := 'RTL';
end;

function TPackageScanner.CalculateFileHash(const AFilePath: string): string;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
  try
    Result := THashMD5.GetHashString(FileStream);
  finally
    FileStream.Free;
  end;
end;

function TPackageScanner.NeedsRescan(const APackagePath, AFileHash: string): Boolean;
var
  StoredHash: string;
begin
  // If force rescan, always return true
  if FForceRescan then
    Exit(True);

  // Check if package exists and hash matches
  FQuery.Close;
  FQuery.SQL.Text := 'SELECT file_hash FROM packages WHERE package_path = :package_path';
  FQuery.ParamByName('package_path').AsString := APackagePath;
  FQuery.Open;

  if FQuery.IsEmpty then
    Result := True // Package not in DB
  else
  begin
    StoredHash := FQuery.FieldByName('file_hash').AsString;
    Result := (StoredHash <> AFileHash); // Rescan if hash changed
  end;

  FQuery.Close;
end;

procedure TPackageScanner.DeletePackage(const APackageName: string);
begin
  // Delete package_files records (CASCADE will handle this, but explicit for clarity)
  FQuery.SQL.Text := 'DELETE FROM package_files WHERE package_name = :package_name';
  FQuery.ParamByName('package_name').AsString := APackageName;
  FQuery.ExecSQL;

  // Delete package record
  FQuery.SQL.Text := 'DELETE FROM packages WHERE package_name = :package_name';
  FQuery.ParamByName('package_name').AsString := APackageName;
  FQuery.ExecSQL;
end;

function TPackageScanner.NormalizePath(const APath: string): string;
begin
  Result := StringReplace(APath, '\', '/', [rfReplaceAll]);
  Result := StringReplace(Result, 'W:/', '/mnt/w/', [rfIgnoreCase]);
  Result := StringReplace(Result, 'C:/', '/mnt/c/', [rfIgnoreCase]);
end;

procedure TPackageScanner.UpsertPackage(const APackageName, APackagePath, AFramework,
  ADetectionMethod, ACategory, AFileHash: string; ARequires, AContains: TStringList);
var
  RequiresStr, ContainsStr: string;
  FilePath: string;
begin
  // Convert lists to comma-separated strings
  RequiresStr := ARequires.CommaText;
  ContainsStr := AContains.CommaText;

  // Delete existing package (if exists)
  DeletePackage(APackageName);

  // Insert package record
  FQuery.SQL.Text := '''
    INSERT INTO packages (
      package_name, package_path, framework, detection_method,
      category, last_scanned, file_hash, requires_clause, contains_clause
    ) VALUES (
      :package_name, :package_path, :framework, :detection_method,
      :category, :last_scanned, :file_hash, :requires_clause, :contains_clause
    )
  ''';

  FQuery.ParamByName('package_name').AsString := APackageName;
  FQuery.ParamByName('package_path').AsString := APackagePath;
  FQuery.ParamByName('framework').AsString := AFramework;
  FQuery.ParamByName('detection_method').AsString := ADetectionMethod;
  FQuery.ParamByName('category').AsString := ACategory;
  FQuery.ParamByName('last_scanned').AsDateTime := Now;
  FQuery.ParamByName('file_hash').AsString := AFileHash;
  FQuery.ParamByName('requires_clause').AsString := RequiresStr;
  FQuery.ParamByName('contains_clause').AsString := ContainsStr;
  FQuery.ExecSQL;

  // Insert file records
  for FilePath in AContains do
  begin
    FQuery.SQL.Text := '''
      INSERT INTO package_files (package_name, file_path, source_clause)
      VALUES (:package_name, :file_path, :source_clause)
    ''';
    FQuery.ParamByName('package_name').AsString := APackageName;
    FQuery.ParamByName('file_path').AsString := FilePath;
    FQuery.ParamByName('source_clause').AsString := 'contains';
    FQuery.ExecSQL;
  end;
end;

procedure TPackageScanner.ScanPackageInternal(const APackageName: string);
var
  DpkPath, PackageNameActual, Framework, FileHash: string;
  PackageLower: string;
  Requires, Contains: TStringList;
begin
  PackageLower := LowerCase(Trim(APackageName));

  // Check if this package is in the pending list
  if not FPendingPackages.ContainsKey(PackageLower) then
  begin
    // Not in pending list - either already scanned or not in this batch
    Exit;
  end;

  // Check for circular dependency
  if FScanningStack.Contains(PackageLower) then
  begin
    WriteLn(Format('  [WARNING] Circular dependency detected: %s', [APackageName]));
    Exit;
  end;

  // Get the .dpk file path
  DpkPath := FPendingPackages[PackageLower];

  // Mark as currently scanning (circular dependency prevention)
  FScanningStack.Add(PackageLower);
  try
    // Calculate file hash
    FileHash := CalculateFileHash(DpkPath);

    // Check if rescan needed
    if not NeedsRescan(DpkPath, FileHash) then
    begin
      // Already up to date, remove from pending and exit
      FPendingPackages.Remove(PackageLower);
      Exit;
    end;

    // Extract package information
    PackageNameActual := ExtractPackageName(DpkPath);
    Requires := ExtractRequires(DpkPath);
    try
      Contains := ExtractContains(DpkPath);
      try
        // Check if package has "assume unknown as RTL" tag
        var AssumeUnknownAsRTL := HasAssumeUnknownAsRTL(DpkPath);

        // Detect framework from requires clause
        Framework := DetectFrameworkFromRequires(Requires, AssumeUnknownAsRTL);

        // Store in database
        UpsertPackage(PackageNameActual, DpkPath, Framework, 'requires_analysis',
          FCurrentCategory, FileHash, Requires, Contains);

        // Remove from pending list (successfully scanned)
        FPendingPackages.Remove(PackageLower);

      finally
        Contains.Free;
      end;
    finally
      Requires.Free;
    end;

  finally
    // Remove from scanning stack
    FScanningStack.Remove(PackageLower);
  end;
end;

procedure TPackageScanner.ScanPackage(const ADpkPath: string; const ACategory: string = 'user');
var
  PackageName, Framework, FileHash: string;
  Requires, Contains: TStringList;
begin
  if not FileExists(ADpkPath) then
  begin
    WriteLn(Format('Error: Package file not found: %s', [ADpkPath]));
    Exit;
  end;

  try
    WriteLn(Format('Scanning package: %s', [ADpkPath]));
  except
    // Ignore console encoding errors
    WriteLn('Scanning package: [path contains non-displayable characters]');
  end;

  // Calculate file hash
  try
    FileHash := CalculateFileHash(ADpkPath);
  except
    on E: Exception do
    begin
      WriteLn(Format('  [ERROR] Failed to calculate file hash: %s', [E.Message]));
      Exit;
    end;
  end;

  // Check if rescan needed
  if not NeedsRescan(ADpkPath, FileHash) then
  begin
    WriteLn('  Package unchanged, skipping (use --force to rescan)');
    Exit;
  end;

  // Extract package information
  try
    PackageName := ExtractPackageName(ADpkPath);
    Requires := ExtractRequires(ADpkPath);
    Contains := ExtractContains(ADpkPath);
    Framework := DetectFrameworkFromRequires(Requires);
  except
    on E: Exception do
    begin
      WriteLn(Format('  [ERROR] Failed to parse package file: %s', [E.Message]));
      Exit;
    end;
  end;

  try
    WriteLn(Format('  Package: %s', [PackageName]));
    WriteLn(Format('  Framework: %s (detected from requires)', [Framework]));
    WriteLn(Format('  Requires: %d packages', [Requires.Count]));
    WriteLn(Format('  Contains: %d files', [Contains.Count]));
  except
    // Ignore console encoding errors
    WriteLn('  Package info contains non-displayable characters');
  end;

  // Store in database
  try
    UpsertPackage(PackageName, ADpkPath, Framework, 'requires_analysis',
      ACategory, FileHash, Requires, Contains);
    WriteLn('  [OK] Package scanned successfully');
  except
    on E: Exception do
      WriteLn(Format('  [ERROR] Error storing package: %s', [E.Message]));
  end;

  Requires.Free;
  Contains.Free;
end;

procedure TPackageScanner.ScanDirectory(const ADirPath: string;
  const ACategory: string = 'user'; AForceRescan: Boolean = False);
var
  DpkFiles: TArray<string>;
  DpkFile, PackageName, PackageLower: string;
  I, Scanned: Integer;
  PendingList: TList<string>;
begin
  FForceRescan := AForceRescan;
  FCurrentCategory := ACategory;

  // Clear dependency tracking structures
  FPendingPackages.Clear;
  FScanningStack.Clear;

  WriteLn(Format('Scanning directory: %s', [ADirPath]));
  WriteLn(Format('Category: %s', [ACategory]));
  WriteLn(Format('Force rescan: %s', [BoolToStr(AForceRescan, True)]));
  WriteLn;

  // Find all .dpk files recursively
  DpkFiles := TDirectory.GetFiles(ADirPath, '*.dpk', TSearchOption.soAllDirectories);

  WriteLn(Format('Found %d package files', [Length(DpkFiles)]));
  WriteLn;

  // Phase 1: Build pending packages dictionary (PackageName -> DpkPath)
  WriteLn('Building dependency map...');
  for DpkFile in DpkFiles do
  begin
    try
      PackageName := ExtractPackageName(DpkFile);
      PackageLower := LowerCase(Trim(PackageName));
      if not FPendingPackages.ContainsKey(PackageLower) then
        FPendingPackages.Add(PackageLower, DpkFile);
    except
      on E: Exception do
        WriteLn(Format('  [WARNING] Failed to extract package name from %s: %s', [DpkFile, E.Message]));
    end;
  end;
  WriteLn(Format('  %d packages in dependency map', [FPendingPackages.Count]));
  WriteLn;

  // Phase 2: Scan all packages (with recursive dependency resolution)
  WriteLn('Scanning packages (with dependency resolution)...');
  PendingList := TList<string>.Create;
  try
    // Get list of package names to scan
    PendingList.AddRange(FPendingPackages.Keys.ToArray);

    // Scan each package
    Scanned := 0;
    for I := 0 to PendingList.Count - 1 do
    begin
      PackageLower := PendingList[I];

      // Skip if already scanned (may have been processed as a dependency)
      if not FPendingPackages.ContainsKey(PackageLower) then
        Continue;

      Inc(Scanned);
      WriteLn(Format('[%d/%d] Scanning: %s', [Scanned, PendingList.Count, PackageLower]));

      try
        ScanPackageInternal(PackageLower);
        WriteLn('  [OK] Scanned successfully');
      except
        on E: Exception do
          WriteLn(Format('  [ERROR] %s', [E.Message]));
      end;
    end;

  finally
    PendingList.Free;
  end;

  WriteLn;
  WriteLn(Format('Completed: %d packages processed', [Scanned]));
end;

procedure TPackageScanner.ListPackages;
begin
  FQuery.Close;
  FQuery.SQL.Text := '''
    SELECT
      package_name,
      framework,
      category,
      (SELECT COUNT(*) FROM package_files WHERE package_name = packages.package_name) as file_count,
      last_scanned
    FROM packages
    ORDER BY package_name
  ''';
  FQuery.Open;

  WriteLn;
  WriteLn('=== Scanned Packages ===');
  WriteLn;
  WriteLn(Format('%-30s %-10s %-15s %-10s %s', ['Package', 'Framework', 'Category', 'Files', 'Last Scanned']));
  WriteLn(StringOfChar('-', 100));

  while not FQuery.Eof do
  begin
    WriteLn(Format('%-30s %-10s %-15s %-10d %s',
      [
        FQuery.FieldByName('package_name').AsString,
        FQuery.FieldByName('framework').AsString,
        FQuery.FieldByName('category').AsString,
        FQuery.FieldByName('file_count').AsInteger,
        FormatDateTime('yyyy-mm-dd hh:nn', FQuery.FieldByName('last_scanned').AsDateTime)
      ]));
    FQuery.Next;
  end;

  WriteLn;
  WriteLn(Format('Total packages: %d', [FQuery.RecordCount]));
  FQuery.Close;
end;

end.
