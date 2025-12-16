unit uFolderScanner;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils;

type
  TParsedFile = class
  private
    FFilePath: string;
    FContent: string;
  public
    constructor Create(const AFilePath, AContent: string);
    property FilePath: string read FFilePath;
    property Content: string read FContent;
  end;

  TFolderScanner = class
  private
    FIncludeImplementation: Boolean;
    function ProcessPascalFile(const AFilePath: string): string;
    function TruncateAtImplementation(const AContent: string): string;
    function ShouldExcludeFolder(const AFolderPath: string): Boolean;
    function ShouldExcludeFile(const AFilePath: string): Boolean;
    function HasNoIndexFile(const AFolderPath: string): Boolean;
    procedure ScanFolderRecursive(const AFolderPath: string; AFileList: TStringList);
  public
    constructor Create(AIncludeImplementation: Boolean = False);
    function ScanFolder(const AFolderPath: string): TStringList;
    /// <summary>If True, returns full file content including implementation section.
    /// If False (default), truncates at 'implementation' keyword for FTS-only indexing.</summary>
    property IncludeImplementation: Boolean read FIncludeImplementation write FIncludeImplementation;
  end;

implementation

{ TParsedFile }

constructor TParsedFile.Create(const AFilePath, AContent: string);
begin
  inherited Create;
  FFilePath := AFilePath;
  FContent := AContent;
end;

{ TFolderScanner }

constructor TFolderScanner.Create(AIncludeImplementation: Boolean);
begin
  inherited Create;
  FIncludeImplementation := AIncludeImplementation;
end;

function TFolderScanner.HasNoIndexFile(const AFolderPath: string): Boolean;
var
  NoIndexPath: string;
begin
  NoIndexPath := TPath.Combine(AFolderPath, '.noindex');
  Result := TFile.Exists(NoIndexPath);
end;

function TFolderScanner.ShouldExcludeFolder(const AFolderPath: string): Boolean;
var
  FolderName: string;
begin
  FolderName := ExtractFileName(ExcludeTrailingPathDelimiter(AFolderPath));

  // Check for .noindex file (highest priority)
  if HasNoIndexFile(AFolderPath) then
    Exit(True);

  // Exclude version control folders
  if SameText(FolderName, '.git') or
     SameText(FolderName, '.svn') or
     SameText(FolderName, '.hg') then
    Exit(True);

  // Exclude Delphi history and backup folders
  if SameText(FolderName, '__history') or
     SameText(FolderName, '__recovery') or
     SameText(FolderName, 'backup') then
    Exit(True);

  // Exclude build output folders
  if SameText(FolderName, 'Win32') or
     SameText(FolderName, 'Win64') or
     SameText(FolderName, 'Debug') or
     SameText(FolderName, 'Release') then
    Exit(True);

  Result := False;
end;

function TFolderScanner.ShouldExcludeFile(const AFilePath: string): Boolean;
var
  FileName: string;
begin
  FileName := ExtractFileName(AFilePath);

  // Exclude auto-generated type library files (huge, not useful for search)
  if FileName.EndsWith('_TLB.pas', True) then
    Exit(True);

  // Exclude auto-generated implementation files
  if FileName.EndsWith('_IMPL.pas', True) then
    Exit(True);

  Result := False;
end;

procedure TFolderScanner.ScanFolderRecursive(const AFolderPath: string; AFileList: TStringList);
var
  Files: TArray<string>;
  Directories: TArray<string>;
  FilePath: string;
  DirPath: string;
begin
  // Check if this folder should be excluded
  if ShouldExcludeFolder(AFolderPath) then
  begin
    WriteLn(Format('[SKIP] Excluded folder: %s', [AFolderPath]));
    Exit;
  end;

  try
    // Get all .pas files in current directory (not recursive)
    Files := TDirectory.GetFiles(AFolderPath, '*.pas', TSearchOption.soTopDirectoryOnly);

    // Add all Pascal files from this directory (excluding auto-generated files)
    for FilePath in Files do
    begin
      if ShouldExcludeFile(FilePath) then
        WriteLn(Format('[SKIP] Excluded file: %s', [ExtractFileName(FilePath)]))
      else
        AFileList.Add(FilePath);
    end;

    // Get all subdirectories
    Directories := TDirectory.GetDirectories(AFolderPath);

    // Recursively scan subdirectories
    for DirPath in Directories do
      ScanFolderRecursive(DirPath, AFileList);

  except
    on E: Exception do
      WriteLn(Format('[ERROR] Scanning folder %s: %s', [AFolderPath, E.Message]));
  end;
end;

function TFolderScanner.ScanFolder(const AFolderPath: string): TStringList;
var
  FileList: TStringList;
  FilePath: string;
  FileContent: string;
  ParsedFile: TParsedFile;
begin
  Result := TStringList.Create;

  try
    if not TDirectory.Exists(AFolderPath) then
    begin
      WriteLn(Format('[ERROR] Folder does not exist: %s', [AFolderPath]));
      Exit;
    end;

    // Get all Pascal files recursively (respecting .noindex files)
    FileList := TStringList.Create;
    try
      ScanFolderRecursive(AFolderPath, FileList);
      WriteLn(Format('[INFO] Found %d Pascal files (after exclusions)', [FileList.Count]));

      // Process each file
      for FilePath in FileList do
      begin
        try
          FileContent := ProcessPascalFile(FilePath);

          if Length(Trim(FileContent)) > 0 then
          begin
            ParsedFile := TParsedFile.Create(FilePath, Trim(FileContent));
            Result.AddObject(FilePath, ParsedFile);
          end;

        except
          on E: Exception do
            WriteLn(Format('[ERROR] Processing file %s: %s', [FilePath, E.Message]));
        end;
      end;

      WriteLn(Format('[INFO] Processed %d Pascal files', [Result.Count]));

    finally
      FileList.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn(Format('[ERROR] Folder scanning error: %s', [E.Message]));
      Result.Free;
      Result := TStringList.Create;
    end;
  end;
end;

function TFolderScanner.ProcessPascalFile(const AFilePath: string): string;
var
  FileContent: TStringList;
begin
  Result := '';

  try
    if not FileExists(AFilePath) then
      Exit;

    // Use TStringList.LoadFromFile which has better encoding detection
    // It handles UTF-8 with BOM, ANSI/ISO-8859-1, and other encodings automatically
    FileContent := TStringList.Create;
    try
      FileContent.LoadFromFile(AFilePath);

      // If embeddings are enabled, keep full content for richer semantic search
      // Otherwise, truncate at implementation section for FTS-only indexing
      if FIncludeImplementation then
        Result := FileContent.Text
      else
        Result := TruncateAtImplementation(FileContent.Text);
    finally
      FileContent.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn(Format('Error reading file %s: %s', [AFilePath, E.Message]));
      Result := '';
    end;
  end;
end;

function TFolderScanner.TruncateAtImplementation(const AContent: string): string;
var
  Lines: TStringList;
  I: Integer;
  Line: string;
  TrimmedLine, UpperLine: string;
  ConditionalDepth: Integer;
begin
  Result := AContent; // Default to full content if no implementation found

  Lines := TStringList.Create;
  try
    Lines.Text := AContent;
    ConditionalDepth := 0;

    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];
      TrimmedLine := Trim(Line);
      UpperLine := UpperCase(TrimmedLine);

      // Track conditional depth to avoid truncating inside {$IFDEF} blocks
      // This handles patterns like: {$ifdef OSPOSIX} implementation {$else} ... {$endif}
      if (Pos('{$IFDEF ', UpperLine) > 0) or (Pos('{$IFNDEF ', UpperLine) > 0) or
         (Pos('{$IF ', UpperLine) > 0) or (Pos('{$IFOPT ', UpperLine) > 0) then
        Inc(ConditionalDepth)
      else if (Pos('{$ENDIF', UpperLine) > 0) or (Pos('{$IFEND', UpperLine) > 0) then
        Dec(ConditionalDepth)
      else if Pos('{$ELSE', UpperLine) > 0 then
      begin
        // {$ELSE} toggles but doesn't change depth
        // We're still inside a conditional block
      end;

      // Check if this line contains only "implementation" (case-insensitive)
      // Only truncate if we're NOT inside a conditional block
      if SameText(TrimmedLine, 'implementation') and (ConditionalDepth = 0) then
      begin
        // Found implementation at top level - keep it but add minimal ending
        while Lines.Count > I + 1 do
          Lines.Delete(I + 1);

        Lines.Add('');
        Lines.Add('end.');
        Result := Lines.Text;
        Break;
      end;
    end;

  finally
    Lines.Free;
  end;
end;

end.