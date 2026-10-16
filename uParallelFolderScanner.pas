unit uParallelFolderScanner;

{
  TParallelFolderScanner - Parallel folder scanning for faster file discovery

  REQ-006: Parallel Folder Scanning
  ---------------------------------
  Uses TParallel.For to scan multiple subdirectories concurrently.
  Target: Speedup >= 2x on CPUs with 4+ cores.

  Algorithm:
  ----------
  1. Get top-level subdirectories (non-parallel, fast)
  2. TParallel.For iterates subdirectories in parallel:
     - Each thread has its own local TStringList
     - Each thread scans its assigned subfolder recursively
     - No cross-thread synchronization during scan
  3. Main thread merges results from all workers
  4. File reading (ProcessPascalFile) remains sequential for now

  Thread Safety:
  --------------
  - Each worker uses thread-local TStringList (no locks needed)
  - Final merge happens after TParallel.For completes
  - FExcludedFolders is read-only (initialized in constructor)
  - No shared mutable state during parallel execution

  Performance Considerations:
  ---------------------------
  - TParallel.For uses TThreadPool.Default (auto-scales to CPU count)
  - Minimum work threshold: only parallelize if 2+ subdirectories
  - I/O bound operations benefit less from parallelism than CPU-bound
  - Expected speedup: 2-4x depending on filesystem and CPU cores

  Usage:
  ------
    var
      Scanner: TParallelFolderScanner;
      Files: TStringList;
    begin
      Scanner := TParallelFolderScanner.Create(True); // Include implementation
      try
        Files := Scanner.ScanFolder('W:\MyProject');
        // Files contains list of .pas file paths
        // Files.Objects[i] contains TParsedFile with content
      finally
        Scanner.Free;
      end;
    end;

  Fallback:
  ---------
  If the folder has fewer than 2 subdirectories, falls back to sequential scan.
  This avoids parallel overhead for small folder structures.
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Threading,
  System.SyncObjs,
  System.Generics.Collections;

type
  /// <summary>Parsed file with content</summary>
  TParallelParsedFile = class
  private
    FFilePath: string;
    FContent: string;
  public
    constructor Create(const AFilePath, AContent: string);
    property FilePath: string read FFilePath;
    property Content: string read FContent;
  end;

  /// <summary>Parallel folder scanner using TParallel.For</summary>
  TParallelFolderScanner = class
  private
    FIncludeImplementation: Boolean;
    FExcludedFolders: TStringList;
    FMinSubfoldersForParallel: Integer;

    // Statistics (updated atomically)
    FFilesFound: Integer;
    FFoldersScanned: Integer;
    FFoldersSkipped: Integer;

    procedure InitializeExcludedFolders;
    function ShouldExcludeFolder(const AFolderPath: string): Boolean;
    function ShouldExcludeFile(const AFilePath: string): Boolean;
    function HasNoIndexFile(const AFolderPath: string): Boolean;

    // Sequential recursive scan (used by each worker)
    procedure ScanFolderRecursive(const AFolderPath: string;
      AFileList: TStringList; var AFoldersScanned, AFoldersSkipped: Integer);

    // Parallel scan of subdirectories
    procedure ScanSubfoldersParallel(const ASubfolders: TArray<string>;
      AFileList: TStringList);

    // File content processing
    function ProcessPascalFile(const AFilePath: string): string;
  public
    constructor Create(AIncludeImplementation: Boolean = False);
    destructor Destroy; override;

    /// <summary>
    /// Scans a folder recursively, discovering all .pas files.
    /// Uses parallel scanning for top-level subdirectories.
    /// </summary>
    /// <param name="AFolderPath">Root folder to scan</param>
    /// <returns>TStringList with file paths and TParsedFile objects</returns>
    function ScanFolder(const AFolderPath: string): TStringList;

    /// <summary>
    /// Scans a folder and returns only file paths (no content loading).
    /// Faster than ScanFolder when content is not needed immediately.
    /// </summary>
    /// <param name="AFolderPath">Root folder to scan</param>
    /// <returns>TStringList with file paths only</returns>
    function ScanFolderPathsOnly(const AFolderPath: string): TStringList;

    /// <summary>If True, returns full file content including implementation section.
    /// If False (default), truncates at 'implementation' keyword.</summary>
    property IncludeImplementation: Boolean read FIncludeImplementation write FIncludeImplementation;

    /// <summary>Minimum number of subdirectories to trigger parallel scan.
    /// Default: 2. Set to higher value on systems where parallel overhead is significant.</summary>
    property MinSubfoldersForParallel: Integer read FMinSubfoldersForParallel write FMinSubfoldersForParallel;

    // Statistics (read after scan completes)
    property FilesFound: Integer read FFilesFound;
    property FoldersScanned: Integer read FFoldersScanned;
    property FoldersSkipped: Integer read FFoldersSkipped;
  end;

implementation

uses
  uFolderScanner;

{ TParallelParsedFile }

constructor TParallelParsedFile.Create(const AFilePath, AContent: string);
begin
  inherited Create;
  FFilePath := AFilePath;
  FContent := AContent;
end;

{ TParallelFolderScanner }

constructor TParallelFolderScanner.Create(AIncludeImplementation: Boolean);
begin
  inherited Create;
  FIncludeImplementation := AIncludeImplementation;
  FMinSubfoldersForParallel := 2;

  FExcludedFolders := TStringList.Create;
  FExcludedFolders.CaseSensitive := False;
  InitializeExcludedFolders;

  // Reset statistics
  FFilesFound := 0;
  FFoldersScanned := 0;
  FFoldersSkipped := 0;
end;

destructor TParallelFolderScanner.Destroy;
begin
  FExcludedFolders.Free;
  inherited Destroy;
end;

procedure TParallelFolderScanner.InitializeExcludedFolders;
begin
  // Match exclusions from uFolderScanner.pas and uChangeDetector.pas
  FExcludedFolders.Add('.git');
  FExcludedFolders.Add('.svn');
  FExcludedFolders.Add('.hg');
  FExcludedFolders.Add('__history');
  FExcludedFolders.Add('__recovery');
  FExcludedFolders.Add('backup');
  FExcludedFolders.Add('Win32');
  FExcludedFolders.Add('Win64');
  FExcludedFolders.Add('Debug');
  FExcludedFolders.Add('Release');
  FExcludedFolders.Add('__DCU_CACHE');
end;

function TParallelFolderScanner.HasNoIndexFile(const AFolderPath: string): Boolean;
var
  NoIndexPath: string;
begin
  NoIndexPath := TPath.Combine(AFolderPath, '.noindex');
  Result := TFile.Exists(NoIndexPath);
end;

function TParallelFolderScanner.ShouldExcludeFolder(const AFolderPath: string): Boolean;
var
  FolderName: string;
begin
  FolderName := ExtractFileName(ExcludeTrailingPathDelimiter(AFolderPath));

  // Check for .noindex file (highest priority)
  if HasNoIndexFile(AFolderPath) then
    Exit(True);

  // Check against excluded folder names
  Result := FExcludedFolders.IndexOf(FolderName) >= 0;
end;

function TParallelFolderScanner.ShouldExcludeFile(const AFilePath: string): Boolean;
var
  FileName: string;
begin
  FileName := ExtractFileName(AFilePath);

  // Exclude auto-generated type library files
  if FileName.EndsWith('_TLB.pas', True) then
    Exit(True);

  // Exclude auto-generated implementation files
  if FileName.EndsWith('_IMPL.pas', True) then
    Exit(True);

  Result := False;
end;

procedure TParallelFolderScanner.ScanFolderRecursive(const AFolderPath: string;
  AFileList: TStringList; var AFoldersScanned, AFoldersSkipped: Integer);
var
  Files: TArray<string>;
  Directories: TArray<string>;
  FilePath: string;
  DirPath: string;
begin
  // Check exclusions
  if ShouldExcludeFolder(AFolderPath) then
  begin
    Inc(AFoldersSkipped);
    Exit;
  end;

  Inc(AFoldersScanned);

  try
    // Get all .pas files in current directory (not recursive)
    Files := TDirectory.GetFiles(AFolderPath, '*.pas', TSearchOption.soTopDirectoryOnly);

    // Add all Pascal files from this directory (excluding auto-generated files)
    for FilePath in Files do
    begin
      if not ShouldExcludeFile(FilePath) then
        AFileList.Add(FilePath);
    end;

    // Get all subdirectories
    Directories := TDirectory.GetDirectories(AFolderPath);

    // Recursively scan subdirectories
    for DirPath in Directories do
      ScanFolderRecursive(DirPath, AFileList, AFoldersScanned, AFoldersSkipped);

  except
    on E: Exception do
    begin
      // Log error but continue (don't break entire scan)
      {$IFDEF DEBUG}
      WriteLn(Format('[ERROR] Scanning folder %s: %s', [AFolderPath, E.Message]));
      {$ENDIF}
    end;
  end;
end;

procedure TParallelFolderScanner.ScanSubfoldersParallel(const ASubfolders: TArray<string>;
  AFileList: TStringList);
var
  WorkerResults: TObjectList<TStringList>;
  I: Integer;
  WorkerList: TStringList;
  Lock: TCriticalSection;
  TotalFoldersScanned, TotalFoldersSkipped: Integer;
begin
  // Pre-allocate worker result lists
  WorkerResults := TObjectList<TStringList>.Create(True);
  Lock := TCriticalSection.Create;
  try
    // Create one result list per subfolder (thread-local storage)
    for I := 0 to Length(ASubfolders) - 1 do
      WorkerResults.Add(TStringList.Create);

    TotalFoldersScanned := 0;
    TotalFoldersSkipped := 0;

    // Parallel scan of subdirectories
    // Each iteration gets its own pre-allocated TStringList
    TParallel.For(0, Length(ASubfolders) - 1,
      procedure(Index: Integer)
      var
        LocalList: TStringList;
        LocalFoldersScanned, LocalFoldersSkipped: Integer;
      begin
        // Get thread-local result list (no synchronization needed)
        LocalList := WorkerResults[Index];
        LocalFoldersScanned := 0;
        LocalFoldersSkipped := 0;

        // Scan this subfolder recursively (completely thread-local)
        ScanFolderRecursive(ASubfolders[Index], LocalList,
          LocalFoldersScanned, LocalFoldersSkipped);

        // Update global statistics atomically
        Lock.Enter;
        try
          Inc(TotalFoldersScanned, LocalFoldersScanned);
          Inc(TotalFoldersSkipped, LocalFoldersSkipped);
        finally
          Lock.Leave;
        end;
      end
    );

    // Merge results from all workers (sequential, after parallel completes)
    for I := 0 to WorkerResults.Count - 1 do
    begin
      WorkerList := WorkerResults[I];
      AFileList.AddStrings(WorkerList);
    end;

    // Update global statistics
    FFoldersScanned := FFoldersScanned + TotalFoldersScanned;
    FFoldersSkipped := FFoldersSkipped + TotalFoldersSkipped;

  finally
    Lock.Free;
    WorkerResults.Free;
  end;
end;

function TParallelFolderScanner.ScanFolderPathsOnly(const AFolderPath: string): TStringList;
var
  RootFiles: TArray<string>;
  RootDirectories: TArray<string>;
  FilePath: string;
  DirPath: string;
  ValidSubfolders: TList<string>;
  SubfoldersArray: TArray<string>;
  LocalFoldersScanned, LocalFoldersSkipped: Integer;
begin
  Result := TStringList.Create;

  // Reset statistics
  FFilesFound := 0;
  FFoldersScanned := 0;
  FFoldersSkipped := 0;

  try
    if not TDirectory.Exists(AFolderPath) then
    begin
      WriteLn(Format('[ERROR] Folder does not exist: %s', [AFolderPath]));
      Exit;
    end;

    // Check if root folder should be excluded
    if ShouldExcludeFolder(AFolderPath) then
    begin
      WriteLn(Format('[SKIP] Excluded root folder: %s', [AFolderPath]));
      FFoldersSkipped := 1;
      Exit;
    end;

    Inc(FFoldersScanned);

    // Step 1: Get files in root folder (non-parallel)
    try
      RootFiles := TDirectory.GetFiles(AFolderPath, '*.pas', TSearchOption.soTopDirectoryOnly);
      for FilePath in RootFiles do
      begin
        if not ShouldExcludeFile(FilePath) then
          Result.Add(FilePath);
      end;
    except
      on E: Exception do
        WriteLn(Format('[ERROR] Getting root files: %s', [E.Message]));
    end;

    // Step 2: Get subdirectories and filter excluded ones
    try
      RootDirectories := TDirectory.GetDirectories(AFolderPath);
    except
      on E: Exception do
      begin
        WriteLn(Format('[ERROR] Getting subdirectories: %s', [E.Message]));
        SetLength(RootDirectories, 0);
      end;
    end;

    ValidSubfolders := TList<string>.Create;
    try
      for DirPath in RootDirectories do
      begin
        if not ShouldExcludeFolder(DirPath) then
          ValidSubfolders.Add(DirPath);
      end;

      // Step 3: Decide parallel vs sequential based on subfolder count
      if ValidSubfolders.Count >= FMinSubfoldersForParallel then
      begin
        // Parallel scan
        WriteLn(Format('[INFO] Parallel scan: %d subdirectories (threshold: %d)',
          [ValidSubfolders.Count, FMinSubfoldersForParallel]));
        SubfoldersArray := ValidSubfolders.ToArray;
        ScanSubfoldersParallel(SubfoldersArray, Result);
      end
      else
      begin
        // Sequential scan (too few subdirectories for parallelism to help)
        WriteLn(Format('[INFO] Sequential scan: %d subdirectories', [ValidSubfolders.Count]));
        LocalFoldersScanned := 0;
        LocalFoldersSkipped := 0;
        for DirPath in ValidSubfolders do
          ScanFolderRecursive(DirPath, Result, LocalFoldersScanned, LocalFoldersSkipped);
        FFoldersScanned := FFoldersScanned + LocalFoldersScanned;
        FFoldersSkipped := FFoldersSkipped + LocalFoldersSkipped;
      end;

    finally
      ValidSubfolders.Free;
    end;

    FFilesFound := Result.Count;
    WriteLn(Format('[INFO] Found %d Pascal files (scanned %d folders, skipped %d)',
      [FFilesFound, FFoldersScanned, FFoldersSkipped]));

  except
    on E: Exception do
    begin
      WriteLn(Format('[ERROR] Folder scanning error: %s', [E.Message]));
    end;
  end;
end;

function TParallelFolderScanner.ScanFolder(const AFolderPath: string): TStringList;
var
  FileList: TStringList;
  FilePath: string;
  FileContent: string;
  ParsedFile: TParallelParsedFile;
begin
  // Get list of file paths (potentially parallel)
  FileList := ScanFolderPathsOnly(AFolderPath);

  // Create result with parsed content
  Result := TStringList.Create;

  try
    // Process each file sequentially (file I/O is I/O bound, not CPU bound)
    for FilePath in FileList do
    begin
      try
        FileContent := ProcessPascalFile(FilePath);

        if Length(Trim(FileContent)) > 0 then
        begin
          ParsedFile := TParallelParsedFile.Create(FilePath, Trim(FileContent));
          Result.AddObject(FilePath, ParsedFile);
        end;

      except
        on E: Exception do
          WriteLn(Format('[ERROR] Processing file %s: %s', [FilePath, E.Message]));
      end;
    end;

    WriteLn(Format('[INFO] Processed %d Pascal files with content', [Result.Count]));

  finally
    FileList.Free;
  end;
end;

function TParallelFolderScanner.ProcessPascalFile(const AFilePath: string): string;
var
  FileContent: TStringList;
begin
  Result := '';

  try
    if not FileExists(AFilePath) then
      Exit;

    FileContent := TStringList.Create;
    try
      FileContent.LoadFromFile(AFilePath);

      // Truncate at implementation if not including full content
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
      WriteLn(Format('[ERROR] Reading file %s: %s', [AFilePath, E.Message]));
      Result := '';
    end;
  end;
end;

end.
