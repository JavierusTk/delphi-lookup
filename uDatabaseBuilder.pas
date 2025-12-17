unit uDatabaseBuilder;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  System.DateUtils,
  Data.DB,
  FireDAC.Comp.Client,
  uConfig,
  uASTProcessor,
  uIndexerEmbeddings.Ollama,
  uFrameworkDetector,
  uDatabaseConnection;

type
  TFileStatus = (fsNew, fsModified, fsUnchanged, fsSkippedByTimestamp);

  TDatabaseBuilder = class
  private
    FConnection: TFDConnection;
    FQuery: TFDQuery;
    FEmbeddingDimensions: Integer;
    FOllamaURL: string;
    FOllamaModel: string;
    FContentType: string;
    FSourceCategory: string;
    FExplicitFramework: string;
    FFrameworkDetector: TFrameworkDetector;
    FMappingFile: string;
    FDatabaseFile: string;
    FDatabaseExisted: Boolean;  // Track if database existed before indexing

    procedure CreateConnection(const ADatabaseFile: string; ALoadVec0: Boolean = True);
    procedure CreateTables;
    procedure InsertSymbol(AChunk: TCodeChunk; AEmbedding: TEmbedding); overload;
    procedure CreateIndexes;
    function DetectFramework(const AFilePath, AUnitName: string): string;
    procedure StoreMetadata(const AKey, AValue: string);
    procedure RecordFolderIndexing(const AFolderPath: string;
      AStartTime, AEndTime: TDateTime; AFilesCount, AChunksCount: Integer);

    // Incremental indexing methods
    function CalculateFileHash(const AFilePath: string): string;
    function CheckFileStatus(const AFilePath: string;
      const AFolderLastIndexed: TDateTime;
      out AStoredHash: string;
      out AStoredChunkCount: Integer): TFileStatus;
    procedure RemoveFileData(const AFilePath: string);
    procedure StoreFileHash(const AFilePath, AFileHash: string; AChunkCount: Integer);
    function GetFolderLastIndexed(const AFolderPath: string): TDateTime;
    procedure ValidateMetadata(const AOllamaModel: string; AEmbeddingDimensions: Integer);
    function GroupChunksByFile(AChunks: TCodeChunkList): TDictionary<string, TList<TCodeChunk>>;

  public
    constructor Create;
    destructor Destroy; override;

    function FilterFilesToProcess(const AFileList: TStringList; const AFolderPath: string;
      const ADatabaseFile: string; AForceFullReindex: Boolean): TStringList;
    procedure BuildDatabase(AChunks: TCodeChunkList; AEmbeddings: TEmbeddingList;
      const ADatabaseFile: string; const AOllamaURL, AOllamaModel: string;
      AEmbeddingDimensions: Integer; const AFolderPath: string;
      AForceFullReindex: Boolean; const AContentType, ASourceCategory: string;
      const AExplicitFramework: string = ''; const AMappingFile: string = '';
      ABatchSize: Integer = 50);

    // === Hybrid Pipeline Methods ===
    /// <summary>Initialize database for hybrid processing</summary>
    procedure InitializeForHybrid(const ADatabaseFile: string;
      const AOllamaURL, AOllamaModel: string; AEmbeddingDimensions: Integer;
      const AContentType, ASourceCategory: string;
      const AExplicitFramework: string = ''; const AMappingFile: string = '');

    /// <summary>Insert a batch of chunks with their embeddings</summary>
    procedure InsertChunkBatch(AChunks: TCodeChunkList; AEmbeddings: TEmbeddingList);

    /// <summary>Store file hash after processing (within current transaction)</summary>
    procedure CheckpointFile(const AFilePath: string; AChunkCount: Integer);

    /// <summary>Commit current transaction and start new one</summary>
    procedure CommitAndContinue;

    /// <summary>Finalize folder indexing (update indexed_folders, final commit)</summary>
    /// <param name="AFilesModified">Number of files actually modified (0 = skip cache invalidation)</param>
    procedure FinalizeFolder(const AFolderPath: string; AFilesCount, AChunksCount: Integer;
      AStartTime: TDateTime; AFilesModified: Integer);

    /// <summary>Get the database connection (for shared access)</summary>
    property Connection: TFDConnection read FConnection;

    /// <summary>Invalidate all cached query results</summary>
    procedure InvalidateQueryCache;

    /// <summary>Clear old query logs and invalid cache entries</summary>
    procedure CleanupQueryLogs(ADaysToKeep: Integer = 90);

    /// <summary>Migrate schema to support documentation fields</summary>
    procedure MigrateToDocSchema;

    /// <summary>Check if a column exists in a table</summary>
    function ColumnExists(const ATable, AColumn: string): Boolean;

    /// <summary>Insert symbol with documentation fields (for CHM indexing)</summary>
    procedure InsertSymbol(const AName, AFullName, AType, AFilePath, AContent: string;
      const AComments, AParentClass: string; AEmbedding: TEmbedding;
      const AFramework, APlatforms, ADelphiVersion: string); overload;
  end;

implementation

uses
  System.Math,
  System.IOUtils,
  System.Hash,
  System.StrUtils,
  FireDAC.Stan.Param;

/// <summary>Extract method signature from full implementation code.
/// For FTS indexing, we only want the signature, not the body.</summary>
function ExtractMethodSignature(const AContent: string): string;
var
  Lines: TStringList;
  I: Integer;
  Line, TrimmedLine, UpperLine: string;
  SignatureLines: TStringList;
  InSignature: Boolean;
  ParenDepth: Integer;
begin
  // For short content (likely already just a declaration), return as-is
  if Length(AContent) < 300 then
    Exit(AContent);

  Lines := TStringList.Create;
  SignatureLines := TStringList.Create;
  try
    Lines.Text := AContent;
    InSignature := True;
    ParenDepth := 0;

    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];
      TrimmedLine := Trim(Line);
      UpperLine := UpperCase(TrimmedLine);

      // Track parenthesis depth for multi-line parameter lists
      for var C in Line do
      begin
        if C = '(' then Inc(ParenDepth)
        else if C = ')' then Dec(ParenDepth);
      end;

      // Check for end of signature markers
      if InSignature then
      begin
        // Stop at 'var', 'const' (local), 'begin', 'asm' when not inside parentheses
        if (ParenDepth = 0) and
           ((UpperLine = 'VAR') or (UpperLine = 'CONST') or
            (UpperLine = 'BEGIN') or (UpperLine = 'ASM') or
            StartsText('VAR ', UpperLine) or StartsText('CONST ', UpperLine)) then
          Break;

        SignatureLines.Add(Line);

        // If line ends with ';' and we're not inside parentheses, signature is complete
        if (ParenDepth = 0) and TrimmedLine.EndsWith(';') then
          Break;
      end;
    end;

    Result := Trim(SignatureLines.Text);

    // Fallback: if we got nothing, return first 300 chars
    if Result = '' then
      Result := Copy(AContent, 1, 300);

  finally
    Lines.Free;
    SignatureLines.Free;
  end;
end;

{ TDatabaseBuilder }

constructor TDatabaseBuilder.Create;
begin
  inherited Create;
  FConnection := TFDConnection.Create(nil);
  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;
end;

destructor TDatabaseBuilder.Destroy;
begin
  if Assigned(FFrameworkDetector) then
    FFrameworkDetector.Free;
  FQuery.Free;
  FConnection.Free;
  inherited Destroy;
end;

procedure TDatabaseBuilder.CreateConnection(const ADatabaseFile: string; ALoadVec0: Boolean);
var
  FullPath: string;
begin
  try
    // Ensure we have an absolute path
    if not TPath.IsPathRooted(ADatabaseFile) then
      FullPath := TPath.Combine(GetCurrentDir, ADatabaseFile)
    else
      FullPath := ADatabaseFile;

    WriteLn(Format('Attempting to create database at: %s', [FullPath]));

    // Track if database existed before we create/connect
    FDatabaseExisted := FileExists(FullPath);

    // Configure connection using helper (handles directory creation)
    TDatabaseConnectionHelper.ConfigureConnectionCreate(FConnection, FullPath, True);

    WriteLn('Connecting to database...');
    FConnection.Open;
    WriteLn('Connection successful');

    // Enable WAL mode for concurrent access
    FQuery.SQL.Text := 'PRAGMA journal_mode=WAL';
    FQuery.ExecSQL;
    WriteLn('WAL mode enabled');

    // Load sqlite-vec extension (only needed for embeddings)
    if ALoadVec0 then
    begin
      try
        WriteLn('Loading sqlite-vec extension...');
        TDatabaseConnectionHelper.LoadVec0Extension(FConnection);
        WriteLn('sqlite-vec extension loaded successfully');
      except
        on E: Exception do
        begin
          WriteLn(Format('Warning: Failed to load sqlite-vec extension: %s', [E.Message]));
          WriteLn('Vector search will not be available.');
        end;
      end;
    end;

    WriteLn(Format('Successfully connected to database: %s', [FullPath]));

  except
    on E: Exception do
    begin
      WriteLn('=== DATABASE CONNECTION ERROR ===');
      WriteLn(Format('Error: %s', [E.Message]));
      WriteLn('');
      WriteLn('Possible solutions:');
      WriteLn('1. Run as administrator if using system directories');
      WriteLn('2. Ensure the target directory is writable');
      WriteLn('3. Check that sqlite3.dll is in the executable directory');
      WriteLn('');
      raise Exception.CreateFmt('Failed to connect to database: %s', [E.Message]);
    end;
  end;
end;

procedure TDatabaseBuilder.CreateTables;
begin
  try
    // Create metadata table for self-describing database
    FQuery.SQL.Text := '''
      CREATE TABLE IF NOT EXISTS metadata (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL
      )
    ''';
    FQuery.ExecSQL;

    // Create indexed_folders table for tracking processed folders
    FQuery.SQL.Text := '''
      CREATE TABLE IF NOT EXISTS indexed_folders (
        folder_path TEXT PRIMARY KEY,
        indexed_at TIMESTAMP NOT NULL,
        duration_seconds INTEGER NOT NULL,
        files_count INTEGER NOT NULL,
        chunks_count INTEGER NOT NULL,
        content_type TEXT NOT NULL DEFAULT 'code',
        source_category TEXT NOT NULL DEFAULT 'user'
      )
    ''';
    FQuery.ExecSQL;

    // Create file_hashes table for file-level change detection
    FQuery.SQL.Text := '''
      CREATE TABLE IF NOT EXISTS file_hashes (
        file_path TEXT PRIMARY KEY,
        file_hash TEXT NOT NULL,
        file_size INTEGER NOT NULL,
        last_indexed TIMESTAMP NOT NULL,
        chunk_count INTEGER NOT NULL
      )
    ''';
    FQuery.ExecSQL;
    WriteLn('file_hashes table created');

    // Create index for faster folder queries
    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_file_hashes_path ON file_hashes(file_path)';
    FQuery.ExecSQL;

    // Create main symbols table
    FQuery.SQL.Text := '''
      CREATE TABLE IF NOT EXISTS symbols (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        full_name TEXT,
        type TEXT NOT NULL,
        file_path TEXT NOT NULL,
        content TEXT NOT NULL,
        enriched_text TEXT,
        spanish_terms TEXT,
        domain_tags TEXT,
        comments TEXT,
        parent_class TEXT,
        implemented_interfaces TEXT,
        visibility TEXT,
        start_line INTEGER,
        end_line INTEGER,
        content_type TEXT NOT NULL DEFAULT 'code',
        source_category TEXT NOT NULL DEFAULT 'user',
        framework TEXT,
        platforms TEXT,
        delphi_version TEXT,
        introduced_version TEXT,
        deprecated_version TEXT,
        is_inherited INTEGER DEFAULT 0,
        inherited_from TEXT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

        UNIQUE(name, file_path, type)
      )
    ''';
    FQuery.ExecSQL;

    // Create vec0 virtual table for vector embeddings (only if embeddings enabled)
    if FEmbeddingDimensions > 0 then
    begin
      FQuery.SQL.Text := Format('''
        CREATE VIRTUAL TABLE IF NOT EXISTS vec_embeddings USING vec0(
          symbol_id INTEGER,
          chunk_id TEXT,
          embedding_text TEXT,
          embedding FLOAT[%d]
        )
      ''', [FEmbeddingDimensions]);
      FQuery.ExecSQL;
    end;
    
    // Create query logging table for usage analysis (also serves as cache)
    FQuery.SQL.Text := '''
      CREATE TABLE IF NOT EXISTS query_log (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        executed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        query_text TEXT NOT NULL,
        query_hash TEXT NOT NULL,

        -- Cache fields (reuse log as cache!)
        result_ids TEXT,
        cache_valid INTEGER DEFAULT 1,

        -- Filters used
        content_type_filter TEXT,
        source_category_filter TEXT,
        prefer_category TEXT,
        domain_tags_filter TEXT,
        symbol_type_filter TEXT,

        -- Search configuration
        num_results_requested INTEGER,
        max_distance REAL,
        use_semantic_search INTEGER DEFAULT 0,
        use_reranker INTEGER DEFAULT 0,
        candidate_count INTEGER,

        -- Performance metrics (milliseconds)
        duration_ms INTEGER NOT NULL,
        exact_search_ms INTEGER,
        fuzzy_search_ms INTEGER,
        fts_search_ms INTEGER,
        vector_search_ms INTEGER,
        reranker_ms INTEGER,

        -- Results
        result_count INTEGER NOT NULL,
        exact_match_found INTEGER DEFAULT 0,
        cache_hit INTEGER DEFAULT 0,

        -- Search method breakdown
        exact_results INTEGER DEFAULT 0,
        fuzzy_results INTEGER DEFAULT 0,
        fts_results INTEGER DEFAULT 0,
        vector_results INTEGER DEFAULT 0
      )
    ''';
    FQuery.ExecSQL;

    // Create packages table (for framework detection via --scan-packages)
    FQuery.SQL.Text := '''
      CREATE TABLE IF NOT EXISTS packages (
        package_name TEXT PRIMARY KEY,
        package_path TEXT NOT NULL,
        framework TEXT,
        detection_method TEXT NOT NULL,
        category TEXT,
        last_scanned TIMESTAMP NOT NULL,
        file_hash TEXT NOT NULL,
        requires_clause TEXT,
        contains_clause TEXT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        UNIQUE(package_path)
      )
    ''';
    FQuery.ExecSQL;

    // Create package_files table
    FQuery.SQL.Text := '''
      CREATE TABLE IF NOT EXISTS package_files (
        package_name TEXT NOT NULL,
        file_path TEXT NOT NULL,
        source_clause TEXT NOT NULL,
        PRIMARY KEY (package_name, file_path),
        FOREIGN KEY (package_name) REFERENCES packages(package_name) ON DELETE CASCADE
      )
    ''';
    FQuery.ExecSQL;

    // Force WAL checkpoint to release locks before other connections attempt to access
    FQuery.SQL.Text := 'PRAGMA wal_checkpoint(TRUNCATE)';
    FQuery.ExecSQL;

    WriteLn('Database tables created successfully');

  except
    on E: Exception do
      raise Exception.CreateFmt('Failed to create tables: %s', [E.Message]);
  end;
end;

procedure TDatabaseBuilder.CreateIndexes;
begin
  try
    WriteLn('Creating database indexes...');
    
    // Primary search indexes
    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_symbols_name ON symbols(name)';
    FQuery.ExecSQL;
    
    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_symbols_type ON symbols(type)';
    FQuery.ExecSQL;
    
    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_symbols_file ON symbols(file_path)';
    FQuery.ExecSQL;
    
    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_symbols_parent ON symbols(parent_class)';
    FQuery.ExecSQL;

    // Classification indexes for filtering
    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_symbols_content_type ON symbols(content_type)';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_symbols_source_category ON symbols(source_category)';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_symbols_classification ON symbols(content_type, source_category)';
    FQuery.ExecSQL;

    // Domain tags index for glossary enrichment filtering
    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_symbols_domains ON symbols(domain_tags)';
    FQuery.ExecSQL;

    // Package indexes for framework detection
    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_package_files_by_path ON package_files(file_path)';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_packages_by_framework ON packages(framework)';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_packages_by_category ON packages(category)';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_packages_by_scan_date ON packages(last_scanned)';
    FQuery.ExecSQL;

    // Full-text search index on content and comments (optional - skip if FTS5 not available)
    try
      // Drop existing FTS5 table if it exists (to rebuild from scratch)
      try
        FQuery.SQL.Text := 'DROP TABLE IF EXISTS symbols_fts';
        FQuery.ExecSQL;
      except
        // Ignore errors if table doesn't exist
      end;

      // Create FTS5 virtual table
      FQuery.SQL.Text := '''
        CREATE VIRTUAL TABLE symbols_fts USING fts5(
          name, full_name, content, comments,
          content='symbols', content_rowid='id'
        )
      ''';
      FQuery.ExecSQL;

      // Populate FTS table with all symbols
      FQuery.SQL.Text := '''
        INSERT INTO symbols_fts(rowid, name, full_name, content, comments)
        SELECT id, name, full_name, content, comments FROM symbols
      ''';
      FQuery.ExecSQL;
      WriteLn('Full-text search index created successfully');
    except
      on E: Exception do
      begin
        WriteLn('Warning: FTS5 not available, skipping full-text search index');
        WriteLn(Format('FTS5 error: %s', [E.Message]));
      end;
    end;
    
    // Note: vec0 virtual tables don't support traditional indexes
    // Vector similarity search is handled internally by sqlite-vec

    // Query log indexes for analysis and caching
    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_query_log_cache ON query_log(query_hash, cache_valid)';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_query_log_executed ON query_log(executed_at)';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_query_log_hash ON query_log(query_hash)';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_query_log_duration ON query_log(duration_ms)';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_query_log_text ON query_log(query_text)';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_query_log_valid ON query_log(cache_valid)';
    FQuery.ExecSQL;

    WriteLn('Database indexes created successfully');
    
  except
    on E: Exception do
      raise Exception.CreateFmt('Failed to create indexes: %s', [E.Message]);
  end;
end;

procedure TDatabaseBuilder.StoreMetadata(const AKey, AValue: string);
begin
  FQuery.SQL.Text := 'INSERT OR REPLACE INTO metadata (key, value) VALUES (:key, :value)';
  FQuery.ParamByName('key').AsString := AKey;
  FQuery.ParamByName('value').AsString := AValue;
  FQuery.ExecSQL;
end;

procedure TDatabaseBuilder.RecordFolderIndexing(const AFolderPath: string;
  AStartTime, AEndTime: TDateTime; AFilesCount, AChunksCount: Integer);
var
  DurationSeconds: Integer;
begin
  DurationSeconds := SecondsBetween(AEndTime, AStartTime);

  FQuery.SQL.Text := '''
    INSERT OR REPLACE INTO indexed_folders (folder_path, indexed_at, duration_seconds, files_count, chunks_count, content_type, source_category)
    VALUES (:folder_path, :indexed_at, :duration_seconds, :files_count, :chunks_count, :content_type, :source_category)
  ''';
  FQuery.ParamByName('folder_path').AsString := AFolderPath;
  FQuery.ParamByName('indexed_at').AsString := FormatDateTime('yyyy-mm-dd hh:nn:ss', AEndTime);
  FQuery.ParamByName('duration_seconds').AsInteger := DurationSeconds;
  FQuery.ParamByName('files_count').AsInteger := AFilesCount;
  FQuery.ParamByName('chunks_count').AsInteger := AChunksCount;
  FQuery.ParamByName('content_type').AsString := FContentType;
  FQuery.ParamByName('source_category').AsString := FSourceCategory;
  FQuery.ExecSQL;

  WriteLn(Format('Recorded folder indexing: %s (%d files, %d chunks, %ds)',
    [AFolderPath, AFilesCount, AChunksCount, DurationSeconds]));
end;

{ Incremental Indexing Methods }

function TDatabaseBuilder.CalculateFileHash(const AFilePath: string): string;
var
  FileStream: TFileStream;
begin
  try
    FileStream := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
    try
      // MD5 is sufficient for change detection (not cryptographic security)
      Result := THashMD5.GetHashString(FileStream);
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(Format('Warning: Failed to calculate hash for %s: %s',
        [ExtractFileName(AFilePath), E.Message]));
      Result := ''; // Empty hash indicates error
    end;
  end;
end;

function TDatabaseBuilder.CheckFileStatus(const AFilePath: string;
  const AFolderLastIndexed: TDateTime;
  out AStoredHash: string;
  out AStoredChunkCount: Integer): TFileStatus;
var
  FileModTime: TDateTime;
  CurrentHash: string;
  FileExistsInDb: Boolean;
begin
  // Stage 1: Timestamp Pre-Filter
  if AFolderLastIndexed > 0 then
  begin
    FileModTime := TFile.GetLastWriteTime(AFilePath);

    if FileModTime <= AFolderLastIndexed then
    begin
      // File is older than last indexing - check if it's in database
      FQuery.SQL.Text := 'SELECT 1 FROM file_hashes WHERE file_path = :file_path';
      FQuery.ParamByName('file_path').AsString := AFilePath;
      FQuery.Open;
      FileExistsInDb := not FQuery.Eof;
      FQuery.Close;

      if FileExistsInDb then
      begin
        // Definitely existed before and hasn't been modified since
        // Skip expensive hash calculation
        AStoredHash := '';
        AStoredChunkCount := 0;
        Exit(fsSkippedByTimestamp);
      end;
      // Else: Old file but not in DB - treat as new
    end;
  end;

  // Stage 2: Hash Verification (for candidates)
  FQuery.SQL.Text := 'SELECT file_hash, chunk_count FROM file_hashes WHERE file_path = :file_path';
  FQuery.ParamByName('file_path').AsString := AFilePath;
  FQuery.Open;

  try
    if FQuery.Eof then
    begin
      // File not in database - it's new
      AStoredHash := '';
      AStoredChunkCount := 0;
      Result := fsNew;
    end
    else
    begin
      // File exists in database - check if modified
      AStoredHash := FQuery.FieldByName('file_hash').AsString;
      AStoredChunkCount := FQuery.FieldByName('chunk_count').AsInteger;

      // Calculate current hash
      CurrentHash := CalculateFileHash(AFilePath);

      if CurrentHash = '' then
      begin
        // Hash calculation failed - treat as modified to be safe
        Result := fsModified;
      end
      else if CurrentHash = AStoredHash then
      begin
        // Hash matches - file unchanged (edge case: touched but not modified)
        Result := fsUnchanged;
      end
      else
      begin
        // Hash differs - file modified
        Result := fsModified;
      end;
    end;
  finally
    FQuery.Close;
  end;
end;

procedure TDatabaseBuilder.RemoveFileData(const AFilePath: string);
var
  DeletedSymbols: Integer;
begin
  try
    // Get count of symbols to be deleted (for reporting)
    FQuery.SQL.Text := 'SELECT COUNT(*) as cnt FROM symbols WHERE file_path = :file_path';
    FQuery.ParamByName('file_path').AsString := AFilePath;
    FQuery.Open;
    DeletedSymbols := FQuery.FieldByName('cnt').AsInteger;
    FQuery.Close;

    if DeletedSymbols = 0 then
      Exit; // Nothing to delete

    WriteLn(Format('  Removing %d old symbols from: %s',
      [DeletedSymbols, ExtractFileName(AFilePath)]));

    // Remove vector embeddings first (they reference symbols)
    FQuery.SQL.Text := '''
      DELETE FROM vec_embeddings
      WHERE symbol_id IN (
        SELECT id FROM symbols WHERE file_path = :file_path
      )
    ''';
    FQuery.ParamByName('file_path').AsString := AFilePath;
    FQuery.ExecSQL;

    // Remove symbols
    FQuery.SQL.Text := 'DELETE FROM symbols WHERE file_path = :file_path';
    FQuery.ParamByName('file_path').AsString := AFilePath;
    FQuery.ExecSQL;

    // Remove FTS entries (if FTS is enabled)
    try
      FQuery.SQL.Text := '''
        DELETE FROM symbols_fts
        WHERE rowid IN (
          SELECT id FROM symbols WHERE file_path = :file_path
        )
      ''';
      FQuery.ParamByName('file_path').AsString := AFilePath;
      FQuery.ExecSQL;
    except
      // FTS5 might not be enabled, ignore errors
    end;

    // Remove hash entry
    FQuery.SQL.Text := 'DELETE FROM file_hashes WHERE file_path = :file_path';
    FQuery.ParamByName('file_path').AsString := AFilePath;
    FQuery.ExecSQL;

  except
    on E: Exception do
      WriteLn(Format('Warning: Error removing file data for %s: %s',
        [ExtractFileName(AFilePath), E.Message]));
  end;
end;

procedure TDatabaseBuilder.StoreFileHash(const AFilePath, AFileHash: string;
  AChunkCount: Integer);
var
  FileSize: Int64;
begin
  try
    // Get file size
    if FileExists(AFilePath) then
      FileSize := TFile.GetSize(AFilePath)
    else
      FileSize := 0;

    // Store hash and metadata
    FQuery.SQL.Text := '''
      INSERT OR REPLACE INTO file_hashes
        (file_path, file_hash, file_size, last_indexed, chunk_count)
      VALUES
        (:file_path, :file_hash, :file_size, :last_indexed, :chunk_count)
    ''';
    FQuery.ParamByName('file_path').AsString := AFilePath;
    FQuery.ParamByName('file_hash').AsString := AFileHash;
    FQuery.ParamByName('file_size').AsInteger := FileSize;
    FQuery.ParamByName('last_indexed').AsString := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
    FQuery.ParamByName('chunk_count').AsInteger := AChunkCount;
    FQuery.ExecSQL;

  except
    on E: Exception do
      WriteLn(Format('Warning: Failed to store hash for %s: %s',
        [ExtractFileName(AFilePath), E.Message]));
  end;
end;

function TDatabaseBuilder.GetFolderLastIndexed(const AFolderPath: string): TDateTime;
begin
  FQuery.SQL.Text := 'SELECT indexed_at FROM indexed_folders WHERE folder_path = :folder_path';
  FQuery.ParamByName('folder_path').AsString := AFolderPath;
  FQuery.Open;

  try
    if not FQuery.Eof then
      Result := FQuery.FieldByName('indexed_at').AsDateTime
    else
      Result := 0;  // Folder never indexed
  finally
    FQuery.Close;
  end;
end;

procedure TDatabaseBuilder.ValidateMetadata(const AOllamaModel: string;
  AEmbeddingDimensions: Integer);
begin
  // Skip validation if embeddings are disabled (FTS5-only mode)
  // Dimensions = 0 means no embeddings are being generated
  if (AOllamaModel = '') or (AEmbeddingDimensions = 0) then
  begin
    WriteLn('Skipping metadata validation (FTS5-only mode)');
    WriteLn('');
    Exit;
  end;

  WriteLn('Validating database metadata...');

  // Check embedding model
  FQuery.SQL.Text := 'SELECT value FROM metadata WHERE key = ''embedding_model''';
  FQuery.Open;
  if not FQuery.Eof then
  begin
    var ExistingModel := FQuery.FieldByName('value').AsString;
    if ExistingModel <> AOllamaModel then
    begin
      FQuery.Close;
      raise Exception.CreateFmt(
        'ERROR: Database was created with model "%s" but you''re using "%s". ' + sLineBreak +
        'Vector embeddings are incompatible. ' + sLineBreak +
        'Either: 1) Use the same model, or 2) Delete database and start fresh, or 3) Use --force flag',
        [ExistingModel, AOllamaModel]);
    end;
    WriteLn(Format('  Model validation passed: %s', [ExistingModel]));
  end;
  FQuery.Close;

  // Check embedding dimensions
  FQuery.SQL.Text := 'SELECT value FROM metadata WHERE key = ''embedding_dimensions''';
  FQuery.Open;
  if not FQuery.Eof then
  begin
    var ExistingDimensions := StrToInt(FQuery.FieldByName('value').AsString);
    if ExistingDimensions <> AEmbeddingDimensions then
    begin
      FQuery.Close;
      raise Exception.CreateFmt(
        'ERROR: Database has %d dimensions but you''re using %d. ' + sLineBreak +
        'Vector embeddings are incompatible. ' + sLineBreak +
        'Either: 1) Use the same model, or 2) Delete database and start fresh, or 3) Use --force flag',
        [ExistingDimensions, AEmbeddingDimensions]);
    end;
    WriteLn(Format('  Dimensions validation passed: %d', [ExistingDimensions]));
  end;
  FQuery.Close;

  WriteLn('Metadata validation completed successfully');
  WriteLn('');
end;

function TDatabaseBuilder.GroupChunksByFile(AChunks: TCodeChunkList): TDictionary<string, TList<TCodeChunk>>;
var
  Chunk: TCodeChunk;
  FileList: TList<TCodeChunk>;
begin
  Result := TDictionary<string, TList<TCodeChunk>>.Create;

  for Chunk in AChunks do
  begin
    if not Result.TryGetValue(Chunk.FileName, FileList) then
    begin
      FileList := TList<TCodeChunk>.Create;
      Result.Add(Chunk.FileName, FileList);
    end;
    FileList.Add(Chunk);
  end;
end;

procedure TDatabaseBuilder.InsertSymbol(const AName, AFullName, AType, AFilePath,
  AContent: string; const AComments, AParentClass: string; AEmbedding: TEmbedding;
  const AFramework, APlatforms, ADelphiVersion: string);
var
  SymbolID: Integer;
begin
  try
    // Insert symbol with documentation fields
    FQuery.SQL.Text := '''
      INSERT OR REPLACE INTO symbols (
        name, full_name, type, file_path, content, comments,
        parent_class, content_type, source_category,
        framework, platforms, delphi_version
      ) VALUES (
        :name, :full_name, :type, :file_path, :content, :comments,
        :parent_class, :content_type, :source_category,
        :framework, :platforms, :delphi_version
      )
    ''';

    // Truncate name fields to avoid FireDAC "Too long identifier" error (> 255)
    FQuery.ParamByName('name').AsString := Copy(AName, 1, 255);
    FQuery.ParamByName('full_name').AsString := Copy(AFullName, 1, 500);
    FQuery.ParamByName('type').AsString := AType;
    FQuery.ParamByName('file_path').AsString := AFilePath;
    // Use AsWideMemo for large text fields to avoid FireDAC identifier length issues
    FQuery.ParamByName('content').AsWideMemo := AContent;
    FQuery.ParamByName('comments').AsWideMemo := AComments;
    FQuery.ParamByName('parent_class').AsString := AParentClass;
    FQuery.ParamByName('content_type').AsString := FContentType;
    FQuery.ParamByName('source_category').AsString := FSourceCategory;

    // Documentation fields
    // FireDAC requires DataType before Clear for nullable parameters
    if AFramework <> '' then
      FQuery.ParamByName('framework').AsString := AFramework
    else
    begin
      FQuery.ParamByName('framework').DataType := ftString;
      FQuery.ParamByName('framework').Clear;
    end;

    if APlatforms <> '' then
      FQuery.ParamByName('platforms').AsString := APlatforms
    else
    begin
      FQuery.ParamByName('platforms').DataType := ftString;
      FQuery.ParamByName('platforms').Clear;
    end;

    if ADelphiVersion <> '' then
      FQuery.ParamByName('delphi_version').AsString := ADelphiVersion
    else
    begin
      FQuery.ParamByName('delphi_version').DataType := ftString;
      FQuery.ParamByName('delphi_version').Clear;
    end;

    FQuery.ExecSQL;

    // Get the symbol ID
    FQuery.SQL.Text := 'SELECT last_insert_rowid() as id';
    FQuery.Open;
    SymbolID := FQuery.FieldByName('id').AsInteger;
    FQuery.Close;

    // Insert embedding into vec0 virtual table
    if Assigned(AEmbedding) then
    begin
      var ActualDimensions := Length(AEmbedding.Vector);

      // Build vector JSON with dimension padding/truncation if needed
      var USFormat: TFormatSettings := TFormatSettings.Create('en-US');
      var VectorJSON: string := '[';
      for var I := 0 to FEmbeddingDimensions - 1 do
      begin
        if I > 0 then
          VectorJSON := VectorJSON + ',';

        if I < ActualDimensions then
          VectorJSON := VectorJSON + FloatToStr(AEmbedding.Vector[I], USFormat)
        else
          VectorJSON := VectorJSON + '0.0';
      end;
      VectorJSON := VectorJSON + ']';

      if ActualDimensions <> FEmbeddingDimensions then
        WriteLn(Format('Warning: Vector dimension mismatch for "%s": expected %d, got %d',
          [AName, FEmbeddingDimensions, ActualDimensions]));

      // Use parameterized query to avoid SQL too long / identifier issues with large vectors
      FQuery.SQL.Text := 'INSERT INTO vec_embeddings (symbol_id, chunk_id, embedding_text, embedding) ' +
                         'VALUES (:symbol_id, :chunk_id, :embedding_text, :embedding)';
      FQuery.ParamByName('symbol_id').AsInteger := SymbolID;
      FQuery.ParamByName('chunk_id').AsString := AEmbedding.ChunkID;
      FQuery.ParamByName('embedding_text').AsString := AEmbedding.Text;
      FQuery.ParamByName('embedding').AsString := VectorJSON;
      FQuery.ExecSQL;
    end;

  except
    on E: Exception do
      raise Exception.CreateFmt('Failed to insert symbol "%s": %s', [AName, E.Message]);
  end;
end;

procedure TDatabaseBuilder.InsertSymbol(AChunk: TCodeChunk; AEmbedding: TEmbedding);
var
  SymbolID: Integer;
  ChunkTypeStr: string;
  Framework: string;
  UnitName: string;
  FTSContent: string;
  IsMethodType: Boolean;
begin
  try
    // Convert chunk type to string
    case AChunk.ChunkType of
      ctClass: ChunkTypeStr := 'class';
      ctInterface: ChunkTypeStr := 'interface';
      ctRecord: ChunkTypeStr := 'record';
      ctProcedure: ChunkTypeStr := 'procedure';
      ctFunction: ChunkTypeStr := 'function';
      ctConstructor: ChunkTypeStr := 'constructor';
      ctDestructor: ChunkTypeStr := 'destructor';
      ctProperty: ChunkTypeStr := 'property';
      ctType: ChunkTypeStr := 'type';
      ctConst: ChunkTypeStr := 'const';
      ctVar: ChunkTypeStr := 'var';
    else
      ChunkTypeStr := 'unknown';
    end;

    // For FTS indexing: use only signature for method implementations
    // This keeps FTS index small and focused on declarations
    // Full content is preserved in EnrichedText for embeddings
    IsMethodType := AChunk.ChunkType in [ctFunction, ctProcedure, ctConstructor, ctDestructor];
    if IsMethodType and (not AChunk.IsDeclaration) then
      FTSContent := ExtractMethodSignature(AChunk.Content)
    else
      FTSContent := AChunk.Content;

    // Use explicit framework if provided, otherwise auto-detect
    if FExplicitFramework <> '' then
      Framework := FExplicitFramework
    else
    begin
      UnitName := ChangeFileExt(ExtractFileName(AChunk.FileName), '');
      Framework := DetectFramework(AChunk.FileName, UnitName);
    end;

    // Insert symbol
    FQuery.SQL.Text := '''
      INSERT OR REPLACE INTO symbols (
        name, full_name, type, file_path, content, enriched_text,
        spanish_terms, domain_tags, comments,
        parent_class, implemented_interfaces, visibility,
        start_line, end_line, content_type, source_category, framework
      ) VALUES (
        :name, :full_name, :type, :file_path, :content, :enriched_text,
        :spanish_terms, :domain_tags, :comments,
        :parent_class, :implemented_interfaces, :visibility,
        :start_line, :end_line, :content_type, :source_category, :framework
      )
    ''';

    FQuery.ParamByName('name').AsString := AChunk.Name;
    FQuery.ParamByName('full_name').AsString := AChunk.FullName;
    FQuery.ParamByName('type').AsString := ChunkTypeStr;
    FQuery.ParamByName('file_path').AsString := AChunk.FileName;
    FQuery.ParamByName('content').AsString := FTSContent;  // Use signature for implementations
    FQuery.ParamByName('enriched_text').AsString := AChunk.EnrichedText;
    FQuery.ParamByName('spanish_terms').AsString := AChunk.SpanishTerms;
    FQuery.ParamByName('domain_tags').AsString := AChunk.DomainTags;
    FQuery.ParamByName('comments').AsString := AChunk.Comments;
    FQuery.ParamByName('parent_class').AsString := AChunk.ParentClass;
    FQuery.ParamByName('implemented_interfaces').AsString := AChunk.ImplementedInterfaces.CommaText;
    FQuery.ParamByName('visibility').AsString := AChunk.Visibility;
    FQuery.ParamByName('start_line').AsInteger := AChunk.StartLine;
    FQuery.ParamByName('end_line').AsInteger := AChunk.EndLine;
    FQuery.ParamByName('content_type').AsString := FContentType;
    FQuery.ParamByName('source_category').AsString := FSourceCategory;

    // Set framework (can be empty string for non-framework code)
    // FireDAC requires DataType before Clear for nullable parameters
    if Framework <> '' then
      FQuery.ParamByName('framework').AsString := Framework
    else
    begin
      FQuery.ParamByName('framework').DataType := ftString;
      FQuery.ParamByName('framework').Clear;
    end;

    FQuery.ExecSQL;
    
    // Get the symbol ID
    FQuery.SQL.Text := 'SELECT last_insert_rowid() as id';
    FQuery.Open;
    SymbolID := FQuery.FieldByName('id').AsInteger;
    FQuery.Close;
    
    // Insert embedding into vec0 virtual table
    if Assigned(AEmbedding) then
    begin
      var ActualDimensions := Length(AEmbedding.Vector);

      // Build vector JSON with dimension padding/truncation if needed
      // IMPORTANT: Use US format settings to ensure period as decimal separator
      var USFormat: TFormatSettings := TFormatSettings.Create('en-US');
      var VectorJSON: string := '[';
      for var I := 0 to FEmbeddingDimensions - 1 do
      begin
        if I > 0 then
          VectorJSON := VectorJSON + ',';

        // Use actual value if available, otherwise pad with 0.0
        // CRITICAL: Use FloatToStr with USFormat to force period as decimal separator
        if I < ActualDimensions then
          VectorJSON := VectorJSON + FloatToStr(AEmbedding.Vector[I], USFormat)
        else
          VectorJSON := VectorJSON + '0.0';
      end;
      VectorJSON := VectorJSON + ']';

      // Log dimension mismatch warning
      if ActualDimensions <> FEmbeddingDimensions then
        WriteLn(Format('Warning: Vector dimension mismatch for "%s": expected %d, got %d (padding applied)',
          [AChunk.Name, FEmbeddingDimensions, ActualDimensions]));

      // Use parameterized query to avoid SQL too long / identifier issues with large vectors
      FQuery.SQL.Text := 'INSERT INTO vec_embeddings (symbol_id, chunk_id, embedding_text, embedding) ' +
                         'VALUES (:symbol_id, :chunk_id, :embedding_text, :embedding)';
      FQuery.ParamByName('symbol_id').AsInteger := SymbolID;
      FQuery.ParamByName('chunk_id').AsString := AEmbedding.ChunkID;
      FQuery.ParamByName('embedding_text').AsString := AEmbedding.Text;
      FQuery.ParamByName('embedding').AsString := VectorJSON;
      FQuery.ExecSQL;
    end;
    
  except
    on E: Exception do
      raise Exception.CreateFmt('Failed to insert symbol "%s": %s', [AChunk.Name, E.Message]);
  end;
end;

function TDatabaseBuilder.FilterFilesToProcess(const AFileList: TStringList;
  const AFolderPath: string; const ADatabaseFile: string;
  AForceFullReindex: Boolean): TStringList;
var
  I: Integer;
  FilePath: string;
  FolderLastIndexed: TDateTime;
  StoredHash: string;
  StoredChunkCount: Integer;
  FileStatus: TFileStatus;
begin
  Result := TStringList.Create;

  // Connect to database to check file status
  if not FileExists(ADatabaseFile) then
  begin
    // Database doesn't exist - all files are new
    Result.AddStrings(AFileList);
    Exit;
  end;

  try
    // Initialize connection if not already done
    if not FConnection.Connected then
    begin
      TDatabaseConnectionHelper.ConfigureConnection(FConnection, ADatabaseFile, False);
      FConnection.Open;

      // Enable WAL mode for concurrent access
      FQuery.SQL.Text := 'PRAGMA journal_mode=WAL';
      FQuery.ExecSQL;
    end;

    // Get folder's last index date
    FolderLastIndexed := GetFolderLastIndexed(AFolderPath);

    // Check each file
    for I := 0 to AFileList.Count - 1 do
    begin
      FilePath := AFileList[I];

      if AForceFullReindex then
        FileStatus := fsModified
      else
        FileStatus := CheckFileStatus(FilePath, FolderLastIndexed,
          StoredHash, StoredChunkCount);

      // Only include new or modified files
      if (FileStatus = fsNew) or (FileStatus = fsModified) then
        Result.Add(FilePath);
    end;

    WriteLn(Format('Pre-filter: %d files need processing (out of %d scanned)',
      [Result.Count, AFileList.Count]));

  except
    on E: Exception do
    begin
      WriteLn('Warning: Could not pre-filter files - processing all: ' + E.Message);
      Result.AddStrings(AFileList);  // Fallback: process everything
    end;
  end;
end;

procedure TDatabaseBuilder.BuildDatabase(AChunks: TCodeChunkList; AEmbeddings: TEmbeddingList;
  const ADatabaseFile: string; const AOllamaURL, AOllamaModel: string;
  AEmbeddingDimensions: Integer; const AFolderPath: string;
  AForceFullReindex: Boolean; const AContentType, ASourceCategory: string;
  const AExplicitFramework: string = ''; const AMappingFile: string = '';
  ABatchSize: Integer = 50);
var
  FolderLastIndexed: TDateTime;
  ScannedFiles: TStringList;
  ChunksByFile: TDictionary<string, TList<TCodeChunk>>;
  NewCount, ModifiedCount, UnchangedCount, SkippedCount, DeletedCount: Integer;
  ProcessedSymbols: Integer;
  StartTime, EndTime: TDateTime;
  FilePath: string;
  FileChunks: TList<TCodeChunk>;
  StoredHash: string;
  StoredChunkCount: Integer;
  FileStatus: TFileStatus;
  ChunkCount: Integer;
  FileHash: string;
  Chunk: TCodeChunk;
  Embedding: TEmbedding;
  DbFilePath: string;
  // Batch commit variables
  TotalBatches: Integer;
  CurrentBatch: Integer;
  FilesInBatch: Integer;
  TotalFilesToProcess: Integer;
  FileKeys: TArray<string>;
begin
  try
    StartTime := Now;
    EndTime := StartTime;  // Initialize to avoid warning; will be updated at end

    // Store configuration for later use
    FEmbeddingDimensions := AEmbeddingDimensions;
    FOllamaURL := AOllamaURL;
    FOllamaModel := AOllamaModel;
    FContentType := AContentType;
    FSourceCategory := ASourceCategory;
    FExplicitFramework := AExplicitFramework;
    FMappingFile := AMappingFile;
    FDatabaseFile := ADatabaseFile;

    if AEmbeddings.Count > 0 then
      WriteLn(Format('Building database with %d chunks and %d embeddings...', [AChunks.Count, AEmbeddings.Count]))
    else
      WriteLn(Format('Building database with %d chunks (no vector embeddings)...', [AChunks.Count]));

    // Check if database exists - preserve for incremental updates
    if FileExists(ADatabaseFile) then
      WriteLn('Database exists - performing incremental update')
    else
      WriteLn('Creating new database');

    // Create connection and tables (load vec0 only if embeddings enabled)
    CreateConnection(ADatabaseFile, AEmbeddingDimensions > 0);
    CreateTables;

    // Initialize framework detector (if not using explicit framework)
    // Only initialize if database existed before - avoids database lock when creating new DB
    if (FExplicitFramework = '') and not Assigned(FFrameworkDetector) and FDatabaseExisted then
    begin
      WriteLn('Initializing framework detector...');
      if FMappingFile <> '' then
        WriteLn(Format('  Using mapping file: %s', [FMappingFile]));
      // Use shared connection to avoid "schema locked" error
      FFrameworkDetector := TFrameworkDetector.Create(FConnection, FMappingFile);
    end;

    // Migrate schema to support documentation fields (if needed)
    MigrateToDocSchema;

    // Validate metadata compatibility for existing databases
    if FileExists(ADatabaseFile) and not AForceFullReindex then
      ValidateMetadata(AOllamaModel, AEmbeddingDimensions);

    // Store/update metadata (last_update is set at the end when indexing completes)
    StoreMetadata('schema_version', '1');
    if AEmbeddingDimensions > 0 then
    begin
      StoreMetadata('embedding_model', AOllamaModel);
      StoreMetadata('embedding_dimensions', IntToStr(AEmbeddingDimensions));
      StoreMetadata('ollama_url', AOllamaURL);
      WriteLn(Format('Stored metadata: model=%s, dimensions=%d', [AOllamaModel, AEmbeddingDimensions]));
    end
    else
      WriteLn('FTS5-only mode: no embedding metadata stored');

    // Get folder's last indexing timestamp
    FolderLastIndexed := GetFolderLastIndexed(AFolderPath);
    if FolderLastIndexed > 0 then
      WriteLn(Format('Folder last indexed: %s',
        [FormatDateTime('yyyy-mm-dd hh:nn:ss', FolderLastIndexed)]))
    else
      WriteLn('First-time indexing of this folder');

    if AForceFullReindex then
      WriteLn('FORCE MODE: All files will be reindexed regardless of changes');

    WriteLn('');

    // Group chunks by file
    ChunksByFile := GroupChunksByFile(AChunks);
    ScannedFiles := TStringList.Create;
    ScannedFiles.Sorted := True;

    try
      // Build list of scanned files
      for FilePath in ChunksByFile.Keys do
        ScannedFiles.Add(FilePath);

      WriteLn(Format('Processing %d files (batch size: %d)...', [ScannedFiles.Count, ABatchSize]));
      WriteLn('');

      // Initialize counters
      NewCount := 0;
      DeletedCount := 0;
      ProcessedSymbols := 0;
      ModifiedCount := 0;
      UnchangedCount := 0;
      SkippedCount := 0;
      FilesInBatch := 0;
      CurrentBatch := 1;

      // Get file keys as array for indexed access
      FileKeys := ChunksByFile.Keys.ToArray;
      TotalFilesToProcess := Length(FileKeys);
      TotalBatches := (TotalFilesToProcess + ABatchSize - 1) div ABatchSize;
      if TotalBatches = 0 then
        TotalBatches := 1;

      // Begin first transaction
      FConnection.StartTransaction;

      try
        // Process each file
        for var FileIndex := 0 to TotalFilesToProcess - 1 do
        begin
          FilePath := FileKeys[FileIndex];

          // Check file status (timestamp + hash)
          if AForceFullReindex then
            FileStatus := fsModified  // Force treats all as modified
          else
            FileStatus := CheckFileStatus(FilePath, FolderLastIndexed,
              StoredHash, StoredChunkCount);

          case FileStatus of
            fsSkippedByTimestamp:
            begin
              WriteLn(Format('  [SKIP/OLD] %s', [ExtractFileName(FilePath)]));
              Inc(SkippedCount);
              Continue;  // Skip this file entirely (doesn't count toward batch)
            end;

            fsUnchanged:
            begin
              WriteLn(Format('  [SKIP/SAME] %s', [ExtractFileName(FilePath)]));
              Inc(UnchangedCount);
              Continue;  // Skip this file (doesn't count toward batch)
            end;

            fsModified:
            begin
              WriteLn(Format('  [MODIFIED] %s (was %d chunks)',
                [ExtractFileName(FilePath), StoredChunkCount]));
              RemoveFileData(FilePath);  // Delete old symbols
              Inc(ModifiedCount);
            end;

            fsNew:
            begin
              WriteLn(Format('  [NEW] %s', [ExtractFileName(FilePath)]));
              Inc(NewCount);
            end;
          end;

          // Index the file (new or modified)
          FileChunks := ChunksByFile[FilePath];
          ChunkCount := 0;
          FileHash := CalculateFileHash(FilePath);

          for Chunk in FileChunks do
          begin
            // Find corresponding embedding by unique GUID
            Embedding := nil;
            for var I := 0 to AEmbeddings.Count - 1 do
            begin
              if AEmbeddings[I].ChunkID = Chunk.ChunkGUID then
              begin
                Embedding := AEmbeddings[I];
                Break;
              end;
            end;

            InsertSymbol(Chunk, Embedding);
            Inc(ChunkCount);
            Inc(ProcessedSymbols);
          end;

          // Store file hash
          StoreFileHash(FilePath, FileHash, ChunkCount);
          Inc(FilesInBatch);

          // Check if we should commit this batch
          var IsLastFile := (FileIndex = TotalFilesToProcess - 1);

          if (FilesInBatch >= ABatchSize) or IsLastFile then
          begin
            // If this is the last batch, also handle deleted files and indexing
            if IsLastFile then
            begin
              WriteLn('');
              WriteLn('Checking for deleted files...');

              // Detect deleted files (in DB but not scanned)
              FQuery.SQL.Text := 'SELECT file_path FROM file_hashes WHERE file_path LIKE :folder_pattern';
              FQuery.ParamByName('folder_pattern').AsString := AFolderPath + '%';
              FQuery.Open;

              while not FQuery.Eof do
              begin
                DbFilePath := FQuery.FieldByName('file_path').AsString;
                if ScannedFiles.IndexOf(DbFilePath) = -1 then
                begin
                  WriteLn(Format('  [DELETED] %s', [ExtractFileName(DbFilePath)]));
                  RemoveFileData(DbFilePath);
                  Inc(DeletedCount);
                end;
                FQuery.Next;
              end;
              FQuery.Close;

              // Create indexes (will update FTS incrementally)
              CreateIndexes;

              // Record folder indexing statistics INSIDE transaction
              EndTime := Now;
              StoreMetadata('last_update', FormatDateTime('yyyy-mm-dd hh:nn:ss', EndTime));
              RecordFolderIndexing(AFolderPath, StartTime, EndTime,
                NewCount + ModifiedCount, ProcessedSymbols);
            end;

            // Commit current batch
            FConnection.Commit;

            if IsLastFile then
              WriteLn(Format('*** FINAL COMMIT: Batch %d/%d complete (%d files, %d symbols total) ***',
                [CurrentBatch, TotalBatches, FilesInBatch, ProcessedSymbols]))
            else
              WriteLn(Format('*** CHECKPOINT: Batch %d/%d committed (%d files, %d symbols so far) ***',
                [CurrentBatch, TotalBatches, FilesInBatch, ProcessedSymbols]));

            // Start new transaction for next batch (if not last)
            if not IsLastFile then
            begin
              FConnection.StartTransaction;
              Inc(CurrentBatch);
              FilesInBatch := 0;
            end;
          end;
        end;

        // Handle edge case: no files were actually processed (all skipped)
        if FilesInBatch = 0 then
        begin
          // Still need to check for deleted files and update indexes
          WriteLn('');
          WriteLn('Checking for deleted files...');

          FQuery.SQL.Text := 'SELECT file_path FROM file_hashes WHERE file_path LIKE :folder_pattern';
          FQuery.ParamByName('folder_pattern').AsString := AFolderPath + '%';
          FQuery.Open;

          while not FQuery.Eof do
          begin
            DbFilePath := FQuery.FieldByName('file_path').AsString;
            if ScannedFiles.IndexOf(DbFilePath) = -1 then
            begin
              WriteLn(Format('  [DELETED] %s', [ExtractFileName(DbFilePath)]));
              RemoveFileData(DbFilePath);
              Inc(DeletedCount);
            end;
            FQuery.Next;
          end;
          FQuery.Close;

          CreateIndexes;

          EndTime := Now;
          StoreMetadata('last_update', FormatDateTime('yyyy-mm-dd hh:nn:ss', EndTime));
          RecordFolderIndexing(AFolderPath, StartTime, EndTime, 0, 0);

          FConnection.Commit;
          WriteLn('*** COMMIT: No files to process, deleted files checked ***');
        end;

      except
        on E: Exception do
        begin
          FConnection.Rollback;
          raise Exception.CreateFmt('Database transaction failed (batch %d): %s', [CurrentBatch, E.Message]);
        end;
      end;

    finally
      // Free the dictionary and all lists
      for FileChunks in ChunksByFile.Values do
        FileChunks.Free;
      ChunksByFile.Free;
      ScannedFiles.Free;
    end;

    // Analyze database for optimal query performance
    FQuery.SQL.Text := 'ANALYZE';
    FQuery.ExecSQL;

    WriteLn('');
    WriteLn('======================================');
    WriteLn('=== Indexing Summary ===');
    WriteLn('======================================');
    WriteLn(Format('New files:           %4d', [NewCount]));
    WriteLn(Format('Modified files:      %4d', [ModifiedCount]));
    WriteLn(Format('Unchanged files:     %4d', [UnchangedCount]));
    WriteLn(Format('Skipped (old):       %4d', [SkippedCount]));
    WriteLn(Format('Deleted files:       %4d', [DeletedCount]));
    WriteLn('--------------------------------------');
    WriteLn(Format('Total symbols:       %4d', [ProcessedSymbols]));
    WriteLn(Format('Duration:            %s', [FormatDateTime('nn:ss', EndTime - StartTime)]));
    WriteLn('======================================');

  except
    on E: Exception do
      raise Exception.CreateFmt('Failed to build database: %s', [E.Message]);
  end;
end;

procedure TDatabaseBuilder.InvalidateQueryCache;
begin
  try
    if not FConnection.Connected then
      Exit;

    WriteLn('Invalidating query cache...');

    FQuery.SQL.Text := 'UPDATE query_log SET cache_valid = 0';
    FQuery.ExecSQL;

    WriteLn(Format('Cache invalidated (%d entries marked invalid)', [FQuery.RowsAffected]));

  except
    on E: Exception do
      WriteLn(Format('Warning: Failed to invalidate cache: %s', [E.Message]));
  end;
end;

procedure TDatabaseBuilder.CleanupQueryLogs(ADaysToKeep: Integer);
var
  DeletedCount: Integer;
begin
  try
    if not FConnection.Connected then
      Exit;

    WriteLn(Format('Cleaning up query logs older than %d days...', [ADaysToKeep]));

    // Delete old invalid cache entries
    FQuery.SQL.Text :=
      'DELETE FROM query_log ' +
      'WHERE cache_valid = 0 ' +
      '  AND executed_at < datetime(''now'', :days_ago)';
    FQuery.ParamByName('days_ago').AsString := Format('-%d days', [ADaysToKeep]);
    FQuery.ExecSQL;
    DeletedCount := FQuery.RowsAffected;

    WriteLn(Format('Deleted %d old query log entries', [DeletedCount]));

    // Compact database if we deleted a lot
    if DeletedCount > 1000 then
    begin
      WriteLn('Compacting database...');
      FQuery.SQL.Text := 'VACUUM';
      FQuery.ExecSQL;
      WriteLn('Database compacted');
    end;

  except
    on E: Exception do
      WriteLn(Format('Warning: Failed to cleanup query logs: %s', [E.Message]));
  end;
end;

function TDatabaseBuilder.ColumnExists(const ATable, AColumn: string): Boolean;
begin
  Result := False;
  FQuery.SQL.Text := Format('PRAGMA table_info(%s)', [ATable]);
  FQuery.Open;
  try
    while not FQuery.Eof do
    begin
      if SameText(FQuery.FieldByName('name').AsString, AColumn) then
      begin
        Result := True;
        Break;
      end;
      FQuery.Next;
    end;
  finally
    FQuery.Close;
  end;
end;

procedure TDatabaseBuilder.MigrateToDocSchema;
begin
  // Check if columns already exist
  if not ColumnExists('symbols', 'framework') then
  begin
    WriteLn('Migrating database schema for documentation support...');

    FQuery.SQL.Text := 'ALTER TABLE symbols ADD COLUMN framework TEXT';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'ALTER TABLE symbols ADD COLUMN platforms TEXT';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'ALTER TABLE symbols ADD COLUMN delphi_version TEXT';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'ALTER TABLE symbols ADD COLUMN introduced_version TEXT';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'ALTER TABLE symbols ADD COLUMN deprecated_version TEXT';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'ALTER TABLE symbols ADD COLUMN is_inherited INTEGER DEFAULT 0';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'ALTER TABLE symbols ADD COLUMN inherited_from TEXT';
    FQuery.ExecSQL;

    WriteLn('Schema migration complete');
  end
  else
    WriteLn('Schema already migrated (framework column exists)');
end;

function TDatabaseBuilder.DetectFramework(const AFilePath, AUnitName: string): string;
begin
  // Use new multi-tier framework detector if available
  if Assigned(FFrameworkDetector) then
  begin
    Result := FFrameworkDetector.DetectFramework(AFilePath, False); // Don't validate to avoid warnings during indexing
  end
  else
  begin
    // Fallback to simple detection (for backward compatibility)
    Result := '';

    // Strategy 1: Detect from unit name prefix
    if Pos('.', AUnitName) > 0 then
    begin
      var UnitPrefix := Copy(AUnitName, 1, Pos('.', AUnitName) - 1);

      if SameText(UnitPrefix, 'Vcl') then
        Exit('VCL')
      else if SameText(UnitPrefix, 'FMX') then
        Exit('FMX')
      else if SameText(UnitPrefix, 'System') then
        Exit('RTL')
      else if SameText(UnitPrefix, 'Data') then
        Exit('RTL');
    end;

    // Strategy 2: Detect from folder path
    var LowerPath := LowerCase(AFilePath);

    if (Pos('\vcl\', LowerPath) > 0) or (Pos('/vcl/', LowerPath) > 0) then
      Exit('VCL')
    else if (Pos('\fmx\', LowerPath) > 0) or (Pos('/fmx/', LowerPath) > 0) then
      Exit('FMX')
    else if (Pos('\rtl\', LowerPath) > 0) or (Pos('/rtl/', LowerPath) > 0) then
      Exit('RTL')
    else if (Pos('\source\data\', LowerPath) > 0) or (Pos('/source/data/', LowerPath) > 0) then
      Exit('RTL')
    else if (Pos('\source\soap\', LowerPath) > 0) or (Pos('/source/soap/', LowerPath) > 0) then
      Exit('RTL')
    else if (Pos('\source\xml\', LowerPath) > 0) or (Pos('/source/xml/', LowerPath) > 0) then
      Exit('RTL');
  end;
end;

{ Hybrid Pipeline Methods }

procedure TDatabaseBuilder.InitializeForHybrid(const ADatabaseFile: string;
  const AOllamaURL, AOllamaModel: string; AEmbeddingDimensions: Integer;
  const AContentType, ASourceCategory: string;
  const AExplicitFramework: string; const AMappingFile: string);
begin
  // Store configuration
  FOllamaURL := AOllamaURL;
  FOllamaModel := AOllamaModel;
  FEmbeddingDimensions := AEmbeddingDimensions;
  FContentType := AContentType;
  FSourceCategory := ASourceCategory;
  FExplicitFramework := AExplicitFramework;
  FMappingFile := AMappingFile;

  // Create connection and tables (load vec0 only if embeddings enabled)
  CreateConnection(ADatabaseFile, AEmbeddingDimensions > 0);
  CreateTables;
  CreateIndexes;

  // Validate metadata (model compatibility)
  ValidateMetadata(AOllamaModel, AEmbeddingDimensions);

  // Store metadata
  StoreMetadata('schema_version', '1');
  if AEmbeddingDimensions > 0 then
  begin
    StoreMetadata('embedding_model', AOllamaModel);
    StoreMetadata('embedding_dimensions', IntToStr(AEmbeddingDimensions));
    StoreMetadata('ollama_url', AOllamaURL);
    WriteLn(Format('  Stored metadata: model=%s, dimensions=%d', [AOllamaModel, AEmbeddingDimensions]));
  end
  else
    WriteLn('  FTS5-only mode: no embedding metadata stored');

  // Initialize framework detector if needed
  if (FExplicitFramework = '') and not Assigned(FFrameworkDetector) and FDatabaseExisted then
  begin
    WriteLn('  Initializing framework detector...');
    if FMappingFile <> '' then
      WriteLn(Format('  Using mapping file: %s', [FMappingFile]));
    FFrameworkDetector := TFrameworkDetector.Create(FConnection, FMappingFile);
  end;

  // Start initial transaction
  FConnection.StartTransaction;
  WriteLn('  Hybrid mode initialized, transaction started');
end;

procedure TDatabaseBuilder.InsertChunkBatch(AChunks: TCodeChunkList; AEmbeddings: TEmbeddingList);
const
  PROGRESS_INTERVAL = 100;  // Update every 100 chunks for smooth progress
var
  Chunk: TCodeChunk;
  Embedding: TEmbedding;
  EmbeddingMap: TDictionary<string, TEmbedding>;
  I: Integer;
  TotalChunks: Integer;
  ShowProgress: Boolean;
begin
  TotalChunks := AChunks.Count;
  ShowProgress := TotalChunks >= PROGRESS_INTERVAL;

  // Build embedding lookup map O(n) instead of O(n²) linear search
  EmbeddingMap := TDictionary<string, TEmbedding>.Create;
  try
    for var Emb in AEmbeddings do
      EmbeddingMap.AddOrSetValue(Emb.ChunkID, Emb);

    for I := 0 to TotalChunks - 1 do
    begin
      Chunk := AChunks[I];

      // Find corresponding embedding by GUID - O(1) lookup
      if not EmbeddingMap.TryGetValue(Chunk.ChunkGUID, Embedding) then
        Embedding := nil;

      // Insert the symbol
      InsertSymbol(Chunk, Embedding);

      // Progress feedback - overwrite same line with #13
      if ShowProgress and ((I + 1) mod PROGRESS_INTERVAL = 0) then
      begin
        Write(Format(#13'  [DB] Saving chunks: %d/%d (%.1f%%)   ',
          [I + 1, TotalChunks, (I + 1) * 100.0 / TotalChunks]));
        Flush(Output);
      end;
    end;
  finally
    EmbeddingMap.Free;
  end;

  // Final message - complete the line
  if ShowProgress then
    WriteLn(Format(#13'  [DB] Saved %d chunks to database.      ', [TotalChunks]));
end;

procedure TDatabaseBuilder.CheckpointFile(const AFilePath: string; AChunkCount: Integer);
var
  FileHash: string;
begin
  // Calculate and store file hash
  FileHash := CalculateFileHash(AFilePath);
  StoreFileHash(AFilePath, FileHash, AChunkCount);
end;

procedure TDatabaseBuilder.CommitAndContinue;
begin
  // Commit current transaction
  FConnection.Commit;

  // Checkpoint WAL to keep file size manageable
  FQuery.SQL.Text := 'PRAGMA wal_checkpoint(PASSIVE)';
  FQuery.ExecSQL;

  // Start new transaction
  FConnection.StartTransaction;
end;

procedure TDatabaseBuilder.FinalizeFolder(const AFolderPath: string;
  AFilesCount, AChunksCount: Integer; AStartTime: TDateTime; AFilesModified: Integer);
var
  EndTime: TDateTime;
begin
  EndTime := Now;

  // Update last_update timestamp (when indexing completed, not started)
  StoreMetadata('last_update', FormatDateTime('yyyy-mm-dd hh:nn:ss', EndTime));

  // Record folder indexing
  RecordFolderIndexing(AFolderPath, AStartTime, EndTime, AFilesCount, AChunksCount);

  // Only invalidate query cache if files were actually modified
  if AFilesModified > 0 then
    InvalidateQueryCache
  else
    WriteLn('  No files modified - query cache preserved');

  // Final commit
  FConnection.Commit;

  // Final WAL checkpoint
  FQuery.SQL.Text := 'PRAGMA wal_checkpoint(TRUNCATE)';
  FQuery.ExecSQL;

  WriteLn(Format('  Folder finalized: %d files, %d chunks', [AFilesCount, AChunksCount]));
end;

end.