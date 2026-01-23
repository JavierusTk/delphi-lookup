unit uDatabaseBuilder;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  System.DateUtils,
  System.SyncObjs,
  Data.DB,
  FireDAC.Comp.Client,
  uConfig,
  uASTProcessor,
  uIndexerEmbeddings.Ollama,
  uFrameworkDetector,
  uDatabaseConnection;

type
  TFileStatus = (fsNew, fsModified, fsUnchanged, fsSkippedByTimestamp);

  /// <summary>
  /// Parsed symbol result from parallel workers (REQ-008).
  /// This is a lightweight record that can be safely passed across threads.
  /// The embedding is stored as raw float array to avoid object lifetime issues.
  /// </summary>
  TParsedSymbol = record
    // Core identification
    ChunkGUID: string;           // Unique GUID for this chunk (for embedding matching)
    Name: string;
    FullName: string;
    SymbolType: string;          // 'class', 'function', 'procedure', etc.
    FilePath: string;

    // Content
    Content: string;             // Source code content (may be signature-only for FTS)
    EnrichedText: string;        // Full content for embeddings
    Comments: string;

    // Metadata
    ParentClass: string;
    ImplementedInterfaces: string;  // Comma-separated list
    Visibility: string;
    StartLine: Integer;
    EndLine: Integer;
    IsDeclaration: Boolean;

    // Classification
    ContentType: string;         // 'code', 'help', 'markdown'
    SourceCategory: string;      // 'user', 'stdlib', 'third_party'
    Framework: string;           // 'VCL', 'FMX', 'RTL', ''

    // Embedding (stored as array, not object)
    HasEmbedding: Boolean;
    EmbeddingVector: TArray<Single>;
    EmbeddingText: string;
  end;

  TParsedSymbolArray = TArray<TParsedSymbol>;

  /// <summary>
  /// Thread-safe queue for passing parsed symbols from workers to database writer.
  /// Uses a simple lock-based approach for producer-consumer pattern.
  /// </summary>
  TSymbolQueue = class
  private
    FQueue: TList<TParsedSymbol>;
    FLock: TCriticalSection;
    FClosed: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Add a symbol to the queue (thread-safe)</summary>
    procedure Enqueue(const ASymbol: TParsedSymbol);

    /// <summary>Add multiple symbols to the queue (thread-safe, more efficient)</summary>
    procedure EnqueueBatch(const ASymbols: TParsedSymbolArray);

    /// <summary>Get up to AMaxCount symbols from the queue (thread-safe).
    /// Returns empty array if queue is empty.</summary>
    function DequeueBatch(AMaxCount: Integer): TParsedSymbolArray;

    /// <summary>Returns current queue count (thread-safe)</summary>
    function Count: Integer;

    /// <summary>Signal that no more items will be added</summary>
    procedure Close;

    /// <summary>Check if queue is closed and empty</summary>
    function IsFinished: Boolean;
  end;

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
    FDeferFTS5: Boolean;        // REQ-007: Defer FTS5 population for full reindex

    procedure CreateConnection(const ADatabaseFile: string; ALoadVec0: Boolean = True);
    procedure CreateTables;
    procedure InsertSymbol(AChunk: TCodeChunk; AEmbedding: TEmbedding); overload;
    procedure CreateIndexes;
    function DetectFramework(const AFilePath, AUnitName: string): string;
    procedure StoreMetadata(const AKey, AValue: string);
    procedure RecordFolderIndexing(const AFolderPath: string;
      AStartTime, AEndTime: TDateTime; AFilesCount, AChunksCount: Integer;
      const AFolderHash: string = ''; const ASubfoldersList: string = '[]');

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

    // REQ-007: FTS5 management for deferred population
    procedure InsertSymbolToFTS5(ASymbolID: Integer; const AName, AFullName, AContent, AComments: string);
    procedure PopulateFTS5Bulk;
    procedure DropFTS5Table;
    procedure RecreateFTS5Table;

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

    /// <summary>Check if a table exists in the database</summary>
    function TableExists(const ATable: string): Boolean;

    /// <summary>Migrate query_log data to query_cache table</summary>
    procedure MigrateQueryLogToCache;

    /// <summary>Migrate schema to support indexed_files table (REQ-001)</summary>
    procedure MigrateToIndexedFilesSchema;

    /// <summary>Migrate indexed_folders schema for Merkle-tree support (REQ-002)</summary>
    procedure MigrateToFolderHierarchySchema;

    /// <summary>Insert symbol with documentation fields (for CHM indexing)</summary>
    procedure InsertSymbol(const AName, AFullName, AType, AFilePath, AContent: string;
      const AComments, AParentClass: string; AEmbedding: TEmbedding;
      const AFramework, APlatforms, ADelphiVersion: string); overload;

    // === REQ-008: Parallel Results Queue Consumer ===

    /// <summary>Insert a batch of parsed symbols (from parallel workers).
    /// Uses a single transaction for the entire batch. Thread-safe when called from main thread.</summary>
    /// <param name="ASymbols">Array of parsed symbols with optional embeddings</param>
    procedure InsertSymbolsBatch(const ASymbols: TParsedSymbolArray);

    /// <summary>Consume symbols from queue until empty or closed.
    /// Processes in batches of ABatchSize symbols each.
    /// Call this from the main thread to write to database.</summary>
    /// <param name="AQueue">Thread-safe queue to consume from</param>
    /// <param name="ABatchSize">Number of symbols per batch (default: 100)</param>
    /// <param name="AOnProgress">Optional progress callback (symbols processed, total time ms)</param>
    procedure ConsumeFromQueue(AQueue: TSymbolQueue; ABatchSize: Integer = 100;
      AOnProgress: TProc<Integer, Int64> = nil);

    /// <summary>Default batch size for queue consumption</summary>
    class var DefaultBatchSize: Integer;
  end;

implementation

uses
  System.Math,
  System.IOUtils,
  System.Hash,
  System.StrUtils,
  System.Diagnostics,
  System.JSON,
  FireDAC.Stan.Param,
  uChangeDetector;

{ TSymbolQueue }

constructor TSymbolQueue.Create;
begin
  inherited Create;
  FQueue := TList<TParsedSymbol>.Create;
  FLock := TCriticalSection.Create;
  FClosed := False;
end;

destructor TSymbolQueue.Destroy;
begin
  FLock.Free;
  FQueue.Free;
  inherited Destroy;
end;

procedure TSymbolQueue.Enqueue(const ASymbol: TParsedSymbol);
begin
  FLock.Enter;
  try
    if not FClosed then
      FQueue.Add(ASymbol);
  finally
    FLock.Leave;
  end;
end;

procedure TSymbolQueue.EnqueueBatch(const ASymbols: TParsedSymbolArray);
var
  I: Integer;
begin
  FLock.Enter;
  try
    if not FClosed then
    begin
      for I := 0 to High(ASymbols) do
        FQueue.Add(ASymbols[I]);
    end;
  finally
    FLock.Leave;
  end;
end;

function TSymbolQueue.DequeueBatch(AMaxCount: Integer): TParsedSymbolArray;
var
  ActualCount, I: Integer;
begin
  FLock.Enter;
  try
    ActualCount := Min(AMaxCount, FQueue.Count);
    SetLength(Result, ActualCount);

    for I := 0 to ActualCount - 1 do
      Result[I] := FQueue[I];

    // Remove dequeued items
    FQueue.DeleteRange(0, ActualCount);
  finally
    FLock.Leave;
  end;
end;

function TSymbolQueue.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FQueue.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TSymbolQueue.Close;
begin
  FLock.Enter;
  try
    FClosed := True;
  finally
    FLock.Leave;
  end;
end;

function TSymbolQueue.IsFinished: Boolean;
begin
  FLock.Enter;
  try
    Result := FClosed and (FQueue.Count = 0);
  finally
    FLock.Leave;
  end;
end;

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

    // Reduce fsync calls for better performance (safe with WAL)
    FQuery.SQL.Text := 'PRAGMA synchronous=NORMAL';
    FQuery.ExecSQL;

    // Increase page cache to 64MB for better performance (negative = KB)
    FQuery.SQL.Text := 'PRAGMA cache_size=-64000';
    FQuery.ExecSQL;

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
    // Includes Merkle-tree support fields for hierarchical change detection (REQ-002)
    FQuery.SQL.Text := '''
      CREATE TABLE IF NOT EXISTS indexed_folders (
        folder_path TEXT PRIMARY KEY,
        indexed_at TIMESTAMP NOT NULL,
        duration_seconds INTEGER NOT NULL,
        files_count INTEGER NOT NULL,
        chunks_count INTEGER NOT NULL,
        content_type TEXT NOT NULL DEFAULT 'code',
        source_category TEXT NOT NULL DEFAULT 'user',
        folder_hash TEXT DEFAULT '',
        parent_folder_path TEXT,
        subfolders_list TEXT DEFAULT '[]',
        FOREIGN KEY (parent_folder_path) REFERENCES indexed_folders(folder_path) ON DELETE SET NULL
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
        content_hash TEXT,
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
    
    // Create query logging table for usage analysis
    FQuery.SQL.Text := '''
      CREATE TABLE IF NOT EXISTS query_log (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        executed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        query_text TEXT NOT NULL,
        query_hash TEXT NOT NULL,

        -- Cache fields (legacy - use query_cache instead)
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

    // Create query cache table (one row per unique query, tracks popularity)
    FQuery.SQL.Text := '''
      CREATE TABLE IF NOT EXISTS query_cache (
        query_hash TEXT PRIMARY KEY,
        query_text TEXT NOT NULL,
        result_ids TEXT,
        result_count INTEGER NOT NULL,
        cache_valid INTEGER DEFAULT 1,
        hit_count INTEGER DEFAULT 1,
        first_seen TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        last_seen TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        avg_duration_ms INTEGER
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

    // Create FTS5 virtual table for full-text search (external content table)
    // This is created once with IF NOT EXISTS - no rebuild needed on incremental indexing
    // FTS5 triggers sync content automatically when symbols table is modified
    try
      FQuery.SQL.Text := '''
        CREATE VIRTUAL TABLE IF NOT EXISTS symbols_fts USING fts5(
          name, full_name, content, comments,
          content='symbols', content_rowid='id'
        )
      ''';
      FQuery.ExecSQL;
      WriteLn('FTS5 virtual table created (or already exists)');
    except
      on E: Exception do
      begin
        WriteLn('Warning: FTS5 not available, full-text search will be disabled');
        WriteLn(Format('FTS5 error: %s', [E.Message]));
      end;
    end;

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

    // Note: FTS5 virtual table is created in CreateTables with IF NOT EXISTS
    // FTS5 content sync is handled by triggers when symbols are inserted/updated/deleted

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

    // Symbol content hash index for cache validation
    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_symbols_id_hash ON symbols(id, content_hash)';
    FQuery.ExecSQL;

    // Query cache indexes
    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_query_cache_valid ON query_cache(cache_valid)';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_query_cache_hits ON query_cache(hit_count DESC)';
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
  AStartTime, AEndTime: TDateTime; AFilesCount, AChunksCount: Integer;
  const AFolderHash: string; const ASubfoldersList: string);
var
  DurationSeconds: Integer;
begin
  DurationSeconds := SecondsBetween(AEndTime, AStartTime);

  FQuery.SQL.Text := '''
    INSERT OR REPLACE INTO indexed_folders
    (folder_path, indexed_at, duration_seconds, files_count, chunks_count,
     content_type, source_category, folder_hash, subfolders_list)
    VALUES
    (:folder_path, :indexed_at, :duration_seconds, :files_count, :chunks_count,
     :content_type, :source_category, :folder_hash, :subfolders_list)
  ''';
  FQuery.ParamByName('folder_path').AsString := AFolderPath;
  FQuery.ParamByName('indexed_at').AsString := FormatDateTime('yyyy-mm-dd hh:nn:ss', AEndTime);
  FQuery.ParamByName('duration_seconds').AsInteger := DurationSeconds;
  FQuery.ParamByName('files_count').AsInteger := AFilesCount;
  FQuery.ParamByName('chunks_count').AsInteger := AChunksCount;
  FQuery.ParamByName('content_type').AsString := FContentType;
  FQuery.ParamByName('source_category').AsString := FSourceCategory;
  FQuery.ParamByName('folder_hash').AsString := AFolderHash;
  FQuery.ParamByName('subfolders_list').AsString := ASubfoldersList;
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

    // Remove FTS entries BEFORE symbols (subquery needs symbols table)
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

    // Remove symbols (after FTS and embeddings are cleaned up)
    FQuery.SQL.Text := 'DELETE FROM symbols WHERE file_path = :file_path';
    FQuery.ParamByName('file_path').AsString := AFilePath;
    FQuery.ExecSQL;

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

{ REQ-007: FTS5 Management Methods }

procedure TDatabaseBuilder.InsertSymbolToFTS5(ASymbolID: Integer;
  const AName, AFullName, AContent, AComments: string);
begin
  // Skip if FTS5 population is deferred (will be done in bulk at end)
  if FDeferFTS5 then
    Exit;

  try
    FQuery.SQL.Text := '''
      INSERT INTO symbols_fts(rowid, name, full_name, content, comments)
      VALUES (:rowid, :name, :full_name, :content, :comments)
    ''';
    FQuery.ParamByName('rowid').AsInteger := ASymbolID;
    FQuery.ParamByName('name').AsString := AName;
    FQuery.ParamByName('full_name').AsString := AFullName;
    FQuery.ParamByName('content').AsWideMemo := AContent;
    FQuery.ParamByName('comments').AsWideMemo := AComments;
    FQuery.ExecSQL;
  except
    // FTS5 might not be available - ignore errors
  end;
end;

procedure TDatabaseBuilder.PopulateFTS5Bulk;
var
  Stopwatch: TStopwatch;
  RowCount: Integer;
begin
  WriteLn('Populating FTS5 index in bulk...');
  Stopwatch := TStopwatch.StartNew;

  try
    // Count rows to insert
    FQuery.SQL.Text := 'SELECT COUNT(*) as cnt FROM symbols';
    FQuery.Open;
    RowCount := FQuery.FieldByName('cnt').AsInteger;
    FQuery.Close;

    // Bulk insert all symbols into FTS5
    FQuery.SQL.Text := '''
      INSERT INTO symbols_fts(rowid, name, full_name, content, comments)
      SELECT id, name, full_name, content, comments FROM symbols
    ''';
    FQuery.ExecSQL;

    Stopwatch.Stop;
    WriteLn(Format('FTS5 index populated: %d rows in %dms',
      [RowCount, Stopwatch.ElapsedMilliseconds]));
  except
    on E: Exception do
      WriteLn(Format('Warning: FTS5 bulk population failed: %s', [E.Message]));
  end;
end;

procedure TDatabaseBuilder.DropFTS5Table;
begin
  try
    FQuery.SQL.Text := 'DROP TABLE IF EXISTS symbols_fts';
    FQuery.ExecSQL;
    WriteLn('Dropped existing FTS5 table for full reindex');
  except
    on E: Exception do
      WriteLn(Format('Warning: Could not drop FTS5 table: %s', [E.Message]));
  end;
end;

procedure TDatabaseBuilder.RecreateFTS5Table;
begin
  try
    FQuery.SQL.Text := '''
      CREATE VIRTUAL TABLE IF NOT EXISTS symbols_fts USING fts5(
        name, full_name, content, comments,
        content='symbols', content_rowid='id'
      )
    ''';
    FQuery.ExecSQL;
    WriteLn('Recreated FTS5 table');
  except
    on E: Exception do
      WriteLn(Format('Warning: FTS5 not available: %s', [E.Message]));
  end;
end;

procedure TDatabaseBuilder.InsertSymbol(const AName, AFullName, AType, AFilePath,
  AContent: string; const AComments, AParentClass: string; AEmbedding: TEmbedding;
  const AFramework, APlatforms, ADelphiVersion: string);
var
  SymbolID: Integer;
  ContentHash: string;
begin
  try
    // Calculate content hash for cache validation
    ContentHash := THashMD5.GetHashString(AContent);

    // Insert symbol with documentation fields
    FQuery.SQL.Text := '''
      INSERT OR REPLACE INTO symbols (
        name, full_name, type, file_path, content, comments,
        parent_class, content_type, source_category,
        framework, platforms, delphi_version, content_hash
      ) VALUES (
        :name, :full_name, :type, :file_path, :content, :comments,
        :parent_class, :content_type, :source_category,
        :framework, :platforms, :delphi_version, :content_hash
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

    // Set content hash for cache validation
    FQuery.ParamByName('content_hash').AsString := ContentHash;

    FQuery.ExecSQL;

    // Get the symbol ID
    FQuery.SQL.Text := 'SELECT last_insert_rowid() as id';
    FQuery.Open;
    SymbolID := FQuery.FieldByName('id').AsInteger;
    FQuery.Close;

    // REQ-007: Insert into FTS5 (unless deferred for bulk population)
    InsertSymbolToFTS5(SymbolID, AName, AFullName, AContent, AComments);

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
  ContentHash: string;
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

    // Calculate content hash for cache validation (MD5 of full content)
    ContentHash := THashMD5.GetHashString(AChunk.Content);

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
        start_line, end_line, content_type, source_category, framework, content_hash
      ) VALUES (
        :name, :full_name, :type, :file_path, :content, :enriched_text,
        :spanish_terms, :domain_tags, :comments,
        :parent_class, :implemented_interfaces, :visibility,
        :start_line, :end_line, :content_type, :source_category, :framework, :content_hash
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

    // Set content hash for cache validation
    FQuery.ParamByName('content_hash').AsString := ContentHash;

    FQuery.ExecSQL;

    // Get the symbol ID
    FQuery.SQL.Text := 'SELECT last_insert_rowid() as id';
    FQuery.Open;
    SymbolID := FQuery.FieldByName('id').AsInteger;
    FQuery.Close;

    // REQ-007: Insert into FTS5 (unless deferred for bulk population)
    InsertSymbolToFTS5(SymbolID, AChunk.Name, AChunk.FullName, FTSContent, AChunk.Comments);

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

    // REQ-007: For full reindex, defer FTS5 population until end
    // REQ-008: Use larger batch size for full reindex
    if AForceFullReindex then
    begin
      FDeferFTS5 := True;
      DropFTS5Table;  // Will be recreated after bulk population
      RecreateFTS5Table;  // Create empty FTS5 table structure
      // Override batch size for full reindex (REQ-008)
      if ABatchSize < 500 then
        ABatchSize := 500;
      WriteLn(Format('Full reindex mode: FTS5 deferred, batch size = %d', [ABatchSize]));
    end
    else
    begin
      FDeferFTS5 := False;
      // For incremental indexing, use smaller batch size for better checkpointing
      if ABatchSize > 100 then
        ABatchSize := 100;
    end;

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

              // Calculate folder hash for incremental indexing
              var ChangeDetector := TChangeDetector.Create(FConnection);
              try
                var FolderHash := ChangeDetector.CalculateFolderHashForStorage(AFolderPath);
                var SubfoldersList := ChangeDetector.GetSubfolderNames(AFolderPath);
                RecordFolderIndexing(AFolderPath, StartTime, EndTime,
                  NewCount + ModifiedCount, ProcessedSymbols, FolderHash, SubfoldersList);
              finally
                ChangeDetector.Free;
              end;
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

          // Calculate folder hash even for empty runs (to detect future changes)
          var ChangeDetector := TChangeDetector.Create(FConnection);
          try
            var FolderHash := ChangeDetector.CalculateFolderHashForStorage(AFolderPath);
            var SubfoldersList := ChangeDetector.GetSubfolderNames(AFolderPath);
            RecordFolderIndexing(AFolderPath, StartTime, EndTime, 0, 0, FolderHash, SubfoldersList);
          finally
            ChangeDetector.Free;
          end;

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

    // REQ-007: Populate FTS5 in bulk if it was deferred (full reindex mode)
    if FDeferFTS5 then
    begin
      PopulateFTS5Bulk;
      FDeferFTS5 := False;  // Reset for next indexing operation
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
var
  CacheInvalidated, LogInvalidated: Integer;
begin
  try
    if not FConnection.Connected then
      Exit;

    WriteLn('Invalidating query cache...');

    // Invalidate query_cache (new table)
    FQuery.SQL.Text := 'UPDATE query_cache SET cache_valid = 0';
    FQuery.ExecSQL;
    CacheInvalidated := FQuery.RowsAffected;

    // Also invalidate query_log for backwards compatibility
    FQuery.SQL.Text := 'UPDATE query_log SET cache_valid = 0';
    FQuery.ExecSQL;
    LogInvalidated := FQuery.RowsAffected;

    WriteLn(Format('Cache invalidated (%d query_cache + %d query_log entries)',
      [CacheInvalidated, LogInvalidated]));

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

function TDatabaseBuilder.TableExists(const ATable: string): Boolean;
begin
  FQuery.SQL.Text := 'SELECT name FROM sqlite_master WHERE type=''table'' AND name=:tablename';
  FQuery.ParamByName('tablename').AsString := ATable;
  FQuery.Open;
  try
    Result := not FQuery.Eof;
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

  // Migrate content_hash column (for cache validation)
  if not ColumnExists('symbols', 'content_hash') then
  begin
    WriteLn('Adding content_hash column to symbols table...');
    FQuery.SQL.Text := 'ALTER TABLE symbols ADD COLUMN content_hash TEXT';
    FQuery.ExecSQL;
    WriteLn('content_hash column added');
  end;

  // Create query_cache table if it doesn't exist
  FQuery.SQL.Text := '''
    CREATE TABLE IF NOT EXISTS query_cache (
      query_hash TEXT PRIMARY KEY,
      query_text TEXT NOT NULL,
      result_ids TEXT,
      result_count INTEGER NOT NULL,
      cache_valid INTEGER DEFAULT 1,
      hit_count INTEGER DEFAULT 1,
      first_seen TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      last_seen TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      avg_duration_ms INTEGER
    )
  ''';
  FQuery.ExecSQL;

  // Create query_cache indexes
  FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_query_cache_valid ON query_cache(cache_valid)';
  FQuery.ExecSQL;
  FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_query_cache_hits ON query_cache(hit_count DESC)';
  FQuery.ExecSQL;

  // Migrate existing query_log data to query_cache (one-time migration)
  MigrateQueryLogToCache;

  // REQ-001: Migrate to indexed_files table for incremental indexing
  MigrateToIndexedFilesSchema;

  // REQ-002: Migrate indexed_folders for Merkle-tree hierarchical change detection
  MigrateToFolderHierarchySchema;
end;

procedure TDatabaseBuilder.MigrateQueryLogToCache;
var
  MigratedCount: Integer;
begin
  try
    // Check if query_cache is empty and query_log has data
    FQuery.SQL.Text := 'SELECT COUNT(*) as cnt FROM query_cache';
    FQuery.Open;
    var CacheCount := FQuery.FieldByName('cnt').AsInteger;
    FQuery.Close;

    if CacheCount > 0 then
    begin
      WriteLn('query_cache already has data, skipping migration');
      Exit;
    end;

    // Check if query_log has data to migrate
    FQuery.SQL.Text := 'SELECT COUNT(*) as cnt FROM query_log WHERE cache_valid = 1';
    FQuery.Open;
    var LogCount := FQuery.FieldByName('cnt').AsInteger;
    FQuery.Close;

    if LogCount = 0 then
    begin
      WriteLn('No query_log data to migrate');
      Exit;
    end;

    WriteLn(Format('Migrating %d query_log entries to query_cache...', [LogCount]));

    // Migrate data: group by query_hash, count hits, get most recent result_ids
    FQuery.SQL.Text := '''
      INSERT INTO query_cache (query_hash, query_text, result_ids, result_count, cache_valid, hit_count, first_seen, last_seen, avg_duration_ms)
      SELECT
        query_hash,
        MAX(query_text) as query_text,
        (SELECT result_ids FROM query_log ql2 WHERE ql2.query_hash = query_log.query_hash ORDER BY executed_at DESC LIMIT 1) as result_ids,
        MAX(result_count) as result_count,
        1 as cache_valid,
        COUNT(*) as hit_count,
        MIN(executed_at) as first_seen,
        MAX(executed_at) as last_seen,
        AVG(duration_ms) as avg_duration_ms
      FROM query_log
      WHERE cache_valid = 1
      GROUP BY query_hash
    ''';
    FQuery.ExecSQL;
    MigratedCount := FQuery.RowsAffected;

    WriteLn(Format('Migrated %d unique queries to query_cache', [MigratedCount]));

  except
    on E: Exception do
    begin
      WriteLn(Format('Migration failed: %s - clearing both tables and starting fresh', [E.Message]));
      try
        FQuery.SQL.Text := 'DELETE FROM query_cache';
        FQuery.ExecSQL;
        FQuery.SQL.Text := 'DELETE FROM query_log';
        FQuery.ExecSQL;
        WriteLn('Cache tables cleared - will rebuild on next use');
      except
        // Ignore cleanup errors
      end;
    end;
  end;
end;

procedure TDatabaseBuilder.MigrateToIndexedFilesSchema;
var
  MigratedCount: Integer;
begin
  // REQ-001: Create indexed_files table for individual file tracking
  // This table enables efficient incremental indexing by tracking file-level changes
  if not TableExists('indexed_files') then
  begin
    WriteLn('Creating indexed_files table (REQ-001)...');

    // Create the indexed_files table
    // - file_path: Absolute path to the file (PRIMARY KEY)
    // - folder_path: Reference to indexed_folders.folder_path (FK)
    // - file_hash: MD5 hash of file content for change detection
    // - last_modified: Unix timestamp of filesystem last modified time
    // - indexed_at: Unix timestamp when file was indexed
    // - chunks_count: Number of symbols extracted from this file
    FQuery.SQL.Text := '''
      CREATE TABLE indexed_files (
        file_path TEXT PRIMARY KEY,
        folder_path TEXT NOT NULL,
        file_hash TEXT NOT NULL,
        last_modified INTEGER NOT NULL,
        indexed_at INTEGER NOT NULL,
        chunks_count INTEGER NOT NULL DEFAULT 0,
        FOREIGN KEY (folder_path) REFERENCES indexed_folders(folder_path) ON DELETE CASCADE
      )
    ''';
    FQuery.ExecSQL;

    // Create index on folder_path for efficient folder queries
    FQuery.SQL.Text := 'CREATE INDEX idx_indexed_files_folder ON indexed_files(folder_path)';
    FQuery.ExecSQL;

    // Create index on file_hash for change detection queries
    FQuery.SQL.Text := 'CREATE INDEX idx_indexed_files_hash ON indexed_files(file_hash)';
    FQuery.ExecSQL;

    // Create index on indexed_at for temporal queries
    FQuery.SQL.Text := 'CREATE INDEX idx_indexed_files_indexed_at ON indexed_files(indexed_at)';
    FQuery.ExecSQL;

    WriteLn('indexed_files table created with indexes');

    // Migrate data from file_hashes table if it exists
    if TableExists('file_hashes') then
    begin
      WriteLn('Migrating data from file_hashes to indexed_files...');

      // Migrate existing data, extracting folder_path from file_path
      // Note: last_modified is set to 0 for migrated records (will be updated on next index)
      FQuery.SQL.Text := '''
        INSERT INTO indexed_files (file_path, folder_path, file_hash, last_modified, indexed_at, chunks_count)
        SELECT
          fh.file_path,
          COALESCE(
            (SELECT folder_path FROM indexed_folders
             WHERE fh.file_path LIKE folder_path || '%'
             ORDER BY LENGTH(folder_path) DESC
             LIMIT 1),
            ''
          ) as folder_path,
          fh.file_hash,
          0 as last_modified,
          CAST(strftime('%s', fh.last_indexed) AS INTEGER) as indexed_at,
          fh.chunk_count
        FROM file_hashes fh
        WHERE fh.file_path NOT IN (SELECT file_path FROM indexed_files)
      ''';
      FQuery.ExecSQL;
      MigratedCount := FQuery.RowsAffected;

      WriteLn(Format('Migrated %d records from file_hashes to indexed_files', [MigratedCount]));
    end;
  end
  else
    WriteLn('indexed_files table already exists');
end;

procedure TDatabaseBuilder.MigrateToFolderHierarchySchema;
begin
  // REQ-002: Extend indexed_folders table for Merkle-tree hierarchical change detection
  // New columns:
  //   - folder_hash: MD5 hash of folder structure (sorted child names + file hashes)
  //   - parent_folder_path: FK to parent indexed_folders entry (nullable for root folders)
  //   - subfolders_list: JSON array of direct subfolder names for deletion detection

  // Add folder_hash column if it doesn't exist
  if not ColumnExists('indexed_folders', 'folder_hash') then
  begin
    WriteLn('Migrating indexed_folders schema for Merkle-tree support (REQ-002)...');

    // Add folder_hash column - stores MD5 hash of folder content structure
    // Empty string default for existing rows, will be populated on next index
    FQuery.SQL.Text := 'ALTER TABLE indexed_folders ADD COLUMN folder_hash TEXT DEFAULT ''''';
    FQuery.ExecSQL;
    WriteLn('  Added folder_hash column');

    // Add parent_folder_path column - references another indexed_folders entry
    // Nullable because root folders don't have a parent
    FQuery.SQL.Text := 'ALTER TABLE indexed_folders ADD COLUMN parent_folder_path TEXT';
    FQuery.ExecSQL;
    WriteLn('  Added parent_folder_path column');

    // Add subfolders_list column - JSON array of direct subfolder names
    // Used for fast deletion detection without filesystem scan
    // Format: ["subfolder1", "subfolder2", ...]
    FQuery.SQL.Text := 'ALTER TABLE indexed_folders ADD COLUMN subfolders_list TEXT DEFAULT ''[]''';
    FQuery.ExecSQL;
    WriteLn('  Added subfolders_list column');

    // Create index on parent_folder_path for hierarchical queries
    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_indexed_folders_parent ON indexed_folders(parent_folder_path)';
    FQuery.ExecSQL;
    WriteLn('  Created index on parent_folder_path');

    // Note: SQLite doesn't enforce FK constraints by default, and ALTER TABLE
    // cannot add FK constraints. The constraint is enforced at application level.
    // New databases get the FK from CreateTables.

    WriteLn('Folder hierarchy schema migration complete');
  end
  else
    WriteLn('Folder hierarchy schema already migrated (folder_hash column exists)');
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

  // Run migrations for existing databases (adds new columns, creates new tables)
  MigrateToDocSchema;

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
  ChangeDetector: TChangeDetector;
  FolderHash: string;
  SubfoldersList: string;
begin
  EndTime := Now;

  // REQ-007: Populate FTS5 in bulk if it was deferred
  if FDeferFTS5 then
  begin
    PopulateFTS5Bulk;
    FDeferFTS5 := False;  // Reset for next indexing operation
  end;

  // Calculate folder hash for incremental indexing (REQ-003)
  // This hash is stored and compared on next run to detect changes
  ChangeDetector := TChangeDetector.Create(FConnection);
  try
    FolderHash := ChangeDetector.CalculateFolderHashForStorage(AFolderPath);
    SubfoldersList := ChangeDetector.GetSubfolderNames(AFolderPath);
  finally
    ChangeDetector.Free;
  end;

  // Update last_update timestamp (when indexing completed, not started)
  StoreMetadata('last_update', FormatDateTime('yyyy-mm-dd hh:nn:ss', EndTime));

  // Record folder indexing with hash for change detection
  RecordFolderIndexing(AFolderPath, AStartTime, EndTime, AFilesCount, AChunksCount,
    FolderHash, SubfoldersList);

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

{ REQ-008: Parallel Results Queue Consumer }

procedure TDatabaseBuilder.InsertSymbolsBatch(const ASymbols: TParsedSymbolArray);
const
  PROGRESS_INTERVAL = 100;
var
  I: Integer;
  Symbol: TParsedSymbol;
  SymbolID: Integer;
  ContentHash: string;
  VectorJSON: string;
  USFormat: TFormatSettings;
  TotalSymbols: Integer;
  ShowProgress: Boolean;
begin
  TotalSymbols := Length(ASymbols);
  if TotalSymbols = 0 then
    Exit;

  ShowProgress := TotalSymbols >= PROGRESS_INTERVAL;
  USFormat := TFormatSettings.Create('en-US');

  for I := 0 to TotalSymbols - 1 do
  begin
    Symbol := ASymbols[I];

    try
      // Calculate content hash for cache validation
      ContentHash := THashMD5.GetHashString(Symbol.Content);

      // Insert symbol
      FQuery.SQL.Text := '''
        INSERT OR REPLACE INTO symbols (
          name, full_name, type, file_path, content, enriched_text,
          comments, parent_class, implemented_interfaces, visibility,
          start_line, end_line, content_type, source_category, framework, content_hash
        ) VALUES (
          :name, :full_name, :type, :file_path, :content, :enriched_text,
          :comments, :parent_class, :implemented_interfaces, :visibility,
          :start_line, :end_line, :content_type, :source_category, :framework, :content_hash
        )
      ''';

      FQuery.ParamByName('name').AsString := Copy(Symbol.Name, 1, 255);
      FQuery.ParamByName('full_name').AsString := Copy(Symbol.FullName, 1, 500);
      FQuery.ParamByName('type').AsString := Symbol.SymbolType;
      FQuery.ParamByName('file_path').AsString := Symbol.FilePath;
      FQuery.ParamByName('content').AsWideMemo := Symbol.Content;
      FQuery.ParamByName('enriched_text').AsWideMemo := Symbol.EnrichedText;
      FQuery.ParamByName('comments').AsWideMemo := Symbol.Comments;
      FQuery.ParamByName('parent_class').AsString := Symbol.ParentClass;
      FQuery.ParamByName('implemented_interfaces').AsString := Symbol.ImplementedInterfaces;
      FQuery.ParamByName('visibility').AsString := Symbol.Visibility;
      FQuery.ParamByName('start_line').AsInteger := Symbol.StartLine;
      FQuery.ParamByName('end_line').AsInteger := Symbol.EndLine;
      FQuery.ParamByName('content_type').AsString := Symbol.ContentType;
      FQuery.ParamByName('source_category').AsString := Symbol.SourceCategory;

      // Framework (nullable)
      if Symbol.Framework <> '' then
        FQuery.ParamByName('framework').AsString := Symbol.Framework
      else
      begin
        FQuery.ParamByName('framework').DataType := ftString;
        FQuery.ParamByName('framework').Clear;
      end;

      FQuery.ParamByName('content_hash').AsString := ContentHash;
      FQuery.ExecSQL;

      // Get the symbol ID
      FQuery.SQL.Text := 'SELECT last_insert_rowid() as id';
      FQuery.Open;
      SymbolID := FQuery.FieldByName('id').AsInteger;
      FQuery.Close;

      // REQ-007: Insert into FTS5 (unless deferred for bulk population)
      InsertSymbolToFTS5(SymbolID, Symbol.Name, Symbol.FullName, Symbol.Content, Symbol.Comments);

      // Insert embedding if present
      if Symbol.HasEmbedding and (Length(Symbol.EmbeddingVector) > 0) then
      begin
        // Build vector JSON
        VectorJSON := '[';
        for var J := 0 to High(Symbol.EmbeddingVector) do
        begin
          if J > 0 then
            VectorJSON := VectorJSON + ',';
          VectorJSON := VectorJSON + FloatToStr(Symbol.EmbeddingVector[J], USFormat);
        end;
        VectorJSON := VectorJSON + ']';

        FQuery.SQL.Text := 'INSERT INTO vec_embeddings (symbol_id, chunk_id, embedding_text, embedding) ' +
                           'VALUES (:symbol_id, :chunk_id, :embedding_text, :embedding)';
        FQuery.ParamByName('symbol_id').AsInteger := SymbolID;
        FQuery.ParamByName('chunk_id').AsString := Symbol.ChunkGUID;
        FQuery.ParamByName('embedding_text').AsString := Symbol.EmbeddingText;
        FQuery.ParamByName('embedding').AsString := VectorJSON;
        FQuery.ExecSQL;
      end;

      // Progress feedback
      if ShowProgress and ((I + 1) mod PROGRESS_INTERVAL = 0) then
      begin
        Write(Format(#13'  [DB] Batch insert: %d/%d (%.1f%%)   ',
          [I + 1, TotalSymbols, (I + 1) * 100.0 / TotalSymbols]));
        Flush(Output);
      end;

    except
      on E: Exception do
      begin
        WriteLn(Format(#13'  [ERROR] Failed to insert symbol "%s": %s   ',
          [Symbol.Name, E.Message]));
        // Continue with next symbol instead of failing entire batch
      end;
    end;
  end;

  // Final progress message
  if ShowProgress then
    WriteLn(Format(#13'  [DB] Batch complete: %d symbols inserted.      ', [TotalSymbols]));
end;

procedure TDatabaseBuilder.ConsumeFromQueue(AQueue: TSymbolQueue; ABatchSize: Integer;
  AOnProgress: TProc<Integer, Int64>);
var
  Batch: TParsedSymbolArray;
  TotalProcessed: Integer;
  BatchCount: Integer;
  Stopwatch: TStopwatch;
  InTransaction: Boolean;
begin
  if ABatchSize <= 0 then
    ABatchSize := 100;

  TotalProcessed := 0;
  BatchCount := 0;
  InTransaction := False;
  Stopwatch := TStopwatch.StartNew;

  WriteLn(Format('  [DB] Starting queue consumption (batch size: %d)', [ABatchSize]));

  try
    // Start transaction for batched writes
    if not FConnection.InTransaction then
    begin
      FConnection.StartTransaction;
      InTransaction := True;
    end;

    while True do
    begin
      // Get batch from queue
      Batch := AQueue.DequeueBatch(ABatchSize);

      if Length(Batch) = 0 then
      begin
        // Queue is empty - check if finished
        if AQueue.IsFinished then
          Break;

        // Queue not closed but empty - wait a bit
        Sleep(10);
        Continue;
      end;

      Inc(BatchCount);

      // Insert batch
      InsertSymbolsBatch(Batch);
      TotalProcessed := TotalProcessed + Length(Batch);

      // Commit periodically to prevent large transaction logs
      if (BatchCount mod 10 = 0) and InTransaction then
      begin
        FConnection.Commit;

        // Checkpoint WAL
        FQuery.SQL.Text := 'PRAGMA wal_checkpoint(PASSIVE)';
        FQuery.ExecSQL;

        FConnection.StartTransaction;

        WriteLn(Format('  >>> Checkpoint: %d symbols (%d batches, %dms)',
          [TotalProcessed, BatchCount, Stopwatch.ElapsedMilliseconds]));
      end;

      // Progress callback
      if Assigned(AOnProgress) then
        AOnProgress(TotalProcessed, Stopwatch.ElapsedMilliseconds);
    end;

    // Final commit
    if InTransaction and FConnection.InTransaction then
      FConnection.Commit;

    Stopwatch.Stop;

    WriteLn(Format('  [DB] Queue consumption complete: %d symbols in %d batches (%dms)',
      [TotalProcessed, BatchCount, Stopwatch.ElapsedMilliseconds]));

  except
    on E: Exception do
    begin
      // Rollback on error
      if InTransaction and FConnection.InTransaction then
        FConnection.Rollback;
      raise Exception.CreateFmt('Queue consumption failed after %d symbols: %s',
        [TotalProcessed, E.Message]);
    end;
  end;
end;

initialization
  TDatabaseBuilder.DefaultBatchSize := 100;

end.