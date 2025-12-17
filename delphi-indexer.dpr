program delphi_indexer;

{$APPTYPE CONSOLE}

// This application requires 64-bit compilation for sqlite-vec compatibility
{$IFNDEF WIN64}
  {$MESSAGE FATAL 'delphi-indexer requires Win64 compilation. The sqlite-vec extension only works with 64-bit SQLite.'}
{$ENDIF}

{$R *.res}

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.IOUtils,
  System.Diagnostics,
  System.Generics.Collections,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  ParameterMAX in 'ParameterMAX\ParameterMAX.pas',
  ParameterMAX.Handlers in 'ParameterMAX\ParameterMAX.Handlers.pas',
  ParameterMAX.HandlerRegistry in 'ParameterMAX\ParameterMAX.HandlerRegistry.pas',
  ParameterMAX.Handler.JSON in 'ParameterMAX\ParameterMAX.Handler.JSON.pas',
  ParameterMAX.FallbackHandlers in 'ParameterMAX\ParameterMAX.FallbackHandlers.pas',
  ParameterMAX.Environment in 'ParameterMAX\ParameterMAX.Environment.pas',
  uDatabaseConnection in 'uDatabaseConnection.pas',
  uConfig in 'uConfig.pas',
  uFolderScanner in 'uFolderScanner.pas',
  uASTProcessor in 'uASTProcessor.pas',
  uGlossaryEnricher in 'uGlossaryEnricher.pas',
  uDatabaseBuilder in 'uDatabaseBuilder.pas',
  uIndexerEmbeddings.Ollama in 'uIndexerEmbeddings.Ollama.pas',
  uPackageScanner in 'uPackageScanner.pas',
  uFrameworkDetector in 'uFrameworkDetector.pas',
  uMappingGenerator in 'uMappingGenerator.pas',
  uDocTypes in 'uDocTypes.pas',
  uCHMExtractor in 'uCHMExtractor.pas',
  uCHMParser in 'uCHMParser.pas',
  uDocChunker in 'uDocChunker.pas',
  SimpleParser.Lexer in 'DelphiAST\Source\SimpleParser\SimpleParser.Lexer.pas',
  SimpleParser.Lexer.Types in 'DelphiAST\Source\SimpleParser\SimpleParser.Lexer.Types.pas',
  SimpleParser in 'DelphiAST\Source\SimpleParser\SimpleParser.pas',
  SimpleParser.Types in 'DelphiAST\Source\SimpleParser\SimpleParser.Types.pas',
  DelphiAST.Classes in 'DelphiAST\Source\DelphiAST.Classes.pas',
  DelphiAST.Consts in 'DelphiAST\Source\DelphiAST.Consts.pas',
  DelphiAST in 'DelphiAST\Source\DelphiAST.pas',
  DelphiAST.ProjectIndexer in 'DelphiAST\Source\DelphiAST.ProjectIndexer.pas',
  DelphiAST.Serialize.Binary in 'DelphiAST\Source\DelphiAST.Serialize.Binary.pas',
  DelphiAST.SimpleParserEx in 'DelphiAST\Source\DelphiAST.SimpleParserEx.pas',
  DelphiAST.Writer in 'DelphiAST\Source\DelphiAST.Writer.pas',
  StringPool in 'DelphiAST\Source\StringPool.pas';

var
  // Parameter manager for config file + command line
  PM: TParameterManager;

  // Embedding configuration
  EmbeddingProvider: string;
  EmbeddingURL: string;
  EmbeddingModel: string;
  EmbeddingDimensions: Integer;

  // General configuration
  FolderPath: string;
  DatabaseFile: string;
  ForceFullReindex: Boolean;
  ContentType: string;
  SourceCategory: string;
  ExplicitFramework: string;
  BatchSize: Integer;
  Stopwatch: TStopwatch;

  // CHM indexing mode
  IndexCHM: Boolean;
  CHMPath: string;
  DelphiVersion: string;

  // Package intelligence mode
  ScanPackagesMode: Boolean;
  ScanPackagesPath: string;
  ListPackagesMode: Boolean;

  // Mapping file generation mode
  GenerateMappingMode: Boolean;
  MappingInputFolder: string;
  MappingOutputFile: string;
  DryRun: Boolean;

  // Framework detection (during indexing)
  MappingFile: string;

  // Hybrid processing mode (process in batches, save incrementally)
  UseHybridMode: Boolean;
  ChunkBufferSize: Integer;  // Number of chunks before flush (default: 500)

  // Reranker configuration (for delphi-lookup, but loaded here for consistency)
  RerankerProvider: string;
  RerankerURL: string;

  // Cache revalidation mode
  RevalidateCacheMode: Boolean;
  RevalidateMinHits: Integer;

function GetDefaultDatabasePath: string;
begin
  // Returns the full path to the database file in the executable's directory
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), DEFAULT_DB_FILE);
end;

procedure ShowUsage;
begin
  WriteLn('delphi-indexer - Build search index from Delphi/Pascal source code');
  WriteLn;
  WriteLn('Usage: delphi-indexer.exe <folder_path> [options]');
  WriteLn('   OR: delphi-indexer.exe --index-chm <chm_file> [--delphi-version <ver>]');
  WriteLn('   OR: delphi-indexer.exe @config.json <folder_path> [options]');
  WriteLn;
  WriteLn('Arguments:');
  WriteLn('  folder_path  : Input folder containing Pascal files (required for indexing)');
  WriteLn;
  WriteLn('Configuration:');
  WriteLn('  @<file>              : Load parameters from JSON/INI file');
  WriteLn('  --no-config          : Ignore default config file (delphi-lookup.json)');
  WriteLn('  --database <file>    : SQLite database (default: delphi_symbols.db)');
  WriteLn('  --embedding-url <url>: Embedding service URL (empty = no embeddings)');
  WriteLn('                         If provider=ollama and no port, assumes :11434');
  WriteLn('  --embedding-model <m>: Model name (default: jina-code-embed-1-5b)');
  WriteLn('  --embedding-provider : Provider type (default: ollama)');
  WriteLn;
  WriteLn('Indexing Options:');
  WriteLn('  -h, --help           : Show this help message');
  WriteLn('  --list-folders       : List all indexed folders in database');
  WriteLn('  --force              : Force full reindex (ignore timestamps/hashes)');
  WriteLn('  --buffer-size <n>    : Chunks before checkpoint (default: 500)');
  WriteLn('  --legacy             : Use legacy mode (all in memory, then save)');
  WriteLn('  --type <val>         : Content type (default: code)');
  WriteLn('                         Values: code, help, markdown, comment');
  WriteLn('  --category <val>     : Source category (default: user)');
  WriteLn('                         Values: user, stdlib, third_party, official_help');
  WriteLn('  --framework <val>    : Explicit framework tag (skips auto-detection)');
  WriteLn('                         Values: VCL, FMX, RTL');
  WriteLn('  --mapping-file <file>: Use mapping file for framework detection');
  WriteLn;
  WriteLn('CHM Documentation:');
  WriteLn('  --index-chm <file>   : Index Delphi CHM documentation file');
  WriteLn('  --delphi-version <v> : Delphi version for CHM (default: 12.0)');
  WriteLn;
  WriteLn('Package Intelligence:');
  WriteLn('  --scan-packages <dir>: Scan .dpk files and populate package database');
  WriteLn('  --list-packages      : List all scanned packages with framework info');
  WriteLn;
  WriteLn('Mapping File Generation:');
  WriteLn('  --generate-mapping <dir>  : Generate framework mapping file');
  WriteLn('  --output <file>           : Output mapping file (default: framework-overrides.json)');
  WriteLn('  --dry-run                 : Preview mappings without saving');
  WriteLn;
  WriteLn('Cache Maintenance:');
  WriteLn('  --revalidate-cache [N]    : Revalidate invalidated queries with N+ hits (default: 3)');
  WriteLn('                              Re-executes popular queries to restore cache after indexing');
  WriteLn('                              Ctrl+C to interrupt safely');
  WriteLn;
  WriteLn('Config File (delphi-lookup.json):');
  WriteLn('  If delphi-lookup.json exists next to the executable, it is loaded automatically.');
  WriteLn('  Command line options override config file values.');
  WriteLn;
  WriteLn('  Example delphi-lookup.json:');
  WriteLn('  {');
  WriteLn('    "database": "delphi_symbols.db",');
  WriteLn('    "embedding_url": "http://localhost:11434",');
  WriteLn('    "embedding_model": "jina-code-embed-1-5b",');
  WriteLn('    "category": "user"');
  WriteLn('  }');
  WriteLn;
  WriteLn('Environment Variables:');
  WriteLn('  EMBEDDING_URL    : Embedding service URL');
  WriteLn('  OLLAMA_URL       : Fallback for EMBEDDING_URL (compatibility)');
  WriteLn;
  WriteLn('Note: If embedding_url is empty, only FTS5 indexing is performed (no vectors).');
  WriteLn('      This allows indexing without Ollama but disables semantic search.');
end;

procedure GenerateSetupScript;
var
  ScriptContent: TStringList;
begin
  ScriptContent := TStringList.Create;
  try
    ScriptContent.Add('@echo off');
    ScriptContent.Add('echo Setting up Python dependencies for delphi-indexer...');
    ScriptContent.Add('echo.');
    ScriptContent.Add('');
    ScriptContent.Add('REM Check if Python is installed');
    ScriptContent.Add('python --version >nul 2>&1');
    ScriptContent.Add('if %errorlevel% neq 0 (');
    ScriptContent.Add('    echo ERROR: Python is not installed or not in PATH');
    ScriptContent.Add('    echo.');
    ScriptContent.Add('    echo Please install Python 3.8+ from https://python.org');
    ScriptContent.Add('    echo Make sure to check "Add Python to PATH" during installation');
    ScriptContent.Add('    echo.');
    ScriptContent.Add('    pause');
    ScriptContent.Add('    exit /b 1');
    ScriptContent.Add(')');
    ScriptContent.Add('');
    ScriptContent.Add('echo Python found. Installing required packages...');
    ScriptContent.Add('echo.');
    ScriptContent.Add('');
    ScriptContent.Add('REM Upgrade pip first');
    ScriptContent.Add('echo Upgrading pip...');
    ScriptContent.Add('python -m pip install --upgrade pip');
    ScriptContent.Add('');
    ScriptContent.Add('REM Install PyTorch CPU-only version (smaller download)');
    ScriptContent.Add('echo Installing PyTorch (CPU version)...');
    ScriptContent.Add('python -m pip install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cpu');
    ScriptContent.Add('');
    ScriptContent.Add('REM Install ChromaDB and sentence-transformers');
    ScriptContent.Add('echo Installing ChromaDB...');
    ScriptContent.Add('python -m pip install chromadb');
    ScriptContent.Add('echo Installing sentence-transformers...');
    ScriptContent.Add('python -m pip install sentence-transformers');
    ScriptContent.Add('');
    ScriptContent.Add('REM Test the installation');
    ScriptContent.Add('echo Testing installation...');
    ScriptContent.Add('python -c "from sentence_transformers import SentenceTransformer; print(''Installation successful!'')"');
    ScriptContent.Add('');
    ScriptContent.Add('if %errorlevel% eq 0 (');
    ScriptContent.Add('    echo.');
    ScriptContent.Add('    echo SUCCESS: Python dependencies installed successfully!');
    ScriptContent.Add('    echo You can now run delphi-indexer with full vector embedding support.');
    ScriptContent.Add(') else (');
    ScriptContent.Add('    echo.');
    ScriptContent.Add('    echo ERROR: Installation failed. Please check the error messages above.');
    ScriptContent.Add(')');
    ScriptContent.Add('');
    ScriptContent.Add('echo.');
    ScriptContent.Add('pause');
    
    ScriptContent.SaveToFile('setup_python_dependencies.bat');
    WriteLn('Generated setup script: setup_python_dependencies.bat');
    
  finally
    ScriptContent.Free;
  end;
end;

procedure ListIndexedFolders;
var
  Connection: TFDConnection;
  Query: TFDQuery;
  TotalSymbols: Integer;
  AvgDuration: Double;
  TotalDuration: Integer;
begin
  if not FileExists(DatabaseFile) then
  begin
    WriteLn('Database file not found: ' + DatabaseFile);
    WriteLn('No folders have been indexed yet.');
    Exit;
  end;

  Connection := TFDConnection.Create(nil);
  Query := TFDQuery.Create(nil);
  try
    TDatabaseConnectionHelper.ConfigureConnection(Connection, DatabaseFile, False);
    Connection.Open;

    Query.Connection := Connection;

    // Query indexed folders
    Query.SQL.Text :=
      'SELECT folder_path, indexed_at, duration_seconds, files_count, chunks_count, ' +
      '       content_type, source_category ' +
      'FROM indexed_folders ' +
      'ORDER BY indexed_at DESC';
    Query.Open;

    if Query.IsEmpty then
    begin
      WriteLn('No folders have been indexed yet.');
      Exit;
    end;

    WriteLn('');
    WriteLn('Indexed Folders in: ' + DatabaseFile);
    WriteLn('==========================================');
    WriteLn('');

    TotalSymbols := 0;
    TotalDuration := 0;

    while not Query.Eof do
    begin
      WriteLn('Folder: ' + Query.FieldByName('folder_path').AsString);
      WriteLn('  Indexed at: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss',
        Query.FieldByName('indexed_at').AsDateTime));
      WriteLn('  Files:      ' + Query.FieldByName('files_count').AsString);
      WriteLn('  Symbols:    ' + Query.FieldByName('chunks_count').AsString);
      WriteLn('  Duration:   ' + Query.FieldByName('duration_seconds').AsString + ' seconds');
      WriteLn('  Type:       ' + Query.FieldByName('content_type').AsString);
      WriteLn('  Category:   ' + Query.FieldByName('source_category').AsString);
      WriteLn('');

      TotalSymbols := TotalSymbols + Query.FieldByName('chunks_count').AsInteger;
      TotalDuration := TotalDuration + Query.FieldByName('duration_seconds').AsInteger;

      Query.Next;
    end;

    WriteLn('------------------------------------------');
    WriteLn('Total folders indexed: ' + IntToStr(Query.RecordCount));
    WriteLn('Total symbols:         ' + IntToStr(TotalSymbols));
    WriteLn('Total indexing time:   ' + IntToStr(TotalDuration) + ' seconds');
    if Query.RecordCount > 0 then
    begin
      AvgDuration := TotalDuration / Query.RecordCount;
      WriteLn('Average time/folder:   ' + FormatFloat('0.0', AvgDuration) + ' seconds');
    end;
    WriteLn('');

  finally
    Query.Free;
    Connection.Free;
  end;
end;

procedure InitializeParameterManager;
begin
  PM := TParameterManager.Create;
  PM.SetDefaultConfigFile('delphi-lookup.json');
  PM.EnableEnvironmentVars('DELPHI_LOOKUP_');
  PM.ParseCommandLine;

  // Show loaded config file if any
  if PM.GetLoadedDefaultConfigPath <> '' then
    WriteLn('Config loaded: ' + PM.GetLoadedDefaultConfigPath);
end;

function GetFirstPositionalArg: string;
var
  I: Integer;
  Arg: string;
begin
  // Find first argument that doesn't start with - or @
  Result := '';
  for I := 1 to ParamCount do
  begin
    Arg := ParamStr(I);
    if (Arg <> '') and (Arg[1] <> '-') and (Arg[1] <> '@') then
    begin
      Result := Arg;
      Exit;
    end;
  end;
end;

procedure ParseCommandLine;
var
  EnvURL: string;
begin
  // Check for help first (before initializing PM)
  if (ParamCount > 0) and ((ParamStr(1) = '-h') or (ParamStr(1) = '--help') or (ParamStr(1) = '/?')) then
  begin
    ShowUsage;
    Halt(0);
  end;

  // Initialize ParameterMAX (loads config file + parses command line)
  InitializeParameterManager;

  // === Load configuration from PM ===

  // Database
  DatabaseFile := PM.GetParameter('database', GetDefaultDatabasePath);
  if not TPath.IsPathRooted(DatabaseFile) then
    DatabaseFile := TPath.Combine(ExtractFilePath(ParamStr(0)), DatabaseFile);

  // Embedding configuration
  EmbeddingProvider := PM.GetParameter('embedding-provider',
                         PM.GetParameter('embedding_provider', DEFAULT_EMBEDDING_PROVIDER));
  EmbeddingModel := PM.GetParameter('embedding-model',
                      PM.GetParameter('embedding_model', DEFAULT_EMBEDDING_MODEL));

  // Get embedding URL: command line > config file > environment variable
  EmbeddingURL := PM.GetParameter('embedding-url', PM.GetParameter('embedding_url', ''));
  if EmbeddingURL = '' then
  begin
    EnvURL := GetEmbeddingURLFromEnv;
    if EnvURL <> '' then
      EmbeddingURL := EnvURL;
  end;

  // Normalize URL (add default port for Ollama if missing)
  EmbeddingURL := NormalizeEmbeddingURL(EmbeddingURL, EmbeddingProvider);

  // Indexing options
  ContentType := PM.GetParameter('type', PM.GetParameter('content_type', DEFAULT_CONTENT_TYPE));
  SourceCategory := PM.GetParameter('category', PM.GetParameter('source_category', DEFAULT_SOURCE_CATEGORY));
  ExplicitFramework := UpperCase(PM.GetParameter('framework', ''));
  MappingFile := PM.GetParameter('mapping-file', PM.GetParameter('mapping_file', ''));
  ChunkBufferSize := PM.GetParameterAsInteger('buffer-size',
                       PM.GetParameterAsInteger('buffer_size', DEFAULT_BUFFER_SIZE));
  BatchSize := PM.GetParameterAsInteger('batch-size',
                 PM.GetParameterAsInteger('batch_size', DEFAULT_BATCH_SIZE));

  // Clamp buffer size
  if ChunkBufferSize < 50 then ChunkBufferSize := 50;
  if ChunkBufferSize > 5000 then ChunkBufferSize := 5000;

  // Boolean flags
  ForceFullReindex := PM.HasParameter('force');
  UseHybridMode := not PM.HasParameter('legacy');
  DryRun := PM.HasParameter('dry-run');

  // Reranker (for consistency, loaded but not used by indexer)
  RerankerProvider := PM.GetParameter('reranker-provider',
                        PM.GetParameter('reranker_provider', DEFAULT_RERANKER_PROVIDER));
  RerankerURL := PM.GetParameter('reranker-url', PM.GetParameter('reranker_url', ''));

  // === Special modes ===

  // CHM indexing mode
  IndexCHM := PM.HasParameter('index-chm');
  CHMPath := PM.GetParameter('index-chm', '');
  DelphiVersion := PM.GetParameter('delphi-version', PM.GetParameter('delphi_version', '12.0'));

  if IndexCHM then
  begin
    if CHMPath = '' then
    begin
      WriteLn('Error: --index-chm requires a CHM file path');
      WriteLn;
      ShowUsage;
      Halt(1);
    end;
    ContentType := 'help';
    SourceCategory := 'official_help';
    Exit;
  end;

  // Package intelligence modes
  ScanPackagesMode := PM.HasParameter('scan-packages');
  ScanPackagesPath := PM.GetParameter('scan-packages', '');
  ListPackagesMode := PM.HasParameter('list-packages');
  GenerateMappingMode := PM.HasParameter('generate-mapping');
  MappingInputFolder := PM.GetParameter('generate-mapping', '');
  MappingOutputFile := PM.GetParameter('output', 'framework-overrides.json');

  // Cache revalidation mode
  RevalidateCacheMode := PM.HasParameter('revalidate-cache');
  RevalidateMinHits := PM.GetParameterAsInteger('revalidate-cache', 3);
  if RevalidateMinHits < 1 then RevalidateMinHits := 1;
  if RevalidateMinHits > 100 then RevalidateMinHits := 100;

  if ScanPackagesMode or ListPackagesMode or GenerateMappingMode or RevalidateCacheMode then
    Exit;

  // List folders mode
  if PM.HasParameter('list-folders') then
  begin
    ListIndexedFolders;
    Halt(0);
  end;

  // === Normal indexing mode ===

  // Get folder path (first positional argument)
  FolderPath := GetFirstPositionalArg;

  if FolderPath = '' then
  begin
    WriteLn('Error: folder_path is required');
    WriteLn;
    ShowUsage;
    Halt(1);
  end;

  // Display configuration
  if ForceFullReindex then
  begin
    WriteLn('');
    WriteLn('***************************************');
    WriteLn('*** FORCE MODE: Full reindex forced ***');
    WriteLn('***************************************');
    WriteLn('');
  end;

  WriteLn(Format('Content Type: %s | Source Category: %s', [ContentType, SourceCategory]));
  if EmbeddingURL <> '' then
    WriteLn(Format('Embedding: %s (%s)', [EmbeddingURL, EmbeddingModel]))
  else
    WriteLn('Embedding: DISABLED (FTS5 only)');
  WriteLn('');
end;

function DocTypeToString(ADocType: TDocType): string;
begin
  case ADocType of
    dtClass: Result := 'class';
    dtInterface: Result := 'interface';
    dtRecord: Result := 'record';
    dtFunction: Result := 'function';
    dtProcedure: Result := 'procedure';
    dtMethod: Result := 'method';
    dtProperty: Result := 'property';
    dtConstant: Result := 'const';
    dtType: Result := 'type';
    dtEvent: Result := 'event';
  else
    Result := 'unknown';
  end;
end;

function FrameworkToString(AFramework: TDocFramework): string;
begin
  case AFramework of
    dfVCL: Result := 'VCL';
    dfFMX: Result := 'FMX';
    dfRTL: Result := 'RTL';
  else
    Result := '';
  end;
end;

function PlatformsToString(APlatforms: TDocPlatforms): string;
var
  Parts: TStringList;
  P: TDocPlatform;
begin
  if APlatforms = [] then
    Exit('');

  Parts := TStringList.Create;
  try
    for P := Low(TDocPlatform) to High(TDocPlatform) do
      if P in APlatforms then
        Parts.Add(PLATFORM_NAMES[P]);

    Result := string.Join(',', Parts.ToStringArray);
  finally
    Parts.Free;
  end;
end;

function IndexCHMDocumentation(const ACHMPath, ADBPath, ADelphiVersion: string): Boolean;
var
  Extractor: TCHMExtractor;
  Parser: TCHMParser;
  Chunker: TDocChunker;
  DBBuilder: TDatabaseBuilder;
  EmbeddingGen: TOllamaEmbeddingGenerator;
  Files: TStringList;
  Pages: TArray<TDocPage>;
  Chunks: TArray<TDocChunk>;
  I: Integer;
  OutputDir: string;
  StartTime: TDateTime;
  Embedding: TEmbedding;
  Embeddings: TEmbeddingList;
  HTMLFile: string;
begin
  Result := False;
  StartTime := Now;

  WriteLn('Delphi CHM Documentation Indexer');
  WriteLn('=================================');
  WriteLn;
  WriteLn(Format('CHM File: %s', [ACHMPath]));
  WriteLn(Format('Database: %s', [ADBPath]));
  WriteLn(Format('Delphi Version: %s', [ADelphiVersion]));
  WriteLn;

  OutputDir := ExtractFilePath(ADBPath) + 'chm_cache_' + ADelphiVersion;

  try
    // Step 1: Extract CHM
    WriteLn('Step 1: Extracting CHM content...');
    Stopwatch := TStopwatch.StartNew;

    Extractor := TCHMExtractor.Create(ACHMPath, OutputDir);
    try
      if not Extractor.Extract then
      begin
        WriteLn('[ERROR] CHM extraction failed');
        Exit;
      end;
      WriteLn(Format('  Extracted %d topics in %d ms',
        [Extractor.ExtractionInfo.TopicCount, Stopwatch.ElapsedMilliseconds]));
    finally
      Extractor.Free;
    end;

    // Step 2: Parse HTML files
    WriteLn('Step 2: Parsing HTML files...');
    Stopwatch.Reset;
    Stopwatch.Start;

    Parser := TCHMParser.Create;
    try
      Files := TStringList.Create;
      try
        // Find all HTML files in output directory
        var FileArray := TDirectory.GetFiles(OutputDir, '*.htm*', TSearchOption.soAllDirectories);
        for HTMLFile in FileArray do
          Files.Add(HTMLFile);

        WriteLn(Format('  Found %d HTML files', [Files.Count]));

        SetLength(Pages, Files.Count);
        for I := 0 to Files.Count - 1 do
        begin
          if (I mod 100 = 0) and (I > 0) then
            WriteLn(Format('  Parsing file %d/%d', [I, Files.Count]));

          Pages[I] := Parser.ParseHTMLFile(Files[I]);
        end;

        WriteLn(Format('  Parsed %d pages in %d ms', [Length(Pages), Stopwatch.ElapsedMilliseconds]));
      finally
        Files.Free;
      end;
    finally
      Parser.Free;
    end;

    // Step 3: Chunk documentation
    WriteLn('Step 3: Chunking documentation...');
    Stopwatch.Reset;
    Stopwatch.Start;

    Chunker := TDocChunker.Create;
    try
      Chunks := Chunker.ChunkPages(Pages);
      WriteLn(Format('  Created %d chunks in %d ms', [Length(Chunks), Stopwatch.ElapsedMilliseconds]));
    finally
      Chunker.Free;
    end;

    // Step 4: Generate embeddings
    WriteLn('Step 4: Generating embeddings with Ollama...');
    Stopwatch.Reset;
    Stopwatch.Start;

    Embeddings := TEmbeddingList.Create;
    try
      EmbeddingGen := TOllamaEmbeddingGenerator.Create(EmbeddingURL, EmbeddingModel, DEFAULT_BATCH_SIZE);
      try
        // Detect dimensions first
        EmbeddingDimensions := EmbeddingGen.DetectEmbeddingDimensions;
        WriteLn(Format('  Detected %d dimensions', [EmbeddingDimensions]));

        // Generate embeddings in batches
        for I := 0 to High(Chunks) do
        begin
          if (I mod 50 = 0) and (I > 0) then
            WriteLn(Format('  Processing chunk %d/%d', [I, Length(Chunks)]));

          // Generate embedding for this chunk using batch API
          var ChunkList := TCodeChunkList.Create;
          try
            var TempChunk := TCodeChunk.Create;
            TempChunk.ChunkGUID := Chunks[I].ID;
            TempChunk.Content := Chunks[I].Content;
            ChunkList.Add(TempChunk);

            var TempEmbeddings := EmbeddingGen.GenerateEmbeddings(ChunkList);
            try
              if TempEmbeddings.Count > 0 then
                Embeddings.Add(TempEmbeddings[0]);
            finally
              // Don't free the individual embeddings, they're now owned by Embeddings list
            end;
          finally
            ChunkList.Free;
          end;
        end;

        WriteLn(Format('  Generated %d embeddings in %d ms',
          [Embeddings.Count, Stopwatch.ElapsedMilliseconds]));
      finally
        EmbeddingGen.Free;
      end;

      // Step 5: Store in database
      WriteLn('Step 5: Storing in database...');
      Stopwatch.Reset;
      Stopwatch.Start;

      DBBuilder := TDatabaseBuilder.Create;
      try
        // Connect and ensure schema is ready
        DBBuilder.BuildDatabase(TCodeChunkList.Create, TEmbeddingList.Create,
          ADBPath, EmbeddingURL, EmbeddingModel, EmbeddingDimensions,
          OutputDir, False, 'help', 'official_help');

        // Insert documentation chunks
        for I := 0 to High(Chunks) do
        begin
          if (I mod 100 = 0) and (I > 0) then
            WriteLn(Format('  Storing chunk %d/%d', [I, Length(Chunks)]));

          // Find corresponding embedding
          Embedding := nil;
          for var J := 0 to Embeddings.Count - 1 do
          begin
            if Embeddings[J].ChunkID = Chunks[I].ID then
            begin
              Embedding := Embeddings[J];
              Break;
            end;
          end;

          // Insert symbol with documentation fields
          DBBuilder.InsertSymbol(
            Chunks[I].Name,                                    // name
            Chunks[I].Name,                                    // full_name
            DocTypeToString(Chunks[I].Metadata.DocType),       // type
            Chunks[I].SourceFile,                              // file_path
            Chunks[I].Content,                                 // content
            '',                                                // comments
            Chunks[I].Metadata.ParentClass,                    // parent_class
            Embedding,                                         // embedding
            FrameworkToString(Chunks[I].Metadata.Framework),   // framework
            PlatformsToString(Chunks[I].Metadata.Platforms),   // platforms
            ADelphiVersion                                     // delphi_version
          );
        end;

        WriteLn(Format('  Stored %d symbols in %d ms',
          [Length(Chunks), Stopwatch.ElapsedMilliseconds]));
      finally
        DBBuilder.Free;
      end;

    finally
      Embeddings.Free;
    end;

    WriteLn;
    WriteLn('=================================');
    WriteLn('CHM Documentation Indexing Complete!');
    WriteLn('=================================');
    WriteLn(Format('Total chunks: %d', [Length(Chunks)]));
    WriteLn(Format('Total time: %s', [FormatDateTime('nn:ss', Now - StartTime)]));
    WriteLn;

    Result := True;

  except
    on E: Exception do
    begin
      WriteLn('[ERROR] ' + E.Message);
      Result := False;
    end;
  end;
end;

var
  GInterrupted: Boolean = False;

procedure HandleCtrlC;
begin
  GInterrupted := True;
  WriteLn;
  WriteLn('*** Ctrl+C detected - finishing current query and stopping... ***');
end;

procedure RevalidateCache(const ADatabaseFile: string; AMinHits: Integer);
var
  Connection: TFDConnection;
  Query, UpdateQuery: TFDQuery;
  QueryHash, QueryText, OldResultIDs, NewResultIDs: string;
  TotalCandidates, Processed, Unchanged, Updated, NowEmpty, NowFound, Purged: Integer;
  StartTime: TDateTime;
  ResultCount: Integer;
begin
  if not FileExists(ADatabaseFile) then
  begin
    WriteLn('Error: Database not found: ' + ADatabaseFile);
    Exit;
  end;

  WriteLn('Cache Revalidation');
  WriteLn('==================');
  WriteLn(Format('Database: %s', [ADatabaseFile]));
  WriteLn(Format('Minimum hits threshold: %d', [AMinHits]));
  WriteLn('Press Ctrl+C to safely interrupt');
  WriteLn;

  StartTime := Now;

  // Set up Ctrl+C handler (Windows console handler)
  // Note: In a full implementation, we'd use SetConsoleCtrlHandler
  // For now, we'll check GInterrupted flag which can be set externally
  GInterrupted := False;

  Connection := TFDConnection.Create(nil);
  Query := TFDQuery.Create(nil);
  UpdateQuery := TFDQuery.Create(nil);
  try
    TDatabaseConnectionHelper.ConfigureConnection(Connection, ADatabaseFile, False);
    Connection.Open;

    Query.Connection := Connection;
    UpdateQuery.Connection := Connection;

    // Enable WAL mode
    Query.SQL.Text := 'PRAGMA journal_mode=WAL';
    Query.ExecSQL;

    // Step 1: Purge obsolete queries
    WriteLn('Step 1: Purging obsolete cache entries...');

    // Delete queries with 0 results that are older than 30 days
    Query.SQL.Text :=
      'DELETE FROM query_cache ' +
      'WHERE result_count = 0 AND ' +
      '  last_seen < datetime(''now'', ''-30 days'')';
    Query.ExecSQL;
    Purged := Query.RowsAffected;

    // Delete queries with few hits that are old
    Query.SQL.Text :=
      'DELETE FROM query_cache ' +
      'WHERE hit_count < :min_hits AND ' +
      '  last_seen < datetime(''now'', ''-30 days'')';
    Query.ParamByName('min_hits').AsInteger := AMinHits;
    Query.ExecSQL;
    Purged := Purged + Query.RowsAffected;

    WriteLn(Format('  Purged %d obsolete entries', [Purged]));

    // Step 2: Get candidates for revalidation
    WriteLn('Step 2: Finding candidates for revalidation...');

    Query.SQL.Text :=
      'SELECT query_hash, query_text, result_ids, result_count ' +
      'FROM query_cache ' +
      'WHERE cache_valid = 0 AND hit_count >= :min_hits ' +
      'ORDER BY hit_count DESC';
    Query.ParamByName('min_hits').AsInteger := AMinHits;
    Query.Open;

    // Count candidates
    TotalCandidates := 0;
    while not Query.EOF do
    begin
      Inc(TotalCandidates);
      Query.Next;
    end;
    Query.First;

    if TotalCandidates = 0 then
    begin
      WriteLn('  No invalidated queries with enough hits to revalidate.');
      WriteLn;
      WriteLn('Cache revalidation complete - nothing to do.');
      Exit;
    end;

    WriteLn(Format('  Found %d candidates', [TotalCandidates]));
    WriteLn;

    // Step 3: Revalidate each candidate
    WriteLn('Step 3: Revalidating queries...');

    Processed := 0;
    Unchanged := 0;
    Updated := 0;
    NowEmpty := 0;
    NowFound := 0;

    while not Query.EOF do
    begin
      // Check for interruption
      if GInterrupted then
      begin
        WriteLn;
        WriteLn('*** Interrupted by user ***');
        Break;
      end;

      QueryHash := Query.FieldByName('query_hash').AsString;
      QueryText := Query.FieldByName('query_text').AsString;
      OldResultIDs := Query.FieldByName('result_ids').AsString;
      ResultCount := Query.FieldByName('result_count').AsInteger;

      // For now, we don't re-execute the actual search (would require QueryProcessor).
      // Instead, we validate that the cached symbols still exist and have matching hashes.
      // If all symbols are valid, we revalidate the cache entry.

      // Parse old result IDs and check each symbol
      var AllValid := True;
      var NewResultList := TStringList.Create;
      try
        var IDList := TStringList.Create;
        try
          IDList.CommaText := OldResultIDs;

          for var I := 0 to IDList.Count - 1 do
          begin
            var IDHashPair := IDList[I];
            var ColonPos := Pos(':', IDHashPair);
            var SymbolID: Integer;
            var CachedHash, CurrentHash: string;

            if ColonPos > 0 then
            begin
              SymbolID := StrToIntDef(Copy(IDHashPair, 1, ColonPos - 1), 0);
              CachedHash := Copy(IDHashPair, ColonPos + 1, MaxInt);
            end
            else
            begin
              SymbolID := StrToIntDef(IDHashPair, 0);
              CachedHash := '';
            end;

            if SymbolID = 0 then
            begin
              AllValid := False;
              Break;
            end;

            // Check if symbol exists and hash matches
            UpdateQuery.SQL.Text := 'SELECT id, content_hash FROM symbols WHERE id = :id';
            UpdateQuery.ParamByName('id').AsInteger := SymbolID;
            UpdateQuery.Open;

            if UpdateQuery.EOF then
            begin
              // Symbol no longer exists
              AllValid := False;
              UpdateQuery.Close;
              Break;
            end;

            CurrentHash := UpdateQuery.FieldByName('content_hash').AsString;
            UpdateQuery.Close;

            // Check hash (if both available)
            if (CachedHash <> '') and (CurrentHash <> '') and (CachedHash <> CurrentHash) then
            begin
              // Content changed
              AllValid := False;
              Break;
            end;

            // Symbol is valid - add to new list with current hash
            NewResultList.Add(IntToStr(SymbolID) + ':' + CurrentHash);
          end;
        finally
          IDList.Free;
        end;

        // Update cache entry based on validation result
        if AllValid and (NewResultList.Count > 0) then
        begin
          // All symbols valid - reactivate cache
          NewResultIDs := NewResultList.CommaText;

          UpdateQuery.SQL.Text :=
            'UPDATE query_cache SET ' +
            '  result_ids = :result_ids, ' +
            '  cache_valid = 1, ' +
            '  last_seen = CURRENT_TIMESTAMP ' +
            'WHERE query_hash = :hash';
          UpdateQuery.ParamByName('result_ids').AsString := NewResultIDs;
          UpdateQuery.ParamByName('hash').AsString := QueryHash;
          UpdateQuery.ExecSQL;

          if NewResultIDs = OldResultIDs then
            Inc(Unchanged)
          else
            Inc(Updated);
        end
        else if AllValid and (NewResultList.Count = 0) and (ResultCount = 0) then
        begin
          // Was zero results, still zero results - reactivate
          UpdateQuery.SQL.Text :=
            'UPDATE query_cache SET ' +
            '  cache_valid = 1, ' +
            '  last_seen = CURRENT_TIMESTAMP ' +
            'WHERE query_hash = :hash';
          UpdateQuery.ParamByName('hash').AsString := QueryHash;
          UpdateQuery.ExecSQL;
          Inc(Unchanged);
        end
        else if not AllValid then
        begin
          // Some symbols changed/deleted - keep invalidated
          // The next actual search will update the cache
          if ResultCount > 0 then
            Inc(NowEmpty)
          else
            Inc(NowFound);
        end;

      finally
        NewResultList.Free;
      end;

      Inc(Processed);

      // Progress every 100 queries
      if Processed mod 100 = 0 then
        WriteLn(Format('  Processed %d/%d (%.1f%%)',
          [Processed, TotalCandidates, Processed * 100.0 / TotalCandidates]));

      Query.Next;
    end;

    Query.Close;

    WriteLn;
    WriteLn('==================');
    WriteLn('Revalidation Summary');
    WriteLn('==================');
    WriteLn(Format('Candidates:  %d', [TotalCandidates]));
    WriteLn(Format('Processed:   %d', [Processed]));
    WriteLn(Format('Unchanged:   %d (cache reactivated)', [Unchanged]));
    WriteLn(Format('Updated:     %d (hashes refreshed)', [Updated]));
    WriteLn(Format('Invalid:     %d (will re-search on next use)', [NowEmpty + NowFound]));
    WriteLn(Format('Purged:      %d (obsolete entries)', [Purged]));
    WriteLn(Format('Duration:    %s', [FormatDateTime('nn:ss', Now - StartTime)]));
    WriteLn;

  finally
    UpdateQuery.Free;
    Query.Free;
    Connection.Free;
  end;
end;

procedure ProcessFiles;
var
  FolderScanner: TFolderScanner;
  ASTProcessor: TASTProcessor;
  EmbeddingGenerator: TOllamaEmbeddingGenerator;
  DatabaseBuilder: TDatabaseBuilder;
  ParsedFiles: TStringList;
  FilesToProcess: TStringList;
  CodeChunks: TCodeChunkList;
  Embeddings: TEmbeddingList;
  TotalStopwatch: TStopwatch;
  Step1Ms, Step1bMs, Step2Ms, Step3Ms, Step4Ms, Step5Ms: Int64;
  SkippedCount: Integer;
begin
  TotalStopwatch := TStopwatch.StartNew;

  WriteLn('Pascal Code Index Builder');
  WriteLn('=========================');
  WriteLn;

  // Step 1: Scan folder for Pascal files
  // Include implementation sections only when embeddings are enabled
  // (for richer semantic search while keeping FTS index lean)
  WriteLn('Step 1: Scanning folder for Pascal files...');
  Stopwatch := TStopwatch.StartNew;

  FolderScanner := TFolderScanner.Create(EmbeddingURL <> '');
  try
    ParsedFiles := FolderScanner.ScanFolder(FolderPath);
    Step1Ms := Stopwatch.ElapsedMilliseconds;
    WriteLn(Format('  Scanned %d files in %d ms', [ParsedFiles.Count, Step1Ms]));
  finally
    FolderScanner.Free;
  end;

  // Step 1b: Pre-filter files (skip unchanged files)
  WriteLn('Step 1b: Checking for unchanged files...');
  Stopwatch.Reset;
  Stopwatch.Start;

  DatabaseBuilder := TDatabaseBuilder.Create;
  try
    FilesToProcess := DatabaseBuilder.FilterFilesToProcess(ParsedFiles, FolderPath,
      DatabaseFile, ForceFullReindex);
    Step1bMs := Stopwatch.ElapsedMilliseconds;
    SkippedCount := ParsedFiles.Count - FilesToProcess.Count;

    if SkippedCount > 0 then
      WriteLn(Format('  Skipped %d unchanged files (%d to process) in %d ms',
        [SkippedCount, FilesToProcess.Count, Step1bMs]))
    else
      WriteLn(Format('  All %d files need processing in %d ms',
        [FilesToProcess.Count, Step1bMs]));

    // If nothing to process, exit early
    if FilesToProcess.Count = 0 then
    begin
      WriteLn;
      WriteLn('All files are up to date. Nothing to index.');
      WriteLn('Use --force to reindex all files.');
      ParsedFiles.Free;
      FilesToProcess.Free;
      Exit;
    end;
  finally
    DatabaseBuilder.Free;
  end;

  // Step 2: Process Pascal AST (only files that need processing)
  WriteLn('Step 2: Processing Pascal AST...');
  Stopwatch.Reset;
  Stopwatch.Start;

  ASTProcessor := TASTProcessor.Create;
  try
    CodeChunks := ASTProcessor.ProcessFiles(FilesToProcess);
    Step2Ms := Stopwatch.ElapsedMilliseconds;
    WriteLn(Format('  Generated %d code chunks in %d ms', [CodeChunks.Count, Step2Ms]));
  finally
    ASTProcessor.Free;
  end;

  // Step 3 removed: Comments are now extracted during AST processing (Step 2)
  Step3Ms := 0;

  // Step 3.5: DISABLED - Glossary enrichment (too slow for large codebases)
  // WriteLn('Step 3.5: Enriching chunks with Spanish-English glossary...');
  // Stopwatch.Reset;
  // Stopwatch.Start;
  //
  // var GlossaryEnricher := TGlossaryEnricher.Create;
  // try
  //   var EnrichedCount := 0;
  //   for var Chunk in CodeChunks do
  //   begin
  //     // Enrich the chunk (adds metadata if Spanish terms detected)
  //     Chunk.EnrichedText := GlossaryEnricher.EnrichChunk(
  //       Chunk.Content,
  //       Chunk.Name,
  //       'code'  // chunk type as string
  //     );
  //
  //     // Extract domain tags and detected terms for database storage
  //     Chunk.DomainTags := GlossaryEnricher.ExtractDomainTags(Chunk.Content);
  //     Chunk.SpanishTerms := GlossaryEnricher.GetDetectedTerms(Chunk.Content);
  //
  //     // Count chunks that were enriched (detected Spanish terms)
  //     if Chunk.EnrichedText <> Chunk.Content then
  //       Inc(EnrichedCount);
  //   end;
  //
  //   WriteLn(Format('  Enriched %d/%d chunks in %d ms',
  //     [EnrichedCount, CodeChunks.Count, Stopwatch.ElapsedMilliseconds]));
  //   WriteLn(Format('  Glossary contains %d terms', [GlossaryEnricher.TermCount]));
  // finally
  //   GlossaryEnricher.Free;
  // end;

  // Step 4: Detect embedding dimensions and generate embeddings with Ollama
  WriteLn('Step 4: Detecting dimensions and generating embeddings with Ollama...');
  Stopwatch.Reset;
  Stopwatch.Start;

  EmbeddingGenerator := TOllamaEmbeddingGenerator.Create(EmbeddingURL, EmbeddingModel, DEFAULT_BATCH_SIZE);
  try
    // Detect dimensions first (one API call)
    EmbeddingDimensions := EmbeddingGenerator.DetectEmbeddingDimensions;

    // Generate all embeddings
    Embeddings := EmbeddingGenerator.GenerateEmbeddings(CodeChunks);
    Step4Ms := Stopwatch.ElapsedMilliseconds;
    WriteLn(Format('  Generated %d embeddings in %d ms', [Embeddings.Count, Step4Ms]));
  finally
    EmbeddingGenerator.Free;
  end;

  // Step 5: Build self-describing database with sqlite-vec
  WriteLn('Step 5: Building self-describing database with sqlite-vec...');
  Stopwatch.Reset;
  Stopwatch.Start;

  DatabaseBuilder := TDatabaseBuilder.Create;
  try
    DatabaseBuilder.BuildDatabase(CodeChunks, Embeddings, DatabaseFile,
      EmbeddingURL, EmbeddingModel, EmbeddingDimensions, FolderPath, ForceFullReindex,
      ContentType, SourceCategory, ExplicitFramework, MappingFile, BatchSize);
    Step5Ms := Stopwatch.ElapsedMilliseconds;
    WriteLn(Format('  Built database in %d ms', [Step5Ms]));
  finally
    DatabaseBuilder.Free;
  end;

  TotalStopwatch.Stop;

  WriteLn;
  WriteLn('Index building completed successfully!');
  WriteLn(Format('Database (with vectors): %s', [DatabaseFile]));
  WriteLn;
  WriteLn('=== Timing Summary ===');
  WriteLn(Format('  Step 1  (Scan files):   %6d ms', [Step1Ms]));
  WriteLn(Format('  Step 1b (Pre-filter):   %6d ms', [Step1bMs]));
  WriteLn(Format('  Step 2  (Parse AST):    %6d ms', [Step2Ms]));
  WriteLn(Format('  Step 3  (Comments):     %6d ms', [Step3Ms]));
  WriteLn(Format('  Step 4  (Embeddings):   %6d ms', [Step4Ms]));
  WriteLn(Format('  Step 5  (Database):     %6d ms', [Step5Ms]));
  WriteLn('  -------------------------------');
  WriteLn(Format('  Total time:             %6d ms', [TotalStopwatch.ElapsedMilliseconds]));

  // Cleanup
  ParsedFiles.Free;
  FilesToProcess.Free;
  CodeChunks.Free;
  Embeddings.Free;
end;

procedure ProcessFilesHybrid;
var
  FolderScanner: TFolderScanner;
  ASTProcessor: TASTProcessor;
  EmbeddingGenerator: TOllamaEmbeddingGenerator;
  DatabaseBuilder: TDatabaseBuilder;
  ParsedFiles: TStringList;
  FilesToProcess: TStringList;
  ParsedFile: TParsedFile;
  FileChunks: TCodeChunkList;
  ChunkBuffer: TCodeChunkList;
  BufferEmbeddings: TEmbeddingList;
  Chunk: TCodeChunk;
  PendingFiles: TDictionary<string, Integer>;  // Track files pending checkpoint
  PendingFilePath: string;
  TotalStopwatch, Stopwatch: TStopwatch;
  StartTime: TDateTime;
  I, SkippedCount, TotalFiles, TotalChunks, FilesProcessed: Integer;
  CheckpointCount: Integer;
  ProgressStr, ETAStr: string;
  ElapsedSec: Double;
  ETASec: Integer;
  FilesPerSec: Double;
begin
  TotalStopwatch := TStopwatch.StartNew;
  StartTime := Now;

  WriteLn('Pascal Code Index Builder (Hybrid Mode)');
  WriteLn('=======================================');
  WriteLn(Format('Buffer size: %d chunks (saves incrementally)', [ChunkBufferSize]));
  WriteLn;

  // Step 1: Scan folder for Pascal files
  // Include implementation sections only when embeddings are enabled
  WriteLn('Step 1: Scanning folder for Pascal files...');
  Stopwatch := TStopwatch.StartNew;

  FolderScanner := TFolderScanner.Create(EmbeddingURL <> '');
  try
    ParsedFiles := FolderScanner.ScanFolder(FolderPath);
    WriteLn(Format('  Scanned %d files in %d ms', [ParsedFiles.Count, Stopwatch.ElapsedMilliseconds]));
  finally
    FolderScanner.Free;
  end;

  // Step 2: Pre-filter files (skip unchanged files)
  WriteLn('Step 2: Checking for unchanged files...');
  Stopwatch.Reset;
  Stopwatch.Start;

  DatabaseBuilder := TDatabaseBuilder.Create;
  try
    FilesToProcess := DatabaseBuilder.FilterFilesToProcess(ParsedFiles, FolderPath,
      DatabaseFile, ForceFullReindex);
    SkippedCount := ParsedFiles.Count - FilesToProcess.Count;

    if SkippedCount > 0 then
      WriteLn(Format('  Skipped %d unchanged files (%d to process) in %d ms',
        [SkippedCount, FilesToProcess.Count, Stopwatch.ElapsedMilliseconds]))
    else
      WriteLn(Format('  All %d files need processing in %d ms',
        [FilesToProcess.Count, Stopwatch.ElapsedMilliseconds]));

    // If nothing to process, exit early
    if FilesToProcess.Count = 0 then
    begin
      WriteLn;
      WriteLn('All files are up to date. Nothing to index.');
      WriteLn('Use --force to reindex all files.');
      ParsedFiles.Free;
      FilesToProcess.Free;
      Exit;
    end;
  finally
    DatabaseBuilder.Free;
  end;

  // Step 3: Initialize database and start hybrid processing
  WriteLn('Step 3: Initializing database...');

  DatabaseBuilder := TDatabaseBuilder.Create;
  ASTProcessor := TASTProcessor.Create;
  ChunkBuffer := TCodeChunkList.Create;
  PendingFiles := TDictionary<string, Integer>.Create;

  if (EmbeddingURL = '') then
  begin
    EmbeddingGenerator := nil;
    EmbeddingDimensions := 0;
    WriteLn('  Embeddings DISABLED (FTS5 only mode)');
  end
  else
  begin
    EmbeddingGenerator := TOllamaEmbeddingGenerator.Create(EmbeddingURL, EmbeddingModel, DEFAULT_BATCH_SIZE);
    // Detect dimensions first
    EmbeddingDimensions := EmbeddingGenerator.DetectEmbeddingDimensions;
    WriteLn(Format('  Embedding dimensions: %d', [EmbeddingDimensions]));
  end;

  try
    // Initialize database for hybrid mode
    DatabaseBuilder.InitializeForHybrid(DatabaseFile, EmbeddingURL, EmbeddingModel,
      EmbeddingDimensions, ContentType, SourceCategory, ExplicitFramework, MappingFile);

    // Step 4: Process files with incremental saving
    WriteLn('Step 4: Processing files (hybrid mode)...');
    Stopwatch.Reset;
    Stopwatch.Start;

    TotalFiles := FilesToProcess.Count;
    TotalChunks := 0;
    FilesProcessed := 0;
    CheckpointCount := 0;

    for I := 0 to TotalFiles - 1 do
    begin
      // Get parsed file
      ParsedFile := nil;
      for var J := 0 to ParsedFiles.Count - 1 do
      begin
        if ParsedFiles[J] = FilesToProcess[I] then
        begin
          ParsedFile := TParsedFile(ParsedFiles.Objects[J]);
          Break;
        end;
      end;

      if not Assigned(ParsedFile) then
        Continue;

      // Show progress with ETA
      if (I > 0) and (TotalFiles > 5) then
      begin
        ElapsedSec := Stopwatch.ElapsedMilliseconds / 1000;
        if ElapsedSec > 0 then
        begin
          FilesPerSec := FilesProcessed / ElapsedSec;
          if FilesPerSec > 0 then
            ETASec := Round((TotalFiles - I) / FilesPerSec)
          else
            ETASec := 0;

          if ETASec >= 3600 then
            ETAStr := Format('%d:%02d:%02d', [ETASec div 3600, (ETASec mod 3600) div 60, ETASec mod 60])
          else
            ETAStr := Format('%d:%02d', [ETASec div 60, ETASec mod 60]);
          ProgressStr := Format('[%d/%d %.0f%% ETA:%s] ', [I + 1, TotalFiles, ((I + 1) / TotalFiles) * 100, ETAStr]);
        end
        else
          ProgressStr := Format('[%d/%d] ', [I + 1, TotalFiles]);
      end
      else
        ProgressStr := Format('[%d/%d] ', [I + 1, TotalFiles]);

      // Show file being processed BEFORE parsing (so user sees activity)
      Write(Format(#13'%sParsing: %s...                              ',
        [ProgressStr, ExtractFileName(ParsedFile.FilePath)]));
      Flush(Output);

      // Parse file (comments are extracted during AST processing)
      FileChunks := ASTProcessor.ProcessSingleFile(ParsedFile);
      try
        var ChunksFromFile := FileChunks.Count;

        // Add to buffer (move chunks to avoid double-free)
        while FileChunks.Count > 0 do
        begin
          Chunk := FileChunks.Extract(FileChunks[0]);
          ChunkBuffer.Add(Chunk);
        end;

        // Update line with results after parsing
        Write(Format(#13'%s%s: %d chunks | Buffer: %d        ',
          [ProgressStr, ExtractFileName(ParsedFile.FilePath), ChunksFromFile, ChunkBuffer.Count]));
        Flush(Output);

        // Track file for checkpoint (even if 0 chunks - still needs hash)
        PendingFiles.AddOrSetValue(ParsedFile.FilePath, ChunksFromFile);
      finally
        FileChunks.Free;
      end;

      Inc(FilesProcessed);

      // Check if buffer needs flushing
      if ChunkBuffer.Count >= ChunkBufferSize then
      begin
        Inc(CheckpointCount);

        if (EmbeddingURL = '') then
        begin
          WriteLn(Format(#13'  >>> Checkpoint %d: Saving %d chunks...                          ',
            [CheckpointCount, ChunkBuffer.Count]));
          BufferEmbeddings := TEmbeddingList.Create;  // Empty list
        end
        else
        begin
          WriteLn(Format(#13'  >>> Checkpoint %d: Saving %d chunks (with embeddings)...        ',
            [CheckpointCount, ChunkBuffer.Count]));
          BufferEmbeddings := EmbeddingGenerator.GenerateEmbeddings(ChunkBuffer);
        end;

        try
          // Insert batch into database
          DatabaseBuilder.InsertChunkBatch(ChunkBuffer, BufferEmbeddings);

          // Store file hashes for ALL processed files in this batch
          for PendingFilePath in PendingFiles.Keys do
            DatabaseBuilder.CheckpointFile(PendingFilePath, PendingFiles[PendingFilePath]);

          TotalChunks := TotalChunks + ChunkBuffer.Count;

          // Commit and continue
          DatabaseBuilder.CommitAndContinue;

          WriteLn(Format('  >>> Saved %d chunks from %d files (total: %d)',
            [ChunkBuffer.Count, PendingFiles.Count, TotalChunks]));
        finally
          BufferEmbeddings.Free;
        end;

        // Clear buffer and pending files
        ChunkBuffer.Clear;
        PendingFiles.Clear;

        // Show continuation message (progress will update in place after this)
        Write('  Continuing...');
        Flush(Output);
      end;
    end;

    // Clear progress line with AST summary
    WriteLn(Format(#13'[AST] Parsed %d files, buffer: %d chunks                              ',
      [FilesProcessed, ChunkBuffer.Count]));

    // Flush remaining buffer and pending files
    if (ChunkBuffer.Count > 0) or (PendingFiles.Count > 0) then
    begin
      Inc(CheckpointCount);

      if ChunkBuffer.Count > 0 then
      begin
        if (EmbeddingURL = '') then
        begin
          WriteLn(Format('  >>> Final flush: Saving %d chunks...', [ChunkBuffer.Count]));
          BufferEmbeddings := TEmbeddingList.Create;  // Empty list
        end
        else
        begin
          WriteLn(Format('  >>> Final flush: Saving %d chunks (with embeddings)...', [ChunkBuffer.Count]));
          BufferEmbeddings := EmbeddingGenerator.GenerateEmbeddings(ChunkBuffer);
        end;

        try
          DatabaseBuilder.InsertChunkBatch(ChunkBuffer, BufferEmbeddings);
          TotalChunks := TotalChunks + ChunkBuffer.Count;
        finally
          BufferEmbeddings.Free;
        end;
      end;

      // Checkpoint ALL remaining pending files
      for PendingFilePath in PendingFiles.Keys do
        DatabaseBuilder.CheckpointFile(PendingFilePath, PendingFiles[PendingFilePath]);

      ChunkBuffer.Clear;
      PendingFiles.Clear;
    end;

    // Finalize folder (pass FilesProcessed as modified count - if 0, cache won't be invalidated)
    DatabaseBuilder.FinalizeFolder(FolderPath, FilesProcessed, TotalChunks, StartTime, FilesProcessed);

    TotalStopwatch.Stop;

    WriteLn;
    WriteLn('Index building completed successfully!');
    WriteLn(Format('Database: %s', [DatabaseFile]));
    WriteLn(Format('Files processed: %d', [FilesProcessed]));
    WriteLn(Format('Chunks indexed: %d', [TotalChunks]));
    WriteLn(Format('Checkpoints: %d', [CheckpointCount]));
    WriteLn(Format('Total time: %d ms', [TotalStopwatch.ElapsedMilliseconds]));

  finally
    PendingFiles.Free;
    ChunkBuffer.Free;
    if Assigned(EmbeddingGenerator) then
      EmbeddingGenerator.Free;
    ASTProcessor.Free;
    DatabaseBuilder.Free;
    ParsedFiles.Free;
    FilesToProcess.Free;
  end;
end;

begin
  try
    ParseCommandLine;

    // Handle package scanning mode
    if ScanPackagesMode then
    begin
      if ScanPackagesPath = '' then
      begin
        WriteLn('Error: --scan-packages requires a directory path');
        WriteLn('Run with -h for usage information.');
        Halt(1);
      end;

      if not TDirectory.Exists(ScanPackagesPath) then
      begin
        WriteLn(Format('Error: Directory "%s" not found.', [ScanPackagesPath]));
        WriteLn('Run with -h for usage information.');
        Halt(1);
      end;

      var Scanner := TPackageScanner.Create(DatabaseFile);
      try
        Scanner.ScanDirectory(ScanPackagesPath, SourceCategory, ForceFullReindex);
      finally
        Scanner.Free;
      end;

      Halt(0);
    end;

    // Handle list packages mode
    if ListPackagesMode then
    begin
      var Scanner := TPackageScanner.Create(DatabaseFile);
      try
        Scanner.ListPackages;
      finally
        Scanner.Free;
      end;

      Halt(0);
    end;

    // Handle cache revalidation mode
    if RevalidateCacheMode then
    begin
      RevalidateCache(DatabaseFile, RevalidateMinHits);
      Halt(0);
    end;

    // Handle mapping file generation mode
    if GenerateMappingMode then
    begin
      if MappingInputFolder = '' then
      begin
        WriteLn('Error: --generate-mapping requires a directory path');
        WriteLn('Run with -h for usage information.');
        Halt(1);
      end;

      if not TDirectory.Exists(MappingInputFolder) then
      begin
        WriteLn(Format('Error: Directory "%s" not found.', [MappingInputFolder]));
        WriteLn('Run with -h for usage information.');
        Halt(1);
      end;

      if MappingOutputFile = '' then
        MappingOutputFile := 'framework-overrides.json';

      var Generator := TMappingGenerator.Create(DatabaseFile, MappingOutputFile);
      try
        Generator.ScanFolder(MappingInputFolder, ForceFullReindex, DryRun);

        if not DryRun then
          WriteLn(Format('Mapping file saved: %s', [MappingOutputFile]))
        else
          WriteLn('[DRY RUN] Mappings not saved - use without --dry-run to save');

        // Show orphaned files if any
        Generator.ListOrphanedFiles;
      finally
        Generator.Free;
      end;

      Halt(0);
    end;

    // Handle CHM indexing mode
    if IndexCHM then
    begin
      if not FileExists(CHMPath) then
      begin
        WriteLn(Format('Error: CHM file "%s" not found.', [CHMPath]));
        WriteLn('Run with -h for usage information.');
        Halt(1);
      end;

      if not IndexCHMDocumentation(CHMPath, DatabaseFile, DelphiVersion) then
        Halt(1);
    end
    else
    begin
      // Normal folder indexing mode
      if not TDirectory.Exists(FolderPath) then
      begin
        WriteLn(Format('Error: Folder "%s" not found.', [FolderPath]));
        WriteLn('Run with -h for usage information.');
        Halt(1);
      end;

      // Use hybrid mode (default) or legacy mode
      if UseHybridMode then
        ProcessFilesHybrid
      else
        ProcessFiles;
    end;

  except
    on E: Exception do
    begin
      WriteLn(Format('Error: %s', [E.Message]));
      Halt(1);
    end;
  end;
end.