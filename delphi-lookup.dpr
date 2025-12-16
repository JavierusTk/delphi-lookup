program delphi_lookup;

{$APPTYPE CONSOLE}

// This application requires 64-bit compilation for sqlite-vec compatibility
{$IFNDEF WIN64}
  {$MESSAGE FATAL 'delphi-lookup requires Win64 compilation. The sqlite-vec extension only works with 64-bit SQLite.'}
{$ENDIF}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Diagnostics,
  System.Hash,
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
  uSearchTypes in 'uSearchTypes.pas',
  uQueryProcessor in 'uQueryProcessor.pas',
  uVectorSearch in 'uVectorSearch.pas',
  uResultFormatter in 'uResultFormatter.pas',
  uReranker in 'uReranker.pas',
  uConfig in 'uConfig.pas',
  uLookupEmbeddings.Ollama in 'uLookupEmbeddings.Ollama.pas';

var
  // Parameter manager for config file + command line
  PM: TParameterManager;

  // Search components
  QueryProcessor: TQueryProcessor;
  VectorSearch: TVectorSearch;
  ResultFormatter: TResultFormatter;
  Stopwatch: TStopwatch;
  SearchDurationMs: Integer;
  IsCacheHit: Boolean;

  // Search parameters
  QueryText: string;
  NumResults: Integer;
  DatabaseFile: string;
  EmbeddingURL: string;
  MaxDistance: Double;
  ContentTypeFilter: string;
  SourceCategoryFilter: string;
  PreferCategory: string;
  DomainTagsFilter: string;
  SymbolTypeFilter: string;
  FrameworkFilter: string;
  UseReranker: Boolean;
  UseSemanticSearch: Boolean;
  CandidateCount: Integer;
  RerankerURL: string;

function GetDefaultDatabasePath: string;
begin
  // Returns the full path to the database file in the executable's directory
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), DEFAULT_DB_FILE);
end;

function GenerateQueryHash(const AQuery: string; const AFilters: array of string): string;
var
  I: Integer;
  Combined: string;
begin
  Combined := AQuery;
  for I := 0 to High(AFilters) do
    Combined := Combined + '|' + AFilters[I];
  Result := THashMD5.GetHashString(Combined);
end;

procedure LogQuery(const ADatabaseFile: string; const AQueryText: string;
  AResultCount, ADurationMs: Integer; const AResultIDs: string; ACacheHit: Boolean);
var
  Connection: TFDConnection;
  Query: TFDQuery;
  QueryHash: string;
  CacheValid: Integer;
begin
  try
    Connection := TFDConnection.Create(nil);
    Query := TFDQuery.Create(nil);
    try
      TDatabaseConnectionHelper.ConfigureConnection(Connection, ADatabaseFile, False);
      Connection.Open;

      Query.Connection := Connection;

      // Enable WAL mode for concurrent access
      Query.SQL.Text := 'PRAGMA journal_mode=WAL';
      Query.ExecSQL;

      // Generate query hash from query + all filters
      QueryHash := GenerateQueryHash(AQueryText, [
        ContentTypeFilter,
        SourceCategoryFilter,
        PreferCategory,
        DomainTagsFilter,
        SymbolTypeFilter,
        FrameworkFilter
      ]);

      // Cache hits are logged with cache_valid=0 (analytics only)
      // Cache misses are logged with cache_valid=1 (cache source)
      if ACacheHit then
        CacheValid := 0  // This is a cache hit (log for analytics, not a cache source)
      else
        CacheValid := 1; // This is a cache miss (becomes cache source)

      // Insert query log entry
      Query.SQL.Text :=
        'INSERT INTO query_log (' +
        '  query_text, query_hash, result_ids, cache_valid, ' +
        '  content_type_filter, source_category_filter, prefer_category, ' +
        '  domain_tags_filter, symbol_type_filter, ' +
        '  num_results_requested, max_distance, ' +
        '  use_semantic_search, use_reranker, candidate_count, ' +
        '  duration_ms, result_count ' +
        ') VALUES (' +
        '  :query_text, :query_hash, :result_ids, :cache_valid, ' +
        '  :content_type_filter, :source_category_filter, :prefer_category, ' +
        '  :domain_tags_filter, :symbol_type_filter, ' +
        '  :num_results, :max_distance, ' +
        '  :use_semantic, :use_reranker, :candidate_count, ' +
        '  :duration_ms, :result_count ' +
        ')';

      Query.ParamByName('query_text').AsString := AQueryText;
      Query.ParamByName('query_hash').AsString := QueryHash;
      Query.ParamByName('result_ids').AsString := AResultIDs;
      Query.ParamByName('cache_valid').AsInteger := CacheValid;

      // Filters - FireDAC requires DataType before Clear for nullable parameters
      if ContentTypeFilter <> '' then
        Query.ParamByName('content_type_filter').AsString := ContentTypeFilter
      else
      begin
        Query.ParamByName('content_type_filter').DataType := ftString;
        Query.ParamByName('content_type_filter').Clear;
      end;

      if SourceCategoryFilter <> '' then
        Query.ParamByName('source_category_filter').AsString := SourceCategoryFilter
      else
      begin
        Query.ParamByName('source_category_filter').DataType := ftString;
        Query.ParamByName('source_category_filter').Clear;
      end;

      if PreferCategory <> '' then
        Query.ParamByName('prefer_category').AsString := PreferCategory
      else
      begin
        Query.ParamByName('prefer_category').DataType := ftString;
        Query.ParamByName('prefer_category').Clear;
      end;

      if DomainTagsFilter <> '' then
        Query.ParamByName('domain_tags_filter').AsString := DomainTagsFilter
      else
      begin
        Query.ParamByName('domain_tags_filter').DataType := ftString;
        Query.ParamByName('domain_tags_filter').Clear;
      end;

      if SymbolTypeFilter <> '' then
        Query.ParamByName('symbol_type_filter').AsString := SymbolTypeFilter
      else
      begin
        Query.ParamByName('symbol_type_filter').DataType := ftString;
        Query.ParamByName('symbol_type_filter').Clear;
      end;

      // Search configuration
      Query.ParamByName('num_results').AsInteger := NumResults;
      Query.ParamByName('max_distance').AsFloat := MaxDistance;
      Query.ParamByName('use_semantic').AsInteger := Integer(UseSemanticSearch);
      Query.ParamByName('use_reranker').AsInteger := Integer(UseReranker);

      if UseReranker then
        Query.ParamByName('candidate_count').AsInteger := CandidateCount
      else
      begin
        Query.ParamByName('candidate_count').DataType := ftInteger;
        Query.ParamByName('candidate_count').Clear;
      end;

      // Performance and results
      Query.ParamByName('duration_ms').AsInteger := ADurationMs;
      Query.ParamByName('result_count').AsInteger := AResultCount;

      Query.ExecSQL;

    finally
      Query.Free;
      Connection.Free;
    end;

  except
    // Silently ignore logging errors - don't fail the search
    on E: Exception do
      WriteLn(Format('Warning: Failed to log query: %s', [E.Message]));
  end;
end;

function ExtractResultIDs(AResults: TSearchResultList): string;
var
  I: Integer;
  IDList: TStringList;
begin
  IDList := TStringList.Create;
  try
    for I := 0 to AResults.Count - 1 do
      IDList.Add(IntToStr(AResults[I].SymbolID));
    Result := IDList.CommaText;
  finally
    IDList.Free;
  end;
end;

function TryLoadFromCache(const ADatabaseFile, AQueryHash: string): TSearchResultList;
var
  Connection: TFDConnection;
  Query: TFDQuery;
  ResultIDs: string;
  IDList: TStringList;
  I: Integer;
  SearchResult: TSearchResult;
begin
  Result := nil;

  try
    Connection := TFDConnection.Create(nil);
    Query := TFDQuery.Create(nil);
    try
      TDatabaseConnectionHelper.ConfigureConnection(Connection, ADatabaseFile, False);
      Connection.Open;

      Query.Connection := Connection;

      // Enable WAL mode for concurrent access
      Query.SQL.Text := 'PRAGMA journal_mode=WAL';
      Query.ExecSQL;

      // Try to find a valid cache entry
      Query.SQL.Text :=
        'SELECT result_ids FROM query_log ' +
        'WHERE query_hash = :hash AND cache_valid = 1 ' +
        'ORDER BY executed_at DESC LIMIT 1';
      Query.ParamByName('hash').AsString := AQueryHash;
      Query.Open;

      if not Query.EOF then
      begin
        ResultIDs := Query.FieldByName('result_ids').AsString;

        if ResultIDs <> '' then
        begin
          Query.Close;

          // Load the actual symbols by IDs
          IDList := TStringList.Create;
          try
            IDList.CommaText := ResultIDs;

            Result := TSearchResultList.Create;

            for I := 0 to IDList.Count - 1 do
            begin
              Query.SQL.Text := 'SELECT * FROM symbols WHERE id = :id';
              Query.ParamByName('id').AsInteger := StrToIntDef(IDList[I], 0);
              Query.Open;

              if not Query.EOF then
              begin
                SearchResult := TSearchResult.Create;
                SearchResult.SymbolID := Query.FieldByName('id').AsInteger;
                SearchResult.Name := Query.FieldByName('name').AsString;
                SearchResult.FullName := Query.FieldByName('full_name').AsString;
                SearchResult.SymbolType := Query.FieldByName('type').AsString;
                SearchResult.FilePath := Query.FieldByName('file_path').AsString;
                SearchResult.Content := Query.FieldByName('content').AsString;
                SearchResult.Comments := Query.FieldByName('comments').AsString;
                SearchResult.ParentClass := Query.FieldByName('parent_class').AsString;
                SearchResult.ImplementedInterfaces := Query.FieldByName('implemented_interfaces').AsString;
                SearchResult.Visibility := Query.FieldByName('visibility').AsString;
                SearchResult.ContentType := Query.FieldByName('content_type').AsString;
                SearchResult.SourceCategory := Query.FieldByName('source_category').AsString;
                SearchResult.MatchType := 'cache_hit';
                SearchResult.Score := 1.0;

                Result.Add(SearchResult);
              end;

              Query.Close;
            end;
          finally
            IDList.Free;
          end;
        end;
      end;

      Query.Close;

    finally
      Query.Free;
      Connection.Free;
    end;

  except
    // Silently ignore cache lookup errors - fall back to normal search
    if Assigned(Result) then
      FreeAndNil(Result);
  end;
end;

procedure ShowUsage;
begin
  WriteLn('delphi-lookup - Fast symbol lookup for Delphi/Pascal source code');
  WriteLn;
  WriteLn('Usage: delphi-lookup.exe <query> [options]');
  WriteLn('   OR: delphi-lookup.exe @config.json <query> [options]');
  WriteLn;
  WriteLn('Arguments:');
  WriteLn('  query       : Search query (class name, method, concept, etc.)');
  WriteLn;
  WriteLn('Configuration:');
  WriteLn('  @<file>              : Load parameters from JSON/INI file');
  WriteLn('  --no-config          : Ignore default config file (delphi-lookup.json)');
  WriteLn('  -d, --database <file>: Database file (default: delphi_symbols.db)');
  WriteLn;
  WriteLn('Search Options:');
  WriteLn('  -n, --num-results <n>: Number of results (default: 5)');
  WriteLn('  --max-distance <val> : Max vector distance for semantic search (default: 1.5)');
  WriteLn('  --type <value>       : Filter by content type (code, help, markdown, comment)');
  WriteLn('  --symbol <value>     : Filter by symbol type (class, function, const, etc.)');
  WriteLn('  --category <value>   : Filter by source category (user, stdlib, third_party)');
  WriteLn('  --prefer <value>     : Boost specific category in results');
  WriteLn('  --domain <tag>       : Filter by domain tag');
  WriteLn('  --framework <value>  : Filter by framework (VCL, FMX, RTL)');
  WriteLn;
  WriteLn('Semantic Search:');
  WriteLn('  --semantic-search    : Enable semantic (vector) search');
  WriteLn('  --embedding-url <url>: Embedding service URL (reads from DB if not set)');
  WriteLn;
  WriteLn('Reranking:');
  WriteLn('  --use-reranker       : Enable two-stage reranking (~95% precision)');
  WriteLn('  --reranker-url <url> : Reranker service URL');
  WriteLn('  --candidates <n>     : Candidates for reranking (default: 50)');
  WriteLn;
  WriteLn('  -h, --help           : Show this help');
  WriteLn;
  WriteLn('Config File (delphi-lookup.json):');
  WriteLn('  If delphi-lookup.json exists next to the executable, it is loaded automatically.');
  WriteLn('  Command line options override config file values.');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  delphi-lookup.exe "TStringList"');
  WriteLn('  delphi-lookup.exe "JSON serialization" -n 10');
  WriteLn('  delphi-lookup.exe "TForm" --category user --framework VCL');
  WriteLn('  delphi-lookup.exe "validation" --use-reranker --candidates 100');
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

function ParseCommandLine: Boolean;
var
  EnvURL: string;
begin
  Result := False;

  // Check for help first
  if (ParamCount > 0) and ((ParamStr(1) = '-h') or (ParamStr(1) = '--help') or (ParamStr(1) = '/?')) then
  begin
    ShowUsage;
    Exit;
  end;

  if ParamCount = 0 then
  begin
    ShowUsage;
    Exit;
  end;

  // Initialize ParameterMAX
  InitializeParameterManager;

  // === Load configuration from PM ===

  // Database
  DatabaseFile := PM.GetParameter('database', PM.GetParameter('d', GetDefaultDatabasePath));
  if not TPath.IsPathRooted(DatabaseFile) then
    DatabaseFile := TPath.Combine(ExtractFilePath(ParamStr(0)), DatabaseFile);

  // Search options
  NumResults := PM.GetParameterAsInteger('num-results',
                  PM.GetParameterAsInteger('n',
                    PM.GetParameterAsInteger('num_results', DEFAULT_NUM_RESULTS)));
  if NumResults < 1 then NumResults := 1;
  if NumResults > MAX_NUM_RESULTS then NumResults := MAX_NUM_RESULTS;

  MaxDistance := PM.GetParameterAsFloat('max-distance',
                   PM.GetParameterAsFloat('max_distance', DEFAULT_MAX_DISTANCE));
  if MaxDistance < MIN_MAX_DISTANCE then MaxDistance := MIN_MAX_DISTANCE;
  if MaxDistance > MAX_MAX_DISTANCE then MaxDistance := MAX_MAX_DISTANCE;

  // Filters
  ContentTypeFilter := PM.GetParameter('type', PM.GetParameter('content_type', ''));
  SymbolTypeFilter := PM.GetParameter('symbol', PM.GetParameter('symbol_type', ''));
  SourceCategoryFilter := PM.GetParameter('category', PM.GetParameter('source_category', ''));
  PreferCategory := PM.GetParameter('prefer', PM.GetParameter('prefer_category', ''));
  DomainTagsFilter := PM.GetParameter('domain', PM.GetParameter('domain_tags', ''));
  FrameworkFilter := UpperCase(PM.GetParameter('framework', ''));

  // Semantic search
  UseSemanticSearch := PM.HasParameter('semantic-search') or
                       PM.HasParameter('enable-semantic') or
                       PM.GetParameterAsBoolean('semantic_search', False);

  // Embedding URL for semantic search
  EmbeddingURL := PM.GetParameter('embedding-url', PM.GetParameter('embedding_url', ''));
  if EmbeddingURL = '' then
  begin
    EnvURL := GetEmbeddingURLFromEnv;
    if EnvURL <> '' then
      EmbeddingURL := EnvURL;
  end;

  // Reranker
  UseReranker := PM.HasParameter('use-reranker') or
                 PM.HasParameter('rerank') or
                 PM.GetParameterAsBoolean('use_reranker', False);
  RerankerURL := PM.GetParameter('reranker-url', PM.GetParameter('reranker_url', ''));
  if RerankerURL = '' then
    RerankerURL := GetRerankerURLFromEnv;

  CandidateCount := PM.GetParameterAsInteger('candidates',
                      PM.GetParameterAsInteger('candidate_count', DEFAULT_RERANKER_CANDIDATE_COUNT));
  if CandidateCount < 10 then CandidateCount := 10;
  if CandidateCount > 200 then CandidateCount := 200;

  // Get query text (first positional argument)
  QueryText := GetFirstPositionalArg;

  if QueryText = '' then
  begin
    WriteLn('Error: Query text is required');
    ShowUsage;
    Exit;
  end;

  Result := True;
end;

procedure PerformSearch;
var
  SearchResults: TSearchResultList;
begin
  SearchResults := nil;

  try
    // Initialize components
    QueryProcessor := TQueryProcessor.Create;
    VectorSearch := nil;  // Will be created only if semantic search is enabled
    ResultFormatter := TResultFormatter.Create;

    try
      // Check if database file exists
      if not FileExists(DatabaseFile) then
      begin
        WriteLn(Format('Error: Database file "%s" not found.', [DatabaseFile]));
        WriteLn('Please run delphi-indexer.exe first to create the index.');
        Halt(1);
      end;

      // Initialize search components
      QueryProcessor.Initialize(DatabaseFile);

      // Apply filters to QueryProcessor
      QueryProcessor.ContentTypeFilter := ContentTypeFilter;
      QueryProcessor.SourceCategoryFilter := SourceCategoryFilter;
      QueryProcessor.PreferCategory := PreferCategory;
      QueryProcessor.DomainTagsFilter := DomainTagsFilter;
      QueryProcessor.SymbolTypeFilter := SymbolTypeFilter;
      QueryProcessor.FrameworkFilter := FrameworkFilter;

      // Initialize vector search (only if enabled)
      if UseSemanticSearch then
      begin
        WriteLn('WARNING: Semantic search enabled - results may have low precision (~40%).');
        WriteLn('         Traditional name matching is more accurate for this codebase.');
        WriteLn;

        // Load vec0 extension on QueryProcessor's connection
        WriteLn('Loading sqlite-vec extension for vector search...');
        TDatabaseConnectionHelper.LoadVec0Extension(QueryProcessor.Connection);
        WriteLn('sqlite-vec extension loaded successfully');

        // Use QueryProcessor's connection to avoid "schema locked" error
        VectorSearch := TVectorSearch.Create(QueryProcessor.Connection);
        try
          VectorSearch.Initialize(DatabaseFile, EmbeddingURL);
        except
          on E: Exception do
          begin
            WriteLn(Format('Warning: Vector search initialization failed: %s', [E.Message]));
            WriteLn('Continuing with symbolic search only...');
            FreeAndNil(VectorSearch);  // Ensure it's nil if initialization failed
          end;
        end;
      end;

      WriteLn(Format('// Context for query: "%s"', [QueryText]));
      WriteLn;

      // Try to load from cache first
      var QueryHash := GenerateQueryHash(QueryText, [
        ContentTypeFilter,
        SourceCategoryFilter,
        PreferCategory,
        DomainTagsFilter,
        SymbolTypeFilter,
        FrameworkFilter
      ]);

      // Start timing
      Stopwatch := TStopwatch.StartNew;

      SearchResults := TryLoadFromCache(DatabaseFile, QueryHash);

      if Assigned(SearchResults) then
      begin
        // Cache hit
        IsCacheHit := True;
        Stopwatch.Stop;
        WriteLn(Format('// [CACHE HIT] Loaded %d results from cache in %d ms',
          [SearchResults.Count, Stopwatch.ElapsedMilliseconds]));
        WriteLn;
      end
      else
      begin
        // Cache miss - perform full search
        IsCacheHit := False;

        // Perform search (with or without reranking)
        if UseReranker then
        begin
          WriteLn(Format('Using two-stage search (Stage 1: %d candidates, Stage 2: rerank to top %d)',
            [CandidateCount, NumResults]));
          WriteLn;
          SearchResults := QueryProcessor.PerformHybridSearchWithReranking(
            QueryText, NumResults, VectorSearch, True, CandidateCount, MaxDistance, RerankerURL);
        end
        else
          SearchResults := QueryProcessor.PerformHybridSearch(QueryText, NumResults, VectorSearch, MaxDistance);

        Stopwatch.Stop;
      end;

      // Format and output results
      ResultFormatter.FormatResults(SearchResults, QueryText);

      WriteLn;
      SearchDurationMs := Stopwatch.ElapsedMilliseconds;
      WriteLn(Format('// Search completed in %d ms', [SearchDurationMs]));

    finally
      // Free connections first to release database locks
      QueryProcessor.Free;
      if Assigned(VectorSearch) then
        VectorSearch.Free;
      ResultFormatter.Free;

      // Log query AFTER closing connections (to avoid lock conflicts)
      LogQuery(DatabaseFile, QueryText, SearchResults.Count, SearchDurationMs,
        ExtractResultIDs(SearchResults), IsCacheHit);

      if Assigned(SearchResults) then
        SearchResults.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn(Format('// Error: %s', [E.Message]));
      Halt(1);
    end;
  end;
end;

begin
  try
    if ParseCommandLine then
      PerformSearch;
  except
    on E: Exception do
    begin
      WriteLn(Format('Fatal error: %s', [E.Message]));
      Halt(1);
    end;
  end;
end.