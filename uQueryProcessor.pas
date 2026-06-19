unit uQueryProcessor;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.RegularExpressions,
  System.Variants,
  Data.DB,
  FireDAC.Comp.Client,
  uSearchTypes,
  uDatabaseConnection;

type

  TQueryProcessor = class
  private
    FConnection: TFDConnection;
    FQuery: TFDQuery;
    FIsInitialized: Boolean;
    FContentTypeFilter: string;
    FSourceCategoryFilter: string;
    FPreferCategory: string;
    FDomainTagsFilter: string;
    FSymbolTypeFilter: string;
    FFrameworkFilter: string;
    FHasIsDeclaration: Boolean;
    FFTS5Available: Boolean;

    function BuildFilterClause: string;
    function ApplyPreferenceBoost(AResult: TSearchResult): TSearchResult;
    function PerformExactSearch(const AQuery: string): TSearchResult;
    function FetchAllExactMatches(const AQuery: string; AMaxResults: Integer): TSearchResultList;
    function PerformFuzzySearch(const AQuery: string; AMaxResults: Integer): TSearchResultList;
    function PerformFullTextSearch(const AQuery: string; AMaxResults: Integer): TSearchResultList;
    function CreateSearchResultFromQuery: TSearchResult;
    function SanitizeQuery(const AQuery: string): string;
    function SanitizeFTS5Query(const AQuery: string): string;
    function ExtractKeywords(const AQuery: string): TStringList;
    function CalculateTextSimilarity(const AText1, AText2: string): Double;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Initialize(const ADatabaseFile: string; AReadOnly: Boolean = False);
    function PerformHybridSearch(const AQuery: string; AMaxResults: Integer;
      AVectorSearch: TObject; AMaxDistance: Double = 0.3): TSearchResultList;
    function PerformHybridSearchWithReranking(const AQuery: string; AMaxResults: Integer;
      AVectorSearch: TObject; AUseReranker: Boolean = True;
      ACandidateCount: Integer = 50; AMaxDistance: Double = 0.3;
      const ARerankerURL: string = ''): TSearchResultList;
    function IsValidQuery(const AQuery: string): Boolean;

    /// <summary>
    /// Get all symbols in a specific file (for textDocument/documentSymbol).
    /// </summary>
    function GetSymbolsByFile(const AFilePath: string): TSearchResultList;

    /// <summary>
    /// Find the symbol that contains the given line number.
    /// Returns nil if no symbol contains that line.
    /// </summary>
    function GetSymbolAtLine(const AFilePath: string; ALine: Integer): TSearchResult;

    /// <summary>
    /// Find symbol definition by exact name match (for textDocument/definition).
    /// Returns nil if not found.
    /// </summary>
    function FindSymbolDefinition(const ASymbolName: string): TSearchResult;

    /// <summary>
    /// Find all references to a symbol name in indexed code (for textDocument/references).
    /// Searches in content for usages of the symbol.
    /// </summary>
    function FindSymbolReferences(const ASymbolName: string; AMaxResults: Integer = 100): TSearchResultList;

    property ContentTypeFilter: string read FContentTypeFilter write FContentTypeFilter;
    property SourceCategoryFilter: string read FSourceCategoryFilter write FSourceCategoryFilter;
    property PreferCategory: string read FPreferCategory write FPreferCategory;
    property DomainTagsFilter: string read FDomainTagsFilter write FDomainTagsFilter;
    property SymbolTypeFilter: string read FSymbolTypeFilter write FSymbolTypeFilter;
    property FrameworkFilter: string read FFrameworkFilter write FFrameworkFilter;
    property Connection: TFDConnection read FConnection;
  end;

implementation

uses
  System.StrUtils,
  System.Math,
  FireDAC.Stan.Param,
  uVectorSearch,
  uReranker,
  uConfig;


{ TQueryProcessor }

constructor TQueryProcessor.Create;
begin
  inherited Create;
  FConnection := TFDConnection.Create(nil);
  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;
  FIsInitialized := False;
end;

destructor TQueryProcessor.Destroy;
begin
  FQuery.Free;
  FConnection.Free;
  inherited Destroy;
end;

procedure TQueryProcessor.Initialize(const ADatabaseFile: string; AReadOnly: Boolean = False);
begin
  try
    // Enable extensions for vec0 support (used by VectorSearch)
    TDatabaseConnectionHelper.ConfigureConnection(FConnection, ADatabaseFile, not AReadOnly);

    FConnection.Open;

    if AReadOnly then
    begin
      // Use PRAGMA query_only instead of OpenMode=ReadOnly
      // (ReadOnly + WAL causes "disk I/O error" when -shm/-wal files exist)
      FQuery.SQL.Text := 'PRAGMA query_only = ON';
      FQuery.ExecSQL;
    end
    else
    begin
      // Enable WAL mode for concurrent access
      FQuery.SQL.Text := 'PRAGMA journal_mode=WAL';
      FQuery.ExecSQL;
    end;

    // Detect if is_declaration column exists (added in v1.1.0)
    FQuery.SQL.Text := 'PRAGMA table_info(symbols)';
    FQuery.Open;
    FHasIsDeclaration := False;
    while not FQuery.EOF do
    begin
      if SameText(FQuery.FieldByName('name').AsString, 'is_declaration') then
      begin
        FHasIsDeclaration := True;
        Break;
      end;
      FQuery.Next;
    end;
    FQuery.Close;

    // Detect if FTS5 table is available and populated
    FFTS5Available := False;
    try
      FQuery.SQL.Text := 'SELECT rowid FROM symbols_fts LIMIT 1';
      FQuery.Open;
      FFTS5Available := not FQuery.EOF;
      FQuery.Close;
    except
      // FTS5 not available - will use LIKE fallback
    end;

    FIsInitialized := True;

  except
    on E: Exception do
      raise Exception.CreateFmt('Failed to initialize database: %s', [E.Message]);
  end;
end;

function TQueryProcessor.SanitizeQuery(const AQuery: string): string;
begin
  Result := Trim(AQuery);
  
  // Remove special characters that could interfere with SQL
  Result := StringReplace(Result, '''', '', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '', [rfReplaceAll]);
  Result := StringReplace(Result, ';', '', [rfReplaceAll]);
  
  // Collapse multiple spaces
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
end;

function TQueryProcessor.SanitizeFTS5Query(const AQuery: string): string;
var
  Keywords: TStringList;
  I: Integer;
begin
  Keywords := ExtractKeywords(AQuery);
  try
    Result := '';
    for I := 0 to Keywords.Count - 1 do
    begin
      if I > 0 then
        Result := Result + ' ';
      // Quote each keyword to prevent FTS5 operator interpretation (AND, OR, NOT, NEAR)
      Result := Result + '"' + Keywords[I] + '"';
    end;
  finally
    Keywords.Free;
  end;
end;

function TQueryProcessor.ExtractKeywords(const AQuery: string): TStringList;
var
  Matches: TMatchCollection;
  Match: TMatch;
  Keyword: string;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := True;
  
  // Extract words (including Pascal identifiers)
  Matches := TRegEx.Matches(AQuery, '\b[A-Za-z_]\w*\b');
  for Match in Matches do
  begin
    Keyword := Match.Value;
    if Length(Keyword) > 2 then // Skip very short words
      Result.Add(LowerCase(Keyword));
  end;
end;

function TQueryProcessor.CalculateTextSimilarity(const AText1, AText2: string): Double;
var
  Keywords1, Keywords2: TStringList;
  I, CommonCount: Integer;
  Text1Lower, Text2Lower: string;
begin
  Result := 0.0;
  
  Text1Lower := LowerCase(AText1);
  Text2Lower := LowerCase(AText2);
  
  // Simple substring match bonus
  if Pos(Text1Lower, Text2Lower) > 0 then
    Result := Result + 0.5;
    
  // Keyword-based similarity
  Keywords1 := ExtractKeywords(AText1);
  Keywords2 := ExtractKeywords(AText2);
  try
    CommonCount := 0;
    for I := 0 to Keywords1.Count - 1 do
    begin
      if Keywords2.IndexOf(Keywords1[I]) >= 0 then
        Inc(CommonCount);
    end;
    
    if Keywords1.Count > 0 then
      Result := Result + (CommonCount / Keywords1.Count) * 0.5;
      
  finally
    Keywords1.Free;
    Keywords2.Free;
  end;
end;

function TQueryProcessor.BuildFilterClause: string;
begin
  Result := '';

  if FContentTypeFilter <> '' then
    Result := Result + Format(' AND content_type = %s', [QuotedStr(FContentTypeFilter)]);

  if FSourceCategoryFilter <> '' then
    Result := Result + Format(' AND source_category = %s', [QuotedStr(FSourceCategoryFilter)]);

  if FDomainTagsFilter <> '' then
    Result := Result + Format(' AND domain_tags LIKE %s', [QuotedStr('%' + FDomainTagsFilter + '%')]);

  if FSymbolTypeFilter <> '' then
    Result := Result + Format(' AND type = %s', [QuotedStr(FSymbolTypeFilter)]);

  if FFrameworkFilter <> '' then
    Result := Result + Format(' AND (framework = %s OR framework IS NULL)', [QuotedStr(FFrameworkFilter)]);
end;

function TQueryProcessor.ApplyPreferenceBoost(AResult: TSearchResult): TSearchResult;
begin
  Result := AResult;

  // Apply 20% boost if the result matches preferred category
  if (FPreferCategory <> '') and (AResult.SourceCategory = FPreferCategory) then
    Result.Score := Result.Score * 1.2;
end;

function TQueryProcessor.CreateSearchResultFromQuery: TSearchResult;
begin
  Result := TSearchResult.Create;

  Result.SymbolID := FQuery.FieldByName('id').AsInteger;
  Result.Name := FQuery.FieldByName('name').AsString;
  Result.FullName := FQuery.FieldByName('full_name').AsString;
  Result.SymbolType := FQuery.FieldByName('type').AsString;
  Result.FilePath := FQuery.FieldByName('file_path').AsString;
  Result.Content := FQuery.FieldByName('content').AsString;
  Result.Comments := FQuery.FieldByName('comments').AsString;
  Result.ParentClass := FQuery.FieldByName('parent_class').AsString;
  Result.ImplementedInterfaces := FQuery.FieldByName('implemented_interfaces').AsString;
  Result.Visibility := FQuery.FieldByName('visibility').AsString;
  Result.ContentType := FQuery.FieldByName('content_type').AsString;
  Result.SourceCategory := FQuery.FieldByName('source_category').AsString;

  // Read optional fields if they exist
  try
    if FQuery.FindField('framework') <> nil then
      Result.Framework := FQuery.FieldByName('framework').AsString;
    if FQuery.FindField('start_line') <> nil then
      Result.StartLine := FQuery.FieldByName('start_line').AsInteger;
    if FQuery.FindField('end_line') <> nil then
      Result.EndLine := FQuery.FieldByName('end_line').AsInteger;
    if FQuery.FindField('is_declaration') <> nil then
      Result.IsDeclaration := FQuery.FieldByName('is_declaration').AsInteger = 1;
  except
    // Ignore if optional fields don't exist (old database)
  end;

  // Apply preference boost if needed
  Result := ApplyPreferenceBoost(Result);
end;

function TQueryProcessor.PerformExactSearch(const AQuery: string): TSearchResult;
var
  CleanQuery: string;
begin
  Result := nil;
  
  if not FIsInitialized then
    raise Exception.Create('QueryProcessor not initialized');
    
  CleanQuery := SanitizeQuery(AQuery);
  
  try
    // Cascade: exact NOCASE → prefix NOCASE → LIKE substring
    // Uses idx_symbols_name_nocase for SEARCH (18x faster than UPPER() SCAN)

    // 1. Exact name match (COLLATE NOCASE uses index)
    FQuery.SQL.Text :=
      'SELECT * FROM symbols ' +
      'WHERE name = :query COLLATE NOCASE ' +
      BuildFilterClause +
      'ORDER BY ' +
      '  CASE type ' +
      '    WHEN ''class'' THEN 1 ' +
      '    WHEN ''interface'' THEN 2 ' +
      '    WHEN ''function'' THEN 3 ' +
      '    WHEN ''procedure'' THEN 4 ' +
      '    ELSE 5 ' +
      '  END' +
      IfThen(FHasIsDeclaration, ', is_declaration DESC ') +
      ' LIMIT 1';
    FQuery.ParamByName('query').AsString := CleanQuery;
    FQuery.Open;

    if not FQuery.EOF then
    begin
      Result := CreateSearchResultFromQuery;
      Result.IsExactMatch := True;
      Result.MatchType := 'exact_name';
      Result.Score := 1.0;
    end;

    FQuery.Close;

    // 2. Prefix match (LIKE 'prefix%' COLLATE NOCASE uses index range scan)
    if not Assigned(Result) then
    begin
      FQuery.SQL.Text :=
        'SELECT * FROM symbols ' +
        'WHERE name LIKE :query_start COLLATE NOCASE ' +
        BuildFilterClause +
        'ORDER BY LENGTH(name) ' +
        'LIMIT 1';
      FQuery.ParamByName('query_start').AsString := CleanQuery + '%';
      FQuery.Open;

      if not FQuery.EOF then
      begin
        Result := CreateSearchResultFromQuery;
        Result.IsExactMatch := False;
        Result.MatchType := 'prefix_name';
        Result.Score := 0.9;
      end;

      FQuery.Close;
    end;

    // 3. Substring match (LIKE '%sub%' requires scan but without UPPER() overhead)
    if not Assigned(Result) then
    begin
      FQuery.SQL.Text :=
        'SELECT * FROM symbols ' +
        'WHERE name LIKE :query COLLATE NOCASE ' +
        BuildFilterClause +
        'ORDER BY LENGTH(name) ' +
        'LIMIT 1';
      FQuery.ParamByName('query').AsString := '%' + CleanQuery + '%';
      FQuery.Open;

      if not FQuery.EOF then
      begin
        Result := CreateSearchResultFromQuery;
        Result.IsExactMatch := False;
        Result.MatchType := 'partial_name';
        Result.Score := 0.8;
      end;

      FQuery.Close;
    end;
    
  except
    on E: Exception do
      raise Exception.CreateFmt('Exact search failed: %s', [E.Message]);
  end;
end;

function TQueryProcessor.FetchAllExactMatches(const AQuery: string; AMaxResults: Integer): TSearchResultList;
var
  SearchResult: TSearchResult;
begin
  Result := TSearchResultList.Create;

  try
    // Fetch ALL symbols with this exact name (overloads, decl+impl).
    // Uses idx_symbols_name_nocase → SEARCH, not SCAN. ~1ms for any count.
    //
    // Ordering tiers (lower wins):
    //   1. type weight (class > interface > function > procedure)
    //   2. is_declaration DESC (interface decls before implementations)
    //   3. full_name dot count ASC — standalone symbols (full_name='Foo') before
    //      class methods (full_name='TBar.Foo'). Without this tier, the canonical
    //      standalone CrearQuery in VCL\DBdata\TableMax.Query.pas was buried under
    //      4 class-method overrides in Clientes\GMC\*.pas that share the same name.
    //   4. 'override;' content marker — demote subclass overrides below the symbol
    //      they override.
    //   5. 'abstract;' content marker — demote abstract-base declarations below
    //      concrete declarations.
    //   6. file_path (stable tie-break)
    FQuery.SQL.Text :=
      'SELECT * FROM symbols ' +
      'WHERE name = :query COLLATE NOCASE ' +
      BuildFilterClause +
      'ORDER BY ' +
      '  CASE type ' +
      '    WHEN ''class'' THEN 1 ' +
      '    WHEN ''interface'' THEN 2 ' +
      '    WHEN ''function'' THEN 3 ' +
      '    WHEN ''procedure'' THEN 4 ' +
      '    ELSE 5 ' +
      '  END' +
      IfThen(FHasIsDeclaration, ', is_declaration DESC ') +
      ', LENGTH(COALESCE(full_name, name, '''')) ' +
      '  - LENGTH(REPLACE(COALESCE(full_name, name, ''''), ''.'', '''')) ASC ' +
      ', CASE WHEN content LIKE ''%override;%'' THEN 1 ELSE 0 END ASC ' +
      ', CASE WHEN content LIKE ''%abstract;%'' THEN 1 ELSE 0 END ASC ' +
      ', file_path ' +
      'LIMIT :max_results';
    FQuery.ParamByName('query').AsString := AQuery;
    FQuery.ParamByName('max_results').AsInteger := AMaxResults;
    FQuery.Open;

    while not FQuery.EOF do
    begin
      SearchResult := CreateSearchResultFromQuery;
      SearchResult.IsExactMatch := True;
      SearchResult.MatchType := 'exact_name';
      SearchResult.Score := 1.0;
      Result.Add(SearchResult);
      FQuery.Next;
    end;

    FQuery.Close;

  except
    on E: Exception do
    begin
      Result.Free;
      raise Exception.CreateFmt('FetchAllExactMatches failed: %s', [E.Message]);
    end;
  end;
end;

function TQueryProcessor.PerformFuzzySearch(const AQuery: string; AMaxResults: Integer): TSearchResultList;
var
  CleanQuery: string;
  SearchResult: TSearchResult;
begin
  Result := TSearchResultList.Create;
  
  if not FIsInitialized then
    raise Exception.Create('QueryProcessor not initialized');
    
  CleanQuery := SanitizeQuery(AQuery);
  
  try
    // Search by name similarity (COLLATE NOCASE — uses indexes for name, full_name, parent_class)
    FQuery.SQL.Text :=
      'SELECT * FROM symbols ' +
      'WHERE (name LIKE :query COLLATE NOCASE ' +
      '   OR full_name LIKE :query COLLATE NOCASE ' +
      '   OR parent_class LIKE :query COLLATE NOCASE) ' +
      BuildFilterClause +
      'ORDER BY ' +
      '  CASE ' +
      '    WHEN name = :query_exact COLLATE NOCASE THEN 1 ' +
      '    WHEN name LIKE :query_start COLLATE NOCASE THEN 2 ' +
      '    WHEN full_name LIKE :query_start COLLATE NOCASE THEN 3 ' +
      '    ELSE 4 ' +
      '  END, ' +
      '  LENGTH(name) ' +
      'LIMIT :max_results';

    FQuery.ParamByName('query').AsString := '%' + CleanQuery + '%';
    FQuery.ParamByName('query_exact').AsString := CleanQuery;
    FQuery.ParamByName('query_start').AsString := CleanQuery + '%';
    FQuery.ParamByName('max_results').AsInteger := AMaxResults;
    FQuery.Open;
    
    while not FQuery.EOF do
    begin
      SearchResult := CreateSearchResultFromQuery;
      SearchResult.MatchType := 'fuzzy_name';
      SearchResult.Score := CalculateTextSimilarity(CleanQuery, SearchResult.Name);
      Result.Add(SearchResult);
      FQuery.Next;
    end;
    
    FQuery.Close;
    
  except
    on E: Exception do
    begin
      Result.Free;
      raise Exception.CreateFmt('Fuzzy search failed: %s', [E.Message]);
    end;
  end;
end;

function TQueryProcessor.PerformFullTextSearch(const AQuery: string; AMaxResults: Integer): TSearchResultList;
var
  CleanQuery: string;
  SearchResult: TSearchResult;
  Keywords: TStringList;
  I: Integer;
  SearchTerm, FTSQuery: string;
  UsedFTS5: Boolean;
begin
  Result := TSearchResultList.Create;

  if not FIsInitialized then
    raise Exception.Create('QueryProcessor not initialized');

  CleanQuery := SanitizeQuery(AQuery);
  Keywords := ExtractKeywords(CleanQuery);

  try
    if Keywords.Count = 0 then
      Exit;

    UsedFTS5 := False;

    // Try FTS5 first (much faster for content/comments search on large tables)
    if FFTS5Available then
    begin
      FTSQuery := SanitizeFTS5Query(CleanQuery);
      if FTSQuery <> '' then
      begin
        try
          // FTS5 MATCH searches across name, full_name, content, comments columns
          // Using subquery to avoid column name ambiguity with BuildFilterClause
          FQuery.SQL.Text :=
            'SELECT s.* FROM symbols s ' +
            'INNER JOIN (' +
            '  SELECT rowid, rank FROM symbols_fts WHERE symbols_fts MATCH :fts_query' +
            ') fts ON s.id = fts.rowid ' +
            'WHERE 1=1 ' +
            BuildFilterClause +
            'ORDER BY fts.rank ' +
            'LIMIT :max_results';

          FQuery.ParamByName('fts_query').AsString := FTSQuery;
          FQuery.ParamByName('max_results').AsInteger := AMaxResults;
          FQuery.Open;

          while not FQuery.EOF do
          begin
            SearchResult := CreateSearchResultFromQuery;
            SearchResult.MatchType := 'full_text_fts5';
            SearchResult.Score := CalculateTextSimilarity(CleanQuery, SearchResult.Comments + ' ' + SearchResult.Content);
            Result.Add(SearchResult);
            FQuery.Next;
          end;

          FQuery.Close;
          UsedFTS5 := Result.Count > 0;
        except
          // FTS5 query failed, fall through to LIKE
          try FQuery.Close; except end;
        end;
      end;
    end;

    // Fallback: LIKE-based search (when FTS5 unavailable, failed, or returned 0 results)
    // This catches compound identifiers like "ControlStock" that FTS5 tokenizes
    // as a single token but LIKE can match as substring
    if not UsedFTS5 then
    begin
      SearchTerm := '';
      for I := 0 to Keywords.Count - 1 do
      begin
        if I > 0 then
          SearchTerm := SearchTerm + '%';
        SearchTerm := SearchTerm + Keywords[I];
      end;
      SearchTerm := '%' + SearchTerm + '%';

      FQuery.SQL.Text :=
        'SELECT * FROM symbols ' +
        'WHERE (content LIKE :search_term ' +
        '   OR comments LIKE :search_term ' +
        '   OR name LIKE :search_term COLLATE NOCASE) ' +
        BuildFilterClause +
        'ORDER BY ' +
        '  CASE ' +
        '    WHEN name LIKE :search_term COLLATE NOCASE THEN 1 ' +
        '    WHEN comments LIKE :search_term THEN 2 ' +
        '    ELSE 3 ' +
        '  END ' +
        'LIMIT :max_results';

      FQuery.ParamByName('search_term').AsString := SearchTerm;
      FQuery.ParamByName('max_results').AsInteger := AMaxResults;
      FQuery.Open;

      while not FQuery.EOF do
      begin
        SearchResult := CreateSearchResultFromQuery;
        SearchResult.MatchType := 'full_text';
        SearchResult.Score := CalculateTextSimilarity(CleanQuery, SearchResult.Comments + ' ' + SearchResult.Content);
        Result.Add(SearchResult);
        FQuery.Next;
      end;

      FQuery.Close;
    end;

  finally
    Keywords.Free;
  end;
end;

function TQueryProcessor.IsValidQuery(const AQuery: string): Boolean;
var
  CleanQuery: string;
begin
  CleanQuery := Trim(AQuery);
  Result := (Length(CleanQuery) >= 2) and (Length(CleanQuery) <= 200);
end;

function TQueryProcessor.PerformHybridSearch(const AQuery: string; AMaxResults: Integer;
  AVectorSearch: TObject; AMaxDistance: Double = 0.3): TSearchResultList;
var
  ExactResult: TSearchResult;
  FuzzyResults: TSearchResultList;
  FTSResults: TSearchResultList;
  VectorResults: TSearchResultList;
  AllResults: TSearchResultList;
begin
  if not IsValidQuery(AQuery) then
    raise Exception.Create('Invalid query: must be 2-200 characters');

  AllResults := TSearchResultList.Create;

  try
    // 1. Exact search (highest priority)
    ExactResult := PerformExactSearch(AQuery);
    if Assigned(ExactResult) then
    begin
      // Short-circuit: if exact name match, fetch ALL symbols with that name
      // (overloads, declaration+implementation) but skip expensive fuzzy/FTS.
      // Still ~1ms via NOCASE index. Covers 83% of production queries.
      if ExactResult.IsExactMatch then
      begin
        ExactResult.Free;
        AllResults.Free;
        AllResults := FetchAllExactMatches(SanitizeQuery(AQuery), AMaxResults);
        Result := AllResults;
        AllResults := nil;
        Exit;
      end;
      AllResults.Add(ExactResult);
    end;

    // 2. Fuzzy name search
    FuzzyResults := PerformFuzzySearch(AQuery, AMaxResults);
    try
      // Use Extract from the end to avoid index issues
      while FuzzyResults.Count > 0 do
        AllResults.Add(FuzzyResults.Extract(FuzzyResults[FuzzyResults.Count - 1]));
    finally
      FuzzyResults.Free;
    end;

    // 3. Full-text search
    FTSResults := PerformFullTextSearch(AQuery, AMaxResults);
    try
      // Use Extract from the end to avoid index issues
      while FTSResults.Count > 0 do
        AllResults.Add(FTSResults.Extract(FTSResults[FTSResults.Count - 1]));
    finally
      FTSResults.Free;
    end;

    // 4. Vector similarity search (with configurable max distance)
    if Assigned(AVectorSearch) then
    begin
      VectorResults := (AVectorSearch as TVectorSearch).SearchSimilar(AQuery, AMaxResults, AMaxDistance);
      try
        // Use Extract from the end to avoid index issues
        while VectorResults.Count > 0 do
          AllResults.Add(VectorResults.Extract(VectorResults[VectorResults.Count - 1]));
      finally
        VectorResults.Free;
      end;
    end;
    
    // Remove duplicates and sort by relevance
    AllResults.RemoveDuplicates;
    AllResults.SortByRelevance;
    
    // Limit results
    while AllResults.Count > AMaxResults do
      AllResults.Delete(AllResults.Count - 1);
    
    Result := AllResults;
    AllResults := nil; // Transfer ownership
    
  finally
    if Assigned(AllResults) then
      AllResults.Free;
  end;
end;

function TQueryProcessor.PerformHybridSearchWithReranking(const AQuery: string;
  AMaxResults: Integer; AVectorSearch: TObject; AUseReranker: Boolean;
  ACandidateCount: Integer; AMaxDistance: Double;
  const ARerankerURL: string): TSearchResultList;
var
  Candidates: TSearchResultList;
  Reranker: TJinaReranker;
  RerankedResults: TSearchResultList;
begin
  RerankedResults := nil;

  if not IsValidQuery(AQuery) then
    raise Exception.Create('Invalid query: must be 2-200 characters');

  // Stage 1: Get candidates using hybrid search (embeddings + exact/fuzzy/FTS)
  WriteLn(Format('Stage 1: Fetching top %d candidates...', [ACandidateCount]));
  Candidates := PerformHybridSearch(AQuery, ACandidateCount, AVectorSearch, AMaxDistance);

  try
    // If reranker is disabled or not enough candidates, return as-is
    if not AUseReranker or (Candidates.Count <= AMaxResults) then
    begin
      WriteLn(Format('Returning %d results without reranking', [Min(Candidates.Count, AMaxResults)]));

      // Limit to AMaxResults
      while Candidates.Count > AMaxResults do
        Candidates.Delete(Candidates.Count - 1);

      Result := Candidates;
      Candidates := nil; // Transfer ownership
      Exit;
    end;

    // Stage 2: Rerank the candidates
    WriteLn(Format('Stage 2: Reranking %d candidates to get top %d...', [Candidates.Count, AMaxResults]));

    Reranker := TJinaReranker.Create(ARerankerURL, DEFAULT_RERANKER_TIMEOUT);
    try
      RerankedResults := Reranker.RerankDocuments(AQuery, Candidates, AMaxResults);

      if Assigned(RerankedResults) and (RerankedResults.Count > 0) then
      begin
        WriteLn(Format('Reranking successful: %d results', [RerankedResults.Count]));
        Result := RerankedResults;
        RerankedResults := nil; // Transfer ownership
      end
      else
      begin
        WriteLn('Reranking failed, falling back to original candidates');

        // Fallback: return original candidates
        while Candidates.Count > AMaxResults do
          Candidates.Delete(Candidates.Count - 1);

        Result := Candidates;
        Candidates := nil; // Transfer ownership
      end;

    finally
      Reranker.Free;
      if Assigned(RerankedResults) then
        RerankedResults.Free;
    end;

  finally
    if Assigned(Candidates) then
      Candidates.Free;
  end;
end;

function TQueryProcessor.GetSymbolsByFile(const AFilePath: string): TSearchResultList;
var
  SearchResult: TSearchResult;
begin
  Result := TSearchResultList.Create;

  if not FIsInitialized then
    raise Exception.Create('QueryProcessor not initialized');

  try
    FQuery.SQL.Text :=
      'SELECT * FROM symbols ' +
      'WHERE file_path = :file_path ' +
      'ORDER BY start_line, name';
    FQuery.ParamByName('file_path').AsString := AFilePath;
    FQuery.Open;

    while not FQuery.EOF do
    begin
      SearchResult := CreateSearchResultFromQuery;
      SearchResult.MatchType := 'file_symbol';
      SearchResult.Score := 1.0;
      Result.Add(SearchResult);
      FQuery.Next;
    end;

    FQuery.Close;

  except
    on E: Exception do
    begin
      Result.Free;
      raise Exception.CreateFmt('GetSymbolsByFile failed: %s', [E.Message]);
    end;
  end;
end;

function TQueryProcessor.GetSymbolAtLine(const AFilePath: string; ALine: Integer): TSearchResult;
begin
  Result := nil;

  if not FIsInitialized then
    raise Exception.Create('QueryProcessor not initialized');

  try
    // Find the symbol that contains the given line (1-indexed in DB, 0-indexed from LSP)
    // We add 1 to convert from LSP 0-indexed to DB 1-indexed
    FQuery.SQL.Text :=
      'SELECT * FROM symbols ' +
      'WHERE file_path = :file_path ' +
      '  AND start_line <= :line ' +
      '  AND (end_line >= :line OR end_line IS NULL) ' +
      'ORDER BY start_line DESC ' +  // Innermost symbol first
      'LIMIT 1';
    FQuery.ParamByName('file_path').AsString := AFilePath;
    FQuery.ParamByName('line').AsInteger := ALine + 1;  // Convert to 1-indexed
    FQuery.Open;

    if not FQuery.EOF then
    begin
      Result := CreateSearchResultFromQuery;
      Result.MatchType := 'line_match';
      Result.Score := 1.0;
    end;

    FQuery.Close;

  except
    on E: Exception do
      raise Exception.CreateFmt('GetSymbolAtLine failed: %s', [E.Message]);
  end;
end;

function TQueryProcessor.FindSymbolDefinition(const ASymbolName: string): TSearchResult;
begin
  // Reuse existing exact search - it already does what we need
  Result := PerformExactSearch(ASymbolName);
end;

function TQueryProcessor.FindSymbolReferences(const ASymbolName: string; AMaxResults: Integer): TSearchResultList;
var
  CleanName: string;
  SearchResult: TSearchResult;
  FTSQuery: string;
  UsedFTS5: Boolean;
begin
  Result := TSearchResultList.Create;

  if not FIsInitialized then
    raise Exception.Create('QueryProcessor not initialized');

  CleanName := SanitizeQuery(ASymbolName);
  if CleanName = '' then
    Exit;

  try
    UsedFTS5 := False;

    // Try FTS5 first (faster and more accurate word boundary matching than LIKE)
    // FTS5 tokenizer properly handles Pascal delimiters (.:;,()[] etc.)
    // while LIKE word-boundary patterns only handle spaces
    if FFTS5Available then
    begin
      try
        // Search for symbol name as token in content column
        // Replace underscores with spaces for proper FTS5 phrase matching
        // (unicode61 tokenizer splits on underscores)
        FTSQuery := 'content:"' + StringReplace(CleanName, '_', ' ', [rfReplaceAll]) + '"';

        FQuery.SQL.Text :=
          'SELECT s.* FROM symbols s ' +
          'WHERE (' +
          '  s.id IN (' +
          '    SELECT rowid FROM symbols_fts WHERE symbols_fts MATCH :fts_query' +
          '  )' +
          '  OR s.name = :name' +
          ') ' +
          BuildFilterClause +
          'ORDER BY ' +
          '  CASE WHEN s.name = :name THEN 0 ELSE 1 END, ' +
          '  s.file_path, s.start_line ' +
          'LIMIT :max_results';

        FQuery.ParamByName('fts_query').AsString := FTSQuery;
        FQuery.ParamByName('name').AsString := CleanName;
        FQuery.ParamByName('max_results').AsInteger := AMaxResults;
        FQuery.Open;

        while not FQuery.EOF do
        begin
          SearchResult := CreateSearchResultFromQuery;
          if SameText(SearchResult.Name, CleanName) then
            SearchResult.MatchType := 'definition'
          else
            SearchResult.MatchType := 'reference';
          SearchResult.Score := 0.8;
          Result.Add(SearchResult);
          FQuery.Next;
        end;

        FQuery.Close;
        UsedFTS5 := True;
      except
        // FTS5 query failed, fall through to LIKE
        try FQuery.Close; except end;
      end;
    end;

    // Fallback: LIKE-based search with space-based word boundary patterns
    if not UsedFTS5 then
    begin
      FQuery.SQL.Text :=
        'SELECT * FROM symbols ' +
        'WHERE (' +
        '  content LIKE :pattern1 ' +        // word at start
        '  OR content LIKE :pattern2 ' +     // word in middle
        '  OR content LIKE :pattern3 ' +     // word at end
        '  OR name = :name ' +               // definition itself
        ') ' +
        BuildFilterClause +
        'ORDER BY ' +
        '  CASE WHEN name = :name THEN 0 ELSE 1 END, ' +
        '  file_path, start_line ' +
        'LIMIT :max_results';

      FQuery.ParamByName('pattern1').AsString := CleanName + ' %';
      FQuery.ParamByName('pattern2').AsString := '% ' + CleanName + ' %';
      FQuery.ParamByName('pattern3').AsString := '% ' + CleanName;
      FQuery.ParamByName('name').AsString := CleanName;
      FQuery.ParamByName('max_results').AsInteger := AMaxResults;
      FQuery.Open;

      while not FQuery.EOF do
      begin
        SearchResult := CreateSearchResultFromQuery;
        if SameText(SearchResult.Name, CleanName) then
          SearchResult.MatchType := 'definition'
        else
          SearchResult.MatchType := 'reference';
        SearchResult.Score := 0.8;
        Result.Add(SearchResult);
        FQuery.Next;
      end;

      FQuery.Close;
    end;

  except
    on E: Exception do
    begin
      Result.Free;
      raise Exception.CreateFmt('FindSymbolReferences failed: %s', [E.Message]);
    end;
  end;
end;

end.