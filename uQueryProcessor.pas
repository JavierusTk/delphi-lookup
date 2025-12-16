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

    function BuildFilterClause: string;
    function ApplyPreferenceBoost(AResult: TSearchResult): TSearchResult;
    function PerformExactSearch(const AQuery: string): TSearchResult;
    function PerformFuzzySearch(const AQuery: string; AMaxResults: Integer): TSearchResultList;
    function PerformFullTextSearch(const AQuery: string; AMaxResults: Integer): TSearchResultList;
    function CreateSearchResultFromQuery: TSearchResult;
    function SanitizeQuery(const AQuery: string): string;
    function ExtractKeywords(const AQuery: string): TStringList;
    function CalculateTextSimilarity(const AText1, AText2: string): Double;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Initialize(const ADatabaseFile: string);
    function PerformHybridSearch(const AQuery: string; AMaxResults: Integer;
      AVectorSearch: TObject; AMaxDistance: Double = 0.3): TSearchResultList;
    function PerformHybridSearchWithReranking(const AQuery: string; AMaxResults: Integer;
      AVectorSearch: TObject; AUseReranker: Boolean = True;
      ACandidateCount: Integer = 50; AMaxDistance: Double = 0.3;
      const ARerankerURL: string = ''): TSearchResultList;
    function IsValidQuery(const AQuery: string): Boolean;

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

procedure TQueryProcessor.Initialize(const ADatabaseFile: string);
begin
  try
    // Enable extensions for vec0 support (used by VectorSearch)
    TDatabaseConnectionHelper.ConfigureConnection(FConnection, ADatabaseFile, True);
    FConnection.Open;

    // Enable WAL mode for concurrent access
    FQuery.SQL.Text := 'PRAGMA journal_mode=WAL';
    FQuery.ExecSQL;

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

  // Read framework field if it exists
  try
    if FQuery.FindField('framework') <> nil then
      Result.Framework := FQuery.FieldByName('framework').AsString;
  except
    // Ignore if framework field doesn't exist (old database)
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
    // Try exact name match first
    FQuery.SQL.Text :=
      'SELECT * FROM symbols ' +
      'WHERE UPPER(name) = UPPER(:query) ' +
      BuildFilterClause +
      'ORDER BY ' +
      '  CASE type ' +
      '    WHEN ''class'' THEN 1 ' +
      '    WHEN ''interface'' THEN 2 ' +
      '    WHEN ''function'' THEN 3 ' +
      '    WHEN ''procedure'' THEN 4 ' +
      '    ELSE 5 ' +
      '  END ' +
      'LIMIT 1';
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
    
    // If no exact match, try partial name match
    if not Assigned(Result) then
    begin
      FQuery.SQL.Text :=
        'SELECT * FROM symbols ' +
        'WHERE UPPER(name) LIKE UPPER(:query) ' +
        BuildFilterClause +
        'ORDER BY ' +
        '  CASE ' +
        '    WHEN UPPER(name) LIKE UPPER(:query_start) THEN 1 ' +
        '    ELSE 2 ' +
        '  END, ' +
        '  LENGTH(name) ' +
        'LIMIT 1';
      FQuery.ParamByName('query').AsString := '%' + CleanQuery + '%';
      FQuery.ParamByName('query_start').AsString := CleanQuery + '%';
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
    // Search by name similarity
    FQuery.SQL.Text :=
      'SELECT * FROM symbols ' +
      'WHERE (UPPER(name) LIKE UPPER(:query) ' +
      '   OR UPPER(full_name) LIKE UPPER(:query) ' +
      '   OR UPPER(parent_class) LIKE UPPER(:query)) ' +
      BuildFilterClause +
      'ORDER BY ' +
      '  CASE ' +
      '    WHEN UPPER(name) LIKE UPPER(:query_exact) THEN 1 ' +
      '    WHEN UPPER(name) LIKE UPPER(:query_start) THEN 2 ' +
      '    WHEN UPPER(full_name) LIKE UPPER(:query_start) THEN 3 ' +
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
  SearchTerm: string;
begin
  Result := TSearchResultList.Create;
  
  if not FIsInitialized then
    raise Exception.Create('QueryProcessor not initialized');
    
  CleanQuery := SanitizeQuery(AQuery);
  Keywords := ExtractKeywords(CleanQuery);
  
  try
    if Keywords.Count > 0 then
    begin
      // Build LIKE search query
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
        'WHERE (UPPER(content) LIKE UPPER(:search_term) ' +
        '   OR UPPER(comments) LIKE UPPER(:search_term) ' +
        '   OR UPPER(name) LIKE UPPER(:search_term)) ' +
        BuildFilterClause +
        'ORDER BY ' +
        '  CASE ' +
        '    WHEN UPPER(name) LIKE UPPER(:search_term) THEN 1 ' +
        '    WHEN UPPER(comments) LIKE UPPER(:search_term) THEN 2 ' +
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
      AllResults.Add(ExactResult);

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

end.