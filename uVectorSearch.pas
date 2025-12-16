unit uVectorSearch;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Math,
  System.Variants,
  Data.DB,
  FireDAC.Comp.Client,
  uConfig,
  uSearchTypes,
  uLookupEmbeddings.Ollama,
  uDatabaseConnection;

type
  TVectorSearch = class
  private
    FConnection: TFDConnection;
    FOwnsConnection: Boolean;
    FQuery: TFDQuery;
    FEmbeddingGenerator: TOllamaEmbeddingGenerator;
    FIsInitialized: Boolean;
    FOllamaURL: string;
    FOllamaModel: string;
    FEmbeddingDimensions: Integer;

    function CreateSearchResultFromQuery: TSearchResult;
    procedure LoadExtension;
    function ReadMetadata(const AKey: string): string;

  public
    constructor Create; overload;
    constructor Create(AExternalConnection: TFDConnection); overload;
    destructor Destroy; override;

    procedure Initialize(const ADatabaseFile: string; const AOllamaURL: string = '');
    function SearchSimilar(const AQuery: string; AMaxResults: Integer; AMaxDistance: Double = 0.3): TSearchResultList;

    property IsInitialized: Boolean read FIsInitialized;
    property EmbeddingModel: string read FOllamaModel;
    property EmbeddingDimensions: Integer read FEmbeddingDimensions;
  end;

implementation

uses
  System.IOUtils,
  FireDAC.Stan.Param;

{ TVectorSearch }

constructor TVectorSearch.Create;
begin
  inherited Create;
  FConnection := TFDConnection.Create(nil);
  FOwnsConnection := True;
  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;
  FEmbeddingGenerator := nil;
  FIsInitialized := False;
  FOllamaURL := '';
  FOllamaModel := '';
end;

constructor TVectorSearch.Create(AExternalConnection: TFDConnection);
begin
  inherited Create;
  FConnection := AExternalConnection;
  FOwnsConnection := False;
  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;
  FEmbeddingGenerator := nil;
  FIsInitialized := False;
  FOllamaURL := '';
  FOllamaModel := '';
end;

destructor TVectorSearch.Destroy;
begin
  if Assigned(FEmbeddingGenerator) then
    FEmbeddingGenerator.Free;
  FQuery.Free;
  if FOwnsConnection then
    FConnection.Free;
  inherited Destroy;
end;

procedure TVectorSearch.LoadExtension;
begin
  try
    WriteLn('Loading sqlite-vec extension for vector search...');
    TDatabaseConnectionHelper.LoadVec0Extension(FConnection);
    WriteLn('sqlite-vec extension loaded successfully');
  except
    on E: Exception do
    begin
      WriteLn(Format('Failed to load sqlite-vec extension: %s', [E.Message]));
      raise;
    end;
  end;
end;

function TVectorSearch.ReadMetadata(const AKey: string): string;
begin
  Result := '';
  FQuery.SQL.Text := 'SELECT value FROM metadata WHERE key = :key';
  FQuery.ParamByName('key').AsString := AKey;
  FQuery.Open;
  try
    if not FQuery.EOF then
      Result := FQuery.FieldByName('value').AsString;
  finally
    FQuery.Close;
  end;
end;

procedure TVectorSearch.Initialize(const ADatabaseFile: string; const AOllamaURL: string = '');
begin
  try
    // If we own the connection, configure and open it
    // If external connection, it should already be open with vec0 loaded
    if FOwnsConnection then
    begin
      TDatabaseConnectionHelper.ConfigureConnection(FConnection, ADatabaseFile, True);
      FConnection.Open;
      // Load sqlite-vec extension only for owned connections
      LoadExtension;
    end;

    // Read model configuration from database metadata
    FOllamaModel := ReadMetadata('embedding_model');
    if FOllamaModel = '' then
      raise Exception.Create('Database metadata missing: embedding_model');

    WriteLn(Format('Database indexed with model: %s', [FOllamaModel]));

    // Read embedding dimensions from metadata
    var DimensionsStr := ReadMetadata('embedding_dimensions');
    if DimensionsStr = '' then
      raise Exception.Create('Database metadata missing: embedding_dimensions');

    FEmbeddingDimensions := StrToInt(DimensionsStr);
    WriteLn(Format('Database embedding dimensions: %d', [FEmbeddingDimensions]));

    // Use Ollama URL from parameter or metadata (parameter takes precedence)
    if AOllamaURL <> '' then
      FOllamaURL := AOllamaURL
    else
    begin
      FOllamaURL := ReadMetadata('ollama_url');
      if FOllamaURL = '' then
        FOllamaURL := ReadMetadata('embedding_url'); // Try new name
      if FOllamaURL = '' then
        FOllamaURL := GetEmbeddingURLFromEnv; // Fallback to environment variable
    end;

    WriteLn(Format('Using Ollama server: %s', [FOllamaURL]));

    // Initialize Ollama embedding generator with model from database
    FEmbeddingGenerator := TOllamaEmbeddingGenerator.Create(FOllamaURL, FOllamaModel);

    FIsInitialized := True;
    WriteLn('Vector search initialized successfully');

  except
    on E: Exception do
    begin
      FIsInitialized := False;
      raise Exception.CreateFmt('Failed to initialize vector search: %s', [E.Message]);
    end;
  end;
end;

function TVectorSearch.CreateSearchResultFromQuery: TSearchResult;
begin
  Result := TSearchResult.Create;

  Result.SymbolID := FQuery.FieldByName('symbol_id').AsInteger;
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

  // Distance field for vector similarity
  if not FQuery.FieldByName('distance').IsNull then
  begin
    var Distance := FQuery.FieldByName('distance').AsFloat;
    Result.Score := 1.0 - Distance; // Convert distance to similarity
  end
  else
    Result.Score := 0.0;

  Result.IsExactMatch := False;
  Result.MatchType := 'vector_similarity';
end;

function TVectorSearch.SearchSimilar(const AQuery: string; AMaxResults: Integer; AMaxDistance: Double = 0.3): TSearchResultList;
var
  QueryEmbedding: TEmbedding;
  VectorJSON: string;
  I: Integer;
  SearchResult: TSearchResult;
  MinSimilarity: Double;
begin
  Result := TSearchResultList.Create;

  if not FIsInitialized then
  begin
    WriteLn('Warning: Vector search not initialized, skipping');
    Exit;
  end;

  // Convert max distance to minimum similarity score
  MinSimilarity := 1.0 - AMaxDistance;
  WriteLn(Format('Vector search: max_distance=%.2f (min_similarity=%.2f)', [AMaxDistance, MinSimilarity]));

  try
    WriteLn(Format('Generating embedding for query: "%s"', [AQuery]));

    // Generate embedding for the query using Ollama
    QueryEmbedding := FEmbeddingGenerator.GenerateEmbedding(AQuery);
    try
      if not Assigned(QueryEmbedding) then
      begin
        WriteLn('Warning: Failed to generate query embedding');
        Exit;
      end;

      // Validate dimensions match database
      if Length(QueryEmbedding.Vector) <> FEmbeddingDimensions then
      begin
        WriteLn(Format('Warning: Dimension mismatch - query: %d, database: %d',
          [Length(QueryEmbedding.Vector), FEmbeddingDimensions]));
        WriteLn('This may indicate the model changed. Please re-index the database.');
        Exit;
      end;

      // Convert vector to JSON array string
      // IMPORTANT: Use US format settings to ensure period as decimal separator
      var USFormat: TFormatSettings := TFormatSettings.Create('en-US');
      VectorJSON := '[';
      for I := 0 to Length(QueryEmbedding.Vector) - 1 do
      begin
        if I > 0 then
          VectorJSON := VectorJSON + ',';
        VectorJSON := VectorJSON + FloatToStr(QueryEmbedding.Vector[I], USFormat);
      end;
      VectorJSON := VectorJSON + ']';

      // Perform vector similarity search using sqlite-vec
      // vec_distance_L2 is the L2 (Euclidean) distance function from sqlite-vec
      // Debug: Check how many vectors are in the database (DISABLED - too verbose)
      // FQuery.SQL.Text := 'SELECT COUNT(*) as vec_count FROM vec_embeddings';
      // FQuery.Open;
      // WriteLn(Format('[DEBUG-VEC-SEARCH] Total vectors in database: %d', [FQuery.FieldByName('vec_count').AsInteger]));
      // FQuery.Close;

      FQuery.SQL.Text := Format('''
        SELECT
          v.symbol_id,
          s.name,
          s.full_name,
          s.type,
          s.file_path,
          s.content,
          s.comments,
          s.parent_class,
          s.implemented_interfaces,
          s.visibility,
          s.content_type,
          s.source_category,
          vec_distance_L2(v.embedding, %s) as distance
        FROM vec_embeddings v
        JOIN symbols s ON v.symbol_id = s.id
        WHERE v.embedding MATCH %s
          AND k = %d
        ORDER BY distance
      ''', [QuotedStr(VectorJSON), QuotedStr(VectorJSON), AMaxResults]);

      // WriteLn('[DEBUG-VEC-SEARCH] Executing vector search query with k=', AMaxResults);
      FQuery.Open;
      // WriteLn(Format('[DEBUG-VEC-SEARCH] Query returned %d rows (before similarity filtering)', [FQuery.RecordCount]));

      while not FQuery.EOF do
      begin
        SearchResult := CreateSearchResultFromQuery;

        // Apply minimum similarity threshold (based on max distance parameter)
        if SearchResult.Score > MinSimilarity then
          Result.Add(SearchResult)
        else
          SearchResult.Free;

        FQuery.Next;
      end;

      FQuery.Close;

      WriteLn(Format('Vector search returned %d results', [Result.Count]));

    finally
      QueryEmbedding.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn(Format('Warning: Vector search failed: %s', [E.Message]));
      // Don't raise - allow other search methods to work
    end;
  end;
end;

end.
