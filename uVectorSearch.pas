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
  /// <summary>Type of distance function used for vector similarity</summary>
  TDistanceFunction = (dfL2, dfCosine);

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
    FDistanceFunction: TDistanceFunction;

    function CreateSearchResultFromQuery(ADistance: Double): TSearchResult;
    procedure LoadExtension;
    procedure ReadAllMetadata(out AModel, ADimensions, AOllamaURL: string);
    function DetectDistanceFunction: TDistanceFunction;
    function DistanceToScore(ADistance: Double): Double;
    function GetDistanceFunctionSQL: string;

  public
    constructor Create; overload;
    constructor Create(AExternalConnection: TFDConnection); overload;
    destructor Destroy; override;

    procedure Initialize(const ADatabaseFile: string; const AOllamaURL: string = '');
    function SearchSimilar(const AQuery: string; AMaxResults: Integer; AMaxDistance: Double = 0.3): TSearchResultList;

    property IsInitialized: Boolean read FIsInitialized;
    property EmbeddingModel: string read FOllamaModel;
    property EmbeddingDimensions: Integer read FEmbeddingDimensions;
    property DistanceFunction: TDistanceFunction read FDistanceFunction;
  end;

/// <summary>Convert vector to JSON array string efficiently using TStringBuilder</summary>
function VectorToJSON(const AVector: TArray<Single>): string;

implementation

uses
  System.IOUtils,
  FireDAC.Stan.Param;

var
  /// <summary>Global US format settings for consistent decimal separator</summary>
  GUSFormat: TFormatSettings;

function VectorToJSON(const AVector: TArray<Single>): string;
var
  Builder: TStringBuilder;
  I: Integer;
begin
  // Pre-allocate approximate size: 1536 dims * ~15 chars per float
  Builder := TStringBuilder.Create(Length(AVector) * 16);
  try
    Builder.Append('[');
    for I := 0 to High(AVector) do
    begin
      if I > 0 then
        Builder.Append(',');
      Builder.Append(FloatToStr(AVector[I], GUSFormat));
    end;
    Builder.Append(']');
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

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

procedure TVectorSearch.ReadAllMetadata(out AModel, ADimensions, AOllamaURL: string);
begin
  // Consolidate 3 queries into 1 for better performance
  AModel := '';
  ADimensions := '';
  AOllamaURL := '';

  FQuery.SQL.Text := 'SELECT key, value FROM metadata WHERE key IN ' +
    '(''embedding_model'', ''embedding_dimensions'', ''ollama_url'', ''embedding_url'')';
  FQuery.Open;
  try
    while not FQuery.EOF do
    begin
      var Key := FQuery.FieldByName('key').AsString;
      var Value := FQuery.FieldByName('value').AsString;

      if Key = 'embedding_model' then
        AModel := Value
      else if Key = 'embedding_dimensions' then
        ADimensions := Value
      else if (Key = 'ollama_url') or (Key = 'embedding_url') then
      begin
        if AOllamaURL = '' then  // ollama_url takes precedence
          AOllamaURL := Value;
      end;

      FQuery.Next;
    end;
  finally
    FQuery.Close;
  end;
end;

function TVectorSearch.DetectDistanceFunction: TDistanceFunction;
begin
  // Try vec_distance_cosine first (better for normalized embeddings)
  try
    FQuery.SQL.Text := 'SELECT vec_distance_cosine(''[1,0]'', ''[0,1]'') as dist';
    FQuery.Open;
    FQuery.Close;
    Result := dfCosine;
    WriteLn('Using cosine distance (optimal for normalized embeddings)');
  except
    // Fallback to L2 if cosine not supported
    Result := dfL2;
    WriteLn('Using L2 distance (cosine not available in this sqlite-vec version)');
  end;
end;

function TVectorSearch.GetDistanceFunctionSQL: string;
begin
  case FDistanceFunction of
    dfCosine: Result := 'vec_distance_cosine';
    dfL2: Result := 'vec_distance_L2';
  else
    Result := 'vec_distance_L2';
  end;
end;

function TVectorSearch.DistanceToScore(ADistance: Double): Double;
begin
  // Convert distance to similarity score in range [0, 1]
  case FDistanceFunction of
    dfCosine:
      begin
        // vec_distance_cosine returns 1 - cosine_similarity
        // Range: [0, 2] where 0 = identical, 2 = opposite
        // Convert to score: 1.0 - (distance / 2.0)
        Result := 1.0 - (ADistance / 2.0);
      end;

    dfL2:
      begin
        // L2 distance for normalized vectors
        // Range: [0, 2] where 0 = identical
        // Use exponential decay for smooth scoring
        Result := Exp(-ADistance);

        // Alternative: Convert L2 to cosine for normalized vectors
        // CosineSim := 1.0 - (ADistance * ADistance / 2.0);
        // Result := (CosineSim + 1.0) / 2.0;
      end;

  else
    // Safe fallback
    Result := Exp(-ADistance);
  end;

  // Ensure result is in [0, 1]
  Result := Max(0.0, Min(1.0, Result));
end;

procedure TVectorSearch.Initialize(const ADatabaseFile: string; const AOllamaURL: string = '');
var
  ModelStr, DimensionsStr, MetadataURL: string;
begin
  try
    // If we own the connection, configure and open it
    // If external connection, it should already be open with vec0 loaded
    if FOwnsConnection then
    begin
      TDatabaseConnectionHelper.ConfigureConnection(FConnection, ADatabaseFile, True);
      FConnection.Open;
      // Enable WAL mode for concurrent access (required for parallel search)
      FQuery.SQL.Text := 'PRAGMA journal_mode=WAL';
      FQuery.ExecSQL;
      // Load sqlite-vec extension only for owned connections
      LoadExtension;
    end;

    // Read all metadata in a single query (optimization)
    ReadAllMetadata(ModelStr, DimensionsStr, MetadataURL);

    // Validate required metadata
    if ModelStr = '' then
      raise Exception.Create('Database metadata missing: embedding_model');
    FOllamaModel := ModelStr;
    WriteLn(Format('Database indexed with model: %s', [FOllamaModel]));

    if DimensionsStr = '' then
      raise Exception.Create('Database metadata missing: embedding_dimensions');
    FEmbeddingDimensions := StrToInt(DimensionsStr);
    WriteLn(Format('Database embedding dimensions: %d', [FEmbeddingDimensions]));

    // Use Ollama URL from parameter, metadata, or environment (in order of precedence)
    if AOllamaURL <> '' then
      FOllamaURL := AOllamaURL
    else if MetadataURL <> '' then
      FOllamaURL := MetadataURL
    else
      FOllamaURL := GetEmbeddingURLFromEnv;

    WriteLn(Format('Using Ollama server: %s', [FOllamaURL]));

    // Detect best distance function (cosine preferred for normalized embeddings)
    FDistanceFunction := DetectDistanceFunction;

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

function TVectorSearch.CreateSearchResultFromQuery(ADistance: Double): TSearchResult;
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

  // Convert distance to score using proper formula
  Result.Score := DistanceToScore(ADistance);

  Result.IsExactMatch := False;
  Result.MatchType := 'vector_similarity';
end;

function TVectorSearch.SearchSimilar(const AQuery: string; AMaxResults: Integer; AMaxDistance: Double = 0.3): TSearchResultList;
var
  QueryEmbedding: TEmbedding;
  VectorJSON: string;
  SearchResult: TSearchResult;
  MinScore: Double;
  Distance: Double;
  DistFunc: string;
begin
  Result := TSearchResultList.Create;

  if not FIsInitialized then
  begin
    WriteLn('Warning: Vector search not initialized, skipping');
    Exit;
  end;

  // Convert max distance to minimum score using proper formula
  MinScore := DistanceToScore(AMaxDistance);
  WriteLn(Format('Vector search: max_distance=%.2f (min_score=%.2f)', [AMaxDistance, MinScore]));

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

      // Convert vector to JSON array string efficiently
      VectorJSON := VectorToJSON(QueryEmbedding.Vector);

      // Get the appropriate distance function
      DistFunc := GetDistanceFunctionSQL;

      // Perform vector similarity search using sqlite-vec
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
          %s(v.embedding, %s) as distance
        FROM vec_embeddings v
        JOIN symbols s ON v.symbol_id = s.id
        WHERE v.embedding MATCH %s
          AND k = %d
        ORDER BY distance
      ''', [DistFunc, QuotedStr(VectorJSON), QuotedStr(VectorJSON), AMaxResults * 2]);

      FQuery.Open;

      while not FQuery.EOF do
      begin
        Distance := FQuery.FieldByName('distance').AsFloat;
        SearchResult := CreateSearchResultFromQuery(Distance);

        // Apply minimum score threshold
        if SearchResult.Score >= MinScore then
          Result.Add(SearchResult)
        else
          SearchResult.Free;

        FQuery.Next;
      end;

      FQuery.Close;

      WriteLn(Format('Vector search returned %d results (filtered by min_score=%.2f)',
        [Result.Count, MinScore]));

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

initialization
  // Initialize global US format settings once at startup
  GUSFormat := TFormatSettings.Create('en-US');

end.
