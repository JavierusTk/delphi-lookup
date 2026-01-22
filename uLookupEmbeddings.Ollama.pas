unit uLookupEmbeddings.Ollama;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.JSON,
  System.Diagnostics,
  System.RegularExpressions;

type
  // Vector representation - using Single for memory efficiency
  TVector = TArray<Single>;

  TEmbedding = class
  private
    FChunkID: string;
    FVector: TVector;
    FText: string;
    FDimensions: Integer;
  public
    constructor Create(const AChunkID, AText: string; const AVector: TVector);

    property ChunkID: string read FChunkID;
    property Vector: TVector read FVector;
    property Text: string read FText;
    property Dimensions: Integer read FDimensions;
  end;

  /// <summary>Cache entry for query embeddings</summary>
  TEmbeddingCacheEntry = record
    Vector: TVector;
    Timestamp: TDateTime;
  end;

  TOllamaEmbeddingGenerator = class
  private
    FOllamaURL: string;
    FModelName: string;
    FHttpClient: THTTPClient;
    FQueryCache: TDictionary<string, TEmbeddingCacheEntry>;
    FCacheMaxAge: Double;  // Max age in days (default: 1 hour = 1/24)
    FCacheEnabled: Boolean;
    FCacheHitCount: Integer;
    FCacheMissCount: Integer;

    function CallOllamaAPI(const AText: string): TVector;
    function FormatQueryText(const AQuery: string): string;
    function LooksLikeIdentifier(const AText: string): Boolean;
    function GetCachedVector(const AQuery: string): TVector;
    procedure CacheVector(const AQuery: string; const AVector: TVector);
    procedure CleanExpiredCache;

  public
    constructor Create(const AOllamaURL: string; const AModelName: string);
    destructor Destroy; override;

    function GenerateEmbedding(const AText: string): TEmbedding;
    procedure ClearCache;

    property OllamaURL: string read FOllamaURL write FOllamaURL;
    property ModelName: string read FModelName write FModelName;
    property CacheEnabled: Boolean read FCacheEnabled write FCacheEnabled;
    property CacheHitCount: Integer read FCacheHitCount;
    property CacheMissCount: Integer read FCacheMissCount;
  end;

implementation

uses
  System.Math,
  System.DateUtils;

{ TEmbedding }

constructor TEmbedding.Create(const AChunkID, AText: string; const AVector: TVector);
begin
  inherited Create;
  FChunkID := AChunkID;
  FText := AText;
  FVector := Copy(AVector);
  FDimensions := Length(FVector);
end;

{ TOllamaEmbeddingGenerator }

constructor TOllamaEmbeddingGenerator.Create(const AOllamaURL: string; const AModelName: string);
begin
  inherited Create;
  FOllamaURL := AOllamaURL;
  FModelName := AModelName;
  FHttpClient := THTTPClient.Create;
  FHttpClient.ConnectionTimeout := 30000;  // 30 seconds
  FHttpClient.ResponseTimeout := 120000;   // 2 minutes

  // Initialize query embedding cache
  FQueryCache := TDictionary<string, TEmbeddingCacheEntry>.Create;
  FCacheMaxAge := 1 / 24;  // 1 hour in days
  FCacheEnabled := True;
  FCacheHitCount := 0;
  FCacheMissCount := 0;
end;

destructor TOllamaEmbeddingGenerator.Destroy;
begin
  FQueryCache.Free;
  FHttpClient.Free;
  inherited Destroy;
end;

function TOllamaEmbeddingGenerator.LooksLikeIdentifier(const AText: string): Boolean;
var
  Words: TArray<string>;
begin
  // An identifier typically:
  // - Is a single word (no spaces)
  // - Starts with T, F, I, E (Delphi conventions) or uppercase letter
  // - Uses PascalCase or has underscores
  // - Contains no special query words

  Words := AText.Split([' '], TStringSplitOptions.ExcludeEmpty);

  // Multiple words = description, not identifier
  if Length(Words) > 2 then
    Exit(False);

  // Single word checks
  if Length(Words) = 1 then
  begin
    // Typical Delphi identifier patterns
    // T prefix (TStringList), F prefix (FMyField), I prefix (IInterface), E prefix (EException)
    if TRegEx.IsMatch(AText, '^[TFIE][A-Z][a-zA-Z0-9_]*$') then
      Exit(True);

    // PascalCase without common prefix (Calculate, ShowMessage)
    if TRegEx.IsMatch(AText, '^[A-Z][a-z]+[A-Z][a-zA-Z0-9]*$') then
      Exit(True);

    // Simple uppercase start, no spaces (MyFunction, Button1)
    if TRegEx.IsMatch(AText, '^[A-Z][a-zA-Z0-9_]+$') and (Length(AText) <= 40) then
      Exit(True);
  end;

  // Two words could be "TMyClass.Method" style
  if Length(Words) = 2 then
  begin
    // Class.Method pattern
    if TRegEx.IsMatch(AText, '^[A-Z][a-zA-Z0-9_]*\.[A-Z][a-zA-Z0-9_]*$') then
      Exit(True);
  end;

  Result := False;
end;

function TOllamaEmbeddingGenerator.FormatQueryText(const AQuery: string): string;
var
  CleanQuery: string;
begin
  CleanQuery := Trim(AQuery);

  if CleanQuery = '' then
    Exit(CleanQuery);

  // Format query similar to indexed documents to improve semantic matching
  // This addresses the asymmetry between indexed content and search queries

  if LooksLikeIdentifier(CleanQuery) then
  begin
    // Query looks like an identifier: "TStringList", "Calculate", "TForm.Show"
    // Format as NAME to match the indexed format
    Result := 'NAME: ' + CleanQuery;
  end
  else
  begin
    // Query is a description: "convert string to integer", "validate input"
    // Format as DESCRIPTION to match the indexed format
    Result := 'DESCRIPTION: ' + CleanQuery;
  end;
end;

function TOllamaEmbeddingGenerator.GetCachedVector(const AQuery: string): TVector;
var
  Entry: TEmbeddingCacheEntry;
begin
  SetLength(Result, 0);

  if not FCacheEnabled then
    Exit;

  if FQueryCache.TryGetValue(LowerCase(AQuery), Entry) then
  begin
    // Check if entry is still valid
    if Now - Entry.Timestamp < FCacheMaxAge then
    begin
      Result := Entry.Vector;
      Inc(FCacheHitCount);
    end
    else
    begin
      // Expired entry - remove it
      FQueryCache.Remove(LowerCase(AQuery));
    end;
  end;
end;

procedure TOllamaEmbeddingGenerator.CacheVector(const AQuery: string; const AVector: TVector);
var
  Entry: TEmbeddingCacheEntry;
begin
  if not FCacheEnabled then
    Exit;

  Entry.Vector := Copy(AVector);
  Entry.Timestamp := Now;

  FQueryCache.AddOrSetValue(LowerCase(AQuery), Entry);
end;

procedure TOllamaEmbeddingGenerator.CleanExpiredCache;
var
  Key: string;
  Entry: TEmbeddingCacheEntry;
  KeysToRemove: TList<string>;
begin
  KeysToRemove := TList<string>.Create;
  try
    for Key in FQueryCache.Keys do
    begin
      if FQueryCache.TryGetValue(Key, Entry) then
      begin
        if Now - Entry.Timestamp >= FCacheMaxAge then
          KeysToRemove.Add(Key);
      end;
    end;

    for Key in KeysToRemove do
      FQueryCache.Remove(Key);
  finally
    KeysToRemove.Free;
  end;
end;

procedure TOllamaEmbeddingGenerator.ClearCache;
begin
  FQueryCache.Clear;
  FCacheHitCount := 0;
  FCacheMissCount := 0;
end;

function TOllamaEmbeddingGenerator.CallOllamaAPI(const AText: string): TVector;
var
  URL: string;
  RequestJSON: TJSONObject;
  ResponseJSON: TJSONObject;
  RequestStream: TStringStream;
  ResponseStream: TMemoryStream;
  ResponseText: string;
  EmbeddingArray: TJSONArray;
  I: Integer;
begin
  SetLength(Result, 0);

  URL := FOllamaURL + '/api/embed';

  RequestJSON := TJSONObject.Create;
  try
    RequestJSON.AddPair('model', FModelName);
    RequestJSON.AddPair('input', AText);

    RequestStream := TStringStream.Create(RequestJSON.ToString, TEncoding.UTF8);
    try
      ResponseStream := TMemoryStream.Create;
      try
        FHttpClient.Post(URL, RequestStream, ResponseStream);

        ResponseStream.Position := 0;
        SetLength(ResponseText, ResponseStream.Size);
        if ResponseStream.Size > 0 then
        begin
          var Bytes: TBytes;
          SetLength(Bytes, ResponseStream.Size);
          ResponseStream.ReadBuffer(Bytes[0], ResponseStream.Size);
          ResponseText := TEncoding.UTF8.GetString(Bytes);
        end;

        ResponseJSON := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;
        if Assigned(ResponseJSON) then
        try
          // Check if we have embeddings array
          if ResponseJSON.TryGetValue<TJSONArray>('embeddings', EmbeddingArray) then
          begin
            // Take first embedding (we only sent one input)
            if EmbeddingArray.Count > 0 then
            begin
              var FirstEmbedding := EmbeddingArray.Items[0] as TJSONArray;
              SetLength(Result, FirstEmbedding.Count);

              for I := 0 to FirstEmbedding.Count - 1 do
                Result[I] := FirstEmbedding.Items[I].AsType<Double>;
            end;
          end
          // Fallback: check for single embedding array
          else if ResponseJSON.TryGetValue<TJSONArray>('embedding', EmbeddingArray) then
          begin
            SetLength(Result, EmbeddingArray.Count);
            for I := 0 to EmbeddingArray.Count - 1 do
              Result[I] := EmbeddingArray.Items[I].AsType<Double>;
          end;

        finally
          ResponseJSON.Free;
        end;

      finally
        ResponseStream.Free;
      end;
    finally
      RequestStream.Free;
    end;
  finally
    RequestJSON.Free;
  end;
end;

function TOllamaEmbeddingGenerator.GenerateEmbedding(const AText: string): TEmbedding;
var
  Vector: TVector;
  FormattedQuery: string;
  CacheKey: string;
begin
  Result := nil;

  if Trim(AText) = '' then
    Exit;

  try
    // Format query to match indexed document format (fixes asymmetry issue)
    FormattedQuery := FormatQueryText(AText);
    CacheKey := FormattedQuery;  // Cache by formatted query

    // Check cache first
    Vector := GetCachedVector(CacheKey);

    if Length(Vector) > 0 then
    begin
      // Cache hit - return cached embedding
      WriteLn(Format('Cache hit for query (hits: %d, misses: %d)', [FCacheHitCount, FCacheMissCount]));
      Result := TEmbedding.Create('cached', AText, Vector);
      Exit;
    end;

    // Cache miss - call Ollama API with formatted query
    Inc(FCacheMissCount);
    WriteLn(Format('Calling Ollama API with formatted query: "%s"', [FormattedQuery]));

    Vector := CallOllamaAPI(FormattedQuery);

    if Length(Vector) > 0 then
    begin
      // Cache the result
      CacheVector(CacheKey, Vector);
      Result := TEmbedding.Create('single', AText, Vector);
    end;

  except
    on E: Exception do
    begin
      WriteLn(Format('Error generating embedding: %s', [E.Message]));
      Result := nil;
    end;
  end;
end;

end.
