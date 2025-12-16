unit uReranker;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.JSON,
  uSearchTypes,
  uConfig;

type
  TJinaReranker = class
  private
    FRerankerURL: string;
    FHttpClient: THTTPClient;
    FTimeout: Integer;

    function CallRerankerAPI(const AQuery: string; const ACandidates: TSearchResultList; const ATopK: Integer): TSearchResultList;
    function BuildRequestJSON(const AQuery: string; const ACandidates: TSearchResultList): string;
    function ParseRankingsResponse(const AResponseJSON: string; const ACandidates: TSearchResultList; const ATopK: Integer): TSearchResultList;

  public
    constructor Create(const ARerankerURL: string = ''; const ATimeout: Integer = DEFAULT_RERANKER_TIMEOUT);
    destructor Destroy; override;

    function RerankDocuments(const AQuery: string; const ACandidates: TSearchResultList; const ATopK: Integer = 10): TSearchResultList;

    property RerankerURL: string read FRerankerURL write FRerankerURL;
    property Timeout: Integer read FTimeout write FTimeout;
  end;

implementation

uses
  System.Math;

{ TJinaReranker }

constructor TJinaReranker.Create(const ARerankerURL: string; const ATimeout: Integer);
begin
  inherited Create;
  FRerankerURL := ARerankerURL;
  FTimeout := ATimeout;
  FHttpClient := THTTPClient.Create;
  FHttpClient.ConnectionTimeout := FTimeout;
  FHttpClient.ResponseTimeout := FTimeout;
end;

destructor TJinaReranker.Destroy;
begin
  FHttpClient.Free;
  inherited Destroy;
end;

function TJinaReranker.BuildRequestJSON(const AQuery: string; const ACandidates: TSearchResultList): string;
var
  RequestJSON: TJSONObject;
  DocumentsArray: TJSONArray;
  I: Integer;
  Candidate: TSearchResult;
begin
  RequestJSON := TJSONObject.Create;
  try
    RequestJSON.AddPair('query', AQuery);

    DocumentsArray := TJSONArray.Create;
    for I := 0 to ACandidates.Count - 1 do
    begin
      Candidate := ACandidates[I];
      // Send the content text for reranking
      // For better context, include name and type in the document text
      DocumentsArray.Add(Format('%s (%s)' + sLineBreak + '%s',
        [Candidate.Name, Candidate.SymbolType, Candidate.Content]));
    end;

    RequestJSON.AddPair('documents', DocumentsArray);
    Result := RequestJSON.ToString;
  finally
    RequestJSON.Free;
  end;
end;

function TJinaReranker.ParseRankingsResponse(const AResponseJSON: string;
  const ACandidates: TSearchResultList; const ATopK: Integer): TSearchResultList;
var
  ResponseJSON: TJSONObject;
  RankingsArray: TJSONArray;
  RankingItem: TJSONObject;
  I: Integer;
  Score: Double;
  DocumentIndex: Integer;
  NewResult: TSearchResult;
  OriginalResult: TSearchResult;
begin
  Result := TSearchResultList.Create(True); // Owns objects

  ResponseJSON := TJSONObject.ParseJSONValue(AResponseJSON) as TJSONObject;
  if not Assigned(ResponseJSON) then
  begin
    WriteLn('Error: Failed to parse reranker response JSON');
    Exit;
  end;

  try
    if not ResponseJSON.TryGetValue<TJSONArray>('rankings', RankingsArray) then
    begin
      WriteLn('Error: No "rankings" array in reranker response');
      Exit;
    end;

    // Process top K rankings
    for I := 0 to Min(RankingsArray.Count - 1, ATopK - 1) do
    begin
      RankingItem := RankingsArray.Items[I] as TJSONObject;

      // Extract score
      if not RankingItem.TryGetValue<Double>('score', Score) then
      begin
        WriteLn(Format('Warning: No score in ranking item %d', [I]));
        Continue;
      end;

      // Extract document index (rankings are returned in order, so index = I)
      // But the API might include an "index" field, let's check
      if not RankingItem.TryGetValue<Integer>('index', DocumentIndex) then
        DocumentIndex := I; // Fallback to position in array

      // Validate index
      if (DocumentIndex < 0) or (DocumentIndex >= ACandidates.Count) then
      begin
        WriteLn(Format('Warning: Invalid document index %d (candidates: %d)', [DocumentIndex, ACandidates.Count]));
        Continue;
      end;

      // Get original candidate
      OriginalResult := ACandidates[DocumentIndex];

      // Create new result with updated score
      NewResult := TSearchResult.Create;
      NewResult.SymbolID := OriginalResult.SymbolID;
      NewResult.Name := OriginalResult.Name;
      NewResult.FullName := OriginalResult.FullName;
      NewResult.SymbolType := OriginalResult.SymbolType;
      NewResult.FilePath := OriginalResult.FilePath;
      NewResult.Content := OriginalResult.Content;
      NewResult.Comments := OriginalResult.Comments;
      NewResult.ParentClass := OriginalResult.ParentClass;
      NewResult.ImplementedInterfaces := OriginalResult.ImplementedInterfaces;
      NewResult.Visibility := OriginalResult.Visibility;
      NewResult.Score := Score;
      NewResult.IsExactMatch := OriginalResult.IsExactMatch;
      NewResult.MatchType := 'reranked';
      NewResult.ContentType := OriginalResult.ContentType;
      NewResult.SourceCategory := OriginalResult.SourceCategory;

      Result.Add(NewResult);
    end;

    WriteLn(Format('Reranker returned %d results from %d candidates', [Result.Count, ACandidates.Count]));

  finally
    ResponseJSON.Free;
  end;
end;

function TJinaReranker.CallRerankerAPI(const AQuery: string;
  const ACandidates: TSearchResultList; const ATopK: Integer): TSearchResultList;
var
  URL: string;
  RequestJSON: string;
  RequestStream: TStringStream;
  ResponseStream: TMemoryStream;
  ResponseText: string;
  Bytes: TBytes;
begin
  Result := TSearchResultList.Create(True); // Owns objects

  if ACandidates.Count = 0 then
  begin
    WriteLn('Warning: No candidates to rerank');
    Exit;
  end;

  URL := FRerankerURL + '/rank';

  // Build request JSON
  RequestJSON := BuildRequestJSON(AQuery, ACandidates);

  RequestStream := TStringStream.Create(RequestJSON, TEncoding.UTF8);
  try
    ResponseStream := TMemoryStream.Create;
    try
      // Send POST request
      FHttpClient.Post(URL, RequestStream, ResponseStream);

      // Read response
      ResponseStream.Position := 0;
      SetLength(Bytes, ResponseStream.Size);
      if ResponseStream.Size > 0 then
      begin
        ResponseStream.ReadBuffer(Bytes[0], ResponseStream.Size);
        ResponseText := TEncoding.UTF8.GetString(Bytes);
      end;

      // Parse response
      Result := ParseRankingsResponse(ResponseText, ACandidates, ATopK);

    finally
      ResponseStream.Free;
    end;
  finally
    RequestStream.Free;
  end;
end;

function TJinaReranker.RerankDocuments(const AQuery: string;
  const ACandidates: TSearchResultList; const ATopK: Integer): TSearchResultList;
begin
  Result := nil;

  if Trim(AQuery) = '' then
  begin
    WriteLn('Error: Empty query for reranking');
    Exit;
  end;

  if not Assigned(ACandidates) or (ACandidates.Count = 0) then
  begin
    WriteLn('Error: No candidates provided for reranking');
    Exit;
  end;

  try
    Result := CallRerankerAPI(AQuery, ACandidates, ATopK);
  except
    on E: Exception do
    begin
      WriteLn(Format('Error during reranking: %s', [E.Message]));
      WriteLn('Falling back to original candidates without reranking');

      // Fallback: return original candidates (limited to ATopK)
      Result := TSearchResultList.Create(True);
      var Limit := Min(ATopK, ACandidates.Count);
      for var I := 0 to Limit - 1 do
      begin
        var NewResult := TSearchResult.Create;
        var Original := ACandidates[I];

        NewResult.SymbolID := Original.SymbolID;
        NewResult.Name := Original.Name;
        NewResult.FullName := Original.FullName;
        NewResult.SymbolType := Original.SymbolType;
        NewResult.FilePath := Original.FilePath;
        NewResult.Content := Original.Content;
        NewResult.Comments := Original.Comments;
        NewResult.ParentClass := Original.ParentClass;
        NewResult.ImplementedInterfaces := Original.ImplementedInterfaces;
        NewResult.Visibility := Original.Visibility;
        NewResult.Score := Original.Score;
        NewResult.IsExactMatch := Original.IsExactMatch;
        NewResult.MatchType := Original.MatchType;
        NewResult.ContentType := Original.ContentType;
        NewResult.SourceCategory := Original.SourceCategory;

        Result.Add(NewResult);
      end;
    end;
  end;
end;

end.
