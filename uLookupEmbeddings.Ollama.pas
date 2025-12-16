unit uLookupEmbeddings.Ollama;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.JSON,
  System.Diagnostics;

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

  TOllamaEmbeddingGenerator = class
  private
    FOllamaURL: string;
    FModelName: string;
    FHttpClient: THTTPClient;

    function CallOllamaAPI(const AText: string): TVector;

  public
    constructor Create(const AOllamaURL: string; const AModelName: string);
    destructor Destroy; override;

    function GenerateEmbedding(const AText: string): TEmbedding;

    property OllamaURL: string read FOllamaURL write FOllamaURL;
    property ModelName: string read FModelName write FModelName;
  end;

implementation

uses
  System.Math;

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
  FHttpClient.ResponseTimeout := 120000;    // 2 minutes
end;

destructor TOllamaEmbeddingGenerator.Destroy;
begin
  FHttpClient.Free;
  inherited Destroy;
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
begin
  Result := nil;

  if Trim(AText) = '' then
    Exit;

  try
    Vector := CallOllamaAPI(AText);

    if Length(Vector) > 0 then
      Result := TEmbedding.Create('single', AText, Vector);

  except
    on E: Exception do
    begin
      WriteLn(Format('Error generating embedding: %s', [E.Message]));
      Result := nil;
    end;
  end;
end;

end.
