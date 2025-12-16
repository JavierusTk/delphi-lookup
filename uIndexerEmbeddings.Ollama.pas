unit uIndexerEmbeddings.Ollama;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.JSON,
  System.Diagnostics,
  uASTProcessor;

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

  TEmbeddingList = class(TObjectList<TEmbedding>)
  public
    function FindByChunkID(const AChunkID: string): TEmbedding;
    procedure SaveToBinaryFile(const AFileName: string);
    procedure LoadFromBinaryFile(const AFileName: string);
  end;

  TOllamaEmbeddingGenerator = class
  private
    FOllamaURL: string;
    FModelName: string;
    FBatchSize: Integer;
    FHttpClient: THTTPClient;

    function CallOllamaAPIBatch(const ATexts: TArray<string>): TArray<TVector>;
    function CreateEmbeddingText(AChunk: TCodeChunk): string;

  public
    constructor Create(const AOllamaURL: string;
                      const AModelName: string;
                      ABatchSize: Integer);
    destructor Destroy; override;

    function DetectEmbeddingDimensions: Integer;
    function GenerateEmbeddings(AChunks: TCodeChunkList): TEmbeddingList;

    property OllamaURL: string read FOllamaURL write FOllamaURL;
    property ModelName: string read FModelName write FModelName;
    property BatchSize: Integer read FBatchSize write FBatchSize;
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

{ TEmbeddingList }

function TEmbeddingList.FindByChunkID(const AChunkID: string): TEmbedding;
var
  Embedding: TEmbedding;
begin
  Result := nil;
  for Embedding in Self do
  begin
    if SameText(Embedding.ChunkID, AChunkID) then
    begin
      Result := Embedding;
      Break;
    end;
  end;
end;

procedure TEmbeddingList.SaveToBinaryFile(const AFileName: string);
var
  FileStream: TFileStream;
  I, J, Count, VectorLen: Integer;
  Embedding: TEmbedding;
  ChunkIDLen: Integer;
  ChunkIDBytes: TBytes;
  Value: Single;
begin
  FileStream := TFileStream.Create(AFileName, fmCreate);
  try
    // Write header: version (4 bytes), count (4 bytes)
    I := 1; // Version 1
    FileStream.Write(I, SizeOf(Integer));

    Count := Self.Count;
    FileStream.Write(Count, SizeOf(Integer));

    // Write embeddings
    for Embedding in Self do
    begin
      // Write ChunkID (length + UTF8 bytes)
      ChunkIDBytes := TEncoding.UTF8.GetBytes(Embedding.ChunkID);
      ChunkIDLen := Length(ChunkIDBytes);
      FileStream.Write(ChunkIDLen, SizeOf(Integer));
      if ChunkIDLen > 0 then
        FileStream.Write(ChunkIDBytes[0], ChunkIDLen);

      // Write vector (length + floats)
      VectorLen := Length(Embedding.Vector);
      FileStream.Write(VectorLen, SizeOf(Integer));
      if VectorLen > 0 then
      begin
        for J := 0 to VectorLen - 1 do
        begin
          Value := Embedding.Vector[J];
          FileStream.Write(Value, SizeOf(Single));
        end;
      end;
    end;

  finally
    FileStream.Free;
  end;
end;

procedure TEmbeddingList.LoadFromBinaryFile(const AFileName: string);
var
  FileStream: TFileStream;
  Version, Count, I, J: Integer;
  ChunkIDLen, VectorLen: Integer;
  ChunkIDBytes: TBytes;
  ChunkID: string;
  Vector: TVector;
  Value: Single;
  Embedding: TEmbedding;
begin
  Self.Clear;

  if not FileExists(AFileName) then
    Exit;

  FileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    // Read header
    FileStream.Read(Version, SizeOf(Integer));
    FileStream.Read(Count, SizeOf(Integer));

    // Read embeddings
    for I := 0 to Count - 1 do
    begin
      // Read ChunkID
      FileStream.Read(ChunkIDLen, SizeOf(Integer));
      SetLength(ChunkIDBytes, ChunkIDLen);
      if ChunkIDLen > 0 then
        FileStream.Read(ChunkIDBytes[0], ChunkIDLen);
      ChunkID := TEncoding.UTF8.GetString(ChunkIDBytes);

      // Read vector
      FileStream.Read(VectorLen, SizeOf(Integer));
      SetLength(Vector, VectorLen);
      if VectorLen > 0 then
      begin
        for J := 0 to VectorLen - 1 do
        begin
          FileStream.Read(Value, SizeOf(Single));
          Vector[J] := Value;
        end;
      end;

      // Create embedding
      Embedding := TEmbedding.Create(ChunkID, '', Vector);
      Self.Add(Embedding);
    end;

  finally
    FileStream.Free;
  end;
end;

{ TOllamaEmbeddingGenerator }

constructor TOllamaEmbeddingGenerator.Create(const AOllamaURL, AModelName: string;
  ABatchSize: Integer);
begin
  inherited Create;
  FOllamaURL := AOllamaURL;
  FModelName := AModelName;
  FBatchSize := ABatchSize;
  FHttpClient := THTTPClient.Create;
  FHttpClient.ConnectionTimeout := 30000; // 30 seconds
  FHttpClient.ResponseTimeout := 300000;  // 5 minutes for large batches
end;

destructor TOllamaEmbeddingGenerator.Destroy;
begin
  FHttpClient.Free;
  inherited Destroy;
end;

function TOllamaEmbeddingGenerator.CreateEmbeddingText(AChunk: TCodeChunk): string;
var
  TextParts: TStringList;
  CodeText: string;
begin
  TextParts := TStringList.Create;
  try
    // Prioritize code content
    CodeText := AChunk.EnrichedText;
    if CodeText = '' then
      CodeText := AChunk.Content;

    if CodeText <> '' then
    begin
      TextParts.Add('CODE:');
      // Truncate to 8192 chars to fit in context window (Jina model limit)
      if Length(CodeText) > 8192 then
        TextParts.Add(Copy(CodeText, 1, 8192) + '...')
      else
        TextParts.Add(CodeText);
    end;

    // Add metadata after code
    if AChunk.Name <> '' then
      TextParts.Add('NAME: ' + AChunk.Name);

    if AChunk.FullName <> AChunk.Name then
      TextParts.Add('FULL_NAME: ' + AChunk.FullName);

    case AChunk.ChunkType of
      ctClass: TextParts.Add('TYPE: class');
      ctInterface: TextParts.Add('TYPE: interface');
      ctRecord: TextParts.Add('TYPE: record');
      ctProcedure: TextParts.Add('TYPE: procedure');
      ctFunction: TextParts.Add('TYPE: function');
      ctConstructor: TextParts.Add('TYPE: constructor');
      ctDestructor: TextParts.Add('TYPE: destructor');
      ctProperty: TextParts.Add('TYPE: property');
      ctMethodImplementation: TextParts.Add('TYPE: method implementation');
    end;

    if AChunk.ParentClass <> '' then
      TextParts.Add('INHERITS: ' + AChunk.ParentClass);

    if AChunk.ImplementedInterfaces.Count > 0 then
      TextParts.Add('IMPLEMENTS: ' + AChunk.ImplementedInterfaces.CommaText);

    if AChunk.FileName <> '' then
      TextParts.Add('FILE: ' + ExtractFileName(AChunk.FileName));

    if AChunk.Comments <> '' then
    begin
      TextParts.Add('DESCRIPTION:');
      TextParts.Add(AChunk.Comments);
    end;

    Result := TextParts.Text;
  finally
    TextParts.Free;
  end;
end;

function TOllamaEmbeddingGenerator.CallOllamaAPIBatch(const ATexts: TArray<string>): TArray<TVector>;
var
  RequestJSON: TJSONObject;
  ResponseJSON: TJSONObject;
  InputArray: TJSONArray;
  EmbeddingsArray: TJSONArray;
  EmbeddingArray: TJSONArray;
  RequestStream: TStringStream;
  ResponseStream: TMemoryStream;
  Response: IHTTPResponse;
  URL: string;
  I, J: Integer;
  Text: string;
begin
  SetLength(Result, 0);

  if Length(ATexts) = 0 then
    Exit;

  RequestJSON := TJSONObject.Create;
  try
    RequestJSON.AddPair('model', FModelName);

    // Build input array
    InputArray := TJSONArray.Create;
    for Text in ATexts do
      InputArray.Add(Text);
    RequestJSON.AddPair('input', InputArray);

    RequestStream := TStringStream.Create(RequestJSON.ToJSON, TEncoding.UTF8);
    try
      ResponseStream := TMemoryStream.Create;
      try
        URL := FOllamaURL + '/api/embed';

        try
          Response := FHttpClient.Post(URL, RequestStream, ResponseStream);

          if Response.StatusCode = 200 then
          begin
            ResponseStream.Position := 0;
            var ResponseBytes: TBytes;
            SetLength(ResponseBytes, ResponseStream.Size);
            ResponseStream.ReadBuffer(ResponseBytes[0], ResponseStream.Size);
            var ResponseText := TEncoding.UTF8.GetString(ResponseBytes);
            ResponseJSON := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;
            try
              if Assigned(ResponseJSON) then
              begin
                EmbeddingsArray := ResponseJSON.GetValue<TJSONArray>('embeddings');
                if Assigned(EmbeddingsArray) then
                begin
                  SetLength(Result, EmbeddingsArray.Count);

                  for I := 0 to EmbeddingsArray.Count - 1 do
                  begin
                    EmbeddingArray := EmbeddingsArray.Items[I] as TJSONArray;
                    if Assigned(EmbeddingArray) then
                    begin
                      SetLength(Result[I], EmbeddingArray.Count);
                      for J := 0 to EmbeddingArray.Count - 1 do
                        Result[I][J] := EmbeddingArray.Items[J].AsType<Double>;
                    end;
                  end;
                end;
              end;
            finally
              ResponseJSON.Free;
            end;
          end
          else
          begin
            WriteLn(Format('Ollama API error: HTTP %d', [Response.StatusCode]));
          end;

        except
          on E: Exception do
            WriteLn(Format('Error calling Ollama API: %s', [E.Message]));
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

function TOllamaEmbeddingGenerator.DetectEmbeddingDimensions: Integer;
var
  TestVector: TArray<TVector>;
  TestText: TArray<string>;
begin
  WriteLn('Detecting embedding dimensions...');

  // Generate a single test embedding
  SetLength(TestText, 1);
  TestText[0] := 'test';

  TestVector := CallOllamaAPIBatch(TestText);

  if (Length(TestVector) > 0) and (Length(TestVector[0]) > 0) then
  begin
    Result := Length(TestVector[0]);
    WriteLn(Format('Model "%s" returns %d dimensions', [FModelName, Result]));
  end
  else
    raise Exception.Create('Failed to detect embedding dimensions: no vector returned from model');
end;

function TOllamaEmbeddingGenerator.GenerateEmbeddings(AChunks: TCodeChunkList): TEmbeddingList;
var
  I, BatchStart, BatchEnd: Integer;
  Embedding: TEmbedding;
  BatchTexts: TArray<string>;
  BatchVectors: TArray<TVector>;
  Stopwatch: TStopwatch;
  TotalBatches: Integer;
  CurrentBatch: Integer;
begin
  Result := TEmbeddingList.Create;

  if AChunks.Count = 0 then
    Exit;

  WriteLn(Format('*** OLLAMA: Generating embeddings for %d chunks (batch size: %d) ***',
    [AChunks.Count, FBatchSize]));
  WriteLn(Format('*** OLLAMA: Using model "%s" at %s ***', [FModelName, FOllamaURL]));
  Stopwatch := TStopwatch.StartNew;

  TotalBatches := (AChunks.Count + FBatchSize - 1) div FBatchSize;
  CurrentBatch := 0;

  try
    // Process chunks in batches
    BatchStart := 0;
    while BatchStart < AChunks.Count do
    begin
      Inc(CurrentBatch);
      BatchEnd := Min(BatchStart + FBatchSize - 1, AChunks.Count - 1);

      // Show progress with ETA - single line that updates in place
      if CurrentBatch = 1 then
      begin
        Write(Format(#13'*** EMBEDDING: Batch %d/%d (chunks %d-%d, %.1f%%)                    ',
          [CurrentBatch, TotalBatches, BatchStart + 1, BatchEnd + 1,
           ((BatchEnd + 1) / AChunks.Count) * 100]));
        Flush(Output);
      end
      else
      begin
        // Calculate ETA based on elapsed time and progress
        var ElapsedSec: Double := Stopwatch.ElapsedMilliseconds / 1000;
        var ProcessedChunks: Integer := BatchStart; // Chunks completed before this batch
        var RemainingChunks: Integer := AChunks.Count - ProcessedChunks;
        var ChunksPerSec: Double := ProcessedChunks / ElapsedSec;
        var ETASec: Integer := 0;
        if ChunksPerSec > 0 then
          ETASec := Round(RemainingChunks / ChunksPerSec);

        // Format ETA as mm:ss or hh:mm:ss
        var ETAStr: string;
        if ETASec >= 3600 then
          ETAStr := Format('%d:%02d:%02d', [ETASec div 3600, (ETASec mod 3600) div 60, ETASec mod 60])
        else
          ETAStr := Format('%d:%02d', [ETASec div 60, ETASec mod 60]);

        Write(Format(#13'*** EMBEDDING: Batch %d/%d (chunks %d-%d, %.1f%%) - ETA: %s        ',
          [CurrentBatch, TotalBatches, BatchStart + 1, BatchEnd + 1,
           ((BatchEnd + 1) / AChunks.Count) * 100, ETAStr]));
        Flush(Output);
      end;

      // Prepare batch texts
      SetLength(BatchTexts, BatchEnd - BatchStart + 1);
      for I := BatchStart to BatchEnd do
        BatchTexts[I - BatchStart] := CreateEmbeddingText(AChunks[I]);

      // Call batch API
      BatchVectors := CallOllamaAPIBatch(BatchTexts);

      // Create embeddings from results
      if Length(BatchVectors) = Length(BatchTexts) then
      begin
        for I := 0 to Length(BatchVectors) - 1 do
        begin
          if Length(BatchVectors[I]) > 0 then
          begin
            // Use chunk GUID for unique matching (chunk names aren't unique across files)
            Embedding := TEmbedding.Create(
              AChunks[BatchStart + I].ChunkGUID,  // FIX: Use unique GUID for proper matching
              BatchTexts[I],
              BatchVectors[I]
            );
            Result.Add(Embedding);
          end
          else
          begin
            WriteLn(Format('Warning: Empty vector for chunk %d', [BatchStart + I]));
          end;
        end;
      end
      else
      begin
        WriteLn(Format('Warning: Batch returned %d vectors, expected %d',
          [Length(BatchVectors), Length(BatchTexts)]));
      end;

      BatchStart := BatchEnd + 1;
    end;

    // Clear progress line and show final summary
    WriteLn(Format(#13'*** EMBEDDING: Generated %d embeddings in %d ms (%.1f chunks/sec)        ',
      [Result.Count, Stopwatch.ElapsedMilliseconds,
       Result.Count / (Stopwatch.ElapsedMilliseconds / 1000)]));

  except
    on E: Exception do
    begin
      Result.Free;
      raise Exception.CreateFmt('Failed to generate Ollama embeddings: %s', [E.Message]);
    end;
  end;
end;

end.
