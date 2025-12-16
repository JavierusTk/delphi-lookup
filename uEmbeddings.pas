unit uEmbeddings;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Math,
  System.IOUtils,
  System.JSON,
  System.Diagnostics,
  Winapi.Windows,
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
  end;

  TEmbeddingGenerator = class
  private
    FChromaDBPath: string;
    FBatchSize: Integer;
    FModelName: string;
    
    function CallChromaDB(const ATexts: TStringList; const AMetadatas: TStringList; const ACommand: string): Boolean;
    function CreateEmbeddingText(AChunk: TCodeChunk): string;
    function CreateMetadataJSON(AChunk: TCodeChunk; AIndex: Integer): string;
    procedure WriteBatchToTemp(const ATexts: TStringList; const ATempFile: string);
    procedure WriteMetadataToTemp(const AMetadatas: TStringList; const ATempFile: string);
    
  public
    constructor Create(const AChromaDBPath: string = './chroma_db'; ABatchSize: Integer = 100);
    destructor Destroy; override;
    
    function GenerateEmbeddings(AChunks: TCodeChunkList): TEmbeddingList;
    function SearchSimilar(const AQuery: string; AMaxResults: Integer = 10): string;
    function ClearDatabase: Boolean;
    function GetDatabaseStats: string;
    
    property ChromaDBPath: string read FChromaDBPath write FChromaDBPath;
    property BatchSize: Integer read FBatchSize write FBatchSize;
    property ModelName: string read FModelName write FModelName;
  end;

implementation

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

{ TEmbeddingGenerator }

constructor TEmbeddingGenerator.Create(const AChromaDBPath: string; ABatchSize: Integer);
begin
  inherited Create;
  FBatchSize := ABatchSize;
  FModelName := 'all-MiniLM-L6-v2';
  FChromaDBPath := AChromaDBPath;
end;

destructor TEmbeddingGenerator.Destroy;
begin
  inherited Destroy;
end;

function TEmbeddingGenerator.CreateEmbeddingText(AChunk: TCodeChunk): string;
var
  TextParts: TStringList;
begin
  TextParts := TStringList.Create;
  try
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
    
    if AChunk.Content <> '' then
    begin
      TextParts.Add('CODE:');
      if Length(AChunk.Content) > 500 then
        TextParts.Add(Copy(AChunk.Content, 1, 500) + '...')
      else
        TextParts.Add(AChunk.Content);
    end;
    
    Result := TextParts.Text;
  finally
    TextParts.Free;
  end;
end;

function TEmbeddingGenerator.CreateMetadataJSON(AChunk: TCodeChunk; AIndex: Integer): string;
var
  JSON: TJSONObject;
  ChunkTypeStr: string;
begin
  JSON := TJSONObject.Create;
  try
    case AChunk.ChunkType of
      ctClass: ChunkTypeStr := 'class';
      ctInterface: ChunkTypeStr := 'interface';
      ctRecord: ChunkTypeStr := 'record';
      ctProcedure: ChunkTypeStr := 'procedure';
      ctFunction: ChunkTypeStr := 'function';
      ctConstructor: ChunkTypeStr := 'constructor';
      ctDestructor: ChunkTypeStr := 'destructor';
      ctProperty: ChunkTypeStr := 'property';
      ctType: ChunkTypeStr := 'type';
      ctConst: ChunkTypeStr := 'const';
      ctVar: ChunkTypeStr := 'var';
    else
      ChunkTypeStr := 'unknown';
    end;

    JSON.AddPair('chunk_id', Format('chunk_%d', [AIndex]));
    JSON.AddPair('name', AChunk.Name);
    JSON.AddPair('full_name', AChunk.FullName);
    JSON.AddPair('type', ChunkTypeStr);
    JSON.AddPair('file_path', AChunk.FileName);
    JSON.AddPair('parent_class', AChunk.ParentClass);
    JSON.AddPair('visibility', AChunk.Visibility);
    JSON.AddPair('start_line', TJSONNumber.Create(AChunk.StartLine));
    JSON.AddPair('end_line', TJSONNumber.Create(AChunk.EndLine));
    
    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end;

procedure TEmbeddingGenerator.WriteBatchToTemp(const ATexts: TStringList; const ATempFile: string);
var
  JSON: TJSONArray;
  I: Integer;
  JSONStr: string;
  UTF8Bytes: TBytes;
  FileStream: TFileStream;
begin
  JSON := TJSONArray.Create;
  try
    for I := 0 to ATexts.Count - 1 do
      JSON.AddElement(TJSONString.Create(ATexts[I]));
    JSONStr := JSON.ToJSON;
  finally
    JSON.Free;
  end;
  
  UTF8Bytes := TEncoding.UTF8.GetBytes(JSONStr);
  FileStream := TFileStream.Create(ATempFile, fmCreate);
  try
    FileStream.WriteBuffer(UTF8Bytes[0], Length(UTF8Bytes));
  finally
    FileStream.Free;
  end;
end;

procedure TEmbeddingGenerator.WriteMetadataToTemp(const AMetadatas: TStringList; const ATempFile: string);
var
  JSON: TJSONArray;
  I: Integer;
  JSONStr: string;
  UTF8Bytes: TBytes;
  FileStream: TFileStream;
  MetadataJSON: TJSONObject;
begin
  JSON := TJSONArray.Create;
  try
    for I := 0 to AMetadatas.Count - 1 do
    begin
      MetadataJSON := TJSONObject.ParseJSONValue(AMetadatas[I]) as TJSONObject;
      if Assigned(MetadataJSON) then
        JSON.AddElement(MetadataJSON);
    end;
    JSONStr := JSON.ToJSON;
  finally
    JSON.Free;
  end;
  
  UTF8Bytes := TEncoding.UTF8.GetBytes(JSONStr);
  FileStream := TFileStream.Create(ATempFile, fmCreate);
  try
    FileStream.WriteBuffer(UTF8Bytes[0], Length(UTF8Bytes));
  finally
    FileStream.Free;
  end;
end;

function TEmbeddingGenerator.CallChromaDB(const ATexts: TStringList; const AMetadatas: TStringList; const ACommand: string): Boolean;
var
  ProcessInfo: Winapi.Windows.TProcessInformation;
  StartupInfo: Winapi.Windows.TStartupInfoW;
  TempInputFile, TempMetadataFile: string;
  CommandLine: string;
  ExitCode: DWORD;
  PythonExe: string;
begin
  Result := False;

  try
    TempInputFile := TPath.GetTempFileName + '.json';
    TempMetadataFile := TPath.GetTempFileName + '.json';

    try
      WriteBatchToTemp(ATexts, TempInputFile);
      WriteMetadataToTemp(AMetadatas, TempMetadataFile);

      // Use PYTHON_EXE environment variable or fallback to 'python' in PATH
      PythonExe := GetEnvironmentVariable('PYTHON_EXE');
      if PythonExe = '' then
        PythonExe := 'python';

      CommandLine := Format('"%s" chromadb_manager.py %s "%s" "%s" "%s" "%s"',
        [PythonExe, ACommand, TempInputFile, TempMetadataFile, FChromaDBPath, FModelName]);
      
      WriteLn(Format('CHROMADB CALL: %s', [CommandLine]));
      
      ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
      StartupInfo.cb := SizeOf(StartupInfo);
      StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
      StartupInfo.wShowWindow := SW_HIDE;
      
      if CreateProcess(nil, PChar(CommandLine), nil, nil, False, 0, nil, nil, StartupInfo, ProcessInfo) then
      try
        WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
        GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);
        
        if ExitCode = 0 then
        begin
          Result := True;
          WriteLn('ChromaDB operation completed successfully');
        end
        else
        begin
          WriteLn('=== CHROMADB ERROR ===');
          WriteLn(Format('ChromaDB script failed with exit code: %d', [ExitCode]));
        end;
          
      finally
        CloseHandle(ProcessInfo.hProcess);
        CloseHandle(ProcessInfo.hThread);
      end
      else
      begin
        WriteLn('=== CHROMADB EXECUTION ERROR ===');
        WriteLn(Format('Failed to execute ChromaDB script: %d', [GetLastError]));
      end;
      
    finally
      if FileExists(TempInputFile) then
        TFile.Delete(TempInputFile);
      if FileExists(TempMetadataFile) then
        TFile.Delete(TempMetadataFile);
    end;
    
  except
    on E: Exception do
    begin
      WriteLn(Format('Error calling ChromaDB: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TEmbeddingGenerator.GenerateEmbeddings(AChunks: TCodeChunkList): TEmbeddingList;
var
  Texts, Metadatas: TStringList;
  I, BatchStart, BatchEnd: Integer;
  Embedding: TEmbedding;
  EmbeddingText: string;
  Stopwatch: TStopwatch;
begin
  Result := TEmbeddingList.Create;
  
  if AChunks.Count = 0 then
    Exit;
    
  WriteLn(Format('*** CHROMADB: Generating embeddings for %d chunks ***', [AChunks.Count]));
  Stopwatch := TStopwatch.StartNew;
  
  try
    ClearDatabase;
    
    BatchStart := 0;
    while BatchStart < AChunks.Count do
    begin
      BatchEnd := Min(BatchStart + FBatchSize - 1, AChunks.Count - 1);
      
      WriteLn(Format('*** CHROMADB: Processing batch %d-%d of %d ***', [BatchStart + 1, BatchEnd + 1, AChunks.Count]));
      
      Texts := TStringList.Create;
      Metadatas := TStringList.Create;
      try
        for I := BatchStart to BatchEnd do
        begin
          EmbeddingText := CreateEmbeddingText(AChunks[I]);
          Texts.Add(EmbeddingText);
          Metadatas.Add(CreateMetadataJSON(AChunks[I], I));
        end;
        
        if CallChromaDB(Texts, Metadatas, 'add_batch') then
        begin
          for I := 0 to Texts.Count - 1 do
          begin
            Embedding := TEmbedding.Create(
              Format('chunk_%d', [BatchStart + I]),
              Texts[I],
              TVector.Create()
            );
            Result.Add(Embedding);
          end;
        end;
        
      finally
        Texts.Free;
        Metadatas.Free;
      end;
      
      BatchStart := BatchEnd + 1;
    end;
    
    WriteLn(Format('*** CHROMADB: Generated %d embeddings in %d ms ***', 
      [Result.Count, Stopwatch.ElapsedMilliseconds]));
      
  except
    on E: Exception do
    begin
      Result.Free;
      raise Exception.CreateFmt('Failed to generate ChromaDB embeddings: %s', [E.Message]);
    end;
  end;
end;

function TEmbeddingGenerator.ClearDatabase: Boolean;
var
  Texts, Metadatas: TStringList;
begin
  Texts := TStringList.Create;
  Metadatas := TStringList.Create;
  try
    Result := CallChromaDB(Texts, Metadatas, 'clear');
  finally
    Texts.Free;
    Metadatas.Free;
  end;
end;

function TEmbeddingGenerator.GetDatabaseStats: string;
begin
  Result := '{"count": 0}';
end;

function TEmbeddingGenerator.SearchSimilar(const AQuery: string; AMaxResults: Integer): string;
begin
  Result := '[]';
end;

end.