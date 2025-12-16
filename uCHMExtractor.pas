unit uCHMExtractor;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, uDocTypes;

type
  /// CHM extraction wrapper
  TCHMExtractor = class
  private
    FCHMPath: string;
    FOutputDir: string;
    FPythonPath: string;
    FScriptPath: string;
    FVerbose: Boolean;
    FExtractedFiles: TStringList;
    FExtractionInfo: TExtractionInfo;

    function FindPythonExecutable: string;
    function FindExtractionScript: string;
    function ExecuteCommand(const ACommand: string; const AArgs: array of string;
      out AOutput: string): Boolean;

  public
    constructor Create(const ACHMPath, AOutputDir: string);
    destructor Destroy; override;

    /// Extract CHM file using Python script
    function ExtractUsingPython: Boolean;

    /// Extract CHM file using hh.exe (Windows only)
    function ExtractUsingHHExe: Boolean;

    /// Extract using best available method
    function Extract: Boolean;

    /// Get list of extracted HTML files
    function GetExtractedFiles: TStringList;

    /// Load extraction metadata from JSON
    function LoadMetadata: Boolean;

    property CHMPath: string read FCHMPath;
    property OutputDir: string read FOutputDir;
    property Verbose: Boolean read FVerbose write FVerbose;
    property ExtractionInfo: TExtractionInfo read FExtractionInfo;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.JSON;

{ TCHMExtractor }

constructor TCHMExtractor.Create(const ACHMPath, AOutputDir: string);
begin
  inherited Create;

  FCHMPath := ExpandFileName(ACHMPath);
  FOutputDir := ExpandFileName(AOutputDir);
  FVerbose := False;

  if not FileExists(FCHMPath) then
    raise Exception.CreateFmt('CHM file not found: %s', [FCHMPath]);

  // Ensure output directory exists
  if not DirectoryExists(FOutputDir) then
    ForceDirectories(FOutputDir);

  FExtractedFiles := TStringList.Create;

  // Try to find Python and script
  FPythonPath := FindPythonExecutable;
  FScriptPath := FindExtractionScript;
end;

destructor TCHMExtractor.Destroy;
begin
  FExtractedFiles.Free;
  inherited Destroy;
end;

function TCHMExtractor.FindPythonExecutable: string;
var
  Paths: TStringList;
  I: Integer;
  EnvPython: string;
begin
  Result := '';

  // First, check PYTHON_EXE environment variable
  EnvPython := GetEnvironmentVariable('PYTHON_EXE');
  if (EnvPython <> '') and FileExists(EnvPython) then
  begin
    Result := EnvPython;
    Exit;
  end;

  Paths := TStringList.Create;
  try
    // Common Python paths - try PATH first
    Paths.Add('python');
    Paths.Add('python3');
    {$IFDEF MSWINDOWS}
    // Try common installation locations (Python 3.10+)
    Paths.Add(GetEnvironmentVariable('LOCALAPPDATA') + '\Programs\Python\Python313\python.exe');
    Paths.Add(GetEnvironmentVariable('LOCALAPPDATA') + '\Programs\Python\Python312\python.exe');
    Paths.Add(GetEnvironmentVariable('LOCALAPPDATA') + '\Programs\Python\Python311\python.exe');
    Paths.Add(GetEnvironmentVariable('LOCALAPPDATA') + '\Programs\Python\Python310\python.exe');
    {$ELSE}
    Paths.Add('/usr/bin/python3');
    Paths.Add('/usr/local/bin/python3');
    {$ENDIF}

    // Try each path
    for I := 0 to Paths.Count - 1 do
    begin
      if FileExists(Paths[I]) then
      begin
        Result := Paths[I];
        Break;
      end;
    end;
  finally
    Paths.Free;
  end;
end;

function TCHMExtractor.FindExtractionScript: string;
var
  AppDir, ScriptName: string;
begin
  ScriptName := 'extract_chm.py';
  AppDir := ExtractFilePath(ParamStr(0));

  // Try relative to executable
  Result := AppDir + 'Tools' + PathDelim + 'chm_extractor' + PathDelim + ScriptName;
  if FileExists(Result) then
    Exit;

  Result := AppDir + '..' + PathDelim + 'Tools' + PathDelim + 'chm_extractor' + PathDelim + ScriptName;
  if FileExists(Result) then
    Exit;

  // Try in current directory
  Result := ScriptName;
  if FileExists(Result) then
    Exit;

  Result := '';
end;

function TCHMExtractor.ExecuteCommand(const ACommand: string;
  const AArgs: array of string; out AOutput: string): Boolean;
{$IFDEF MSWINDOWS}
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  SA: TSecurityAttributes;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  CommandLine: string;
  Buffer: array[0..4095] of AnsiChar;
  BytesRead: DWORD;
  I: Integer;
  WasOK: Boolean;
  ExitCode: DWORD;
{$ENDIF}
begin
  Result := False;
  AOutput := '';

  {$IFDEF MSWINDOWS}
  // Build command line
  CommandLine := '"' + ACommand + '"';
  for I := 0 to High(AArgs) do
    CommandLine := CommandLine + ' "' + AArgs[I] + '"';

  if FVerbose then
  begin
    WriteLn('Executing: ', CommandLine);
  end;

  // Create pipe for stdout
  SA.nLength := SizeOf(TSecurityAttributes);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;

  if not CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0) then
    Exit;

  try
    // Set up process startup info
    FillChar(SI, SizeOf(TStartupInfo), 0);
    SI.cb := SizeOf(TStartupInfo);
    SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    SI.wShowWindow := SW_HIDE;
    SI.hStdOutput := StdOutPipeWrite;
    SI.hStdError := StdOutPipeWrite;

    // Create process
    WasOK := CreateProcess(nil, PChar(CommandLine), nil, nil, True,
      CREATE_NO_WINDOW, nil, nil, SI, PI);

    CloseHandle(StdOutPipeWrite);

    if not WasOK then
      Exit;

    try
      // Read output
      repeat
        WasOK := ReadFile(StdOutPipeRead, Buffer, SizeOf(Buffer), BytesRead, nil);
        if BytesRead > 0 then
        begin
          SetLength(AOutput, Length(AOutput) + Integer(BytesRead));
          Move(Buffer[0], AOutput[Length(AOutput) - Integer(BytesRead) + 1], BytesRead);
        end;
      until not WasOK or (BytesRead = 0);

      // Wait for process to finish
      WaitForSingleObject(PI.hProcess, INFINITE);
      GetExitCodeProcess(PI.hProcess, ExitCode);

      Result := ExitCode = 0;

      if FVerbose then
      begin
        WriteLn('Exit code: ', ExitCode);
        if AOutput <> '' then
          WriteLn('Output: ', AOutput);
      end;

    finally
      CloseHandle(PI.hProcess);
      CloseHandle(PI.hThread);
    end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
  {$ELSE}
  WriteLn('Process execution not implemented for non-Windows platforms');
  {$ENDIF}
end;

function TCHMExtractor.ExtractUsingPython: Boolean;
var
  Args: array of string;
  Output: string;
begin
  Result := False;

  if FPythonPath = '' then
  begin
    if FVerbose then
      WriteLn('Python executable not found');
    Exit;
  end;

  if FScriptPath = '' then
  begin
    if FVerbose then
      WriteLn('Extraction script not found');
    Exit;
  end;

  WriteLn('Extracting CHM using Python...');
  WriteLn('  CHM: ', FCHMPath);
  WriteLn('  Output: ', FOutputDir);

  SetLength(Args, 3);
  Args[0] := FScriptPath;
  Args[1] := FCHMPath;
  Args[2] := FOutputDir;

  Result := ExecuteCommand(FPythonPath, Args, Output);

  if Result then
  begin
    WriteLn('Extraction successful');
    // Load metadata
    LoadMetadata;
  end
  else
    WriteLn('Extraction failed');
end;

function TCHMExtractor.ExtractUsingHHExe: Boolean;
begin
  Result := False;

  {$IFDEF MSWINDOWS}
  WriteLn('Extracting CHM using hh.exe...');
  WriteLn('  CHM: ', FCHMPath);
  WriteLn('  Output: ', FOutputDir);

  // Note: hh.exe extraction not fully implemented yet
  WriteLn('Note: hh.exe extraction not fully implemented yet');
  {$ELSE}
  WriteLn('hh.exe is only available on Windows');
  {$ENDIF}
end;

function TCHMExtractor.Extract: Boolean;
begin
  // Try Python first
  Result := ExtractUsingPython;

  // Fallback to hh.exe if Python fails
  if not Result then
  begin
    WriteLn('Trying hh.exe fallback...');
    Result := ExtractUsingHHExe;
  end;

  if not Result then
    WriteLn('All extraction methods failed');
end;

function TCHMExtractor.GetExtractedFiles: TStringList;
var
  SearchRec: TSearchRec;
  HTMLDir: string;
  FindResult: Integer;
begin
  FExtractedFiles.Clear;

  HTMLDir := FOutputDir + PathDelim + 'html';
  if not DirectoryExists(HTMLDir) then
    HTMLDir := FOutputDir;

  // Find all HTML files
  FindResult := FindFirst(HTMLDir + PathDelim + '*.html', faAnyFile, SearchRec);
  if FindResult = 0 then
  begin
    try
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
          FExtractedFiles.Add(HTMLDir + PathDelim + SearchRec.Name);
        FindResult := FindNext(SearchRec);
      until FindResult <> 0;
    finally
      System.SysUtils.FindClose(SearchRec);
    end;
  end;

  // Also check for .htm files
  FindResult := FindFirst(HTMLDir + PathDelim + '*.htm', faAnyFile, SearchRec);
  if FindResult = 0 then
  begin
    try
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
          FExtractedFiles.Add(HTMLDir + PathDelim + SearchRec.Name);
        FindResult := FindNext(SearchRec);
      until FindResult <> 0;
    finally
      System.SysUtils.FindClose(SearchRec);
    end;
  end;

  Result := FExtractedFiles;
end;

function TCHMExtractor.LoadMetadata: Boolean;
var
  MetadataFile: string;
  JSONValue: TJSONValue;
  JSONObj: TJSONObject;
  JSONContent: TStringList;
begin
  Result := False;

  MetadataFile := FOutputDir + PathDelim + 'extraction_metadata.json';
  if not FileExists(MetadataFile) then
    Exit;

  try
    // Read JSON file content
    JSONContent := TStringList.Create;
    try
      JSONContent.LoadFromFile(MetadataFile);

      JSONValue := TJSONObject.ParseJSONValue(JSONContent.Text);
      try
        if JSONValue is TJSONObject then
        begin
          JSONObj := TJSONObject(JSONValue);

          FExtractionInfo.SourceCHM := JSONObj.GetValue<string>('source_chm', '');
          FExtractionInfo.OutputDir := JSONObj.GetValue<string>('output_dir', '');
          FExtractionInfo.TopicCount := JSONObj.GetValue<Integer>('topic_count', 0);
          FExtractionInfo.ExtractionDate := Now;

          Result := True;
        end;
      finally
        JSONValue.Free;
      end;
    finally
      JSONContent.Free;
    end;
  except
    on E: Exception do
    begin
      if FVerbose then
        WriteLn('Error loading metadata: ', E.Message);
      Result := False;
    end;
  end;
end;

end.
