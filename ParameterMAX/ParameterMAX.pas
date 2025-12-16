unit ParameterMAX;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.IOUtils,
  ParameterMAX.Handlers, ParameterMAX.HandlerRegistry, ParameterMAX.FallbackHandlers;

type
  TParameterValue = record
    Value: string;
    Source: string;
    Priority: Integer;
  end;

  TParameterManager = class
  private
    FParameters: TDictionary<string, TParameterValue>;
    FLoadedFiles: TList<string>;
    FFallbackRegistry: TFallbackHandlerRegistry;

    // Default config file support
    FDefaultConfigFile: string;
    FLoadedDefaultConfigPath: string;
    FAutoConfigDisabled: Boolean;

    function ParseCommandLineParameter(const Param: string; out Name, Value: string; out IsBooleanFlag: Boolean): Boolean;
    function NormalizeBooleanValue(const Value: string): string;
    function IsNextParameterValue(Index: Integer; const Value: string): Boolean;
    function IsNextParameterValueInArray(Index: Integer; const Args: TArray<string>; const Value: string): Boolean;
    procedure LoadParameterFile(const FileName: string);
    procedure AddParameter(const Name, Value, Source: string; Priority: Integer);
    function ProcessBooleanParameter(const Name, Value: string): string;
    function FindDefaultConfigFile: string;
    function HasExplicitConfigInArgs(const Args: TArray<string>): Boolean;
    function HasNoConfigFlag(const Args: TArray<string>): Boolean;
    procedure TryLoadDefaultConfig;
  public
    constructor Create;
    destructor Destroy; override;

    procedure EnableEnvironmentVars(const Prefix: string = '');
    procedure EnableRegistry(const Path: string);

    /// <summary>Set default config file name to search for automatically.
    /// Searches in executable directory only.</summary>
    /// <param name="AFileName">File name to search for, e.g. 'myapp.json'</param>
    procedure SetDefaultConfigFile(const AFileName: string);

    /// <summary>Disable automatic loading of default config file</summary>
    procedure DisableAutoConfig;

    procedure ParseCommandLine;

    function GetParameter(const Name: string; const DefaultValue: string = ''): string;
    function GetParameterAsInteger(const Name: string; const DefaultValue: Integer = 0): Integer;
    function GetParameterAsBoolean(const Name: string; const DefaultValue: Boolean = False): Boolean;
    function GetParameterAsFloat(const Name: string; const DefaultValue: Double = 0.0): Double;
    function HasParameter(const Name: string): Boolean;

    function GetParameterCount: Integer;

    procedure Clear;

    /// <summary>Returns the path of the loaded default config file, or empty if none</summary>
    function GetLoadedDefaultConfigPath: string;

    // Testing support methods
    procedure ParseCommandLineFromArray(const Args: TArray<string>);
  end;

const
  PRIORITY_COMMANDLINE = 1000;
  PRIORITY_FILE = 800;
  PRIORITY_DEFAULT = 400;

implementation

{ TParameterManager }

constructor TParameterManager.Create;
begin
  inherited Create;
  FParameters := TDictionary<string, TParameterValue>.Create;
  FLoadedFiles := TList<string>.Create;
  FFallbackRegistry := TFallbackHandlerRegistry.Instance;
  FDefaultConfigFile := '';
  FLoadedDefaultConfigPath := '';
  FAutoConfigDisabled := False;
end;

destructor TParameterManager.Destroy;
begin
  FLoadedFiles.Free;
  FParameters.Free;
  // FFallbackRegistry is a singleton, don't free it
  inherited Destroy;
end;

procedure TParameterManager.EnableEnvironmentVars(const Prefix: string);
begin
  // Configure the Environment fallback handler (if registered)
  FFallbackRegistry.ConfigureHandler('Environment', Prefix);
end;

procedure TParameterManager.EnableRegistry(const Path: string);
begin
  // Configure the Registry fallback handler (if registered)
  FFallbackRegistry.ConfigureHandler('Registry', Path);
end;

procedure TParameterManager.SetDefaultConfigFile(const AFileName: string);
begin
  FDefaultConfigFile := AFileName;
end;

procedure TParameterManager.DisableAutoConfig;
begin
  FAutoConfigDisabled := True;
end;

function TParameterManager.FindDefaultConfigFile: string;
var
  ExeDir, FullPath: string;
begin
  Result := '';

  if FDefaultConfigFile = '' then
    Exit;

  // Search in executable directory only
  ExeDir := ExtractFilePath(ParamStr(0));
  FullPath := TPath.Combine(ExeDir, FDefaultConfigFile);

  if TFile.Exists(FullPath) then
    Result := FullPath;
end;

function TParameterManager.HasExplicitConfigInArgs(const Args: TArray<string>): Boolean;
var
  Arg: string;
begin
  Result := False;
  for Arg in Args do
  begin
    if (Length(Arg) > 0) and (Arg[1] = '@') then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TParameterManager.HasNoConfigFlag(const Args: TArray<string>): Boolean;
var
  Arg: string;
begin
  Result := False;
  for Arg in Args do
  begin
    if (Arg = '--no-config') or (Arg = '/no-config') then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TParameterManager.TryLoadDefaultConfig;
var
  ConfigPath: string;
begin
  if FAutoConfigDisabled then
    Exit;

  if FDefaultConfigFile = '' then
    Exit;

  ConfigPath := FindDefaultConfigFile;
  if ConfigPath <> '' then
  begin
    LoadParameterFile(ConfigPath);
    FLoadedDefaultConfigPath := ConfigPath;
  end;
end;

function TParameterManager.GetLoadedDefaultConfigPath: string;
begin
  Result := FLoadedDefaultConfigPath;
end;

function TParameterManager.ParseCommandLineParameter(const Param: string;
  out Name, Value: string; out IsBooleanFlag: Boolean): Boolean;
var
  EqualPos, ColonPos: Integer;
begin
  Result := False;
  Name := '';
  Value := '';
  IsBooleanFlag := False;

  if Param = '' then
    Exit;

  // Check for file parameter (@filename)
  if Param[1] = '@' then
  begin
    Name := '@';
    Value := Copy(Param, 2, MaxInt);
    Result := True;
    Exit;
  end;

  // Windows style parameters (/param or -param)
  if (Param[1] = '/') or (Param[1] = '-') then
  begin
    if Param[1] = '-' then
    begin
      // Check for GNU style (-- prefix)
      if (Length(Param) > 1) and (Param[2] = '-') then
      begin
        // GNU style: --param or --param=value
        EqualPos := Pos('=', Param);
        if EqualPos > 0 then
        begin
          Name := LowerCase(Copy(Param, 3, EqualPos - 3));
          Value := Copy(Param, EqualPos + 1, MaxInt);
        end
        else
        begin
          Name := LowerCase(Copy(Param, 3, MaxInt));
          IsBooleanFlag := True;
        end;
        Result := True;
        Exit;
      end;
    end;

    // Windows style: /param or /param:value or /param=value
    EqualPos := Pos('=', Param);
    ColonPos := Pos(':', Param);

    if (EqualPos > 0) and ((ColonPos = 0) or (EqualPos < ColonPos)) then
    begin
      Name := LowerCase(Copy(Param, 2, EqualPos - 2));
      Value := Copy(Param, EqualPos + 1, MaxInt);
    end
    else if ColonPos > 0 then
    begin
      Name := LowerCase(Copy(Param, 2, ColonPos - 2));
      Value := Copy(Param, ColonPos + 1, MaxInt);
    end
    else
    begin
      Name := LowerCase(Copy(Param, 2, MaxInt));
      IsBooleanFlag := True;
    end;
    Result := True;
  end;
end;

function TParameterManager.NormalizeBooleanValue(const Value: string): string;
var
  LowerValue: string;
begin
  LowerValue := LowerCase(Value);
  
  if (LowerValue = '1') or (LowerValue = 'true') or 
     (LowerValue = 'yes') or (LowerValue = 'on') then
    Result := '1'
  else if (LowerValue = '0') or (LowerValue = 'false') or 
          (LowerValue = 'no') or (LowerValue = 'off') then
    Result := '0'
  else
    Result := Value; // Return unchanged if not a recognized boolean
end;

function TParameterManager.IsNextParameterValue(Index: Integer; const Value: string): Boolean;
begin
  // Check if the next parameter could be a value for the current flag
  Result := (Index < ParamCount) and 
            (ParamStr(Index + 1)[1] <> '/') and 
            (ParamStr(Index + 1)[1] <> '-') and
            (ParamStr(Index + 1)[1] <> '@');
end;

function TParameterManager.IsNextParameterValueInArray(Index: Integer; 
  const Args: TArray<string>; const Value: string): Boolean;
begin
  Result := (Index < High(Args)) and 
            (Args[Index + 1][1] <> '/') and 
            (Args[Index + 1][1] <> '-') and
            (Args[Index + 1][1] <> '@');
end;

procedure TParameterManager.ParseCommandLine;
var
  i: Integer;
  Param, Name, Value: string;
  IsBooleanFlag: Boolean;
  NextIsValue: Boolean;
  PendingParamName: string;
  Args: TArray<string>;
  HasExplicitConfig, HasNoConfig: Boolean;
begin
  // Build args array from command line
  SetLength(Args, ParamCount);
  for i := 1 to ParamCount do
    Args[i - 1] := ParamStr(i);

  // Check for flags that affect default config loading
  HasNoConfig := HasNoConfigFlag(Args);
  HasExplicitConfig := HasExplicitConfigInArgs(Args);

  // Try to load default config file first (lowest priority, will be overridden)
  if not HasNoConfig and not HasExplicitConfig then
    TryLoadDefaultConfig;

  // Process command line parameters
  NextIsValue := False;
  PendingParamName := '';

  for i := 1 to ParamCount do
  begin
    Param := ParamStr(i);

    // Skip --no-config flag (already processed)
    if (Param = '--no-config') or (Param = '/no-config') then
      Continue;

    if NextIsValue then
    begin
      AddParameter(PendingParamName, Param, 'CommandLine', PRIORITY_COMMANDLINE);
      NextIsValue := False;
      PendingParamName := '';
      Continue;
    end;

    if ParseCommandLineParameter(Param, Name, Value, IsBooleanFlag) then
    begin
      if Name = '@' then
      begin
        LoadParameterFile(Value);
      end
      else if IsBooleanFlag and IsNextParameterValue(i, Value) then
      begin
        NextIsValue := True;
        PendingParamName := Name;
      end
      else
      begin
        if IsBooleanFlag then
          Value := ProcessBooleanParameter(Name, Value)
        else
          Value := NormalizeBooleanValue(Value);

        AddParameter(Name, Value, 'CommandLine', PRIORITY_COMMANDLINE);
      end;
    end;
  end;
end;

procedure TParameterManager.LoadParameterFile(const FileName: string);
var
  FullPath: string;
  Extension: string;
  Registry: TParameterHandlerRegistry;
  HandlerClass: TParameterHandlerClass;
  Handler: IParameterHandler;
  ParamNames: TArray<string>;
  ParamName, ParamValue: string;
begin
  // Prevent loading the same file multiple times
  if FLoadedFiles.Contains(FileName) then
    Exit;

  FullPath := TPath.GetFullPath(FileName);
  
  if not TFile.Exists(FullPath) then
    raise Exception.CreateFmt('Parameter file not found: %s', [FileName]);
    
  FLoadedFiles.Add(FileName);
  
  Extension := LowerCase(ExtractFileExt(FullPath));
  
  Registry := TParameterHandlerRegistry.Instance;
  HandlerClass := Registry.GetHandler(Extension);
  
  if HandlerClass = nil then
    raise Exception.CreateFmt('No handler registered for file type: %s', [Extension]);
    
  Handler := HandlerClass.Create;
  if not Handler.LoadParameters(FullPath) then
    raise Exception.CreateFmt('Failed to load parameters from file: %s', [FileName]);
    
  // Get all parameters from the handler
  ParamNames := Handler.GetAllParameterNames;
  for ParamName in ParamNames do
  begin
    ParamValue := Handler.GetParameter(ParamName);
    ParamValue := NormalizeBooleanValue(ParamValue);
    AddParameter(ParamName, ParamValue, 'File:' + ExtractFileName(FileName), PRIORITY_FILE);
  end;
end;

procedure TParameterManager.AddParameter(const Name, Value, Source: string; Priority: Integer);
var
  Key: string;
  ExistingParam: TParameterValue;
  NewParam: TParameterValue;
begin
  Key := LowerCase(Name);
  
  NewParam.Value := Value;
  NewParam.Source := Source;
  NewParam.Priority := Priority;
  
  if FParameters.TryGetValue(Key, ExistingParam) then
  begin
    // Only update if new parameter has higher or equal priority
    if Priority >= ExistingParam.Priority then
      FParameters.AddOrSetValue(Key, NewParam);
  end
  else
    FParameters.Add(Key, NewParam);
end;

function TParameterManager.ProcessBooleanParameter(const Name, Value: string): string;
begin
  // If value is empty (flag without value), treat as true
  if Value = '' then
    Result := '1'
  else
    Result := NormalizeBooleanValue(Value);
end;

function TParameterManager.GetParameter(const Name: string; const DefaultValue: string): string;
var
  Key: string;
  Param: TParameterValue;
  FallbackValue: string;
begin
  Key := LowerCase(Name);
  
  // 1. Check explicitly set parameters (command line or file)
  if FParameters.TryGetValue(Key, Param) then
  begin
    Result := Param.Value;
  end
  else
  begin
    // 2. Try fallback handlers (Registry, Environment, etc.)
    if FFallbackRegistry.GetFallbackValue(Name, FallbackValue) then
      Result := FallbackValue
    else
      // 3. Default value
      Result := DefaultValue;
  end;
end;

function TParameterManager.GetParameterAsInteger(const Name: string; const DefaultValue: Integer): Integer;
var
  Value: string;
begin
  Value := GetParameter(Name, IntToStr(DefaultValue));
  if not TryStrToInt(Value, Result) then
    Result := DefaultValue;
end;

function TParameterManager.GetParameterAsBoolean(const Name: string; const DefaultValue: Boolean): Boolean;
var
  Value: string;
begin
  Value := GetParameter(Name, BoolToStr(DefaultValue, True));
  Value := NormalizeBooleanValue(Value);
  Result := Value = '1';
end;

function TParameterManager.GetParameterAsFloat(const Name: string; const DefaultValue: Double): Double;
var
  Value: string;
  USFormat: TFormatSettings;
begin
  USFormat := TFormatSettings.Create('en-US');
  Value := GetParameter(Name, FloatToStr(DefaultValue, USFormat));
  if not TryStrToFloat(Value, Result, USFormat) then
    Result := DefaultValue;
end;

function TParameterManager.HasParameter(const Name: string): Boolean;
var
  Key: string;
begin
  Key := LowerCase(Name);
  Result := FParameters.ContainsKey(Key) or 
            FFallbackRegistry.HasFallbackValue(Name);
end;

function TParameterManager.GetParameterCount: Integer;
begin
  Result := FParameters.Count;
end;

procedure TParameterManager.Clear;
begin
  FParameters.Clear;
  FLoadedFiles.Clear;
  FLoadedDefaultConfigPath := '';
end;

// Testing support methods

procedure TParameterManager.ParseCommandLineFromArray(const Args: TArray<string>);
var
  i: Integer;
  Param, Name, Value: string;
  IsBooleanFlag: Boolean;
  NextIsValue: Boolean;
  PendingParamName: string;
  HasExplicitConfig, HasNoConfig: Boolean;
begin
  Clear; // Reset existing parameters

  // Check for flags that affect default config loading
  HasNoConfig := HasNoConfigFlag(Args);
  HasExplicitConfig := HasExplicitConfigInArgs(Args);

  // Try to load default config file first (lowest priority, will be overridden)
  if not HasNoConfig and not HasExplicitConfig then
    TryLoadDefaultConfig;

  // Process command line parameters
  NextIsValue := False;
  PendingParamName := '';

  for i := 0 to High(Args) do
  begin
    Param := Args[i];

    // Skip --no-config flag (already processed)
    if (Param = '--no-config') or (Param = '/no-config') then
      Continue;

    if NextIsValue then
    begin
      AddParameter(PendingParamName, Param, 'CommandLine', PRIORITY_COMMANDLINE);
      NextIsValue := False;
      PendingParamName := '';
      Continue;
    end;

    if ParseCommandLineParameter(Param, Name, Value, IsBooleanFlag) then
    begin
      if Name = '@' then
      begin
        LoadParameterFile(Value);
      end
      else if IsBooleanFlag and IsNextParameterValueInArray(i, Args, Value) then
      begin
        NextIsValue := True;
        PendingParamName := Name;
      end
      else
      begin
        if IsBooleanFlag then
          Value := ProcessBooleanParameter(Name, Value)
        else
          Value := NormalizeBooleanValue(Value);

        AddParameter(Name, Value, 'CommandLine', PRIORITY_COMMANDLINE);
      end;
    end;
  end;
end;

end.