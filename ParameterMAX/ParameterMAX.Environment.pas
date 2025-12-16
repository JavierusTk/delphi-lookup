unit ParameterMAX.Environment;

interface

uses
  System.SysUtils, System.Generics.Collections,
  ParameterMAX.FallbackHandlers;

type
  TEnvironmentFallbackHandler = class(TInterfacedObject, IFallbackHandler)
  private
    FPrefix: string;
    FEnabled: Boolean;
    
    // Mock environment support for testing
    class var FMockEnvironmentVars: TDictionary<string, string>;
    class var FUseMockEnvironment: Boolean;
    
    function GetEnvironmentValue(const Name: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    
    // IFallbackHandler implementation
    function GetParameter(const Name: string; out Value: string): Boolean;
    function HasParameter(const Name: string): Boolean;
    function GetPriority: Integer;
    function GetName: string;
    procedure Configure(const Config: string);
    
    // Testing support
    class procedure SetMockEnvironmentVariable(const Name, Value: string);
    class procedure ClearMockEnvironmentVariables;
    class procedure EnableMockEnvironment(Enable: Boolean);
  end;

implementation

uses
  Winapi.Windows;

{ TEnvironmentFallbackHandler }

constructor TEnvironmentFallbackHandler.Create;
begin
  inherited Create;
  FEnabled := False;
  FPrefix := '';
end;

destructor TEnvironmentFallbackHandler.Destroy;
begin
  inherited Destroy;
end;

procedure TEnvironmentFallbackHandler.Configure(const Config: string);
begin
  // Config is the prefix (can be empty string)
  FPrefix := Config;
  FEnabled := True;
end;

function TEnvironmentFallbackHandler.GetParameter(const Name: string; out Value: string): Boolean;
begin
  Value := '';
  Result := False;
  
  if not FEnabled then
    Exit;
    
  Value := GetEnvironmentValue(Name);
  Result := Value <> '';
end;

function TEnvironmentFallbackHandler.HasParameter(const Name: string): Boolean;
var
  Value: string;
begin
  Result := GetParameter(Name, Value);
end;

function TEnvironmentFallbackHandler.GetPriority: Integer;
begin
  Result := PRIORITY_ENVIRONMENT;
end;

function TEnvironmentFallbackHandler.GetName: string;
begin
  Result := 'Environment';
end;

function TEnvironmentFallbackHandler.GetEnvironmentValue(const Name: string): string;
var
  EnvName: string;
begin
  Result := '';
  
  if not FEnabled then
    Exit;
    
  // Build the environment variable name with prefix
  if FPrefix <> '' then
    EnvName := FPrefix + UpperCase(Name)
  else
    EnvName := UpperCase(Name);
    
  // Check mock environment first (for testing)
  if FUseMockEnvironment then
  begin
    if Assigned(FMockEnvironmentVars) and FMockEnvironmentVars.TryGetValue(EnvName, Result) then
      Exit
    else
    begin
      Result := '';
      Exit;
    end;
  end;
  
  // Get from actual environment
  Result := GetEnvironmentVariable(EnvName);
end;

// Testing support methods

class procedure TEnvironmentFallbackHandler.SetMockEnvironmentVariable(const Name, Value: string);
begin
  if not Assigned(FMockEnvironmentVars) then
    FMockEnvironmentVars := TDictionary<string, string>.Create;
  FMockEnvironmentVars.AddOrSetValue(UpperCase(Name), Value);
end;

class procedure TEnvironmentFallbackHandler.ClearMockEnvironmentVariables;
begin
  if Assigned(FMockEnvironmentVars) then
    FMockEnvironmentVars.Clear;
end;

class procedure TEnvironmentFallbackHandler.EnableMockEnvironment(Enable: Boolean);
begin
  FUseMockEnvironment := Enable;
  if Enable and not Assigned(FMockEnvironmentVars) then
    FMockEnvironmentVars := TDictionary<string, string>.Create;
end;

var
  EnvironmentHandler: IFallbackHandler;

initialization
  // Auto-register the Environment fallback handler
  EnvironmentHandler := TEnvironmentFallbackHandler.Create;
  TFallbackHandlerRegistry.Instance.RegisterFallbackHandler(EnvironmentHandler);

finalization
  // Handler will be cleaned up automatically via interface reference counting
  if Assigned(TEnvironmentFallbackHandler.FMockEnvironmentVars) then
    TEnvironmentFallbackHandler.FMockEnvironmentVars.Free;

end.