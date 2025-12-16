unit ParameterMAX.FallbackHandlers;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults;

type
  // Interface for fallback parameter sources (Registry, Environment, etc.)
  IFallbackHandler = interface
    ['{B1C2D3E4-F5A6-7890-BCDE-F12345678901}']
    function GetParameter(const Name: string; out Value: string): Boolean;
    function HasParameter(const Name: string): Boolean;
    function GetPriority: Integer;
    function GetName: string;  // Unique identifier for the handler
    procedure Configure(const Config: string);
  end;

  TFallbackHandlerRegistry = class
  private
    class var FInstance: TFallbackHandlerRegistry;
    FFallbackHandlers: TList<IFallbackHandler>;
    
    constructor Create;
  public
    destructor Destroy; override;
    class function Instance: TFallbackHandlerRegistry;
    class procedure DestroyInstance;
    
    procedure RegisterFallbackHandler(const Handler: IFallbackHandler);
    procedure UnregisterFallbackHandler(const Handler: IFallbackHandler);
    function GetFallbackValue(const Name: string; out Value: string): Boolean;
    function HasFallbackValue(const Name: string): Boolean;
    function GetHandlerByName(const HandlerName: string): IFallbackHandler;
    procedure ConfigureHandler(const HandlerName, Config: string);
    procedure Clear;
    
    function GetFallbackHandlers: TList<IFallbackHandler>;
  end;

const
  // Priority constants for fallback handlers
  PRIORITY_REGISTRY = 700;
  PRIORITY_ENVIRONMENT = 600;
  PRIORITY_CUSTOM = 500;

implementation

{ TFallbackHandlerRegistry }

constructor TFallbackHandlerRegistry.Create;
begin
  inherited Create;
  FFallbackHandlers := TList<IFallbackHandler>.Create;
end;

destructor TFallbackHandlerRegistry.Destroy;
begin
  FFallbackHandlers.Free;
  inherited Destroy;
end;

class function TFallbackHandlerRegistry.Instance: TFallbackHandlerRegistry;
begin
  if not Assigned(FInstance) then
    FInstance := TFallbackHandlerRegistry.Create;
  Result := FInstance;
end;

class procedure TFallbackHandlerRegistry.DestroyInstance;
begin
  FreeAndNil(FInstance);
end;

procedure TFallbackHandlerRegistry.RegisterFallbackHandler(const Handler: IFallbackHandler);
begin
  if not FFallbackHandlers.Contains(Handler) then
  begin
    FFallbackHandlers.Add(Handler);
    FFallbackHandlers.Sort(TComparer<IFallbackHandler>.Construct(
      function(const L, R: IFallbackHandler): Integer
      begin
        // Sort by priority (higher priority first)
        Result := R.GetPriority - L.GetPriority;
      end
    ));
  end;
end;

procedure TFallbackHandlerRegistry.UnregisterFallbackHandler(const Handler: IFallbackHandler);
begin
  FFallbackHandlers.Remove(Handler);
end;

function TFallbackHandlerRegistry.GetFallbackValue(const Name: string; out Value: string): Boolean;
var
  Handler: IFallbackHandler;
begin
  Result := False;
  Value := '';
  
  // Try each fallback handler in priority order
  for Handler in FFallbackHandlers do
  begin
    if Handler.GetParameter(Name, Value) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TFallbackHandlerRegistry.HasFallbackValue(const Name: string): Boolean;
var
  Handler: IFallbackHandler;
begin
  Result := False;
  
  for Handler in FFallbackHandlers do
  begin
    if Handler.HasParameter(Name) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TFallbackHandlerRegistry.Clear;
begin
  FFallbackHandlers.Clear;
end;

function TFallbackHandlerRegistry.GetHandlerByName(const HandlerName: string): IFallbackHandler;
var
  Handler: IFallbackHandler;
begin
  Result := nil;
  for Handler in FFallbackHandlers do
  begin
    if SameText(Handler.GetName, HandlerName) then
    begin
      Result := Handler;
      Exit;
    end;
  end;
end;

procedure TFallbackHandlerRegistry.ConfigureHandler(const HandlerName, Config: string);
var
  Handler: IFallbackHandler;
begin
  Handler := GetHandlerByName(HandlerName);
  if Assigned(Handler) then
    Handler.Configure(Config);
end;

function TFallbackHandlerRegistry.GetFallbackHandlers: TList<IFallbackHandler>;
begin
  Result := FFallbackHandlers;
end;

initialization

finalization
  TFallbackHandlerRegistry.DestroyInstance;

end.