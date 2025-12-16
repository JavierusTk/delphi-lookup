unit ParameterMAX.HandlerRegistry;

interface

uses
  System.SysUtils, System.Generics.Collections, System.SyncObjs,
  ParameterMAX.Handlers;

type
  TParameterHandlerRegistry = class
  private
    class var FInstance: TParameterHandlerRegistry;
    class var FLock: TCriticalSection;
    
    FHandlers: TDictionary<string, TParameterHandlerClass>;
    
    constructor Create;
  public
    destructor Destroy; override;
    
    class function Instance: TParameterHandlerRegistry;
    class procedure DestroyInstance;
    
    procedure RegisterHandler(HandlerClass: TParameterHandlerClass; const Extension: string);
    function GetHandler(const Extension: string): TParameterHandlerClass;
    function IsHandlerRegistered(const Extension: string): Boolean;
    function GetRegisteredExtensions: TArray<string>;
  end;

implementation

{ TParameterHandlerRegistry }

constructor TParameterHandlerRegistry.Create;
begin
  inherited Create;
  FHandlers := TDictionary<string, TParameterHandlerClass>.Create;
end;

destructor TParameterHandlerRegistry.Destroy;
begin
  FHandlers.Free;
  inherited Destroy;
end;

class function TParameterHandlerRegistry.Instance: TParameterHandlerRegistry;
begin
  if not Assigned(FInstance) then
  begin
    if not Assigned(FLock) then
      FLock := TCriticalSection.Create;
      
    FLock.Enter;
    try
      if not Assigned(FInstance) then
        FInstance := TParameterHandlerRegistry.Create;
    finally
      FLock.Leave;
    end;
  end;
  Result := FInstance;
end;

class procedure TParameterHandlerRegistry.DestroyInstance;
begin
  if Assigned(FLock) then
  begin
    FLock.Enter;
    try
      if Assigned(FInstance) then
      begin
        FInstance.Free;
        FInstance := nil;
      end;
    finally
      FLock.Leave;
    end;
  end;
end;

procedure TParameterHandlerRegistry.RegisterHandler(HandlerClass: TParameterHandlerClass; const Extension: string);
var
  NormalizedExtension: string;
begin
  if not Assigned(FLock) then
    FLock := TCriticalSection.Create;
    
  FLock.Enter;
  try
    NormalizedExtension := LowerCase(Extension);
    if not NormalizedExtension.StartsWith('.') then
      NormalizedExtension := '.' + NormalizedExtension;
      
    FHandlers.AddOrSetValue(NormalizedExtension, HandlerClass);
  finally
    FLock.Leave;
  end;
end;

function TParameterHandlerRegistry.GetHandler(const Extension: string): TParameterHandlerClass;
var
  NormalizedExtension: string;
begin
  NormalizedExtension := LowerCase(Extension);
  if not NormalizedExtension.StartsWith('.') then
    NormalizedExtension := '.' + NormalizedExtension;
    
  if not FHandlers.TryGetValue(NormalizedExtension, Result) then
    Result := nil;
end;

function TParameterHandlerRegistry.IsHandlerRegistered(const Extension: string): Boolean;
var
  NormalizedExtension: string;
begin
  NormalizedExtension := LowerCase(Extension);
  if not NormalizedExtension.StartsWith('.') then
    NormalizedExtension := '.' + NormalizedExtension;
    
  Result := FHandlers.ContainsKey(NormalizedExtension);
end;

function TParameterHandlerRegistry.GetRegisteredExtensions: TArray<string>;
begin
  Result := FHandlers.Keys.ToArray;
end;

initialization

finalization
  TParameterHandlerRegistry.DestroyInstance;
  if Assigned(TParameterHandlerRegistry.FLock) then
  begin
    TParameterHandlerRegistry.FLock.Free;
    TParameterHandlerRegistry.FLock := nil;
  end;

end.