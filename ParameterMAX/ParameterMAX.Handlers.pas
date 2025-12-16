unit ParameterMAX.Handlers;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.IOUtils;

type
  IParameterHandler = interface
  ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function LoadParameters(const FileName: string): Boolean;
    function GetParameter(const Name: string; const DefaultValue: string = ''): string;
    function HasParameter(const Name: string): Boolean;
    function GetAllParameterNames: TArray<string>;
  end;

  TParameterHandlerClass = class of TParameterHandlerBase;

  TParameterHandlerBase = class(TInterfacedObject, IParameterHandler)
  public
    FParameters: TDictionary<string, string>;
    constructor Create;
    destructor Destroy; override;
    
    function LoadParameters(const FileName: string): Boolean; virtual;
    function LoadParametersInternal(const FileName: string): Boolean; virtual; abstract;
    function GetParameter(const Name: string; const DefaultValue: string = ''): string; virtual;
    function HasParameter(const Name: string): Boolean; virtual;
    function GetAllParameterNames: TArray<string>; virtual;
  end;

  TParameterValue = record
    Value: string;
    Source: string;
    Priority: Integer;
    constructor Create(const AValue, ASource: string; APriority: Integer);
  end;

const
  PRIORITY_COMMANDLINE = 1000;
  PRIORITY_FILE = 800;
  PRIORITY_REGISTRY = 700;
  PRIORITY_ENVIRONMENT = 600;
  PRIORITY_DEFAULT = 400;

implementation

{ TParameterHandlerBase }

constructor TParameterHandlerBase.Create;
begin
  inherited Create;
  FParameters := TDictionary<string, string>.Create;
end;

destructor TParameterHandlerBase.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

function TParameterHandlerBase.GetParameter(const Name: string; const DefaultValue: string): string;
begin
  if not FParameters.TryGetValue(LowerCase(Name), Result) then
    Result := DefaultValue;
end;

function TParameterHandlerBase.HasParameter(const Name: string): Boolean;
begin
  Result := FParameters.ContainsKey(LowerCase(Name));
end;

function TParameterHandlerBase.LoadParameters(const FileName: string): Boolean;
begin
  Result := False;
  try
    if not TFile.Exists(FileName) then
      Exit;
    Result := LoadParametersInternal(FileName);
  except
    Result := False;
  end;
end;

function TParameterHandlerBase.GetAllParameterNames: TArray<string>;
begin
  Result := FParameters.Keys.ToArray;
end;

{ TParameterValue }

constructor TParameterValue.Create(const AValue, ASource: string; APriority: Integer);
begin
  Value := AValue;
  Source := ASource;
  Priority := APriority;
end;

end.