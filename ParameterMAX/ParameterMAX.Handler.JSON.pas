unit ParameterMAX.Handler.JSON;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils,
  ParameterMAX.Handlers, ParameterMAX.HandlerRegistry;

type
  TJSONParameterHandler = class(TParameterHandlerBase)
  private
    procedure ProcessJSONObject(JSONObj: TJSONObject; const Prefix: string = '');
    function JSONValueToString(JSONValue: TJSONValue): string;
  public
    function LoadParametersInternal(const FileName: string): Boolean; override;
  end;

implementation

{ TJSONParameterHandler }

function TJSONParameterHandler.LoadParametersInternal(const FileName: string): Boolean;
var
  JSONText: string;
  JSONValue: TJSONValue;
begin
  try
    JSONText := TFile.ReadAllText(FileName);
    
    if Trim(JSONText) = '' then
    begin
      Result := False;
      Exit;
    end;
      
    JSONValue := TJSONObject.ParseJSONValue(JSONText);
    try
      if JSONValue is TJSONObject then
      begin
        FParameters.Clear;
        ProcessJSONObject(TJSONObject(JSONValue));
        Result := True;
      end
      else
        Result := False;
    finally
      JSONValue.Free;
    end;
  except
    Result := False;
  end;
end;

procedure TJSONParameterHandler.ProcessJSONObject(JSONObj: TJSONObject; const Prefix: string);
var
  Pair: TJSONPair;
  Key: string;
  Value: string;
begin
  for Pair in JSONObj do
  begin
    if Prefix <> '' then
      Key := Prefix + '.' + Pair.JsonString.Value
    else
      Key := Pair.JsonString.Value;
      
    if Pair.JsonValue is TJSONObject then
    begin
      ProcessJSONObject(TJSONObject(Pair.JsonValue), Key);
    end
    else
    begin
      Value := JSONValueToString(Pair.JsonValue);
      FParameters.AddOrSetValue(LowerCase(Key), Value);
    end;
  end;
end;

function TJSONParameterHandler.JSONValueToString(JSONValue: TJSONValue): string;
begin
  if JSONValue is TJSONString then
    Result := TJSONString(JSONValue).Value
  else if JSONValue is TJSONNumber then
    Result := TJSONNumber(JSONValue).ToString
  else if JSONValue is TJSONBool then
  begin
    if TJSONBool(JSONValue).AsBoolean then
      Result := '1'
    else
      Result := '0';
  end
  else if JSONValue is TJSONNull then
    Result := ''
  else
    Result := JSONValue.ToString;
end;

initialization
  TParameterHandlerRegistry.Instance.RegisterHandler(TJSONParameterHandler, '.json');

end.