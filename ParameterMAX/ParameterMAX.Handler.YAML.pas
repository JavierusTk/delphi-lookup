unit ParameterMAX.Handler.YAML;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.StrUtils,
  System.Generics.Collections, ParameterMAX.Handlers, ParameterMAX.HandlerRegistry;

type
  TYAMLParameterHandler = class(TParameterHandlerBase)
  private
    function ParseYAMLLines(Lines: TStringList): Boolean;
    procedure ProcessYAMLLine(const Line: string; var CurrentPath: string; Level: Integer; PathStack: TList<string>; LevelStack: TList<Integer>);
    function GetIndentLevel(const Line: string): Integer;
    function ExtractKeyValue(const Line: string; out Key, Value: string): Boolean;
    function IsComment(const Line: string): Boolean;
    function UnescapeYAMLValue(const Value: string): string;
  public
    function LoadParametersInternal(const FileName: string): Boolean; override;
  end;

implementation

{ TYAMLParameterHandler }

function TYAMLParameterHandler.LoadParametersInternal(const FileName: string): Boolean;
var
  Lines: TStringList;
begin
  try
    Lines := TStringList.Create;
    try
      Lines.LoadFromFile(FileName);
      FParameters.Clear;
      Result := ParseYAMLLines(Lines);
    finally
      Lines.Free;
    end;
  except
    Result := False;
  end;
end;

function TYAMLParameterHandler.ParseYAMLLines(Lines: TStringList): Boolean;
var
  i, LineLevel: Integer;
  Line, CurrentPath: string;
  PathStack: TList<string>;
  LevelStack: TList<Integer>;
begin
  Result := True;
  CurrentPath := '';
  PathStack := TList<string>.Create;
  LevelStack := TList<Integer>.Create;
  
  try
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      
      if (Trim(Line) = '') or IsComment(Line) then
        Continue;
        
      LineLevel := GetIndentLevel(Line);
      
      // Pop stack until we find appropriate level
      while (PathStack.Count > 0) and (LineLevel <= LevelStack[LevelStack.Count - 1]) do
      begin
        PathStack.Delete(PathStack.Count - 1);
        LevelStack.Delete(LevelStack.Count - 1);
      end;
      
      // Build current path efficiently
      if PathStack.Count > 0 then
      begin
        CurrentPath := PathStack[0];
        for var j := 1 to PathStack.Count - 1 do
          CurrentPath := CurrentPath + '.' + PathStack[j];
      end
      else
        CurrentPath := '';
        
      ProcessYAMLLine(Line, CurrentPath, LineLevel, PathStack, LevelStack);
    end;
  finally
    LevelStack.Free;
    PathStack.Free;
  end;
end;

procedure TYAMLParameterHandler.ProcessYAMLLine(const Line: string; var CurrentPath: string; Level: Integer; PathStack: TList<string>; LevelStack: TList<Integer>);
var
  Key, Value, FullKey: string;
  TrimmedLine: string;
begin
  TrimmedLine := Trim(Line);
  
  if not ExtractKeyValue(TrimmedLine, Key, Value) then
    Exit;
    
  if Value = '' then
  begin
    // Add to path stack for nested structure
    PathStack.Add(Key);
    LevelStack.Add(Level);
  end
  else
  begin
    if CurrentPath <> '' then
      FullKey := CurrentPath + '.' + Key
    else
      FullKey := Key;
      
    Value := UnescapeYAMLValue(Value);
    FParameters.AddOrSetValue(LowerCase(FullKey), Value);
  end;
end;

function TYAMLParameterHandler.GetIndentLevel(const Line: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Line) do
  begin
    if Line[i] = ' ' then
      Inc(Result)
    else if Line[i] = #9 then
      Inc(Result, 4)
    else
      Break;
  end;
end;

function TYAMLParameterHandler.ExtractKeyValue(const Line: string; out Key, Value: string): Boolean;
var
  ColonPos: Integer;
begin
  Result := False;
  Key := '';
  Value := '';
  
  ColonPos := Pos(':', Line);
  if ColonPos > 0 then
  begin
    Key := Trim(Copy(Line, 1, ColonPos - 1));
    Value := Trim(Copy(Line, ColonPos + 1, Length(Line)));
    
    if Key <> '' then
      Result := True;
  end;
end;

function TYAMLParameterHandler.IsComment(const Line: string): Boolean;
var
  TrimmedLine: string;
begin
  TrimmedLine := Trim(Line);
  Result := (Length(TrimmedLine) > 0) and (TrimmedLine[1] = '#');
end;

function TYAMLParameterHandler.UnescapeYAMLValue(const Value: string): string;
begin
  Result := Value;
  
  if (Length(Result) >= 2) then
  begin
    if ((Result[1] = '"') and (Result[Length(Result)] = '"')) or
       ((Result[1] = '''') and (Result[Length(Result)] = '''')) then
    begin
      Result := Copy(Result, 2, Length(Result) - 2);
    end;
  end;
  
  if LowerCase(Result) = 'null' then
    Result := '';
end;

initialization
  TParameterHandlerRegistry.Instance.RegisterHandler(TYAMLParameterHandler, '.yaml');
  TParameterHandlerRegistry.Instance.RegisterHandler(TYAMLParameterHandler, '.yml');

end.