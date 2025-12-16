unit ParameterMAX.Handler.TXT;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,
  ParameterMAX.Handlers, ParameterMAX.HandlerRegistry;

type
  TTXTParameterHandler = class(TParameterHandlerBase)
  private
    procedure ProcessLine(const Line: string);
    function IsComment(const Line: string): Boolean;
  public
    function LoadParametersInternal(const FileName: string): Boolean; override;
  end;

implementation

{ TTXTParameterHandler }

function TTXTParameterHandler.LoadParametersInternal(const FileName: string): Boolean;
var
  Lines: TStringList;
  i: Integer;
  Line: string;
begin
  try
    Lines := TStringList.Create;
    try
      Lines.LoadFromFile(FileName);
      FParameters.Clear;
      
      for i := 0 to Lines.Count - 1 do
      begin
        Line := Trim(Lines[i]);
        
        // Skip empty lines and comments
        if (Line = '') or IsComment(Line) then
          Continue;
          
        ProcessLine(Line);
      end;
      
      Result := True;
    finally
      Lines.Free;
    end;
  except
    Result := False;
  end;
end;

procedure TTXTParameterHandler.ProcessLine(const Line: string);
var
  EqualPos: Integer;
  Key, Value: string;
begin
  EqualPos := Pos('=', Line);
  
  if EqualPos > 0 then
  begin
    Key := Trim(Copy(Line, 1, EqualPos - 1));
    Value := Trim(Copy(Line, EqualPos + 1, Length(Line)));
    
    // Remove quotes if present
    if (Length(Value) >= 2) then
    begin
      if ((Value[1] = '"') and (Value[Length(Value)] = '"')) or
         ((Value[1] = '''') and (Value[Length(Value)] = '''')) then
      begin
        Value := Copy(Value, 2, Length(Value) - 2);
      end;
    end;
    
    // Store the parameter (supports dot notation like aaa.bbb=ccc)
    if Key <> '' then
      FParameters.AddOrSetValue(LowerCase(Key), Value);
  end;
end;

function TTXTParameterHandler.IsComment(const Line: string): Boolean;
begin
  // Support common comment styles
  Result := (Length(Line) > 0) and 
            ((Line[1] = '#') or        // Shell/Python style
             (Line[1] = ';') or        // INI style
             (Copy(Line, 1, 2) = '//') or    // C++ style
             (Copy(Line, 1, 2) = '--'));     // SQL style
end;

initialization
  TParameterHandlerRegistry.Instance.RegisterHandler(TTXTParameterHandler, '.txt');

end.