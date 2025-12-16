unit ParameterMAX.Handler.INI;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, System.IOUtils,
  ParameterMAX.Handlers, ParameterMAX.HandlerRegistry;

type
  TINIParameterHandler = class(TParameterHandlerBase)
  private
    procedure ProcessINIFile(INIFile: TIniFile);
  public
    function LoadParametersInternal(const FileName: string): Boolean; override;
  end;

implementation

{ TINIParameterHandler }

function TINIParameterHandler.LoadParametersInternal(const FileName: string): Boolean;
var
  INIFile: TIniFile;
begin
  try
    INIFile := TIniFile.Create(FileName);
    try
      FParameters.Clear;
      ProcessINIFile(INIFile);
      Result := True;
    finally
      INIFile.Free;
    end;
  except
    Result := False;
  end;
end;

procedure TINIParameterHandler.ProcessINIFile(INIFile: TIniFile);
var
  Sections: TStringList;
  Keys: TStringList;
  Section, Key, Value, ParamName: string;
  i, j: Integer;
begin
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  try
    INIFile.ReadSections(Sections);
    
    for i := 0 to Sections.Count - 1 do
    begin
      Section := Sections[i];
      Keys.Clear;
      INIFile.ReadSection(Section, Keys);
      
      for j := 0 to Keys.Count - 1 do
      begin
        Key := Keys[j];
        Value := INIFile.ReadString(Section, Key, '');
        
        // Special handling for [global] section - parameters stored without prefix
        if SameText(Section, 'global') then
          ParamName := Key
        else
          ParamName := Section + '.' + Key;
          
        FParameters.AddOrSetValue(LowerCase(ParamName), Value);
      end;
    end;
  finally
    Keys.Free;
    Sections.Free;
  end;
end;

initialization
  TParameterHandlerRegistry.Instance.RegisterHandler(TINIParameterHandler, '.ini');

end.