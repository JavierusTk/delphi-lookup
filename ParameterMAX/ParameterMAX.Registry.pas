unit ParameterMAX.Registry;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, System.Win.Registry,
  ParameterMAX.FallbackHandlers;

type
  TRegistryFallbackHandler = class(TInterfacedObject, IFallbackHandler)
  private
    FRegistryKey: string;
    FRootKey: HKEY;
    FCheckBothRoots: Boolean;
    FEnabled: Boolean;
    
    function RegistryValueToString(Reg: TRegistry; const ValueName: string): string;
    function GetParameterFromSingleRoot(const Name: string; RootKey: HKEY): string;
  public
    constructor Create;
    
    // IFallbackHandler implementation
    function GetParameter(const Name: string; out Value: string): Boolean;
    function HasParameter(const Name: string): Boolean;
    function GetPriority: Integer;
    function GetName: string;
    procedure Configure(const Config: string);
  end;

implementation

{ TRegistryFallbackHandler }

constructor TRegistryFallbackHandler.Create;
begin
  inherited Create;
  FEnabled := False;
  FRootKey := HKEY_CURRENT_USER;
  FCheckBothRoots := False;
end;

procedure TRegistryFallbackHandler.Configure(const Config: string);
var
  SlashPos: Integer;
  RootKeyStr: string;
begin
  if Config = '' then
  begin
    FEnabled := False;
    Exit;
  end;
  
  FEnabled := True;
  
  // Parse registry path format: HKEY_CURRENT_USER\Software\MyApp
  // or just Software\MyApp (check both HKCU and HKLM)
  
  SlashPos := Pos('\', Config);
  if SlashPos > 0 then
  begin
    RootKeyStr := UpperCase(Copy(Config, 1, SlashPos - 1));
    
    // Check if it's a valid root key
    if (RootKeyStr = 'HKEY_CURRENT_USER') or (RootKeyStr = 'HKCU') then
    begin
      FRootKey := HKEY_CURRENT_USER;
      FRegistryKey := Copy(Config, SlashPos + 1, Length(Config));
      FCheckBothRoots := False;
    end
    else if (RootKeyStr = 'HKEY_LOCAL_MACHINE') or (RootKeyStr = 'HKLM') then
    begin
      FRootKey := HKEY_LOCAL_MACHINE;
      FRegistryKey := Copy(Config, SlashPos + 1, Length(Config));
      FCheckBothRoots := False;
    end
    else if (RootKeyStr = 'HKEY_CLASSES_ROOT') or (RootKeyStr = 'HKCR') then
    begin
      FRootKey := HKEY_CLASSES_ROOT;
      FRegistryKey := Copy(Config, SlashPos + 1, Length(Config));
      FCheckBothRoots := False;
    end
    else if (RootKeyStr = 'HKEY_USERS') or (RootKeyStr = 'HKU') then
    begin
      FRootKey := HKEY_USERS;
      FRegistryKey := Copy(Config, SlashPos + 1, Length(Config));
      FCheckBothRoots := False;
    end
    else if (RootKeyStr = 'HKEY_CURRENT_CONFIG') or (RootKeyStr = 'HKCC') then
    begin
      FRootKey := HKEY_CURRENT_CONFIG;
      FRegistryKey := Copy(Config, SlashPos + 1, Length(Config));
      FCheckBothRoots := False;
    end
    else
    begin
      // Unrecognized root key, treat entire path as subkey and check both HKCU and HKLM
      FRootKey := HKEY_CURRENT_USER;
      FRegistryKey := Config;
      FCheckBothRoots := True;
    end;
  end
  else
  begin
    // No root key specified, check both HKCU and HKLM
    FRootKey := HKEY_CURRENT_USER;
    FRegistryKey := Config;
    FCheckBothRoots := True;
  end;
end;

function TRegistryFallbackHandler.GetParameter(const Name: string; out Value: string): Boolean;
begin
  Result := False;
  Value := '';
  
  if not FEnabled or (FRegistryKey = '') then
    Exit;
  
  if FCheckBothRoots then
  begin
    // Check HKCU first, then HKLM
    Value := GetParameterFromSingleRoot(Name, HKEY_CURRENT_USER);
    if Value = '' then
      Value := GetParameterFromSingleRoot(Name, HKEY_LOCAL_MACHINE);
  end
  else
  begin
    // Check only the specified root
    Value := GetParameterFromSingleRoot(Name, FRootKey);
  end;
  
  Result := Value <> '';
end;

function TRegistryFallbackHandler.HasParameter(const Name: string): Boolean;
var
  Value: string;
begin
  Result := GetParameter(Name, Value);
end;

function TRegistryFallbackHandler.GetPriority: Integer;
begin
  Result := PRIORITY_REGISTRY;
end;

function TRegistryFallbackHandler.GetName: string;
begin
  Result := 'Registry';
end;

function TRegistryFallbackHandler.GetParameterFromSingleRoot(const Name: string; RootKey: HKEY): string;
var
  Reg: TRegistry;
  KeyPath, ValueName: string;
  LastDotPos: Integer;
begin
  Result := '';
  
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := RootKey;
    
    // Handle dot notation: database.host -> Key: database, Value: host
    LastDotPos := LastDelimiter('.', Name);
    if LastDotPos > 0 then
    begin
      KeyPath := FRegistryKey + '\' + StringReplace(Copy(Name, 1, LastDotPos - 1), '.', '\', [rfReplaceAll]);
      ValueName := Copy(Name, LastDotPos + 1, Length(Name));
    end
    else
    begin
      KeyPath := FRegistryKey;
      ValueName := Name;
    end;
    
    if Reg.OpenKeyReadOnly(KeyPath) then
    begin
      try
        if Reg.ValueExists(ValueName) then
          Result := RegistryValueToString(Reg, ValueName);
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function TRegistryFallbackHandler.RegistryValueToString(Reg: TRegistry; const ValueName: string): string;
var
  DataType: TRegDataType;
  IntValue: Integer;
begin
  DataType := Reg.GetDataType(ValueName);
  
  case DataType of
    rdString, rdExpandString:
      Result := Reg.ReadString(ValueName);
      
    rdInteger:
      begin
        IntValue := Reg.ReadInteger(ValueName);
        Result := IntToStr(IntValue);
      end;
      
    rdBinary:
      begin
        // For binary data, try to read as integer if 4 bytes
        if Reg.GetDataSize(ValueName) = SizeOf(Integer) then
        begin
          IntValue := Reg.ReadInteger(ValueName);
          Result := IntToStr(IntValue);
        end
        else
          Result := ''; // Cannot convert arbitrary binary data
      end;
      
    else
      begin
        // Try to read as string for other types
        try
          Result := Reg.ReadString(ValueName);
        except
          Result := '';
        end;
      end;
  end;
end;

var
  RegistryHandler: IFallbackHandler;

initialization
  // Auto-register the Registry fallback handler
  RegistryHandler := TRegistryFallbackHandler.Create;
  TFallbackHandlerRegistry.Instance.RegisterFallbackHandler(RegistryHandler);

finalization
  // Handler will be cleaned up automatically via interface reference counting

end.