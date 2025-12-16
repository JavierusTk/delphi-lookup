unit uDatabaseConnection;

{
  Database Connection Helper for FireDAC SQLite

  This unit provides centralized configuration for FireDAC SQLite connections,
  ensuring consistent settings across all database operations:
  - External sqlite3.dll with FTS5 support
  - Extension loading capability (for vec0.dll)
  - WAL mode for concurrent access
}

interface

uses
  System.SysUtils,
  System.IOUtils,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Phys.Intf,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  FireDAC.UI.Intf,
  FireDAC.ConsoleUI.Wait,
  FireDAC.Comp.UI,
  FireDAC.Comp.Client,
  Data.DB;

type
  /// <summary>
  /// Helper class for creating and configuring FireDAC SQLite connections
  /// </summary>
  TDatabaseConnectionHelper = class
  private
    class var FDriverLink: TFDPhysSQLiteDriverLink;
    class var FGUIxWaitCursor: TFDGUIxWaitCursor;
    class var FInitialized: Boolean;
    class procedure EnsureInitialized;
  public
    /// <summary>Initialize the FireDAC SQLite driver with external DLL</summary>
    class procedure Initialize;

    /// <summary>Finalize and cleanup driver resources</summary>
    class procedure Finalize;

    /// <summary>Configure a connection for existing database (read/write)</summary>
    class procedure ConfigureConnection(AConnection: TFDConnection;
      const ADatabaseFile: string; AEnableExtensions: Boolean = False);

    /// <summary>Configure a connection that creates database if not exists</summary>
    class procedure ConfigureConnectionCreate(AConnection: TFDConnection;
      const ADatabaseFile: string; AEnableExtensions: Boolean = False);

    /// <summary>Load vec0 extension for vector search</summary>
    class procedure LoadVec0Extension(AConnection: TFDConnection);

    /// <summary>Get path to sqlite3.dll</summary>
    class function GetSQLiteDLLPath: string;

    /// <summary>Get path to vec0.dll</summary>
    class function GetVec0DLLPath: string;
  end;

implementation

{ TDatabaseConnectionHelper }

class procedure TDatabaseConnectionHelper.EnsureInitialized;
begin
  if not FInitialized then
    Initialize;
end;

class procedure TDatabaseConnectionHelper.Initialize;
var
  SQLiteDLL: string;
begin
  if FInitialized then
    Exit;

  SQLiteDLL := GetSQLiteDLLPath;

  if not FileExists(SQLiteDLL) then
    raise Exception.CreateFmt('sqlite3.dll not found at: %s', [SQLiteDLL]);

  // Create wait cursor handler for console apps
  FGUIxWaitCursor := TFDGUIxWaitCursor.Create(nil);

  // Configure driver link to use external sqlite3.dll
  FDriverLink := TFDPhysSQLiteDriverLink.Create(nil);
  FDriverLink.VendorLib := SQLiteDLL;

  FInitialized := True;
end;

class procedure TDatabaseConnectionHelper.Finalize;
begin
  if not FInitialized then
    Exit;

  FreeAndNil(FDriverLink);
  FreeAndNil(FGUIxWaitCursor);
  FInitialized := False;
end;

class function TDatabaseConnectionHelper.GetSQLiteDLLPath: string;
var
  ExeDir: string;
begin
  ExeDir := ExtractFilePath(ParamStr(0));

  // Try executable directory first
  Result := ExeDir + 'sqlite3.dll';
  if FileExists(Result) then
    Exit;

  // Try bin/ subdirectory
  Result := ExeDir + 'bin' + PathDelim + 'sqlite3.dll';
  if FileExists(Result) then
    Exit;

  // Try parent's bin/ (for Win64/Release builds)
  Result := TPath.Combine(TPath.Combine(TPath.Combine(ExeDir, '..'), '..'), 'bin');
  Result := TPath.Combine(Result, 'sqlite3.dll');
  Result := TPath.GetFullPath(Result);
end;

class function TDatabaseConnectionHelper.GetVec0DLLPath: string;
var
  ExeDir: string;
begin
  ExeDir := ExtractFilePath(ParamStr(0));

  // Try executable directory first
  Result := ExeDir + 'vec0.dll';
  if FileExists(Result) then
    Exit;

  // Try bin/ subdirectory
  Result := ExeDir + 'bin' + PathDelim + 'vec0.dll';
  if FileExists(Result) then
    Exit;

  // Try parent's bin/ (for Win64/Release builds)
  Result := TPath.Combine(TPath.Combine(TPath.Combine(ExeDir, '..'), '..'), 'bin');
  Result := TPath.Combine(Result, 'vec0.dll');
  Result := TPath.GetFullPath(Result);
end;

class procedure TDatabaseConnectionHelper.ConfigureConnection(
  AConnection: TFDConnection; const ADatabaseFile: string;
  AEnableExtensions: Boolean);
var
  FullPath: string;
begin
  EnsureInitialized;

  // Ensure absolute path
  if not TPath.IsPathRooted(ADatabaseFile) then
    FullPath := TPath.Combine(GetCurrentDir, ADatabaseFile)
  else
    FullPath := ADatabaseFile;

  if not FileExists(FullPath) then
    raise Exception.CreateFmt('Database file not found: %s', [FullPath]);

  AConnection.DriverName := 'SQLite';
  AConnection.Params.Database := FullPath;
  AConnection.Params.Values['LockingMode'] := 'Normal';
  AConnection.LoginPrompt := False;

  if AEnableExtensions then
    AConnection.Params.Values['Extensions'] := 'True';
end;

class procedure TDatabaseConnectionHelper.ConfigureConnectionCreate(
  AConnection: TFDConnection; const ADatabaseFile: string;
  AEnableExtensions: Boolean);
var
  FullPath, DatabaseDir: string;
begin
  EnsureInitialized;

  // Ensure absolute path
  if not TPath.IsPathRooted(ADatabaseFile) then
    FullPath := TPath.Combine(GetCurrentDir, ADatabaseFile)
  else
    FullPath := ADatabaseFile;

  // Ensure directory exists
  DatabaseDir := TPath.GetDirectoryName(FullPath);
  if not TDirectory.Exists(DatabaseDir) then
    TDirectory.CreateDirectory(DatabaseDir);

  AConnection.DriverName := 'SQLite';
  AConnection.Params.Database := FullPath;
  AConnection.Params.Values['LockingMode'] := 'Normal';
  AConnection.Params.Values['OpenMode'] := 'CreateUTF8';
  AConnection.LoginPrompt := False;

  if AEnableExtensions then
    AConnection.Params.Values['Extensions'] := 'True';
end;

class procedure TDatabaseConnectionHelper.LoadVec0Extension(
  AConnection: TFDConnection);
var
  Vec0Path: string;
  Query: TFDQuery;
begin
  Vec0Path := GetVec0DLLPath;

  if not FileExists(Vec0Path) then
    raise Exception.CreateFmt('vec0.dll not found at: %s', [Vec0Path]);

  // SQLite expects path without .dll extension and with forward slashes
  Vec0Path := ChangeFileExt(Vec0Path, '');
  Vec0Path := StringReplace(Vec0Path, '\', '/', [rfReplaceAll]);

  // FireDAC requires Open for SELECT statements, not ExecSQL
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := AConnection;
    Query.SQL.Text := 'SELECT load_extension(''' + Vec0Path + ''')';
    Query.Open;
    Query.Close;
  finally
    Query.Free;
  end;
end;

initialization

finalization
  TDatabaseConnectionHelper.Finalize;

end.
