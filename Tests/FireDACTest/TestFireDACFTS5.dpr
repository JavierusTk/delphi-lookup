program TestFireDACFTS5;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.IOUtils,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Stan.Async,
  FireDAC.DApt,
  FireDAC.UI.Intf,
  FireDAC.ConsoleUI.Wait,
  FireDAC.Comp.UI,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.Stan.Def,
  FireDAC.Comp.Client,
  Data.DB;

var
  FDConnection: TFDConnection;
  FDQuery: TFDQuery;
  FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink;
  FDGUIxWaitCursor: TFDGUIxWaitCursor;
  ExePath: string;
  TestDBPath: string;
  SQLiteVersion: string;

procedure Log(const Msg: string);
begin
  WriteLn(FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + Msg);
end;

function TestBasicConnection: Boolean;
begin
  Result := False;
  Log('--- Test 1: Basic Connection with External DLL ---');
  try
    FDConnection.Connected := True;

    // Get SQLite version
    FDQuery.SQL.Text := 'SELECT sqlite_version()';
    FDQuery.Open;
    SQLiteVersion := FDQuery.Fields[0].AsString;
    FDQuery.Close;

    Log('  SQLite Version: ' + SQLiteVersion);
    Log('  [PASS] Basic connection works');
    Result := True;
  except
    on E: Exception do
      Log('  [FAIL] ' + E.Message);
  end;
end;

function TestFTS5Support: Boolean;
begin
  Result := False;
  Log('--- Test 2: FTS5 Support ---');
  try
    // Check if FTS5 is compiled in
    FDQuery.SQL.Text := 'SELECT * FROM pragma_compile_options WHERE compile_options LIKE ''%FTS5%''';
    FDQuery.Open;
    if FDQuery.RecordCount > 0 then
    begin
      Log('  FTS5 compile option found: ' + FDQuery.Fields[0].AsString);
    end
    else
    begin
      Log('  Warning: FTS5 not in compile_options, but may still work');
    end;
    FDQuery.Close;

    // Create FTS5 table
    FDQuery.SQL.Text := 'CREATE VIRTUAL TABLE IF NOT EXISTS test_fts USING fts5(content, tokenize="porter unicode61")';
    FDQuery.ExecSQL;
    Log('  Created FTS5 virtual table');

    // Insert test data
    FDQuery.SQL.Text := 'INSERT INTO test_fts(content) VALUES (''Hello world this is a test'')';
    FDQuery.ExecSQL;
    FDQuery.SQL.Text := 'INSERT INTO test_fts(content) VALUES (''Another test document here'')';
    FDQuery.ExecSQL;
    Log('  Inserted test data');

    // Search using FTS5
    FDQuery.SQL.Text := 'SELECT content FROM test_fts WHERE test_fts MATCH ''test''';
    FDQuery.Open;
    Log('  FTS5 search returned ' + IntToStr(FDQuery.RecordCount) + ' results');
    while not FDQuery.Eof do
    begin
      Log('    - ' + FDQuery.Fields[0].AsString);
      FDQuery.Next;
    end;
    FDQuery.Close;

    // Cleanup
    FDQuery.SQL.Text := 'DROP TABLE test_fts';
    FDQuery.ExecSQL;

    Log('  [PASS] FTS5 works correctly');
    Result := True;
  except
    on E: Exception do
      Log('  [FAIL] ' + E.Message);
  end;
end;

function TestLoadExtension: Boolean;
var
  ExtPath: string;
begin
  Result := False;
  Log('--- Test 3: Load Extension via SQL ---');
  try
    // With Extensions=True, we can now use load_extension()
    // SQLite expects the path without .dll extension
    ExtPath := StringReplace(ExePath + 'vec0', '\', '/', [rfReplaceAll]);
    Log('  Loading extension from: ' + ExtPath);

    // Load the extension
    FDQuery.SQL.Text := 'SELECT load_extension(''' + ExtPath + ''')';
    FDQuery.Open;
    FDQuery.Close;
    Log('  Extension loaded successfully');

    // Check vec0 version
    FDQuery.SQL.Text := 'SELECT vec_version()';
    FDQuery.Open;
    Log('  vec0 version: ' + FDQuery.Fields[0].AsString);
    FDQuery.Close;

    // Test basic vector operations
    FDQuery.SQL.Text := 'SELECT vec_length(vec_f32(''[1.0, 2.0, 3.0]''))';
    FDQuery.Open;
    Log('  Vector length test: ' + FDQuery.Fields[0].AsString);
    FDQuery.Close;

    Log('  [PASS] Extension loading works');
    Result := True;
  except
    on E: Exception do
      Log('  [FAIL] ' + E.Message);
  end;
end;

function TestVec0VirtualTable: Boolean;
begin
  Result := False;
  Log('--- Test 4: vec0 Virtual Table ---');
  try
    // Create vec0 virtual table
    FDQuery.SQL.Text := 'CREATE VIRTUAL TABLE IF NOT EXISTS test_vec USING vec0(embedding float[4])';
    FDQuery.ExecSQL;
    Log('  Created vec0 virtual table');

    // Insert test vectors
    FDQuery.SQL.Text := 'INSERT INTO test_vec(rowid, embedding) VALUES (1, vec_f32(''[1.0, 0.0, 0.0, 0.0]''))';
    FDQuery.ExecSQL;
    FDQuery.SQL.Text := 'INSERT INTO test_vec(rowid, embedding) VALUES (2, vec_f32(''[0.0, 1.0, 0.0, 0.0]''))';
    FDQuery.ExecSQL;
    FDQuery.SQL.Text := 'INSERT INTO test_vec(rowid, embedding) VALUES (3, vec_f32(''[0.5, 0.5, 0.0, 0.0]''))';
    FDQuery.ExecSQL;
    Log('  Inserted test vectors');

    // Vector similarity search
    FDQuery.SQL.Text :=
      'SELECT rowid, distance FROM test_vec ' +
      'WHERE embedding MATCH vec_f32(''[1.0, 0.0, 0.0, 0.0]'') ' +
      'ORDER BY distance LIMIT 3';
    FDQuery.Open;
    Log('  Vector search results:');
    while not FDQuery.Eof do
    begin
      Log('    rowid=' + FDQuery.FieldByName('rowid').AsString +
          ' distance=' + FDQuery.FieldByName('distance').AsString);
      FDQuery.Next;
    end;
    FDQuery.Close;

    // Cleanup
    FDQuery.SQL.Text := 'DROP TABLE test_vec';
    FDQuery.ExecSQL;

    Log('  [PASS] vec0 virtual table works');
    Result := True;
  except
    on E: Exception do
      Log('  [FAIL] ' + E.Message);
  end;
end;

begin
  ExePath := ExtractFilePath(ParamStr(0));
  // Go up to RepoServer root where sqlite3.dll and vec0.dll are
  // If running from Win64/Debug, go up 4 levels; if from project root, go up 2 levels
  if Pos('Win64', ExePath) > 0 then
    ExePath := TPath.GetFullPath(ExePath + '..\..\..\..\')
  else if Pos('Win32', ExePath) > 0 then
    ExePath := TPath.GetFullPath(ExePath + '..\..\..\..\')
  else
    ExePath := TPath.GetFullPath(ExePath + '..\..\');
  TestDBPath := ExePath + 'test_firedac.db';

  WriteLn('============================================');
  WriteLn('FireDAC + FTS5 + vec0 Compatibility Test');
  WriteLn('============================================');
  WriteLn;
  WriteLn('Exe Path: ' + ExePath);
  WriteLn('Test DB:  ' + TestDBPath);
  WriteLn('SQLite DLL: ' + ExePath + 'sqlite3.dll');
  WriteLn('Vec0 DLL:   ' + ExePath + 'vec0.dll');
  WriteLn;

  // Check DLLs exist
  if not FileExists(ExePath + 'sqlite3.dll') then
  begin
    WriteLn('ERROR: sqlite3.dll not found at ' + ExePath);
    WriteLn('Press Enter to exit...');
    ReadLn;
    Exit;
  end;

  if not FileExists(ExePath + 'vec0.dll') then
  begin
    WriteLn('ERROR: vec0.dll not found at ' + ExePath);
    WriteLn('Press Enter to exit...');
    ReadLn;
    Exit;
  end;

  // Delete old test database
  if FileExists(TestDBPath) then
    DeleteFile(TestDBPath);

  FDGUIxWaitCursor := TFDGUIxWaitCursor.Create(nil);
  FDPhysSQLiteDriverLink := TFDPhysSQLiteDriverLink.Create(nil);
  FDConnection := TFDConnection.Create(nil);
  FDQuery := TFDQuery.Create(nil);
  try
    // Configure driver link to use external DLL
    FDPhysSQLiteDriverLink.VendorLib := ExePath + 'sqlite3.dll';
    Log('Configured VendorLib: ' + FDPhysSQLiteDriverLink.VendorLib);

    // Configure connection
    FDConnection.DriverName := 'SQLite';
    FDConnection.Params.Database := TestDBPath;
    FDConnection.Params.Values['LockingMode'] := 'Normal';
    FDConnection.Params.Values['OpenMode'] := 'CreateUTF8';
    FDConnection.Params.Values['EnableSharedCache'] := 'False';
    // Enable extension loading capability
    FDConnection.Params.Values['Extensions'] := 'True';
    FDConnection.LoginPrompt := False;
    Log('Configured Extensions: True (enabled)');

    FDQuery.Connection := FDConnection;

    WriteLn;

    // Run tests
    if TestBasicConnection then
    begin
      // Set WAL mode for better concurrency
      FDQuery.SQL.Text := 'PRAGMA journal_mode=WAL';
      FDQuery.ExecSQL;

      TestFTS5Support;

      // Extension should be loaded via Extensions connection parameter
      TestLoadExtension;
      TestVec0VirtualTable;
    end;

    FDConnection.Connected := False;

  finally
    FDQuery.Free;
    FDConnection.Free;
    FDPhysSQLiteDriverLink.Free;
    FDGUIxWaitCursor.Free;
  end;

  // Cleanup test database
  if FileExists(TestDBPath) then
    DeleteFile(TestDBPath);
  if FileExists(TestDBPath + '-wal') then
    DeleteFile(TestDBPath + '-wal');
  if FileExists(TestDBPath + '-shm') then
    DeleteFile(TestDBPath + '-shm');

  WriteLn;
  WriteLn('============================================');
  WriteLn('Test complete. Press Enter to exit...');
  ReadLn;
end.
