program CheckFTS5;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  Data.DB,
  FireDAC.Comp.Client,
  uDatabaseConnection in 'uDatabaseConnection.pas';

var
  Connection: TFDConnection;
  Query: TFDQuery;
  DatabaseFile: string;

begin
  try
    DatabaseFile := TPath.Combine(ExtractFilePath(ParamStr(0)), 'delphi_symbols.db');

    if not FileExists(DatabaseFile) then
    begin
      WriteLn('Database not found: ', DatabaseFile);
      WriteLn('Please run delphi-indexer first.');
      Halt(1);
    end;

    WriteLn('=== FTS5 Availability Check ===');
    WriteLn('Database: ', DatabaseFile);
    WriteLn;

    Connection := TFDConnection.Create(nil);
    Query := TFDQuery.Create(nil);
    try
      WriteLn('Connecting to database...');
      WriteLn('Using FireDAC with external sqlite3.dll');
      WriteLn;

      TDatabaseConnectionHelper.ConfigureConnection(Connection, DatabaseFile, False);
      Connection.Open;

      Query.Connection := Connection;

      // Test 1: Check SQLite version
      WriteLn('1. SQLite Version:');
      Query.SQL.Text := 'SELECT sqlite_version() as version';
      Query.Open;
      WriteLn('   Version: ', Query.FieldByName('version').AsString);
      Query.Close;
      WriteLn;

      // Test 2: Check compile options
      WriteLn('2. Compile Options (checking for FTS5):');
      try
        Query.SQL.Text := 'PRAGMA compile_options';
        Query.Open;

        var FTS5Found := False;
        while not Query.EOF do
        begin
          var Option := Query.Fields[0].AsString;
          if Pos('FTS5', UpperCase(Option)) > 0 then
          begin
            WriteLn('   ✓ ', Option);
            FTS5Found := True;
          end;
          Query.Next;
        end;

        if not FTS5Found then
          WriteLn('   ✗ FTS5 not found in compile options');

        Query.Close;
      except
        on E: Exception do
          WriteLn('   Error reading compile options: ', E.Message);
      end;
      WriteLn;

      // Test 3: Try to create a test FTS5 table
      WriteLn('3. FTS5 Functionality Test:');
      try
        Query.SQL.Text := 'CREATE VIRTUAL TABLE IF NOT EXISTS test_fts5 USING fts5(content)';
        Query.ExecSQL;
        WriteLn('   ✓ FTS5 table created successfully');

        Query.SQL.Text := 'DROP TABLE test_fts5';
        Query.ExecSQL;
        WriteLn('   ✓ FTS5 is AVAILABLE and working!');
      except
        on E: Exception do
        begin
          WriteLn('   ✗ FTS5 is NOT available');
          WriteLn('   Error: ', E.Message);
        end;
      end;
      WriteLn;

      // Test 4: Check if symbols_fts exists
      WriteLn('4. Checking if symbols_fts table exists:');
      try
        Query.SQL.Text := 'SELECT count(*) as cnt FROM symbols_fts';
        Query.Open;
        WriteLn('   ✓ symbols_fts table exists with ', Query.FieldByName('cnt').AsInteger, ' entries');
        Query.Close;
      except
        on E: Exception do
        begin
          WriteLn('   ✗ symbols_fts table does NOT exist');
          WriteLn('   This means FTS5 was not available during indexing');
        end;
      end;

    finally
      Query.Free;
      Connection.Free;
    end;

    WriteLn;
    WriteLn('=== Summary ===');
    WriteLn('Run delphi-indexer to create/rebuild the FTS5 index if available.');

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      Halt(1);
    end;
  end;
end.
