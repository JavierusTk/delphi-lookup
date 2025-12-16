program CheckFTS5Index;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client,
  uDatabaseConnection in 'uDatabaseConnection.pas';

var
  Connection: TFDConnection;
  Query: TFDQuery;
  HasFTS5Table: Boolean;
  SymbolCount: Integer;
  FTS5Count: Integer;
  DatabaseFile: string;

begin
  try
    WriteLn('Checking FTS5 index status...');
    WriteLn;

    DatabaseFile := ExtractFilePath(ParamStr(0)) + 'delphi_symbols.db';

    Connection := TFDConnection.Create(nil);
    Query := TFDQuery.Create(nil);
    try
      TDatabaseConnectionHelper.ConfigureConnection(Connection, DatabaseFile, False);
      Connection.Open;

      Query.Connection := Connection;

      // Check if symbols_fts table exists
      Query.SQL.Text := 'SELECT name FROM sqlite_master WHERE type=''table'' AND name=''symbols_fts''';
      Query.Open;
      HasFTS5Table := not Query.Eof;
      Query.Close;

      // Count symbols in main table
      Query.SQL.Text := 'SELECT COUNT(*) as cnt FROM symbols';
      Query.Open;
      SymbolCount := Query.FieldByName('cnt').AsInteger;
      Query.Close;

      WriteLn('=== Database Status ===');
      WriteLn(Format('Total symbols indexed: %d', [SymbolCount]));
      WriteLn;

      if HasFTS5Table then
      begin
        // Count entries in FTS5 table
        try
          Query.SQL.Text := 'SELECT COUNT(*) as cnt FROM symbols_fts';
          Query.Open;
          FTS5Count := Query.FieldByName('cnt').AsInteger;
          Query.Close;

          WriteLn('FTS5 Status: ENABLED');
          WriteLn(Format('FTS5 indexed symbols: %d', [FTS5Count]));
          WriteLn;

          if FTS5Count = SymbolCount then
            WriteLn('Result: FTS5 is FULLY SYNCHRONIZED with symbols table')
          else
            WriteLn(Format('WARNING: FTS5 has %d symbols but symbols table has %d (OUT OF SYNC)',
              [FTS5Count, SymbolCount]));
        except
          on E: Exception do
          begin
            WriteLn('FTS5 Status: ERROR');
            WriteLn(Format('Error reading FTS5 table: %s', [E.Message]));
          end;
        end;
      end
      else
      begin
        WriteLn('FTS5 Status: NOT CREATED');
        WriteLn('The symbols_fts table does not exist in the database.');
        WriteLn;
        WriteLn('This means:');
        WriteLn('  - Full-text search is NOT available');
        WriteLn('  - Only exact name matching and fuzzy search are working');
        WriteLn('  - Content-based searches will be slower');
        WriteLn;
        WriteLn('To enable FTS5:');
        WriteLn('  1. The code has been fixed to create FTS5 correctly');
        WriteLn('  2. Re-run delphi-indexer on any folder (it will rebuild FTS5 for ALL symbols)');
      end;

    finally
      Query.Free;
      Connection.Free;
    end;

    WriteLn;
    WriteLn('Press Enter to exit...');
    ReadLn;

  except
    on E: Exception do
    begin
      WriteLn('ERROR: ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
