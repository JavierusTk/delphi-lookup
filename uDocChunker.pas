unit uDocChunker;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, uDocTypes, uCHMParser;

type
  /// Documentation chunker
  /// Converts parsed documentation pages into indexable chunks
  TDocChunker = class
  private
    FParser: TCHMParser;
    FMaxChunkSize: Integer;

    function GenerateChunkID(const AName, AType: string): string;
    function ChunkClassPage(const APage: TDocPage): TArray<TDocChunk>;
    function ChunkConceptualPage(const APage: TDocPage): TArray<TDocChunk>;
    function ChunkSingleTopic(const APage: TDocPage): TDocChunk;
    function SplitByHeadings(const AContent: string): TArray<string>;

  public
    constructor Create;
    destructor Destroy; override;

    /// Chunk a documentation page
    function ChunkPage(const APage: TDocPage): TArray<TDocChunk>;

    /// Chunk multiple pages
    function ChunkPages(const APages: TArray<TDocPage>): TArray<TDocChunk>;

    property MaxChunkSize: Integer read FMaxChunkSize write FMaxChunkSize;
  end;

implementation

uses
  StrUtils;

{ TDocChunker }

constructor TDocChunker.Create;
begin
  inherited Create;
  FParser := TCHMParser.Create;
  FMaxChunkSize := 5000; // Max characters per chunk
end;

destructor TDocChunker.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

function TDocChunker.ChunkPage(const APage: TDocPage): TArray<TDocChunk>;
var
  Metadata: TDocMetadata;
  ParsedPage: TDocPage;
begin
  SetLength(Result, 0);

  if APage.RawHTML = '' then
    Exit;

  // Re-parse to extract metadata
  ParsedPage := FParser.ParseHTMLContent(APage.RawHTML, APage.SourceFile);

  // Extract metadata
  Metadata.Clear;
  Metadata.FullyQualifiedName := ParsedPage.Title;

  // Detect doc type from title/content
  if ContainsText(ParsedPage.Title, ' Property') then
    Metadata.DocType := dtProperty
  else if ContainsText(ParsedPage.Title, ' Method') or
          ContainsText(ParsedPage.Title, ' Procedure') or
          ContainsText(ParsedPage.Title, ' Function') then
    Metadata.DocType := dtMethod
  else if ContainsText(ParsedPage.Title, ' Class') then
    Metadata.DocType := dtClass
  else if ContainsText(ParsedPage.Title, ' Event') then
    Metadata.DocType := dtEvent
  else if ContainsText(ParsedPage.Title, 'Example') then
    Metadata.DocType := dtExample
  else
    Metadata.DocType := dtConceptual;

  // Detect framework
  if StartsText('Vcl.', ParsedPage.Title) then
    Metadata.Framework := dfVCL
  else if StartsText('FMX.', ParsedPage.Title) then
    Metadata.Framework := dfFMX
  else if StartsText('System.', ParsedPage.Title) then
    Metadata.Framework := dfRTL
  else
    Metadata.Framework := dfNone;

  // Extract unit name
  if Pos('.', ParsedPage.Title) > 0 then
    Metadata.UnitName := Copy(ParsedPage.Title, 1, Pos('.', ParsedPage.Title) - 1);

  // Extract short name
  if Pos('.', ParsedPage.Title) > 0 then
    Metadata.ShortName := Copy(ParsedPage.Title, LastDelimiter('.', ParsedPage.Title) + 1, MaxInt)
  else
    Metadata.ShortName := ParsedPage.Title;

  // Choose chunking strategy based on doc type
  case Metadata.DocType of
    dtClass:
      Result := ChunkClassPage(ParsedPage);
    dtConceptual:
      Result := ChunkConceptualPage(ParsedPage);
    else
      // Single chunk for simple topics
      SetLength(Result, 1);
      Result[0] := ChunkSingleTopic(ParsedPage);
  end;
end;

function TDocChunker.ChunkPages(const APages: TArray<TDocPage>): TArray<TDocChunk>;
var
  Page: TDocPage;
  Chunks: TArray<TDocChunk>;
  Chunk: TDocChunk;
  AllChunks: TList;
  I: Integer;
begin
  AllChunks := TList.Create;
  try
    for Page in APages do
    begin
      Chunks := ChunkPage(Page);
      for Chunk in Chunks do
        AllChunks.Add(@Chunk);
    end;

    SetLength(Result, AllChunks.Count);
    for I := 0 to AllChunks.Count - 1 do
      Result[I] := TDocChunk(AllChunks[I]^);
  finally
    AllChunks.Free;
  end;
end;

function TDocChunker.GenerateChunkID(const AName, AType: string): string;
begin
  // Generate unique ID: lowercase name with underscores
  Result := LowerCase(AName);
  Result := StringReplace(Result, '.', '_', [rfReplaceAll]);
  Result := StringReplace(Result, ' ', '_', [rfReplaceAll]);
  Result := Result + '_' + AType;
end;

function TDocChunker.ChunkClassPage(const APage: TDocPage): TArray<TDocChunk>;
var
  Chunk: TDocChunk;
begin
  // For class pages, create a single summary chunk
  // In a full implementation, we would parse the class structure
  // and create separate chunks for each member

  SetLength(Result, 1);

  Chunk.Clear;
  Chunk.ID := GenerateChunkID(APage.Title, 'class');
  Chunk.Name := APage.Title;
  Chunk.Content := APage.ExtractedText;
  Chunk.SourceFile := APage.SourceFile;
  Chunk.Metadata.FullyQualifiedName := APage.Title;
  Chunk.Metadata.DocType := dtClass;

  // Extract framework
  if StartsText('Vcl.', APage.Title) then
    Chunk.Metadata.Framework := dfVCL
  else if StartsText('FMX.', APage.Title) then
    Chunk.Metadata.Framework := dfFMX
  else
    Chunk.Metadata.Framework := dfRTL;

  Result[0] := Chunk;
end;

function TDocChunker.ChunkConceptualPage(const APage: TDocPage): TArray<TDocChunk>;
var
  Sections: TArray<string>;
  I: Integer;
  Chunk: TDocChunk;
begin
  // For large conceptual pages, split by headings
  if Length(APage.ExtractedText) > FMaxChunkSize then
  begin
    Sections := SplitByHeadings(APage.ExtractedText);

    SetLength(Result, Length(Sections));
    for I := 0 to High(Sections) do
    begin
      Chunk.Clear;
      Chunk.ID := GenerateChunkID(APage.Title, Format('section_%d', [I]));
      Chunk.Name := APage.Title;
      Chunk.Content := Sections[I];
      Chunk.SourceFile := APage.SourceFile;
      Chunk.Metadata.FullyQualifiedName := APage.Title;
      Chunk.Metadata.DocType := dtConceptual;
      Chunk.Metadata.Framework := dfNone;

      Result[I] := Chunk;
    end;
  end
  else
  begin
    // Single chunk
    SetLength(Result, 1);
    Result[0] := ChunkSingleTopic(APage);
  end;
end;

function TDocChunker.ChunkSingleTopic(const APage: TDocPage): TDocChunk;
begin
  Result.Clear;
  Result.ID := GenerateChunkID(APage.Title, 'topic');
  Result.Name := APage.Title;
  Result.Content := APage.ExtractedText;
  Result.SourceFile := APage.SourceFile;
  Result.Metadata.FullyQualifiedName := APage.Title;

  // Detect type from title
  if ContainsText(APage.Title, 'Property') then
    Result.Metadata.DocType := dtProperty
  else if ContainsText(APage.Title, 'Method') or
          ContainsText(APage.Title, 'Function') or
          ContainsText(APage.Title, 'Procedure') then
    Result.Metadata.DocType := dtMethod
  else if ContainsText(APage.Title, 'Class') then
    Result.Metadata.DocType := dtClass
  else
    Result.Metadata.DocType := dtConceptual;

  // Detect framework
  if StartsText('Vcl.', APage.Title) then
    Result.Metadata.Framework := dfVCL
  else if StartsText('FMX.', APage.Title) then
    Result.Metadata.Framework := dfFMX
  else if StartsText('System.', APage.Title) then
    Result.Metadata.Framework := dfRTL
  else
    Result.Metadata.Framework := dfNone;
end;

function TDocChunker.SplitByHeadings(const AContent: string): TArray<string>;
var
  Lines: TStringList;
  Sections: TStringList;
  CurrentSection: TStringBuilder;
  I: Integer;
  Line: string;
begin
  SetLength(Result, 0);

  Lines := TStringList.Create;
  Sections := TStringList.Create;
  CurrentSection := TStringBuilder.Create;
  try
    Lines.Text := AContent;

    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];

      // Simple heading detection (lines that are all caps or start with numbers)
      if (Length(Line) > 0) and
         (CharInSet(Line[1], ['0'..'9']) or (UpperCase(Line) = Line)) and
         (Length(Line) < 100) then
      begin
        // Start new section
        if CurrentSection.Length > 0 then
        begin
          Sections.Add(CurrentSection.ToString);
          CurrentSection.Clear;
        end;
      end;

      CurrentSection.AppendLine(Line);
    end;

    // Add last section
    if CurrentSection.Length > 0 then
      Sections.Add(CurrentSection.ToString);

    // Convert to array
    SetLength(Result, Sections.Count);
    for I := 0 to Sections.Count - 1 do
      Result[I] := Sections[I];

  finally
    CurrentSection.Free;
    Sections.Free;
    Lines.Free;
  end;
end;

end.
