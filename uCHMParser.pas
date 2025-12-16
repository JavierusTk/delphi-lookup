unit uCHMParser;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, StrUtils, System.RegularExpressions, uDocTypes;

type
  /// HTML parser for Delphi documentation
  TCHMParser = class
  private
    FHTMLContent: string;
    FSourceFile: string;

    function ExtractTitle: string;
    function ExtractMainContent: string;
    function StripHTMLTags(const HTML: string): string;

  public
    constructor Create;
    destructor Destroy; override;

    /// Parse HTML file and extract documentation
    function ParseHTMLFile(const AFilePath: string): TDocPage;

    /// Parse HTML content directly
    function ParseHTMLContent(const AHTML, ASourceFile: string): TDocPage;
  end;

implementation

{ TCHMParser }

constructor TCHMParser.Create;
begin
  inherited Create;
end;

destructor TCHMParser.Destroy;
begin
  inherited Destroy;
end;

function TCHMParser.ParseHTMLFile(const AFilePath: string): TDocPage;
var
  SL: TStringList;
begin
  Result.Clear;

  if not FileExists(AFilePath) then
    Exit;

  SL := TStringList.Create;
  try
    SL.LoadFromFile(AFilePath);
    Result := ParseHTMLContent(SL.Text, AFilePath);
  finally
    SL.Free;
  end;
end;

function TCHMParser.ParseHTMLContent(const AHTML, ASourceFile: string): TDocPage;
begin
  Result.Clear;
  FHTMLContent := AHTML;
  FSourceFile := ASourceFile;

  Result.RawHTML := AHTML;
  Result.SourceFile := ASourceFile;
  Result.Title := ExtractTitle;
  Result.ExtractedText := ExtractMainContent;

  // Will be chunked later by uDocChunker
end;

function TCHMParser.ExtractTitle: string;
var
  Match: TMatch;
begin
  Result := '';

  // Try to find <title> tag
  Match := TRegEx.Match(FHTMLContent, '<title[^>]*>([^<]+)</title>', [roIgnoreCase]);
  if Match.Success then
  begin
    Result := Trim(Match.Groups[1].Value);
    // Clean up common suffixes
    Result := StringReplace(Result, ' - RAD Studio API Documentation', '', [rfIgnoreCase]);
    Result := StringReplace(Result, ' - RAD Studio', '', [rfIgnoreCase]);
    Exit;
  end;

  // Try to find <h1> tag
  Match := TRegEx.Match(FHTMLContent, '<h1[^>]*>([^<]+)</h1>', [roIgnoreCase]);
  if Match.Success then
  begin
    Result := Trim(Match.Groups[1].Value);
    Exit;
  end;

  // Fallback: extract filename
  Result := ExtractFileName(FSourceFile);
  Result := ChangeFileExt(Result, '');
end;

function TCHMParser.ExtractMainContent: string;
var
  Content: string;
  Match: TMatch;
begin
  Content := FHTMLContent;

  // Remove <script> and <style> tags
  Content := TRegEx.Replace(Content, '<script[^>]*>.*?</script>', '',
    [roIgnoreCase, roSingleLine]);
  Content := TRegEx.Replace(Content, '<style[^>]*>.*?</style>', '',
    [roIgnoreCase, roSingleLine]);

  // Try to extract main content area
  // Look for common content div classes
  Match := TRegEx.Match(Content, '<div[^>]*class="[^"]*content[^"]*"[^>]*>(.*?)</div>',
    [roIgnoreCase, roSingleLine]);
  if Match.Success then
    Content := Match.Groups[1].Value
  else
  begin
    // Fallback: extract body content
    Match := TRegEx.Match(Content, '<body[^>]*>(.*?)</body>',
      [roIgnoreCase, roSingleLine]);
    if Match.Success then
      Content := Match.Groups[1].Value;
  end;

  // Strip HTML tags but preserve structure
  Result := StripHTMLTags(Content);
  Result := Trim(Result);
end;

function TCHMParser.StripHTMLTags(const HTML: string): string;
begin
  Result := HTML;

  // Remove HTML tags
  Result := TRegEx.Replace(Result, '<[^>]+>', ' ', [roIgnoreCase, roSingleLine]);

  // Decode HTML entities
  Result := StringReplace(Result, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll, rfIgnoreCase]);

  // Normalize whitespace
  Result := TRegEx.Replace(Result, '\s+', ' ');

  Result := Trim(Result);
end;

end.
