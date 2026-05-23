unit uResultFormatter;

interface

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  uSearchTypes;

type
  TResultFormatter = class
  private
    function FormatSymbolSignature(AResult: TSearchResult): string;
    function FormatInheritanceInfo(AResult: TSearchResult): string;
    function FormatComments(const AComments: string; AMaxLines: Integer = 10): string;
    function FormatContent(const AContent: string; AMaxLines: Integer = 20): string;
    function CleanupWhitespace(const AText: string): string;
    function GetResultTypeDescription(AResult: TSearchResult): string;
    function ExtractMethodSignature(const AContent: string): string;
    function ExtractSignature(AResult: TSearchResult): string;
    function SanitizeForOutput(const AText: string): string;
    procedure FormatCompactSingleResult(AResult: TSearchResult; AIndex: Integer);

  public
    procedure FormatResults(AResults: TSearchResultList; const AQuery: string);
    procedure FormatSingleResult(AResult: TSearchResult; AIndex: Integer);
    procedure FormatCompactResults(AResults: TSearchResultList; const AQuery: string);
    // AI-optimized: single line per result, no chrome. The default since v1.6.
    // See docs/AI-OUTPUT-COMPACT.md.
    procedure FormatUltraCompactSingleResult(AResult: TSearchResult; AIndex: Integer);
    procedure FormatUltraCompactResults(AResults: TSearchResultList; const AQuery: string);
    procedure FormatResultsAsJSON(AResults: TSearchResultList; const AQuery: string;
      ADurationMs: Integer; AIsCacheHit: Boolean);
  end;

implementation

uses
  System.RegularExpressions,
  System.JSON;

{ TResultFormatter }

function TResultFormatter.SanitizeForOutput(const AText: string): string;
begin
  Result := AText;
  
  // Remove or replace characters that might interfere with output
  Result := StringReplace(Result, #13#10, #10, [rfReplaceAll]); // Normalize line endings
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);    // Convert CR to LF
  Result := StringReplace(Result, #9, '  ', [rfReplaceAll]);    // Convert tabs to spaces
  
  // Remove excessive whitespace
  Result := CleanupWhitespace(Result);
end;

function TResultFormatter.CleanupWhitespace(const AText: string): string;
var
  Lines: TStringList;
  I: Integer;
  Line: string;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := AText;
    
    // Clean up each line
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];
      
      // Remove trailing whitespace
      Line := TrimRight(Line);
      
      // Replace multiple spaces with single space (but preserve indentation)
      if Trim(Line) <> '' then
      begin
        var LeadingSpaces := Length(Line) - Length(TrimLeft(Line));
        var TrimmedLine := TrimLeft(Line);
        
        // Collapse multiple spaces in the trimmed part
        while Pos('  ', TrimmedLine) > 0 do
          TrimmedLine := StringReplace(TrimmedLine, '  ', ' ', [rfReplaceAll]);
          
        Line := StringOfChar(' ', LeadingSpaces) + TrimmedLine;
      end;
      
      Lines[I] := Line;
    end;
    
    Result := Lines.Text;
    
    // Remove trailing newlines
    Result := TrimRight(Result);
    
  finally
    Lines.Free;
  end;
end;

function TResultFormatter.GetResultTypeDescription(AResult: TSearchResult): string;
var
  LowerType: string;
begin
  LowerType := LowerCase(AResult.SymbolType);
  if LowerType = 'class' then
    Result := 'class'
  else if LowerType = 'interface' then
    Result := 'interface'
  else if LowerType = 'record' then
    Result := 'record'
  else if LowerType = 'procedure' then
    Result := 'procedure'
  else if LowerType = 'function' then
    Result := 'function'
  else if LowerType = 'constructor' then
    Result := 'constructor'
  else if LowerType = 'destructor' then
    Result := 'destructor'
  else if LowerType = 'property' then
    Result := 'property'
  else if LowerType = 'type' then
    Result := 'type'
  else if LowerType = 'const' then
    Result := 'constant'
  else if LowerType = 'var' then
    Result := 'variable'
  else
    Result := AResult.SymbolType;
end;

function TResultFormatter.FormatSymbolSignature(AResult: TSearchResult): string;
var
  Parts: TStringList;
  I: Integer;
  Interfaces: string;
begin
  Parts := TStringList.Create;
  try
    // Add the primary name
    if AResult.FullName <> '' then
      Parts.Add(AResult.FullName)
    else
      Parts.Add(AResult.Name);
      
    // Add type information
    Parts.Add(Format('(%s)', [GetResultTypeDescription(AResult)]));
    
    // Add parent class info
    if AResult.ParentClass <> '' then
      Parts.Add(Format('extends %s', [AResult.ParentClass]));
      
    // Add interface implementations
    if AResult.ImplementedInterfaces <> '' then
    begin
      Interfaces := StringReplace(AResult.ImplementedInterfaces, ',', ', ', [rfReplaceAll]);
      Parts.Add(Format('implements %s', [Interfaces]));
    end;
    
    // Add visibility
    if (AResult.Visibility <> '') and (AResult.Visibility <> 'public') then
      Parts.Add(Format('[%s]', [AResult.Visibility]));
    
    // Use older Delphi compatible string joining
    Result := '';
    for I := 0 to Parts.Count - 1 do
    begin
      if I > 0 then
        Result := Result + ' ';
      Result := Result + Parts[I];
    end;
    
  finally
    Parts.Free;
  end;
end;

function TResultFormatter.FormatInheritanceInfo(AResult: TSearchResult): string;
var
  Info: TStringList;
  Interfaces: string;
  I: Integer;
begin
  Info := TStringList.Create;
  try
    if AResult.ParentClass <> '' then
      Info.Add(Format('Inherits from: %s', [AResult.ParentClass]));
      
    if AResult.ImplementedInterfaces <> '' then
    begin
      Interfaces := StringReplace(AResult.ImplementedInterfaces, ',', ', ', [rfReplaceAll]);
      Info.Add(Format('Implements: %s', [Interfaces]));
    end;
    
    if Info.Count > 0 then
    begin
      Result := '';
      for I := 0 to Info.Count - 1 do
      begin
        if I > 0 then
          Result := Result + ', ';
        Result := Result + Info[I];
      end;
    end
    else
      Result := '';
      
  finally
    Info.Free;
  end;
end;

function TResultFormatter.ExtractMethodSignature(const AContent: string): string;
var
  Lines: TStringList;
  I: Integer;
  Line: string;
  Match: TMatch;
begin
  Result := '';
  
  Lines := TStringList.Create;
  try
    Lines.Text := AContent;
    
    // Look for method signatures
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      
      // Match procedure/function/constructor/destructor declarations up to first semicolon
      Match := TRegEx.Match(Line, '\b(procedure|function|constructor|destructor)\s+[^;]+;', [roIgnoreCase]);
      if Match.Success then
      begin
        Result := Match.Value;
        Break;
      end;
    end;
    
  finally
    Lines.Free;
  end;
end;

function TResultFormatter.ExtractSignature(AResult: TSearchResult): string;
var
  Lines: TStringList;
  I: Integer;
  Line: string;
begin
  // Extract first non-empty, non-comment line from Content
  // The indexer structures Content to start with the symbol declaration,
  // so the first meaningful line is always the signature
  if Trim(AResult.Content) <> '' then
  begin
    Lines := TStringList.Create;
    try
      Lines.Text := AResult.Content;
      for I := 0 to Lines.Count - 1 do
      begin
        Line := Trim(Lines[I]);
        // Skip empty lines and comment-only lines
        if (Line = '') or Line.StartsWith('//') or Line.StartsWith('{')
          or Line.StartsWith('(*') then
          Continue;
        // Remove trailing inline comments for cleaner output
        var CommentPos := Pos('//', Line);
        if CommentPos > 1 then
          Line := TrimRight(Copy(Line, 1, CommentPos - 1));
        Result := Line;
        Exit;
      end;
    finally
      Lines.Free;
    end;
  end;

  // Fallback: name + type
  Result := AResult.Name + ' (' + GetResultTypeDescription(AResult) + ')';
end;

function TResultFormatter.FormatComments(const AComments: string; AMaxLines: Integer): string;
var
  Lines: TStringList;
  I, Count: Integer;
  Line: string;
  FormattedLines: TStringList;
begin
  if Trim(AComments) = '' then
  begin
    Result := '';
    Exit;
  end;
  
  Lines := TStringList.Create;
  FormattedLines := TStringList.Create;
  try
    Lines.Text := SanitizeForOutput(AComments);
    Count := 0;
    
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      
      if Line <> '' then
      begin
        // Add comment prefix for readability
        if not Line.StartsWith('//') then
          Line := '// ' + Line;
          
        FormattedLines.Add(Line);
        Inc(Count);
        
        if Count >= AMaxLines then
        begin
          if I < Lines.Count - 1 then
            FormattedLines.Add('// ... (more comments available)');
          Break;
        end;
      end;
    end;
    
    Result := FormattedLines.Text.TrimRight;
    
  finally
    Lines.Free;
    FormattedLines.Free;
  end;
end;

function TResultFormatter.FormatContent(const AContent: string; AMaxLines: Integer): string;
var
  Lines: TStringList;
  I, Count: Integer;
  Line: string;
  FormattedLines: TStringList;
  IsInSignature: Boolean;
begin
  if Trim(AContent) = '' then
  begin
    Result := '';
    Exit;
  end;
  
  Lines := TStringList.Create;
  FormattedLines := TStringList.Create;
  try
    Lines.Text := SanitizeForOutput(AContent);
    Count := 0;
    IsInSignature := True;
    
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];
      
      // Skip empty lines at the beginning
      if (Count = 0) and (Trim(Line) = '') then
        Continue;
        
      FormattedLines.Add(Line);
      Inc(Count);
      
      // Stop at implementation section for interface-only code
      if IsInSignature and (Trim(Line).EndsWith(';') or Trim(Line).EndsWith(':')) then
        IsInSignature := False;
        
      if Count >= AMaxLines then
      begin
        if I < Lines.Count - 1 then
          FormattedLines.Add('// ... (more code available)');
        Break;
      end;
    end;
    
    Result := FormattedLines.Text.TrimRight;
    
  finally
    Lines.Free;
    FormattedLines.Free;
  end;
end;

procedure TResultFormatter.FormatSingleResult(AResult: TSearchResult; AIndex: Integer);
var
  MatchTypeText: string;
  CategoryLabel: string;
  InheritanceInfo: string;
  Comments: string;
  Content: string;
begin
  // Determine match type description
  if AResult.IsExactMatch then
    MatchTypeText := 'Exact Match'
  else
  begin
    if AResult.MatchType = 'exact_name' then
      MatchTypeText := 'Exact Match'
    else if AResult.MatchType = 'partial_name' then
      MatchTypeText := 'Partial Name Match'
    else if AResult.MatchType = 'fuzzy_name' then
      MatchTypeText := 'Fuzzy Name Match'
    else if AResult.MatchType = 'full_text' then
      MatchTypeText := 'Content Match'
    else if AResult.MatchType = 'vector_similarity' then
      MatchTypeText := 'Semantic Match'
    else
      MatchTypeText := 'Match';
  end;

  // Add category label to match type
  if AResult.SourceCategory <> '' then
  begin
    if AResult.SourceCategory = 'user' then
      CategoryLabel := 'USER CODE'
    else if AResult.SourceCategory = 'stdlib' then
      CategoryLabel := 'STDLIB'
    else if AResult.SourceCategory = 'third_party' then
      CategoryLabel := 'THIRD PARTY'
    else if AResult.SourceCategory = 'official_help' then
    begin
      CategoryLabel := 'OFFICIAL HELP';
      // Add framework badge for documentation
      if AResult.Framework <> '' then
        CategoryLabel := CategoryLabel + ' [' + AResult.Framework + ']';
    end
    else if AResult.SourceCategory = 'project_docs' then
      CategoryLabel := 'PROJECT DOCS'
    else
      CategoryLabel := UpperCase(AResult.SourceCategory);

    MatchTypeText := MatchTypeText + ' - ' + CategoryLabel;
  end;

  // Add declaration/implementation badge for method types
  if AResult.IsDeclaration then
    MatchTypeText := MatchTypeText + ' [Declaration]'
  else if (AResult.SymbolType = 'function') or (AResult.SymbolType = 'procedure')
       or (AResult.SymbolType = 'constructor') or (AResult.SymbolType = 'destructor') then
    MatchTypeText := MatchTypeText + ' [Impl]';

  // Format the header
  WriteLn(Format('// Result %d (%s): %s', [AIndex, MatchTypeText, FormatSymbolSignature(AResult)]));

  // Add file information
  if AResult.FilePath <> '' then
  begin
    WriteLn(Format('// File: %s', [AResult.FilePath]));
    WriteLn(Format('// Unit: %s', [ChangeFileExt(ExtractFileName(AResult.FilePath), '')]));
  end;

  // Add classification information
  if (AResult.ContentType <> '') or (AResult.SourceCategory <> '') then
  begin
    var ClassificationInfo := Format('Type: %s | Category: %s', [AResult.ContentType, AResult.SourceCategory]);
    if AResult.Framework <> '' then
      ClassificationInfo := ClassificationInfo + ' | Framework: ' + AResult.Framework;
    WriteLn(Format('// %s', [ClassificationInfo]));
  end;

  // Add inheritance information
  InheritanceInfo := FormatInheritanceInfo(AResult);
  if InheritanceInfo <> '' then
    WriteLn(Format('// %s', [InheritanceInfo]));

  // Add score information for debugging
  if AResult.Score > 0 then
    WriteLn(Format('// Relevance: %.2f', [AResult.Score]));

  WriteLn;
  
  // Format comments if available
  Comments := FormatComments(AResult.Comments);
  if Comments <> '' then
  begin
    WriteLn(Comments);
    WriteLn;
  end;
  
  // Format the main content
  Content := FormatContent(AResult.Content);
  if Content <> '' then
  begin
    WriteLn(Content);
  end
  else
  begin
    // Fallback: show method signature if we have it
    var MethodSig := ExtractMethodSignature(AResult.Content);
    if MethodSig <> '' then
      WriteLn(MethodSig)
    else
      WriteLn('// No content available');
  end;
end;

procedure TResultFormatter.FormatCompactSingleResult(AResult: TSearchResult; AIndex: Integer);
var
  Signature, FileName, UnitName, CategoryInfo: string;
begin
  // Extract signature
  Signature := ExtractSignature(AResult);

  // Build declaration badge
  if AResult.IsDeclaration then
    Signature := '[Decl] ' + Signature;

  // Line 1: index + signature
  WriteLn(Format('%d. %s', [AIndex, Signature]));

  // Build location line
  if AResult.FilePath <> '' then
  begin
    FileName := ExtractFileName(AResult.FilePath);
    UnitName := ChangeFileExt(FileName, '');
  end
  else
  begin
    FileName := '?';
    UnitName := '?';
  end;

  // Build category info
  CategoryInfo := AResult.SourceCategory;
  if AResult.Framework <> '' then
    CategoryInfo := CategoryInfo + ', ' + AResult.Framework;

  // Line 2: → filename [unit: X] (category)
  WriteLn(Format('   %s %s [unit: %s] (%s)', [#$E2#$86#$92, FileName, UnitName, CategoryInfo]));
end;

procedure TResultFormatter.FormatCompactResults(AResults: TSearchResultList; const AQuery: string);
var
  I: Integer;
begin
  if AResults.Count = 0 then
  begin
    WriteLn('No results found for this query.');
    WriteLn;
    WriteLn('Try: different search terms, class names (e.g., "TRestServer"), or --full for verbose output.');
    Exit;
  end;

  WriteLn(Format('Found %d result(s) for "%s":', [AResults.Count, AQuery]));
  WriteLn;

  for I := 0 to AResults.Count - 1 do
  begin
    FormatCompactSingleResult(AResults[I], I + 1);

    // Empty line between results (except after last)
    if I < AResults.Count - 1 then
      WriteLn;
  end;
end;

procedure TResultFormatter.FormatUltraCompactSingleResult(AResult: TSearchResult; AIndex: Integer);
var
  Signature, Location, CategoryInfo: string;
begin
  // One line per result, optimized for AI agent consumption.
  // Format: N. [Decl] signature  full-path:line  [category,framework]
  // The full path + line lets an agent jump directly with Read offset=line
  // — no follow-up Grep needed.

  Signature := ExtractSignature(AResult);
  if AResult.IsDeclaration then
    Signature := '[Decl] ' + Signature;

  // Location: full path + line number (or just path if line is missing/0).
  if AResult.FilePath <> '' then
  begin
    if AResult.StartLine > 0 then
      Location := Format('%s:%d', [AResult.FilePath, AResult.StartLine])
    else
      Location := AResult.FilePath;
  end
  else
    Location := '?';

  // Category and framework in one bracket: [user,VCL] or [stdlib] if no framework.
  if AResult.SourceCategory <> '' then
  begin
    CategoryInfo := AResult.SourceCategory;
    if AResult.Framework <> '' then
      CategoryInfo := CategoryInfo + ',' + AResult.Framework;
    CategoryInfo := ' [' + CategoryInfo + ']';
  end
  else
    CategoryInfo := '';

  WriteLn(Format('%d. %s  %s%s', [AIndex, Signature, Location, CategoryInfo]));
end;

procedure TResultFormatter.FormatUltraCompactResults(AResults: TSearchResultList; const AQuery: string);
var
  I: Integer;
begin
  // No "Found N result(s) for X:" preamble — that's chrome an agent will skip anyway.
  // Zero-result case is a single line, not a multi-line "Try…" prose.
  if AResults.Count = 0 then
  begin
    WriteLn(Format('No results for "%s".', [AQuery]));
    Exit;
  end;

  for I := 0 to AResults.Count - 1 do
    FormatUltraCompactSingleResult(AResults[I], I + 1);
end;

procedure TResultFormatter.FormatResults(AResults: TSearchResultList; const AQuery: string);
var
  I: Integer;
begin
  if AResults.Count = 0 then
  begin
    WriteLn('// No results found for this query.');
    WriteLn('//');
    WriteLn('// Try:');
    WriteLn('// - Different search terms');
    WriteLn('// - Shorter or more specific queries');
    WriteLn('// - Class names (e.g., "TRestServer")');
    WriteLn('// - Concept descriptions (e.g., "database connection")');
    Exit;
  end;

  WriteLn(Format('// Found %d result(s) for query: "%s"', [AResults.Count, AQuery]));
  WriteLn;

  for I := 0 to AResults.Count - 1 do
  begin
    FormatSingleResult(AResults[I], I + 1);

    // Add separator between results (except for the last one)
    if I < AResults.Count - 1 then
    begin
      WriteLn;
      WriteLn('--------------------');
      WriteLn;
    end;
  end;
end;

procedure TResultFormatter.FormatResultsAsJSON(AResults: TSearchResultList;
  const AQuery: string; ADurationMs: Integer; AIsCacheHit: Boolean);
var
  Root: TJSONObject;
  ResultsArr: TJSONArray;
  ResultObj: TJSONObject;
  I: Integer;
  R: TSearchResult;
  UnitName: string;
begin
  Root := TJSONObject.Create;
  try
    Root.AddPair('found', TJSONBool.Create(AResults.Count > 0));
    Root.AddPair('query', AQuery);
    Root.AddPair('result_count', TJSONNumber.Create(AResults.Count));
    Root.AddPair('duration_ms', TJSONNumber.Create(ADurationMs));
    Root.AddPair('cache_hit', TJSONBool.Create(AIsCacheHit));

    ResultsArr := TJSONArray.Create;
    for I := 0 to AResults.Count - 1 do
    begin
      R := AResults[I];
      ResultObj := TJSONObject.Create;

      ResultObj.AddPair('name', R.Name);
      ResultObj.AddPair('type', R.SymbolType);
      ResultObj.AddPair('signature', ExtractSignature(R));
      ResultObj.AddPair('file', R.FilePath);

      // Extract unit name from file path
      if R.FilePath <> '' then
        UnitName := ChangeFileExt(ExtractFileName(R.FilePath), '')
      else
        UnitName := '';
      ResultObj.AddPair('unit', UnitName);

      ResultObj.AddPair('line', TJSONNumber.Create(R.StartLine));
      ResultObj.AddPair('category', R.SourceCategory);
      ResultObj.AddPair('framework', R.Framework);
      ResultObj.AddPair('is_declaration', TJSONBool.Create(R.IsDeclaration));
      ResultObj.AddPair('score', TJSONNumber.Create(R.Score));
      ResultObj.AddPair('match_type', R.MatchType);

      ResultsArr.AddElement(ResultObj);
    end;
    Root.AddPair('results', ResultsArr);

    WriteLn(Root.ToJSON);
  finally
    Root.Free;
  end;
end;

end.