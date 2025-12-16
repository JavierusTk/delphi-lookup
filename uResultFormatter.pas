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
    function SanitizeForOutput(const AText: string): string;
    
  public
    procedure FormatResults(AResults: TSearchResultList; const AQuery: string);
    procedure FormatSingleResult(AResult: TSearchResult; AIndex: Integer);
  end;

implementation

uses
  System.RegularExpressions;

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
      
      // Match procedure/function/constructor/destructor declarations
      Match := TRegEx.Match(Line, '\b(procedure|function|constructor|destructor)\s+\w+.*?[;:]', [roIgnoreCase]);
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

  // Format the header
  WriteLn(Format('// Result %d (%s): %s', [AIndex, MatchTypeText, FormatSymbolSignature(AResult)]));

  // Add file information
  if AResult.FilePath <> '' then
    WriteLn(Format('// File: %s', [ExtractFileName(AResult.FilePath)]));

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

end.