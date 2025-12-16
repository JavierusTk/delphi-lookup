unit uDocTypes;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  /// Documentation type classification
  TDocType = (
    dtUnknown,
    dtUnit,           // Unit documentation
    dtClass,          // Class definition
    dtInterface,      // Interface definition
    dtRecord,         // Record definition
    dtProcedure,      // Standalone procedure
    dtFunction,       // Standalone function
    dtProperty,       // Property documentation
    dtMethod,         // Method documentation
    dtEvent,          // Event property
    dtConstant,       // Constant definition
    dtType,           // Type definition
    dtLanguageRef,    // Language reference (keywords, directives)
    dtConceptual,     // Conceptual guide/tutorial
    dtExample         // Code example
  );

  /// Framework classification
  TDocFramework = (
    dfNone,      // Not framework-specific
    dfVCL,       // Visual Component Library
    dfFMX,       // FireMonkey
    dfRTL,       // Run-Time Library
    dfBoth       // Exists in both VCL and FMX
  );

  /// Platform support
  TDocPlatform = (
    dpWindows,
    dpAndroid,
    dpiOS,
    dpmacOS,
    dpLinux
  );
  TDocPlatforms = set of TDocPlatform;

  /// Member visibility
  TDocVisibility = (
    dvPrivate,
    dvProtected,
    dvPublic,
    dvPublished,
    dvStrict,
    dvUnknown
  );

  /// Documentation metadata
  TDocMetadata = record
    FullyQualifiedName: string;   // e.g., "Vcl.StdCtrls.TButton.Caption"
    ShortName: string;             // e.g., "Caption"
    DocType: TDocType;
    Framework: TDocFramework;
    ParentClass: string;           // For inherited members
    UnitName: string;              // Source unit
    Visibility: TDocVisibility;
    Platforms: TDocPlatforms;
    IntroducedVersion: string;     // e.g., "XE2"
    DeprecatedVersion: string;
    SeeAlsoLinks: TArray<string>;
    IsInherited: Boolean;          // True if inherited from parent
    InheritedFrom: string;         // Parent class if inherited

    procedure Clear;
    function ToJSON: string;
    function PlatformsToString: string;
    function FrameworkToString: string;
    function DocTypeToString: string;
  end;

  /// Documentation chunk (unit of indexing)
  TDocChunk = record
    ID: string;                    // Unique identifier
    Name: string;                  // Symbol name
    Content: string;               // Main documentation text
    Declaration: string;           // Delphi syntax declaration
    Comments: string;              // Additional comments/notes
    CodeExamples: TArray<string>;  // Inline code examples
    SourceFile: string;            // Original HTML file path
    Metadata: TDocMetadata;

    procedure Clear;
    function ToJSON: string;
    function IsValid: Boolean;
  end;

  /// Documentation page (before chunking)
  TDocPage = record
    Title: string;
    RawHTML: string;
    SourceFile: string;
    ExtractedText: string;
    Chunks: TArray<TDocChunk>;

    procedure Clear;
  end;

  /// Extraction metadata
  TExtractionInfo = record
    SourceCHM: string;
    OutputDir: string;
    TopicCount: Integer;
    ExtractionDate: TDateTime;

    procedure Clear;
    function ToJSON: string;
  end;

const
  DOC_TYPE_NAMES: array[TDocType] of string = (
    'unknown',
    'unit',
    'class',
    'interface',
    'record',
    'procedure',
    'function',
    'property',
    'method',
    'event',
    'constant',
    'type',
    'language_ref',
    'conceptual',
    'example'
  );

  FRAMEWORK_NAMES: array[TDocFramework] of string = (
    'none',
    'VCL',
    'FMX',
    'RTL',
    'both'
  );

  PLATFORM_NAMES: array[TDocPlatform] of string = (
    'Windows',
    'Android',
    'iOS',
    'macOS',
    'Linux'
  );

function DocTypeFromString(const S: string): TDocType;
function FrameworkFromString(const S: string): TDocFramework;
function PlatformsFromString(const S: string): TDocPlatforms;

implementation

uses
  StrUtils;

{ TDocMetadata }

procedure TDocMetadata.Clear;
begin
  FullyQualifiedName := '';
  ShortName := '';
  DocType := dtUnknown;
  Framework := dfNone;
  ParentClass := '';
  UnitName := '';
  Visibility := dvUnknown;
  Platforms := [];
  IntroducedVersion := '';
  DeprecatedVersion := '';
  SetLength(SeeAlsoLinks, 0);
  IsInherited := False;
  InheritedFrom := '';
end;

function TDocMetadata.ToJSON: string;
var
  SL: TStringList;
  Link: string;
begin
  SL := TStringList.Create;
  try
    SL.Add('{');
    SL.Add(Format('  "name": "%s",', [FullyQualifiedName]));
    SL.Add(Format('  "shortName": "%s",', [ShortName]));
    SL.Add(Format('  "type": "%s",', [DocTypeToString]));
    SL.Add(Format('  "framework": "%s",', [FrameworkToString]));
    if ParentClass <> '' then
      SL.Add(Format('  "parentClass": "%s",', [ParentClass]));
    if UnitName <> '' then
      SL.Add(Format('  "unit": "%s",', [UnitName]));
    if Platforms <> [] then
      SL.Add(Format('  "platforms": "%s",', [PlatformsToString]));
    if IsInherited then
    begin
      SL.Add('  "inherited": true,');
      if InheritedFrom <> '' then
        SL.Add(Format('  "inheritedFrom": "%s",', [InheritedFrom]));
    end;
    if Length(SeeAlsoLinks) > 0 then
    begin
      SL.Add('  "seeAlso": [');
      for Link in SeeAlsoLinks do
        SL.Add(Format('    "%s",', [Link]));
      SL.Add('  ],');
    end;
    SL.Add('  "dummy": null');
    SL.Add('}');
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TDocMetadata.PlatformsToString: string;
var
  P: TDocPlatform;
  Parts: TStringList;
begin
  if Platforms = [] then
    Exit('');

  Parts := TStringList.Create;
  try
    Parts.Delimiter := ',';
    Parts.StrictDelimiter := True;

    for P := Low(TDocPlatform) to High(TDocPlatform) do
      if P in Platforms then
        Parts.Add(PLATFORM_NAMES[P]);

    Result := Parts.DelimitedText;
  finally
    Parts.Free;
  end;
end;

function TDocMetadata.FrameworkToString: string;
begin
  Result := FRAMEWORK_NAMES[Framework];
end;

function TDocMetadata.DocTypeToString: string;
begin
  Result := DOC_TYPE_NAMES[DocType];
end;

{ TDocChunk }

procedure TDocChunk.Clear;
begin
  ID := '';
  Name := '';
  Content := '';
  Declaration := '';
  Comments := '';
  SetLength(CodeExamples, 0);
  SourceFile := '';
  Metadata.Clear;
end;

function TDocChunk.ToJSON: string;
var
  SL: TStringList;
  Example: string;
begin
  SL := TStringList.Create;
  try
    SL.Add('{');
    SL.Add(Format('  "id": "%s",', [ID]));
    SL.Add(Format('  "name": "%s",', [Name]));
    SL.Add(Format('  "type": "%s",', [Metadata.DocTypeToString]));
    SL.Add(Format('  "framework": "%s",', [Metadata.FrameworkToString]));
    if Declaration <> '' then
      SL.Add(Format('  "declaration": "%s",', [StringReplace(Declaration, '"', '\"', [rfReplaceAll])]));
    SL.Add(Format('  "content": "%s",', [StringReplace(Content, '"', '\"', [rfReplaceAll])]));
    if Comments <> '' then
      SL.Add(Format('  "comments": "%s",', [StringReplace(Comments, '"', '\"', [rfReplaceAll])]));
    if Length(CodeExamples) > 0 then
    begin
      SL.Add('  "codeExamples": [');
      for Example in CodeExamples do
        SL.Add(Format('    "%s",', [StringReplace(Example, '"', '\"', [rfReplaceAll])]));
      SL.Add('  ],');
    end;
    SL.Add(Format('  "sourceFile": "%s",', [SourceFile]));
    SL.Add('  "metadata": ' + Metadata.ToJSON);
    SL.Add('}');
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TDocChunk.IsValid: Boolean;
begin
  Result := (Name <> '') and (Content <> '') and (Metadata.DocType <> dtUnknown);
end;

{ TDocPage }

procedure TDocPage.Clear;
begin
  Title := '';
  RawHTML := '';
  SourceFile := '';
  ExtractedText := '';
  SetLength(Chunks, 0);
end;

{ TExtractionInfo }

procedure TExtractionInfo.Clear;
begin
  SourceCHM := '';
  OutputDir := '';
  TopicCount := 0;
  ExtractionDate := 0;
end;

function TExtractionInfo.ToJSON: string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('{');
    SL.Add(Format('  "sourceCHM": "%s",', [SourceCHM]));
    SL.Add(Format('  "outputDir": "%s",', [OutputDir]));
    SL.Add(Format('  "topicCount": %d,', [TopicCount]));
    SL.Add(Format('  "extractionDate": "%s"', [FormatDateTime('yyyy-mm-dd hh:nn:ss', ExtractionDate)]));
    SL.Add('}');
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

{ Helper functions }

function DocTypeFromString(const S: string): TDocType;
var
  DT: TDocType;
begin
  Result := dtUnknown;
  for DT := Low(TDocType) to High(TDocType) do
    if SameText(S, DOC_TYPE_NAMES[DT]) then
      Exit(DT);
end;

function FrameworkFromString(const S: string): TDocFramework;
var
  DF: TDocFramework;
begin
  Result := dfNone;
  for DF := Low(TDocFramework) to High(TDocFramework) do
    if SameText(S, FRAMEWORK_NAMES[DF]) then
      Exit(DF);
end;

function PlatformsFromString(const S: string): TDocPlatforms;
var
  Parts: TStringList;
  I: Integer;
  P: TDocPlatform;
begin
  Result := [];
  if S = '' then
    Exit;

  Parts := TStringList.Create;
  try
    Parts.Delimiter := ',';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := S;

    for I := 0 to Parts.Count - 1 do
      for P := Low(TDocPlatform) to High(TDocPlatform) do
        if SameText(Trim(Parts[I]), PLATFORM_NAMES[P]) then
          Include(Result, P);
  finally
    Parts.Free;
  end;
end;

end.
