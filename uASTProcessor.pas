unit uASTProcessor;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.RegularExpressions,
  DelphiAST,
  DelphiAST.Classes,
  DelphiAST.Consts,
  DelphiAST.SimpleParserEx,
  DelphiAST.Writer,
  SimpleParser.Lexer.Types,
  uFolderScanner;

type
  TCodeChunkType = (
    ctClass,
    ctInterface,
    ctRecord,
    ctProcedure,
    ctFunction,
    ctConstructor,
    ctDestructor,
    ctProperty,
    ctType,
    ctConst,
    ctVar,
    ctMethodImplementation
  );

  TCodeChunk = class
  private
    FChunkGUID: string;           // Unique GUID for this chunk (for embedding matching)
    FChunkType: TCodeChunkType;
    FName: string;
    FFullName: string;
    FContent: string;
    FEnrichedText: string;        // Code + metadata for embedding
    FSpanishTerms: string;        // JSON array of detected Spanish terms
    FDomainTags: string;          // CSV domain tags for filtering
    FFileName: string;
    FStartLine: Integer;
    FEndLine: Integer;
    FComments: string;
    FParentClass: string;
    FImplementedInterfaces: TStringList;
    FVisibility: string;
    FMethodSignature: string; // To store full method signature for matching
    FIsDeclaration: Boolean;   // True if this chunk represents a declaration, False for implementation
  public
    constructor Create;
    destructor Destroy; override;

    property ChunkGUID: string read FChunkGUID write FChunkGUID;
    property ChunkType: TCodeChunkType read FChunkType write FChunkType;
    property Name: string read FName write FName;
    property FullName: string read FFullName write FFullName;
    property Content: string read FContent write FContent;
    property EnrichedText: string read FEnrichedText write FEnrichedText;
    property SpanishTerms: string read FSpanishTerms write FSpanishTerms;
    property DomainTags: string read FDomainTags write FDomainTags;
    property FileName: string read FFileName write FFileName;
    property StartLine: Integer read FStartLine write FStartLine;
    property EndLine: Integer read FEndLine write FEndLine;
    property Comments: string read FComments write FComments;
    property ParentClass: string read FParentClass write FParentClass;
    property ImplementedInterfaces: TStringList read FImplementedInterfaces;
    property Visibility: string read FVisibility write FVisibility;
    property MethodSignature: string read FMethodSignature write FMethodSignature;
    property IsDeclaration: Boolean read FIsDeclaration write FIsDeclaration;
  end;

  TCodeChunkList = class(TObjectList<TCodeChunk>)
  public
    function FindByName(const AName: string): TCodeChunk;
    function GetByType(AType: TCodeChunkType): TList<TCodeChunk>;
  end;

  TIncludeHandler = class(TInterfacedObject, IIncludeHandler)
  public
    function GetIncludeFileContent(const ParentFileName, IncludeName: string;
      out Content: string; out FileName: string): Boolean;
  end;

  TCurrentSection = (csInterface, csImplementation);

  TASTProcessor = class
  private
    FCurrentSection: TCurrentSection;
    FMethodDeclarations: TDictionary<string, TCodeChunk>; // To store declarations for linking
    procedure ProcessNode(Node: TSyntaxNode; const FileName, SourceCode: string;
      Comments: TObjectList<TCommentNode>; Chunks: TCodeChunkList);
    procedure ProcessClassNode(Node: TSyntaxNode; const FileName, SourceCode: string;
      Comments: TObjectList<TCommentNode>; Chunks: TCodeChunkList);
    procedure ProcessInterfaceNode(Node: TSyntaxNode; const FileName, SourceCode: string;
      Comments: TObjectList<TCommentNode>; Chunks: TCodeChunkList);
    procedure ProcessMethodNode(Node: TSyntaxNode; const FileName, SourceCode: string;
      Comments: TObjectList<TCommentNode>; Chunks: TCodeChunkList; ChunkType: TCodeChunkType;
      AIsImplementation: Boolean = False);
    procedure ProcessTypeNode(Node: TSyntaxNode; const FileName, SourceCode: string;
      Comments: TObjectList<TCommentNode>; Chunks: TCodeChunkList);
    procedure ProcessConstNode(Node: TSyntaxNode; const FileName, SourceCode: string;
      Comments: TObjectList<TCommentNode>; Chunks: TCodeChunkList);
    procedure ProcessVarNode(Node: TSyntaxNode; const FileName, SourceCode: string;
      Comments: TObjectList<TCommentNode>; Chunks: TCodeChunkList);

    function ExtractNodeContent(Node: TSyntaxNode; const SourceCode: string): string;
    function GetNodeName(Node: TSyntaxNode): string;
    function GetParentClassName(Node: TSyntaxNode): string;
    function GetImplementedInterfaces(Node: TSyntaxNode): TStringList;
    function GetVisibilityFromNode(Node: TSyntaxNode): string;
    function AssociateComments(Node: TSyntaxNode; Comments: TObjectList<TCommentNode>): string;

    // Preprocess conditionals for files with platform-specific {$IFDEF}
    function PreprocessConditionals(const AContent: string): string;
    function TryParseContent(const AContent, AFilePath: string; out AChunks: TCodeChunkList): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    // Process a single file, returns chunks for that file
    function ProcessSingleFile(AParsedFile: TParsedFile): TCodeChunkList;

    // Process multiple files (legacy method, uses ProcessSingleFile internally)
    function ProcessFiles(AFiles: TStringList): TCodeChunkList;
  end;

implementation

uses
  System.StrUtils,
  System.Math,
  System.TypInfo,
  System.Diagnostics;

{ TCodeChunk }

constructor TCodeChunk.Create;
begin
  inherited Create;
  FChunkGUID := GUIDToString(TGUID.NewGuid);  // Generate unique GUID for this chunk
  FImplementedInterfaces := TStringList.Create;
  FMethodSignature := '';
  FIsDeclaration := False;
end;

destructor TCodeChunk.Destroy;
begin
  FImplementedInterfaces.Free;
  inherited Destroy;
end;

{ TCodeChunkList }

function TCodeChunkList.FindByName(const AName: string): TCodeChunk;
var
  Chunk: TCodeChunk;
begin
  Result := nil;
  for Chunk in Self do
  begin
    if SameText(Chunk.Name, AName) or SameText(Chunk.FullName, AName) then
    begin
      Result := Chunk;
      Break;
    end;
  end;
end;

function TCodeChunkList.GetByType(AType: TCodeChunkType): TList<TCodeChunk>;
var
  Chunk: TCodeChunk;
begin
  Result := TList<TCodeChunk>.Create;
  for Chunk in Self do
  begin
    if Chunk.ChunkType = AType then
      Result.Add(Chunk);
  end;
end;

{ TIncludeHandler }

function TIncludeHandler.GetIncludeFileContent(const ParentFileName, IncludeName: string;
  out Content: string; out FileName: string): Boolean;
begin
  // For interface-only parsing, we typically don't need include files
  Result := False;
  Content := '';
  FileName := '';
end;

{ TASTProcessor }

constructor TASTProcessor.Create;
begin
  inherited Create;
  FMethodDeclarations := TDictionary<string, TCodeChunk>.Create;
end;

destructor TASTProcessor.Destroy;
begin
  FMethodDeclarations.Free;
  inherited Destroy;
end;

function TASTProcessor.GetNodeName(Node: TSyntaxNode): string;
var
  NameNode: TSyntaxNode;
begin
  Result := '';
  
  // Look for name attribute first
  if Node.HasAttribute(anName) then
  begin
    Result := Node.GetAttribute(anName);
    Exit;
  end;
  
  // Look for name child node
  NameNode := Node.FindNode(ntName);
  if Assigned(NameNode) and NameNode.HasAttribute(anName) then
  begin
    Result := NameNode.GetAttribute(anName);
    Exit;
  end;
  
  // Look for identifier node
  NameNode := Node.FindNode(ntIdentifier);
  if Assigned(NameNode) and NameNode.HasAttribute(anName) then
    Result := NameNode.GetAttribute(anName);
end;

function TASTProcessor.GetParentClassName(Node: TSyntaxNode): string;
var
  TypeNode, IdentifierNode: TSyntaxNode;
begin
  Result := '';
  
  // Look for heritage (inheritance) information
  TypeNode := Node.FindNode([ntType]);
  if Assigned(TypeNode) then
  begin
    IdentifierNode := TypeNode.FindNode(ntIdentifier);
    if Assigned(IdentifierNode) and IdentifierNode.HasAttribute(anName) then
      Result := IdentifierNode.GetAttribute(anName);
  end;
end;

function TASTProcessor.GetImplementedInterfaces(Node: TSyntaxNode): TStringList;
var
  I: Integer;
  InterfaceNode: TSyntaxNode;
begin
  Result := TStringList.Create;
  
  // Look for interface implementations
  for I := 0 to Length(Node.ChildNodes) - 1 do
  begin
    InterfaceNode := Node.ChildNodes[I];
    if (InterfaceNode.Typ = ntIdentifier) and 
       InterfaceNode.HasAttribute(anName) then
    begin
      var InterfaceName := InterfaceNode.GetAttribute(anName);
      if InterfaceName.StartsWith('I') then // Interface naming convention
        Result.Add(InterfaceName);
    end;
  end;
end;

function TASTProcessor.GetVisibilityFromNode(Node: TSyntaxNode): string;
var
  ParentNode: TSyntaxNode;
begin
  Result := 'public'; // Default visibility
  
  ParentNode := Node.ParentNode;
  while Assigned(ParentNode) do
  begin
    case ParentNode.Typ of
      ntPrivate: Result := 'private';
      ntProtected: Result := 'protected';
      ntPublic: Result := 'public';
      ntPublished: Result := 'published';
    end;
    ParentNode := ParentNode.ParentNode;
  end;
end;

function TASTProcessor.ExtractNodeContent(Node: TSyntaxNode; const SourceCode: string): string;
var
  Lines: TStringList;
  StartLine, EndLine: Integer;
  I: Integer;
begin
  Result := '';
  
  if not Assigned(Node) then
    Exit;
    
  StartLine := Node.Line;
  EndLine := StartLine;
  
  // For compound nodes, get the end line
  if Node is TCompoundSyntaxNode then
    EndLine := TCompoundSyntaxNode(Node).EndLine
  else
  begin
    // Estimate end line by looking at child nodes
    for I := 0 to Length(Node.ChildNodes) - 1 do
    begin
      if Node.ChildNodes[I].Line > EndLine then
        EndLine := Node.ChildNodes[I].Line;
    end;
  end;
  
  Lines := TStringList.Create;
  try
    Lines.Text := SourceCode;
    
    if (StartLine > 0) and (StartLine <= Lines.Count) then
    begin
      for I := StartLine - 1 to Min(EndLine - 1, Lines.Count - 1) do
      begin
        if I >= 0 then
          Result := Result + Lines[I] + sLineBreak;
      end;
    end;
    
  finally
    Lines.Free;
  end;
  
  Result := Trim(Result);
end;

function TASTProcessor.AssociateComments(Node: TSyntaxNode; Comments: TObjectList<TCommentNode>): string;
var
  I: Integer;
  Comment: TCommentNode;
  AssociatedComments: TStringList;
  NodeLine: Integer;
begin
  AssociatedComments := TStringList.Create;
  try
    NodeLine := Node.Line;
    
    // Look for comments immediately before the node (within 5 lines)
    for I := 0 to Comments.Count - 1 do
    begin
      Comment := Comments[I];
      
      // Comment should be before the node and close to it
      if (Comment.Line < NodeLine) and (Comment.Line >= NodeLine - 5) then
      begin
        // Clean up comment text
        var CommentText := Trim(Comment.Text);
        
        // Remove comment markers
        // Note: DelphiAST already strips the leading '//' from slash comments,
        // so '/// doc' arrives as '/ doc' and '// comment' arrives as ' comment'
        if CommentText.StartsWith('/') then
          CommentText := Trim(CommentText.Substring(1))  // Handle /// (third slash)
        else if CommentText.StartsWith('(*') and CommentText.EndsWith('*)') then
          CommentText := Trim(CommentText.Substring(2, CommentText.Length - 4))
        else if CommentText.StartsWith('{') and CommentText.EndsWith('}') then
          CommentText := Trim(CommentText.Substring(1, CommentText.Length - 2))
        else
          CommentText := Trim(CommentText);  // Handle // (already stripped)
        
        if CommentText <> '' then
          AssociatedComments.Add(CommentText);
      end;
    end;
    
    Result := AssociatedComments.Text.Trim;
  finally
    AssociatedComments.Free;
  end;
end;

procedure TASTProcessor.ProcessClassNode(Node: TSyntaxNode; const FileName, SourceCode: string; 
  Comments: TObjectList<TCommentNode>; Chunks: TCodeChunkList);
var
  Chunk: TCodeChunk;
begin
  Chunk := TCodeChunk.Create;
  Chunk.ChunkType := ctClass;
  Chunk.FileName := FileName;
  Chunk.Name := GetNodeName(Node);
  Chunk.FullName := Chunk.Name;
  Chunk.StartLine := Node.Line;
  if Node is TCompoundSyntaxNode then
    Chunk.EndLine := TCompoundSyntaxNode(Node).EndLine
  else
    Chunk.EndLine := Node.Line;
  Chunk.Content := ExtractNodeContent(Node, SourceCode);
  Chunk.Comments := AssociateComments(Node, Comments);
  Chunk.ParentClass := GetParentClassName(Node);
  var TempInterfaces := GetImplementedInterfaces(Node);
  try
    Chunk.ImplementedInterfaces.AddStrings(TempInterfaces);
  finally
    TempInterfaces.Free;
  end;
  Chunk.Visibility := GetVisibilityFromNode(Node);
  
  Chunks.Add(Chunk);
end;

procedure TASTProcessor.ProcessInterfaceNode(Node: TSyntaxNode; const FileName, SourceCode: string; 
  Comments: TObjectList<TCommentNode>; Chunks: TCodeChunkList);
var
  Chunk: TCodeChunk;
begin
  Chunk := TCodeChunk.Create;
  Chunk.ChunkType := ctInterface;
  Chunk.FileName := FileName;
  Chunk.Name := GetNodeName(Node);
  Chunk.FullName := Chunk.Name;
  Chunk.StartLine := Node.Line;
  if Node is TCompoundSyntaxNode then
    Chunk.EndLine := TCompoundSyntaxNode(Node).EndLine
  else
    Chunk.EndLine := Node.Line;
  Chunk.Content := ExtractNodeContent(Node, SourceCode);
  Chunk.Comments := AssociateComments(Node, Comments);
  Chunk.Visibility := GetVisibilityFromNode(Node);
  
  Chunks.Add(Chunk);
end;

procedure TASTProcessor.ProcessMethodNode(Node: TSyntaxNode; const FileName, SourceCode: string;
  Comments: TObjectList<TCommentNode>; Chunks: TCodeChunkList; ChunkType: TCodeChunkType;
  AIsImplementation: Boolean);
var
  Chunk: TCodeChunk;
  NormalizedFullName: string;
begin
  Chunk := TCodeChunk.Create;
  Chunk.ChunkType := ChunkType;  // Keep original type (function, procedure, etc.)
  Chunk.FileName := FileName;
  Chunk.Name := GetNodeName(Node);

  // Extract the full name of the method. This might need to be class-qualified.
  // For methods defined within a class, GetNodeName might only return the method name.
  // We need to build the FullName: ClassName.MethodName.
  var ParentClassNode := Node.ParentNode;
  while Assigned(ParentClassNode) and (ParentClassNode.Typ <> ntTypeDecl) do
    ParentClassNode := ParentClassNode.ParentNode;

  if Assigned(ParentClassNode) and ParentClassNode.HasAttribute(anName) then
    Chunk.FullName := ParentClassNode.GetAttribute(anName) + '.' + Chunk.Name
  else
    Chunk.FullName := Chunk.Name; // Not a method of a class, or parent not found

  Chunk.StartLine := Node.Line;
  if Node is TCompoundSyntaxNode then
    Chunk.EndLine := TCompoundSyntaxNode(Node).EndLine
  else
    Chunk.EndLine := Node.Line;
  Chunk.Content := ExtractNodeContent(Node, SourceCode);
  Chunk.Comments := AssociateComments(Node, Comments);
  Chunk.Visibility := GetVisibilityFromNode(Node);

  // Use AIsImplementation flag instead of checking chunk type
  Chunk.IsDeclaration := not AIsImplementation;

  if Chunk.IsDeclaration then
  begin
    Chunk.MethodSignature := ExtractNodeContent(Node, SourceCode);
    NormalizedFullName := LowerCase(Chunk.FullName);
    // Store declaration, replacing if already exists (e.g., from a different file)
    // This assumes the last declaration found is the most relevant, or that
    // multiple declarations (e.g., in different units for partial classes) don't conflict
    FMethodDeclarations.AddOrSetValue(NormalizedFullName, Chunk);
    Chunks.Add(Chunk); // Add declaration to the main list
  end
  else // This is an implementation
  begin
    NormalizedFullName := LowerCase(Chunk.FullName);
    // Try to find a matching declaration
    if FMethodDeclarations.ContainsKey(NormalizedFullName) then
    begin
      // If a declaration exists, we can link them or enrich the existing declaration
      // For now, let's just add the implementation as a separate chunk
      // The content of this chunk is the implementation body
      Chunks.Add(Chunk);
    end
    else
    begin
      // No matching declaration found, add it as a standalone implementation
      Chunks.Add(Chunk);
    end;
  end;
end;

procedure TASTProcessor.ProcessTypeNode(Node: TSyntaxNode; const FileName, SourceCode: string;
  Comments: TObjectList<TCommentNode>; Chunks: TCodeChunkList);
var
  Chunk: TCodeChunk;
begin
  Chunk := TCodeChunk.Create;
  Chunk.ChunkType := ctType;
  Chunk.FileName := FileName;
  Chunk.Name := GetNodeName(Node);
  Chunk.FullName := Chunk.Name;
  Chunk.StartLine := Node.Line;
  if Node is TCompoundSyntaxNode then
    Chunk.EndLine := TCompoundSyntaxNode(Node).EndLine
  else
    Chunk.EndLine := Node.Line;
  Chunk.Content := ExtractNodeContent(Node, SourceCode);
  Chunk.Comments := AssociateComments(Node, Comments);
  Chunk.Visibility := GetVisibilityFromNode(Node);

  Chunks.Add(Chunk);
end;

procedure TASTProcessor.ProcessConstNode(Node: TSyntaxNode; const FileName, SourceCode: string;
  Comments: TObjectList<TCommentNode>; Chunks: TCodeChunkList);
var
  Chunk: TCodeChunk;
  NameNode: TSyntaxNode;
  NameText: string;
  ColonPos, EqualPos: Integer;
begin
  Chunk := TCodeChunk.Create;
  Chunk.ChunkType := ctConst;
  Chunk.FileName := FileName;

  // Extract name from ntName child node
  NameNode := Node.FindNode(ntName);
  if Assigned(NameNode) then
  begin
    NameText := Trim(ExtractNodeContent(NameNode, SourceCode));
    // Handle both:
    //   Untyped: "MAX_CONNECTIONS = 100;" → "MAX_CONNECTIONS"
    //   Typed:   "DEFAULT_PORT: Integer = 8080;" → "DEFAULT_PORT"
    ColonPos := Pos(':', NameText);
    EqualPos := Pos('=', NameText);

    if ColonPos > 0 then
      // Typed constant: extract name before ':'
      Chunk.Name := Trim(Copy(NameText, 1, ColonPos - 1))
    else if EqualPos > 0 then
      // Untyped constant: extract name before '='
      Chunk.Name := Trim(Copy(NameText, 1, EqualPos - 1))
    else
      Chunk.Name := NameText; // Fallback to full text
  end
  else
    Chunk.Name := GetNodeName(Node); // Fallback to GetNodeName

  Chunk.FullName := Chunk.Name;
  Chunk.StartLine := Node.Line;
  if Node is TCompoundSyntaxNode then
    Chunk.EndLine := TCompoundSyntaxNode(Node).EndLine
  else
    Chunk.EndLine := Node.Line;
  Chunk.Content := ExtractNodeContent(Node, SourceCode);
  Chunk.Comments := AssociateComments(Node, Comments);
  Chunk.Visibility := GetVisibilityFromNode(Node);

  Chunks.Add(Chunk);
end;

procedure TASTProcessor.ProcessVarNode(Node: TSyntaxNode; const FileName, SourceCode: string;
  Comments: TObjectList<TCommentNode>; Chunks: TCodeChunkList);
var
  Chunk: TCodeChunk;
  NameNode: TSyntaxNode;
  NameText: string;
  ColonPos: Integer;
begin
  Chunk := TCodeChunk.Create;
  Chunk.ChunkType := ctVar;
  Chunk.FileName := FileName;

  // Extract name from ntName child node
  NameNode := Node.FindNode(ntName);
  if Assigned(NameNode) then
  begin
    NameText := Trim(ExtractNodeContent(NameNode, SourceCode));
    // Extract name before ':' sign: "GlobalLogger: TObject;" → "GlobalLogger"
    ColonPos := Pos(':', NameText);
    if ColonPos > 0 then
      Chunk.Name := Trim(Copy(NameText, 1, ColonPos - 1))
    else
      Chunk.Name := NameText; // Fallback to full text
  end
  else
    Chunk.Name := GetNodeName(Node); // Fallback to GetNodeName

  Chunk.FullName := Chunk.Name;
  Chunk.StartLine := Node.Line;
  if Node is TCompoundSyntaxNode then
    Chunk.EndLine := TCompoundSyntaxNode(Node).EndLine
  else
    Chunk.EndLine := Node.Line;
  Chunk.Content := ExtractNodeContent(Node, SourceCode);
  Chunk.Comments := AssociateComments(Node, Comments);
  Chunk.Visibility := GetVisibilityFromNode(Node);

  Chunks.Add(Chunk);
end;

procedure TASTProcessor.ProcessNode(Node: TSyntaxNode; const FileName, SourceCode: string;
  Comments: TObjectList<TCommentNode>; Chunks: TCodeChunkList);
var
  I: Integer;
begin
  if not Assigned(Node) then
    Exit;

  // Process specific node types based on the current section
  case Node.Typ of
    ntTypeDecl:
      begin
        if FCurrentSection = csInterface then // Only process type declarations in interface
        begin
          if Node.HasAttribute(anClass) then
          begin
            var ClassType := Node.GetAttribute(anClass);
            if SameText(ClassType, 'class') then
              ProcessClassNode(Node, FileName, SourceCode, Comments, Chunks)
            else if SameText(ClassType, 'interface') then
              ProcessInterfaceNode(Node, FileName, SourceCode, Comments, Chunks)
            else
              ProcessTypeNode(Node, FileName, SourceCode, Comments, Chunks);
          end
          else
            ProcessTypeNode(Node, FileName, SourceCode, Comments, Chunks);
        end;
      end;
    ntMethod:
      begin
        var CurrentChunkType: TCodeChunkType;
        // Determine method type from attributes
        if Node.HasAttribute(anKind) then
        begin
          var MethodKind := Node.GetAttribute(anKind);
          if SameText(MethodKind, 'function') then
            CurrentChunkType := ctFunction
          else if SameText(MethodKind, 'procedure') then
            CurrentChunkType := ctProcedure
          else if SameText(MethodKind, 'constructor') then
            CurrentChunkType := ctConstructor
          else if SameText(MethodKind, 'destructor') then
            CurrentChunkType := ctDestructor
          else
            CurrentChunkType := ctFunction;
        end
        else
          CurrentChunkType := ctFunction; // Default for methods without explicit kind

        // Pass the section info to distinguish declarations from implementations
        ProcessMethodNode(Node, FileName, SourceCode, Comments, Chunks, CurrentChunkType,
          FCurrentSection = csImplementation);
      end;
    ntProperty:
      if FCurrentSection = csInterface then // Only process properties in interface
        ProcessMethodNode(Node, FileName, SourceCode, Comments, Chunks, ctProperty);
    ntConstant:
      if FCurrentSection = csInterface then // Only process constants in interface
        ProcessConstNode(Node, FileName, SourceCode, Comments, Chunks);
    ntVariable:
      if FCurrentSection = csInterface then // Only process variables in interface
        ProcessVarNode(Node, FileName, SourceCode, Comments, Chunks);
  end;

  // Recursively process child nodes
  for I := 0 to Length(Node.ChildNodes) - 1 do
    ProcessNode(Node.ChildNodes[I], FileName, SourceCode, Comments, Chunks);
end;

function TASTProcessor.PreprocessConditionals(const AContent: string): string;
(*
  Robust preprocessor that handles:
  - Directives at any position in a line (not just at start)
  - Multiple directives on a single line (e.g. IFDEF X code ENDIF)
  - Inline directives with code
  - Nested conditionals

  Strategy: Process character by character, tracking directive boundaries
  and active/inactive state. Copy active content, skip inactive content.
*)
const
  // Defines that are TRUE for Windows/Delphi environment
  TRUE_DEFINES: array[0..19] of string = (
    'MSWINDOWS', 'OSWINDOWS', 'WINDOWS', 'WIN32', 'WIN64',
    'DELPHI', 'CONDITIONALEXPRESSIONS', 'UNICODE',
    'CPUX64', 'CPUX86', 'CPU64', 'CPU32', 'CPU386',
    'GENERICS', 'HASINTEGER64', 'HASINLINE',
    'PUREPASCAL', 'USERECORDWITHMETHODS', 'HASCODEPAGE', 'ISDELPHI'
  );
  // Defines that are FALSE (non-Windows/non-Delphi platforms)
  FALSE_DEFINES: array[0..19] of string = (
    'OSPOSIX', 'OSDARWIN', 'OSLINUX', 'OSBSD', 'OSANDROID', 'OSIOS',
    'FPC', 'FPCSOURCES', 'FPC_DELPHI', 'FPC_HAS_CPSTRING',
    'LINUX', 'DARWIN', 'BSD', 'ANDROID', 'IOS',
    'CPUARM', 'CPUAARCH64', 'CPUPOWERPC', 'CPUMIPS',
    'OSFREEBSD'
  );

  MAX_NESTING = 100;

type
  TCondState = record
    ParentActive: Boolean;  // Was parent scope active when we entered this level?
    WasKnownDefine: Boolean; // Was this a known define (so ELSE should toggle)?
    BranchTaken: Boolean;    // Has any branch been taken yet?
  end;

var
  ResultBuilder: TStringBuilder;
  I, Len: Integer;
  CondStack: array[0..MAX_NESTING-1] of TCondState;
  NestingLevel: Integer;
  CurrentActive: Boolean;

  function IsDefineTrue(const ADefine: string): Integer;
  // Returns: 1 = TRUE, -1 = FALSE, 0 = UNKNOWN
  var
    J: Integer;
    UpperDefine: string;
  begin
    UpperDefine := UpperCase(Trim(ADefine));
    for J := Low(TRUE_DEFINES) to High(TRUE_DEFINES) do
      if UpperDefine = TRUE_DEFINES[J] then
        Exit(1);
    for J := Low(FALSE_DEFINES) to High(FALSE_DEFINES) do
      if UpperDefine = FALSE_DEFINES[J] then
        Exit(-1);
    Result := 0; // Unknown
  end;

  function ExtractDirective(const AContent: string; AStart: Integer;
    out ADirectiveType: string; out ADefineName: string; out AEndPos: Integer): Boolean;
  // Extract directive starting at AStart (which points to '{')
  // Returns directive type (IFDEF, IFNDEF, ELSE, ENDIF, IF, IFEND, OTHER)
  // and define name if applicable
  var
    J, BraceEnd: Integer;
    DirectiveText, UpperText: string;
    SpacePos: Integer;
  begin
    Result := False;
    ADirectiveType := '';
    ADefineName := '';
    AEndPos := AStart;

    // Must start with {$
    if (AStart > Length(AContent)) or (AContent[AStart] <> '{') then
      Exit;
    if (AStart + 1 > Length(AContent)) or (AContent[AStart + 1] <> '$') then
      Exit;

    // Find closing brace
    BraceEnd := 0;
    for J := AStart + 2 to Length(AContent) do
    begin
      if AContent[J] = '}' then
      begin
        BraceEnd := J;
        Break;
      end;
    end;

    if BraceEnd = 0 then
      Exit; // No closing brace

    DirectiveText := Copy(AContent, AStart + 2, BraceEnd - AStart - 2);
    UpperText := UpperCase(Trim(DirectiveText));
    AEndPos := BraceEnd;
    Result := True;

    // Parse directive type
    if UpperText.StartsWith('IFDEF ') or (UpperText = 'IFDEF') then
    begin
      ADirectiveType := 'IFDEF';
      SpacePos := Pos(' ', DirectiveText);
      if SpacePos > 0 then
        ADefineName := Trim(Copy(DirectiveText, SpacePos + 1, MaxInt));
    end
    else if UpperText.StartsWith('IFNDEF ') or (UpperText = 'IFNDEF') then
    begin
      ADirectiveType := 'IFNDEF';
      SpacePos := Pos(' ', DirectiveText);
      if SpacePos > 0 then
        ADefineName := Trim(Copy(DirectiveText, SpacePos + 1, MaxInt));
    end
    else if UpperText.StartsWith('IFOPT ') then
    begin
      // Compiler option check - treat as unknown, keep content
      ADirectiveType := 'IFOPT';
    end
    else if UpperText.StartsWith('IF ') then
    begin
      // Complex {$IF expression} - treat as unknown for now
      ADirectiveType := 'IF';
    end
    else if UpperText.StartsWith('ELSE') then
    begin
      ADirectiveType := 'ELSE';
    end
    else if UpperText.StartsWith('ENDIF') or UpperText.StartsWith('IFEND') then
    begin
      ADirectiveType := 'ENDIF';
    end
    else if UpperText.StartsWith('ELSEIF ') then
    begin
      ADirectiveType := 'ELSEIF';
    end
    else
    begin
      ADirectiveType := 'OTHER'; // Not a conditional directive
    end;
  end;

  procedure PushCondition(AParentActive, AKnownDefine, ABranchTaken: Boolean);
  begin
    if NestingLevel < MAX_NESTING then
    begin
      CondStack[NestingLevel].ParentActive := AParentActive;
      CondStack[NestingLevel].WasKnownDefine := AKnownDefine;
      CondStack[NestingLevel].BranchTaken := ABranchTaken;
      Inc(NestingLevel);
    end;
  end;

  procedure PopCondition;
  begin
    if NestingLevel > 0 then
      Dec(NestingLevel);
  end;

var
  DirectiveType, DefineName: string;
  EndPos: Integer;
  DefineResult: Integer;
begin
  ResultBuilder := TStringBuilder.Create;
  try
    Len := Length(AContent);
    I := 1;
    NestingLevel := 0;
    CurrentActive := True;

    while I <= Len do
    begin
      // Check for directive start
      if (AContent[I] = '{') and (I + 1 <= Len) and (AContent[I + 1] = '$') then
      begin
        if ExtractDirective(AContent, I, DirectiveType, DefineName, EndPos) then
        begin
          if DirectiveType = 'IFDEF' then
          begin
            // Push current state and evaluate
            DefineResult := IsDefineTrue(DefineName);

            if not CurrentActive then
            begin
              // Parent inactive - this branch is inactive regardless
              PushCondition(False, DefineResult <> 0, False);
              CurrentActive := False;
            end
            else if DefineResult = 1 then
            begin
              // Define is TRUE - enter active branch
              PushCondition(True, True, True);
              CurrentActive := True;
            end
            else if DefineResult = -1 then
            begin
              // Define is FALSE - enter inactive branch
              PushCondition(True, True, False);
              CurrentActive := False;
            end
            else
            begin
              // Unknown define - keep content (assume TRUE)
              PushCondition(True, False, True);
              CurrentActive := True;
            end;

            I := EndPos + 1;
            Continue;
          end
          else if DirectiveType = 'IFNDEF' then
          begin
            DefineResult := IsDefineTrue(DefineName);

            if not CurrentActive then
            begin
              PushCondition(False, DefineResult <> 0, False);
              CurrentActive := False;
            end
            else if DefineResult = -1 then
            begin
              // Define is FALSE, so IFNDEF is TRUE
              PushCondition(True, True, True);
              CurrentActive := True;
            end
            else if DefineResult = 1 then
            begin
              // Define is TRUE, so IFNDEF is FALSE
              PushCondition(True, True, False);
              CurrentActive := False;
            end
            else
            begin
              // Unknown - assume IFNDEF is TRUE
              PushCondition(True, False, True);
              CurrentActive := True;
            end;

            I := EndPos + 1;
            Continue;
          end
          else if (DirectiveType = 'IF') or (DirectiveType = 'IFOPT') then
          begin
            // Complex IF - treat as unknown, keep content
            PushCondition(CurrentActive, False, CurrentActive);
            // CurrentActive stays the same
            I := EndPos + 1;
            Continue;
          end
          else if DirectiveType = 'ELSE' then
          begin
            if NestingLevel > 0 then
            begin
              // Toggle only if parent was active AND it was a known define
              if CondStack[NestingLevel - 1].ParentActive then
              begin
                if CondStack[NestingLevel - 1].WasKnownDefine then
                begin
                  // Known define: toggle state
                  CurrentActive := not CondStack[NestingLevel - 1].BranchTaken;
                end
                else
                begin
                  // Unknown define: we kept first branch, skip ELSE
                  CurrentActive := False;
                end;
              end
              else
              begin
                CurrentActive := False;
              end;
            end;

            I := EndPos + 1;
            Continue;
          end
          else if DirectiveType = 'ELSEIF' then
          begin
            // Treat ELSEIF as ELSE for now (simplified)
            if NestingLevel > 0 then
            begin
              if CondStack[NestingLevel - 1].ParentActive and
                 not CondStack[NestingLevel - 1].BranchTaken then
                CurrentActive := True
              else
                CurrentActive := False;
            end;

            I := EndPos + 1;
            Continue;
          end
          else if DirectiveType = 'ENDIF' then
          begin
            if NestingLevel > 0 then
            begin
              // Restore parent state
              CurrentActive := CondStack[NestingLevel - 1].ParentActive;
              PopCondition;
            end;

            I := EndPos + 1;
            Continue;
          end
          else
          begin
            // OTHER directive (not conditional) - keep if active
            if CurrentActive then
              ResultBuilder.Append(Copy(AContent, I, EndPos - I + 1));
            I := EndPos + 1;
            Continue;
          end;
        end;
      end;

      // Regular character - copy if active
      if CurrentActive then
        ResultBuilder.Append(AContent[I]);

      Inc(I);
    end;

    Result := ResultBuilder.ToString;
  finally
    ResultBuilder.Free;
  end;
end;

function TASTProcessor.TryParseContent(const AContent, AFilePath: string;
  out AChunks: TCodeChunkList): Boolean;
var
  J: Integer;
  Builder: TPasSyntaxTreeBuilder;
  StringStream: TStringStream;
  SyntaxTree: TSyntaxNode;
  IncludeHandler: TIncludeHandler;
  TempChunks: TCodeChunkList;
begin
  Result := False;
  AChunks := nil;
  TempChunks := TCodeChunkList.Create;

  Builder := TPasSyntaxTreeBuilder.Create;
  try
    StringStream := TStringStream.Create(AContent, TEncoding.UTF8);
    try
      IncludeHandler := TIncludeHandler.Create;
      Builder.IncludeHandler := IncludeHandler;
      FCurrentSection := csInterface;

      try
        SyntaxTree := Builder.Run(StringStream);
        try
          for J := 0 to Length(SyntaxTree.ChildNodes) - 1 do
          begin
            var ChildNode := SyntaxTree.ChildNodes[J];
            case ChildNode.Typ of
              ntInterface:
                FCurrentSection := csInterface;
              ntImplementation:
                FCurrentSection := csImplementation;
            end;
            ProcessNode(ChildNode, AFilePath, AContent, Builder.Comments, TempChunks);
          end;
          // Parse succeeded - transfer ownership
          AChunks := TempChunks;
          TempChunks := nil;
          Result := True;
        finally
          SyntaxTree.Free;
        end;
      except
        // Parse failed - TempChunks will be freed below
      end;
    finally
      StringStream.Free;
    end;
  finally
    Builder.Free;
    TempChunks.Free; // Free if still owned (parse failed)
  end;
end;

function TASTProcessor.ProcessSingleFile(AParsedFile: TParsedFile): TCodeChunkList;
var
  PreprocessedContent: string;
begin
  if not Assigned(AParsedFile) then
  begin
    Result := TCodeChunkList.Create;
    Exit;
  end;

  // First attempt: parse original content
  if TryParseContent(AParsedFile.Content, AParsedFile.FilePath, Result) then
    Exit; // Success on first try

  // First attempt failed - try preprocessing conditionals
  PreprocessedContent := PreprocessConditionals(AParsedFile.Content);

  // Second attempt: parse preprocessed content
  if TryParseContent(PreprocessedContent, AParsedFile.FilePath, Result) then
  begin
    // Log that preprocessing helped
    WriteLn(Format(#13'  [INFO] Parsed %s after preprocessing conditionals     ',
      [ExtractFileName(AParsedFile.FilePath)]));
    Exit;
  end;

  // Both attempts failed
  Result := TCodeChunkList.Create;
  WriteLn(Format(#13'  [WARN] Failed to parse %s (even after preprocessing)    ',
    [ExtractFileName(AParsedFile.FilePath)]));
end;

function TASTProcessor.ProcessFiles(AFiles: TStringList): TCodeChunkList;
var
  I: Integer;
  ParsedFile: TParsedFile;
  FileChunks: TCodeChunkList;
  Chunk: TCodeChunk;
  Stopwatch: TStopwatch;
  ElapsedSec: Double;
  ETASec: Integer;
  ETAStr, ProgressStr: string;
  // Hybrid ETA variables
  TotalBytes, BytesProcessed, RemainingBytes: Int64;
  ChunksFromFile: Integer;
  ChunksPerSec, BytesPerSec: Double;
  EstimatedRemainingChunks: Double;
  AvgChunksPerByte: Double;
begin
  Result := TCodeChunkList.Create;
  Stopwatch := TStopwatch.StartNew;

  // Calculate total bytes upfront for better ETA estimation
  TotalBytes := 0;
  for I := 0 to AFiles.Count - 1 do
  begin
    ParsedFile := TParsedFile(AFiles.Objects[I]);
    if Assigned(ParsedFile) then
      TotalBytes := TotalBytes + Length(ParsedFile.Content);
  end;
  BytesProcessed := 0;

  try
    try
    for I := 0 to AFiles.Count - 1 do
    begin
      ParsedFile := TParsedFile(AFiles.Objects[I]);
      if Assigned(ParsedFile) then
      begin
        // Show progress with hybrid ETA after first few files
        if (I > 0) and (AFiles.Count > 5) then
        begin
          ElapsedSec := Stopwatch.ElapsedMilliseconds / 1000;
          if (ElapsedSec > 0) and (BytesProcessed > 0) then
          begin
            RemainingBytes := TotalBytes - BytesProcessed;

            // Hybrid ETA: use chunks/byte ratio to estimate remaining work
            if Result.Count > 0 then
            begin
              // Calculate rates
              ChunksPerSec := Result.Count / ElapsedSec;
              AvgChunksPerByte := Result.Count / BytesProcessed;

              // Estimate remaining chunks based on remaining bytes
              EstimatedRemainingChunks := RemainingBytes * AvgChunksPerByte;

              // ETA based on estimated remaining chunks
              if ChunksPerSec > 0 then
                ETASec := Round(EstimatedRemainingChunks / ChunksPerSec)
              else
                ETASec := 0;
            end
            else
            begin
              // Fallback to bytes-based if no chunks yet
              BytesPerSec := BytesProcessed / ElapsedSec;
              if BytesPerSec > 0 then
                ETASec := Round(RemainingBytes / BytesPerSec)
              else
                ETASec := 0;
            end;

            if ETASec >= 3600 then
              ETAStr := Format('%d:%02d:%02d', [ETASec div 3600, (ETASec mod 3600) div 60, ETASec mod 60])
            else
              ETAStr := Format('%d:%02d', [ETASec div 60, ETASec mod 60]);
            ProgressStr := Format('[%d/%d %.0f%% ETA:%s] ', [I + 1, AFiles.Count, ((I + 1) / AFiles.Count) * 100, ETAStr]);
          end
          else
            ProgressStr := Format('[%d/%d] ', [I + 1, AFiles.Count]);
        end
        else
          ProgressStr := Format('[%d/%d] ', [I + 1, AFiles.Count]);

        // Use ProcessSingleFile to parse this file
        FileChunks := ProcessSingleFile(ParsedFile);
        try
          ChunksFromFile := FileChunks.Count;

          // Move chunks to result (extract to avoid double-free)
          while FileChunks.Count > 0 do
          begin
            Chunk := FileChunks.Extract(FileChunks[0]);
            Result.Add(Chunk);
          end;

          // Single updating line with all info
          Write(Format(#13'%s%s: %d chunks | Total: %d        ',
            [ProgressStr, ExtractFileName(ParsedFile.FilePath), ChunksFromFile, Result.Count]));
          Flush(Output);
        finally
          FileChunks.Free;
        end;

        // Update bytes processed for ETA calculation
        BytesProcessed := BytesProcessed + Length(ParsedFile.Content);
      end;
    end;

    // Clear progress line and show final summary
    WriteLn(Format(#13'[AST] Processed %d files, generated %d chunks                              ',
      [AFiles.Count, Result.Count]));

    except
      on E: Exception do
      begin
        Result.Free;
        raise Exception.CreateFmt('Failed to process files with DelphiAST: %s', [E.Message]);
      end;
    end;
  finally
    FMethodDeclarations.Free; // Free dictionary
  end;
end;

end.