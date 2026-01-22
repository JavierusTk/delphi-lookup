unit uSearchTypes;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  TSearchResult = class
  private
    FSymbolID: Integer;
    FName: string;
    FFullName: string;
    FSymbolType: string;
    FFilePath: string;
    FContent: string;
    FComments: string;
    FParentClass: string;
    FImplementedInterfaces: string;
    FVisibility: string;
    FScore: Double;
    FIsExactMatch: Boolean;
    FMatchType: string;
    FContentType: string;
    FSourceCategory: string;
    FFramework: string;
    FStartLine: Integer;
    FEndLine: Integer;
  public
    constructor Create;

    property SymbolID: Integer read FSymbolID write FSymbolID;
    property Name: string read FName write FName;
    property FullName: string read FFullName write FFullName;
    property SymbolType: string read FSymbolType write FSymbolType;
    property FilePath: string read FFilePath write FFilePath;
    property Content: string read FContent write FContent;
    property Comments: string read FComments write FComments;
    property ParentClass: string read FParentClass write FParentClass;
    property ImplementedInterfaces: string read FImplementedInterfaces write FImplementedInterfaces;
    property Visibility: string read FVisibility write FVisibility;
    property Score: Double read FScore write FScore;
    property IsExactMatch: Boolean read FIsExactMatch write FIsExactMatch;
    property MatchType: string read FMatchType write FMatchType;
    property ContentType: string read FContentType write FContentType;
    property SourceCategory: string read FSourceCategory write FSourceCategory;
    property Framework: string read FFramework write FFramework;
    property StartLine: Integer read FStartLine write FStartLine;
    property EndLine: Integer read FEndLine write FEndLine;
  end;

  TSearchResultList = class(TObjectList<TSearchResult>)
  public
    procedure SortByRelevance;
    function FindBySymbolID(ASymbolID: Integer): TSearchResult;
    procedure RemoveDuplicates;
  end;

implementation

{ TSearchResult }

constructor TSearchResult.Create;
begin
  inherited Create;
  FScore := 0.0;
  FIsExactMatch := False;
  FMatchType := 'semantic';
end;

{ TSearchResultList }

procedure TSearchResultList.SortByRelevance;
begin
  Sort(TComparer<TSearchResult>.Construct(
    function(const Left, Right: TSearchResult): Integer
    begin
      // Exact matches first
      if Left.IsExactMatch and not Right.IsExactMatch then
        Result := -1
      else if Right.IsExactMatch and not Left.IsExactMatch then
        Result := 1
      else
      begin
        // Then by score (higher is better)
        if Left.Score > Right.Score then
          Result := -1
        else if Left.Score < Right.Score then
          Result := 1
        else
          Result := 0;
      end;
    end));
end;

function TSearchResultList.FindBySymbolID(ASymbolID: Integer): TSearchResult;
var
  SearchResult: TSearchResult;
begin
  Result := nil;
  for SearchResult in Self do
  begin
    if SearchResult.SymbolID = ASymbolID then
    begin
      Result := SearchResult;
      Break;
    end;
  end;
end;

procedure TSearchResultList.RemoveDuplicates;
var
  Seen: TDictionary<Integer, TSearchResult>;
  Current, Existing: TSearchResult;
  I: Integer;
  OldOwnsObjects: Boolean;
  ResultList: TList<TSearchResult>;
begin
  // Optimized O(n) algorithm using TDictionary instead of O(n²) nested loops
  Seen := TDictionary<Integer, TSearchResult>.Create;
  ResultList := TList<TSearchResult>.Create;
  try
    // First pass: identify unique items, keeping best score for each SymbolID
    for I := 0 to Count - 1 do
    begin
      Current := Items[I];

      if Seen.TryGetValue(Current.SymbolID, Existing) then
      begin
        // Duplicate found: keep the one with higher score or exact match
        if Current.IsExactMatch or (Current.Score > Existing.Score) then
        begin
          // Current is better - replace in dictionary and free old one
          Seen[Current.SymbolID] := Current;
          Existing.Free;
        end
        else
        begin
          // Existing is better - free current
          Current.Free;
        end;
      end
      else
      begin
        // New SymbolID - add to dictionary
        Seen.Add(Current.SymbolID, Current);
      end;
    end;

    // Collect surviving items
    for Current in Seen.Values do
      ResultList.Add(Current);

    // Rebuild the list without freeing objects (we already handled that)
    OldOwnsObjects := OwnsObjects;
    OwnsObjects := False;
    Clear;
    OwnsObjects := OldOwnsObjects;

    // Add back the deduplicated items
    for Current in ResultList do
      Add(Current);

  finally
    ResultList.Free;
    Seen.Free;
  end;
end;

end.