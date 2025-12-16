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
  I, J: Integer;
  ItemsToDelete: TList<Integer>;
  DeleteIndex: Integer;
begin
  // Use a safer approach: collect indices to delete, then delete in reverse order
  ItemsToDelete := TList<Integer>.Create;
  try
    for I := 0 to Count - 1 do
    begin
      for J := I + 1 to Count - 1 do
      begin
        if (Items[I].SymbolID = Items[J].SymbolID) and (ItemsToDelete.IndexOf(I) = -1) and (ItemsToDelete.IndexOf(J) = -1) then
        begin
          // Keep the one with higher score or exact match
          if Items[I].IsExactMatch or (Items[I].Score >= Items[J].Score) then
            ItemsToDelete.Add(J)
          else
            ItemsToDelete.Add(I);
        end;
      end;
    end;
    
    // Sort deletion indices in ascending order, then delete from high to low
    ItemsToDelete.Sort;
    for DeleteIndex := ItemsToDelete.Count - 1 downto 0 do
    begin
      if (ItemsToDelete[DeleteIndex] >= 0) and (ItemsToDelete[DeleteIndex] < Count) then
        Delete(ItemsToDelete[DeleteIndex]);
    end;
    
  finally
    ItemsToDelete.Free;
  end;
end;

end.