unit epicustomitemlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, epicustombase, epicustomitem;

type

  TEpiCustomItemList           = class;
  TEpiCustomItemListEnumerator = class;
  TEpiOnNewItemClass           = function(Sender: TEpiCustomItemList; DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass of object;
  TEpiValidateRenameEvent      = function(Const NewName: string): boolean of object;
  TEpiPrefixEvent              = function: string of object;

  { TEpiCustomList }

  TEpiCustomItemList = class(TEpiCustomItem)
  private
    FItemOwner: boolean;
    FList: TFPList;
    procedure   SetItemOwner(const AValue: boolean);
    procedure   OnChangeHook(Const Sender: TEpiCustomBase;
       Const Initiator: TEpiCustomBase;
       EventGroup: TEpiEventGroup;
       EventType: Word; Data: Pointer);
    procedure   RegisterItem(Item: TEpiCustomItem);
    procedure   UnRegisterItem(Item: TEpiCustomItem);
  protected
    constructor Create(AOwner: TEpiCustomBase); override;
    function    GetCount: Integer; virtual;
    function    GetItems(Index: integer): TEpiCustomItem; virtual;
    procedure   SetItems(Index: integer; const AValue: TEpiCustomItem); virtual;
    function    WriteNameToXml: boolean; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    property    List: TFPList read FList;
  protected
    function    SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    destructor  Destroy; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
  { Standard Item Methods }
  public
    procedure   Clear;
    procedure   ClearAndFree;
    function    ItemClass: TEpiCustomItemClass; virtual;
    function    NewItem(AItemClass: TEpiCustomItemClass = nil): TEpiCustomItem; virtual;
    // AddItem uses InsertItem internally, so only that method need overriding if needed.
    procedure   AddItem(Item: TEpiCustomItem); virtual;
    procedure   InsertItem(const Index: integer; Item: TEpiCustomItem); virtual;
    // RemoveItem uses DeleteItem internally, so only that method need overriding if needed.
    procedure   RemoveItem(Item: TEpiCustomItem); virtual;
    function    DeleteItem(Index: integer): TEpiCustomItem; virtual;
    function    GetItemByName(AName: string): TEpiCustomItem; virtual;
    function    ItemExistsByName(AName: string): boolean; virtual;
    function    IndexOf(Item: TEpiCustomItem): integer; virtual;
    function    GetEnumerator: TEpiCustomItemListEnumerator;
    property    Count: Integer read GetCount;
    property    Items[Index: integer]: TEpiCustomItem read GetItems write SetItems; default;
    property    ItemOwner: boolean read FItemOwner write SetItemOwner;
  { Naming and Validation }
  private
    FOnGetPrefix: TEpiPrefixEvent;
    FOnValidateRename: TEpiValidateRenameEvent;
    FUniqueNames: boolean;
    procedure SetUniqueNames(AValue: boolean);
  protected
    function  DoPrefix: string;
    function  Prefix: string; virtual;
  public
    function  GetUniqueItemName(AClass: TEpiCustomItemClass): string; virtual;
    function  ValidateRename(Const NewName: string; RenameOnSuccess: boolean): boolean; override;
    property  UniqueNames: boolean read FUniqueNames write SetUniqueNames;
    property  OnValidateRename: TEpiValidateRenameEvent read FOnValidateRename write FOnValidateRename;
    property  OnGetPrefix: TEpiPrefixEvent read FOnGetPrefix write FOnGetPrefix;
  { New Item Hook }
  private
    FOnNewItemClass: TEpiOnNewItemClass;
  public
    property   OnNewItemClass: TEpiOnNewItemClass read FOnNewItemClass write FOnNewItemClass;
  { Change-event hooks overrides }
  protected
    procedure DoChange(const Initiator: TEpiCustomBase;
       EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer); override;
       overload;
  public
    procedure  BeginUpdate; override;
    procedure  EndUpdate; override;
  { Tanslation overrides }
  public
    procedure SetLanguage(Const LangCode: string;
      Const DefaultLanguage: boolean); override;
  { Class properties overrides }
  protected
    procedure SetModified(const AValue: Boolean); override;
    procedure DoAssignList(Const EpiCustomItemList: TEpiCustomItemList); virtual;
    procedure Assign(const AEpiCustomBase: TEpiCustomBase); override;
  { Sorting }
  private
    FOnSort: TListSortCompare;
    FSorted: boolean;
    procedure SetSorted(AValue: boolean);
  protected
    procedure DoSort; virtual;
  public
    procedure Sort;
    property  Sorted: boolean read FSorted write SetSorted;
    property  OnSort: TListSortCompare read FOnSort write FOnSort;
  { Cloning }
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  end;

  { TEpiCustomListEnumerator }

  TEpiCustomItemListEnumerator = class
  private
    FCurrentIndex: Integer;
    FCustomList: TEpiCustomItemList;
  protected
    function GetCurrent: TEpiCustomItem; virtual;
  public
    constructor Create(CustomItemList: TEpiCustomItemList); virtual;
    function MoveNext: Boolean;
    property Current: TEpiCustomItem read GetCurrent;
  end;

implementation

{ TEpiCustomList }

procedure TEpiCustomItemList.SetItemOwner(const AValue: boolean);
begin
  if FItemOwner = AValue then exit;
  FItemOwner := AValue;
end;

procedure TEpiCustomItemList.OnChangeHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  EpiInitiator: TEpiCustomItem absolute Initiator;
begin
  if Initiator = self then exit;
  if EventGroup <> eegCustomBase then exit;
  if not (Initiator is TEpiCustomItem) then exit;
  if IndexOf(EpiInitiator) = -1 then exit;

  case TEpiCustomChangeEventType(EventType) of
    ecceDestroy: RemoveItem(EpiInitiator);
    ecceAddItem: Sort;
    ecceDelItem: Sort;
    ecceUpdate:  Sort;
  end;
end;

procedure TEpiCustomItemList.RegisterItem(Item: TEpiCustomItem);
begin
  if ItemOwner then
  begin
    Item.FOwner := Self;
    Item.SetLanguage(DefaultLang, true);
    Item.SetLanguage(CurrentLang, false);
  end else
    Item.RegisterOnChangeHook(@OnChangeHook, true);

  DoChange(eegCustomBase, Word(ecceAddItem), Item);
  if Sorted then Sort;
end;

procedure TEpiCustomItemList.UnRegisterItem(Item: TEpiCustomItem);
begin
//  if not (ebsDestroying in Item.State) then
  Item.UnRegisterOnChangeHook(@OnChangeHook);

  if ItemOwner then Item.FOwner := nil;
  DoChange(eegCustomBase, Word(ecceDelItem), Item);
  if Sorted then Sort;
end;

constructor TEpiCustomItemList.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FList := TFPList.Create;
  FItemOwner := false;
  FSorted := false;
  FUniqueNames := true;
end;

function TEpiCustomItemList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TEpiCustomItemList.GetItems(Index: integer): TEpiCustomItem;
begin
  result := TEpiCustomItem(FList[Index]);
end;

procedure TEpiCustomItemList.SetItems(Index: integer; const AValue: TEpiCustomItem
  );
var
  Val: Pointer;
begin
  // TODO : Set owner!
  if FList[Index] = Pointer(AValue) then exit;
  Val := FList[Index];
  FList[Index] := AValue;
  DoChange(eegCustomBase, Word(ecceSetItem), Val);
end;

function TEpiCustomItemList.WriteNameToXml: boolean;
begin
  Result := false;
end;

procedure TEpiCustomItemList.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  NItem: TEpiCustomItem;
  Node: TDOMNode;
begin
  inherited LoadFromXml(Root, ReferenceMap);

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    // hack to skip whitespace nodes.
    while NodeIsWhiteSpace(Node) do
      Node := Node.NextSibling;
    if not Assigned(Node) then break;

    NItem := NewItem();
    CheckNode(Node, NItem.XMLName);
    NItem.LoadFromXml(Node, ReferenceMap);

    Node := Node.NextSibling;
  end;
end;

function TEpiCustomItemList.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
var
  Elem: TDOMElement;
  Item: TEpiCustomItem;
begin
  if Count = 0 then
    Exit(nil);

  Result := inherited SaveToDom(RootDoc);

  for Item in Self do
  begin
    Elem := Item.SaveToDom(RootDoc);
    if Assigned(Elem) then
      Result.AppendChild(Elem);
  end;
end;

function TEpiCustomItemList.GetUniqueItemName(AClass: TEpiCustomItemClass): string;
var
  i: Integer;
begin
  i := Count + 1;
  repeat
    result := DoPrefix + IntToStr(i);
    Inc(i);
  until ValidateRename(result, false);
end;

function TEpiCustomItemList.DoPrefix: string;
begin
  if Assigned(OnGetPrefix) then
    result := OnGetPrefix()
  else
    result := Prefix;
end;

function TEpiCustomItemList.Prefix: string;
begin
  result := 'id_';
end;

function TEpiCustomItemList.ValidateRename(const NewName: string;
  RenameOnSuccess: boolean): boolean;
begin
  // if unique names is not required, you can ALWAYS rename item.
  if (not UniqueNames) then
    Exit(true);

  result := not ItemExistsByName(NewName);
  if Assigned(OnValidateRename) then
    result := result and OnValidateRename(NewName);
end;

procedure TEpiCustomItemList.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited DoChange(Initiator, EventGroup, EventType, Data);
  OnChangeHook(Self, Initiator, EventGroup, EventType, Data);
end;

destructor TEpiCustomItemList.Destroy;
var
  F: TEpiCustomItem;
begin
  ClearAndFree;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TEpiCustomItemList.SaveToXml(Content: String; Lvl: integer): string;
var
  S: String;
  i: Integer;
begin
  S := '';
  for i := 0 to Count - 1 do
    S += Items[i].SaveToXml('', Lvl + 1);
  Content += S;
  result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiCustomItemList.Clear;
begin
  while Count > 0 do
    DeleteItem(Count - 1);
end;

procedure TEpiCustomItemList.ClearAndFree;
var
  F: TEpiCustomItem;
begin
  while FList.Count > 0 do
  begin
    // Using this unusual construct in destroying list items (when owned)
    // ensures that destroy notifications from Items is defered until after
    // the item is removed from the list.
    F := TEpiCustomItem(FList.Last);
    RemoveItem(F);
    if ItemOwner then
      F.Free;
  end;
end;

function TEpiCustomItemList.ItemClass: TEpiCustomItemClass;
begin
  result := nil;
end;

function TEpiCustomItemList.NewItem(AItemClass: TEpiCustomItemClass
  ): TEpiCustomItem;
begin
  if not Assigned(AItemClass) then
    AItemClass := ItemClass;

  if Assigned(OnNewItemClass) then
    AItemClass := OnNewItemClass(Self, AItemClass);

  if not Assigned(AItemClass) then
    Exception.Create('TEpiCustomItemList: No ItemClass Defined!');

  Result := AItemClass.Create(Self);
  Result.Name := GetUniqueItemName(AItemClass);
  AddItem(Result);
end;

procedure TEpiCustomItemList.AddItem(Item: TEpiCustomItem);
begin
  // AddItem uses InsertItem internally, so only that method need overriding if needed.
  InsertItem(Count, Item);
end;

procedure TEpiCustomItemList.InsertItem(const Index: integer; Item: TEpiCustomItem
  );
begin
  if (not ValidateRename(Item.Name, false)) then
    raise TEpiCoreException.Create('Item "' + Item.Name + '" already exist in list');
  FList.Insert(Index, Item);
  RegisterItem(Item);
end;

procedure TEpiCustomItemList.RemoveItem(Item: TEpiCustomItem);
begin
  // RemoveItem uses DeleteItem internally, so only that method need overriding if needed.
  DeleteItem(FList.IndexOf(Item));
end;

function TEpiCustomItemList.DeleteItem(Index: integer): TEpiCustomItem;
begin
  Result := TEpiCustomItem(FList[Index]);
  FList.Delete(Index);
  UnRegisterItem(Result);
end;

function TEpiCustomItemList.GetItemByName(AName: string): TEpiCustomItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if TEpiCustomItem(FList[i]).Name = AName then
    begin
      Result := TEpiCustomItem(FList[i]);
      Exit;
    end;
  end;
end;

function TEpiCustomItemList.ItemExistsByName(AName: string): boolean;
begin
  result := Assigned(GetItemByName(AName));
end;

function TEpiCustomItemList.IndexOf(Item: TEpiCustomItem): integer;
begin
  result := FList.IndexOf(Item);
end;

function TEpiCustomItemList.GetEnumerator: TEpiCustomItemListEnumerator;
begin
  result := TEpiCustomItemListEnumerator.Create(Self);
end;

procedure TEpiCustomItemList.SetUniqueNames(AValue: boolean);
begin
  if FUniqueNames = AValue then Exit;
  FUniqueNames := AValue;
end;

procedure TEpiCustomItemList.BeginUpdate;
var
  i: Integer;
begin
  inherited BeginUpdate;
  for i := 0 to Count - 1 do
    Items[i].BeginUpdate;
end;

procedure TEpiCustomItemList.EndUpdate;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].EndUpdate;
  inherited EndUpdate;
end;

procedure TEpiCustomItemList.SetLanguage(const LangCode: string;
  const DefaultLanguage: boolean);
var
  i: Integer;
begin
  inherited SetLanguage(LangCode, DefaultLanguage);
  for i := 0 to Count - 1 do
    Items[i].SetLanguage(LangCode, DefaultLanguage);
end;

procedure TEpiCustomItemList.SetModified(const AValue: Boolean);
var
  i: Integer;
begin
  inherited SetModified(AValue);
  if not AValue then
    for i := 0 to Count - 1 do
      Items[i].Modified := AValue;
end;

procedure TEpiCustomItemList.DoAssignList(const EpiCustomList: TEpiCustomItemList);
var
  i: Integer;
  NItemClass: TEpiCustomItemClass;
  Item: TEpiCustomItem;
begin
  BeginUpdate;
  OnNewItemClass := EpiCustomList.OnNewItemClass;
  if EpiCustomList.Count > 0 then
  begin
    NItemClass := TEpiCustomItemClass(EpiCustomList[0].ClassType);
    for i := 0 to EpiCustomList.Count - 1 do
    begin
      Item := NewItem(NItemClass);
      Item.Assign(EpiCustomList[i]);
    end;
  end;
  EndUpdate;
end;

procedure TEpiCustomItemList.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);

  if AEpiCustomBase is TEpiCustomItemList then
    DoAssignList(TEpiCustomItemList(AEpiCustomBase));
end;

procedure TEpiCustomItemList.SetSorted(AValue: boolean);
begin
  if FSorted = AValue then Exit;
  FSorted := AValue;
end;

procedure TEpiCustomItemList.DoSort;
begin
  if not Sorted then exit;

  if Assigned(FOnSort) then
    FList.Sort(FOnSort)
end;

procedure TEpiCustomItemList.Sort;
begin
  DoSort;
end;

function TEpiCustomItemList.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
var
  i: Integer;
  NItem: TEpiCustomItem;

  function GetRandomName: string;
  var
    GUID: TGUID;
  begin
    // Hack: Create a GUID to use as name.
    //  - the comp. name is not used in other parts of the program anyway,
    //  - so using GUID is a valid way to create random components names... :)
    //  - And the chance of creating to equal component name are very-very-very unlikely.
    CreateGUID(GUID);
    Result := '_' + StringsReplace(GUIDToString(GUID), ['{','}','-'], ['','',''], [rfReplaceAll]);
  end;

begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  // Set itemowner before copying the items - otherwise
  // owner is not set properly.
  TEpiCustomItemList(Result).FItemOwner := FItemOwner;
  for i := 0 to Count - 1 do
  begin
    NItem := TEpiCustomItem(Items[i].DoCloneCreate(Result));
    // Set a random name, otherwise calling Add will fail.
    // Name is correctly set in Items[i].DoClone...
    NItem.FName := GetRandomName;
    if TEpiCustomItemList(Result).IndexOf(NItem) = -1 then
      TEpiCustomItemList(Result).AddItem(NItem);
    Items[i].DoClone(Result, NItem, ReferenceMap);
  end;

  // Set Sorting last, otherwise each AddItem triggers a sort.
  with TEpiCustomItemList(Result) do
  begin
    FOnSort := Self.FOnSort;
    FSorted := Self.FSorted;
    Sort;
  end;
end;

{ TEpiCustomItemListEnumerator }

function TEpiCustomItemListEnumerator.GetCurrent: TEpiCustomItem;
begin
  result := FCustomList[FCurrentIndex];
end;

constructor TEpiCustomItemListEnumerator.Create(CustomItemList: TEpiCustomItemList);
begin
  FCustomList := CustomList;
  FCurrentIndex := -1;
end;

function TEpiCustomItemListEnumerator.MoveNext: Boolean;
begin
  Inc(FCurrentIndex);
  Result := (FCurrentIndex < FCustomList.Count);
end;

end.

