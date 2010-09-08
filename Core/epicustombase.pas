unit epicustombase;

{$codepage UTF8}
{$mode objfpc}{$H+}

{$IFDEF EPI_DEBUG}
 {$IFDEF LINUX}
  {$DEFINE EPI_CONSOLE_DEBUG}
 {$ENDIF LINUX}
{$ENDIF EPI_DEBUG}

interface

uses
  Classes, SysUtils, DOM, DCPrijndael;

type
  TEpiCustomBase = class;
  TEpiCustomItem = class;
  TEpiCustomList = class;

  { TEpiCustomBase }
  // eeg = Epi Event Group
  TEpiEventGroup = (
    // epicustomcase.pas
    eegCustomBase,
    // epidocument.pas
    eegDocument,
    // episettings.pas
    eegXMLSetting,
    eegProjectSettings,
    // epiadmin.pas
    eegAdmin,
    // epistudy.pas
    eegStudy,
    // epidatafiles.pas
    eegDataFiles,
    eegSections,
    eegFields,
    eegGroups,
    eegHeading,
    // epivaluelabels.pas
    eegValueLabels,
    // epirelations.pas
    eegRelates
    );
  // ecce = Epi Custom Change Event
  TEpiCustomChangeEventType = (
    ecceDestroy, ecceUpdate, ecceId, ecceAddItem, ecceDelItem, ecceSetItem,
    ecceSetTop, ecceSetLeft, ecceText
  );
  TEpiChangeEvent = procedure(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer) of object;

  TEpiCustomBaseState = set of (ebsDestroying, ebsUpdating);

  {$static on}
  TEpiCustomBase = class
  { Scrambling }
  private
    FCrypter:   TDCP_rijndael; static;
    function    Get4ByteSalt: Integer;
  protected
    procedure   InitCrypt(Key: string);
    function    EnCrypt(Const S: string): string; overload;
    function    DeCrypt(Root: TDOMNode): TDOMNode; overload;
    function    DeCrypt(S: string): string; overload;
//    property    Crypter: TDCP_rijndael read FCrypter;   // DOES NOT WORK WITH FPC 2.4 - only from 2.5.1

  { Save/Load functionality }
  private
    procedure  RaiseError(const Root: TDOMNode; NodeName: string);
  protected
    function   StringToXml(const S: string): string;
    Function   Indent(Level: integer): string;
    function   ScrambleXml: boolean; virtual;

    { Check methods }
    procedure  CheckNode(const Node: TDOMNode; const NodeName: string); virtual;
    { Load methods }
    function   LoadNode(var Node: TDOMNode; const Root: TDOMNode;
      NodeName: string; Fatal: boolean): boolean; virtual;
    // Direct loading of node are always fatal, since they must return some value.
    function   LoadNodeInt(const Root: TDOMNode; NodeName: string): integer;
    function   LoadNodeFloat(const Root: TDOMNode; NodeName: string): extended;
    function   LoadNodeString(const Root: TDOMNode; NodeName: string): String;
    function   LoadNodeDateTime(const Root: TDOMNode; NodeName: string): TDateTime;
    function   LoadNodeBool(const Root: TDOMNode; NodeName: string): boolean;
    // Singleton saves
    function   SaveNode(const Lvl: integer; const NodeName: string;
      const Val: string): string; overload;
    function   SaveNode(const Lvl: integer; const NodeName: string;
      const Val: integer): string; overload;
    function   SaveNode(const Lvl: integer; const NodeName: string;
      const Val: extended): string; overload;
    function   SaveNode(const Lvl: integer; const NodeName: string;
      const Val: TDateTime): string; overload;
    function   SaveNode(const Lvl: integer; const NodeName: string;
      const Val: boolean): string; overload;
  public
    function   XMLName: string; virtual;
    function   SaveToXml(Content: String; Lvl: integer): string; virtual;
    procedure  LoadFromXml(Root: TDOMNode); virtual;

    { Change-event hooks }
  private
    FOnChangeList: ^TEpiChangeEvent;
    FOnChangeListCount: Integer;
    FOnChangeListIgnoreUpdate: ^TEpiChangeEvent;
    FOnChangeListCountIgnoreUpdate: Integer;
    FUpdateCount: Integer;
    function   GetOnChangeListCount: integer;
    function   GetOnChangeListCountIgnoreUpdate: integer;
  protected
    procedure  DoChange(EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer); virtual;
  public
    procedure  BeginUpdate; virtual;
    procedure  EndUpdate; virtual;
    procedure  RegisterOnChangeHook(Event: TEpiChangeEvent; IgnoreUpdate: boolean = false); virtual;
    procedure  UnRegisterOnChangeHook(Event: TEpiChangeEvent); virtual;

  { Translation }
  private
    FCurrentLang: string;
    FDefaultLang: string;
  public
    procedure   SetLanguage(Const LangCode: string;
      const DefaultLanguage: boolean); virtual;

  { Class properties / inheritance }
  private
    FClassList: TFPList;
    FModified: Boolean;
    FOnModified: TNotifyEvent;
    FOwner:     TEpiCustomBase;
    FState:     TEpiCustomBaseState;
    function    GetRootOwner: TEpiCustomBase;
    procedure   SetOnModified(const AValue: TNotifyEvent);
  protected
    constructor Create(AOwner: TEpiCustomBase); virtual;
    procedure   SetModified(const AValue: Boolean); virtual;
    procedure   RegisterClasses(AClasses: Array of TEpiCustomBase); virtual;
    property    ClassList: TFPList read FClassList;
  public
    destructor  Destroy; override;
    property    Owner: TEpiCustomBase read FOwner;
    property    RootOwner: TEpiCustomBase read GetRootOwner;
    property    State: TEpiCustomBaseState read FState;
    property    Modified: Boolean read FModified write SetModified;
    property    OnModified: TNotifyEvent read FOnModified write SetOnModified;
  end;
  {$static off}

  { TEpiTranslatedText }

  TEpiTranslatedText = class(TEpiCustomBase)
  private
    FTextList: TStringList;
    FCurrentText: String;
    FXMLName: string;
    procedure   SetCurrentText(const AValue: string);
    procedure   SetText(Const LangCode: string; Const AText: string);
    function    GetText(Const LangCode: string): string;
  protected
    procedure   SetLanguage(Const LangCode: string;
      Const DefaultLanguage: boolean); override;
    function    XMLName: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase; Const aXMLName: string);
    destructor  Destroy; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    property    Text: string read FCurrentText write SetCurrentText;
    property    TextLang[LangCode: string]: string read GetText write SetText;
  end;

  { TEpiCustomItem }

  TEpiCustomItem = class(TEpiCustomBase)
  protected
    FId: string;
    function    GetId: string; virtual;
    procedure   SetId(const AValue: string); virtual;
    class function IdString: string; virtual; abstract;
  public
    destructor  Destroy; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    property    Id: string read GetId write SetId;
  end;
  TEpiCustomItemClass = class of TEpiCustomItem;

  { TEpiCustomNamedItem }

{  TEpiCustomNamedItem = Class(TEpiCustomItem)
  private
    FName: TEpiTranslatedText;
  protected
    constructor Create(AOwner: TEpiCustomBase); override;
  public
    destructor  Destroy; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    property    Name: TEpiTranslatedText read FName;
  end;     }

  { TEpiCustomControlItem }

  TEpiCustomControlItem = class(TEpiCustomItem)
  private
    FLeft: integer;
    FTop: integer;
  protected
    function   SaveToXml(Content: String; Lvl: integer): string; override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    procedure  SetLeft(const AValue: Integer); virtual;
    procedure  SetTop(const AValue: Integer); virtual;
  public
    property   Left: Integer read FLeft write SetLeft;
    property   Top: Integer read FTop write SetTop;
  end;

  { TEpiCustomList }

  TEpiOnNewItemClass = function(Sender: TEpiCustomList; DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass of object;
  TEpiCustomList = class(TEpiCustomItem)
  private
    FItemOwner: boolean;
    FList: TFPList;
    procedure   SetItemOwner(const AValue: boolean);
    procedure   OnChangeHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  protected
    constructor Create(AOwner: TEpiCustomBase); override;
    function    GetCount: Integer; virtual;
    function    GetItems(Index: integer): TEpiCustomItem; virtual;
    procedure   SetItems(Index: integer; const AValue: TEpiCustomItem); virtual;
    class function IdString: string; override;
    property    List: TFPList read FList;
  public
    destructor  Destroy; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    function    NewItem(ItemClass: TEpiCustomItemClass): TEpiCustomItem;
    procedure   AddItem(Item: TEpiCustomItem); virtual;
    procedure   RemoveItem(Item: TEpiCustomItem); virtual;
    function    DeleteItem(Index: integer): TEpiCustomItem; virtual;
    function    GetItemById(aId: string): TEpiCustomItem; virtual;
    function    ItemExistsById(aId: string): boolean; virtual;
    function    IndexOf(Item: TEpiCustomItem): integer; virtual;
    function    GetUniqueItemId(AClass: TEpiCustomItemClass): string; virtual;
    property    Count: Integer read GetCount;
    property    Items[Index: integer]: TEpiCustomItem read GetItems write SetItems; default;
    property    ItemOwner: boolean read FItemOwner write SetItemOwner;

  { New Item Hook }
  private
    FOnNewItemClass: TEpiOnNewItemClass;
  public
    property   OnNewItemClass: TEpiOnNewItemClass read FOnNewItemClass write FOnNewItemClass;

  { Change-event hooks overrides }
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
  end;

{$I epixmlconstants.inc}

implementation

uses
  StrUtils, DCPsha256, XMLRead, epistringutils;

{ TEpiCustomBase }

function TEpiCustomBase.Get4ByteSalt: Integer;
begin
  result := Random(maxLongint - 1) + 1;
end;

constructor TEpiCustomBase.Create(AOwner: TEpiCustomBase);
begin
  FOwner := AOwner;
  FClassList := TFPList.Create;
  FState := [];
  FModified := false;
end;

procedure TEpiCustomBase.RegisterClasses(AClasses: array of TEpiCustomBase);
var
  i: Integer;
begin
  for i := Low(AClasses) to High(AClasses) do
    ClassList.Add(AClasses[i]);
end;

destructor TEpiCustomBase.Destroy;
begin
  // Do the last Free notification to the event hooks.
  // - this allows for objects pointing the "self" to remove reference if needed.
  {$IFDEF EPI_CONSOLE_DEBUG}
  writeln('TEpiCustomBase.Destoy: ' + ClassName);
  {$ENDIF EPI_CONSOLE_DEBUG}
  Include(FState, ebsDestroying);
  DoChange(eegCustomBase, Word(ecceDestroy), nil);

  FClassList.Free;
  Freemem(FOnChangeList);
  Freemem(FOnChangeListIgnoreUpdate);
  inherited Destroy;
end;

procedure TEpiCustomBase.InitCrypt(Key: string);
begin
  if not Assigned(FCrypter) then
  begin
    FCrypter := TDCP_rijndael.Create(nil);
    Randomize;
  end;

  FCrypter.InitStr(Key, TDCP_sha256);
end;

function TEpiCustomBase.EnCrypt(const S: string): string;
var
  Salt: Integer;
  SaltStr: Array[0..3] of Char absolute Salt;
begin
  // We salt all encryptions with a 4-byte (random) salt. This is because
  // the plaintext usually consist of XML code which for most parts contain
  // the same initial text. Since we reset the chaining information each time
  // a new encryption starts, the encrypted section end up with the first few
  // bytes of ciphertext being the same - and this is NOT a secure encryption.
  // Hence pre-padding with 4 random bytes, will do the trick for most parts.
  Salt := Get4ByteSalt;
  Result := FCrypter.EncryptString(String(SaltStr) + S);

  FCrypter.Reset;
end;

function TEpiCustomBase.DeCrypt(Root: TDOMNode): TDOMNode;
var
  St: TStringStream;
  XMLDoc: TDOMDocumentFragment;
  s: String;
  Node: TDOMNode;
begin
  Result := nil;

  // Extract the text content from this node (but not subnodes).
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    if Node.NodeType = TEXT_NODE then
      break;
    node := Node.NextSibling;
  end;

  s := FCrypter.DecryptString(Trim(TDOMText(Node).Data));
  St := TStringStream.Create(s);
  // Shift 4 bytes to get rid of scrambling salt...
  St.Position := 4;

  XMLDoc := Root.OwnerDocument.CreateDocumentFragment;
  ReadXMLFragment(XMLDoc, St);
  ST.Free;
  FCrypter.Reset;
  Result := XMLDoc;
end;

function TEpiCustomBase.DeCrypt(S: string): string;
begin
  Result := FCrypter.DecryptString(S);
  FCrypter.Reset;
  Delete(Result, 1, 4);
end;

procedure TEpiCustomBase.RaiseError(const Root: TDOMNode; NodeName: string) ;
begin
  raise Exception.Create('ERROR: A required XML tag was not found.' + LineEnding +
          Format('In section %s the tag "%s" was expected!', [Root.NodeName, NodeName]));
end;

function TEpiCustomBase.StringToXml(const S: string): string;
begin
 result := StringsReplace(S,
   ['&',     '"',      '<',    '>'],
   ['&amp;', '&quot;', '&lt;', '&gt;'],
   [rfReplaceAll]);
end;

function TEpiCustomBase.Indent(Level: integer): string;
begin
  result := DupeString(' ', Level);
end;

function TEpiCustomBase.ScrambleXml: boolean;
begin
  result := false;
end;

procedure TEpiCustomBase.CheckNode(const Node: TDOMNode; const NodeName: string
  );
begin
  if Node.CompareName(NodeName) <> 0 then
    RaiseError(Node, NodeName);
end;

function TEpiCustomBase.LoadNode(var Node: TDOMNode; const Root: TDOMNode;
  NodeName: string; Fatal: boolean): boolean;
begin
  result := true;

  Node := Root.FindNode(NodeName);
  if Assigned(Node) then exit;

  result := false;
  if not Fatal then exit;

  RaiseError(Root, NodeName);
end;

function TEpiCustomBase.LoadNodeInt(const Root: TDOMNode;
  NodeName: string): integer;
var
  Node: TDOMNode;
begin
  LoadNode(Node, Root, NodeName, true);
  result := StrToInt(Node.TextContent);
end;

function TEpiCustomBase.LoadNodeFloat(const Root: TDOMNode;
  NodeName: string): extended;
var
  Node: TDOMNode;
begin
  LoadNode(Node, Root, NodeName, true);
  result := StrToFloat(Node.TextContent);
end;

function TEpiCustomBase.LoadNodeString(const Root: TDOMNode;
  NodeName: string): String;
var
  Node: TDOMNode;
begin
  LoadNode(Node, Root, NodeName, true);
  result := UTF8Encode(Node.TextContent);
end;

function TEpiCustomBase.LoadNodeDateTime(const Root: TDOMNode; NodeName: string
  ): TDateTime;
var
  Node: TDOMNode;
begin
  LoadNode(Node, Root, NodeName, true);
  DefaultFormatSettings.ShortDateFormat := 'YYYY/MM/DD HH:NN';
  result := StrToDateTime(Node.TextContent);
end;

function TEpiCustomBase.LoadNodeBool(const Root: TDOMNode;
  NodeName: string): boolean;
var
  Node: TDOMNode;
begin
  LoadNode(Node, Root, NodeName, true);
  result := WideLowerCase(Node.TextContent) = 'true';
end;

function TEpiCustomBase.SaveNode(const Lvl: integer; const NodeName: string;
  const Val: string): string;
begin
  Result :=
    Indent(Lvl) + '<' + NodeName + '>' + StringToXml(Val) + '</' + NodeName + '>' +
    LineEnding;
end;

function TEpiCustomBase.SaveNode(const Lvl: integer; const NodeName: string;
  const Val: integer): string;
begin
  result := SaveNode(Lvl, NodeName, IntToStr(Val));
end;

function TEpiCustomBase.SaveNode(const Lvl: integer; const NodeName: string;
  const Val: extended): string;
begin
  result := SaveNode(Lvl, NodeName, FloatToStr(Val));
end;

function TEpiCustomBase.SaveNode(const Lvl: integer; const NodeName: string;
  const Val: TDateTime): string;
begin
  result := SaveNode(Lvl, NodeName, FormatDateTime('YYYY/MM/DD HH:NN', Val));
end;

function TEpiCustomBase.SaveNode(const Lvl: integer; const NodeName: string;
  const Val: boolean): string;
begin
  result := SaveNode(Lvl, NodeName, BoolToStr(Val, 'true', 'false'));
end;

function TEpiCustomBase.XMLName: string;
begin
  result := ClassName;
end;

function TEpiCustomBase.SaveToXml(Content: String; Lvl: integer): string;
var
  i: Integer;
  S: String;
begin
  S := Content;
  for i := 0 to ClassList.Count - 1 do
    S += TEpiCustomBase(ClassList[i]).SaveToXml('', Lvl + 1);

  if ScrambleXml then
    S := EnCrypt(S) + LineEnding;

  Result :=
    Indent(Lvl) + '<' + XMLName;

  // For the "id" attribute
  if (Self is TEpiCustomItem) and (TEpiCustomItem(Self).Id <> '') then
    Result += ' id="' + TEpiCustomItem(Self).Id + '" ';

  Result += '>' + LineEnding +
    S +
    Indent(Lvl) + '</' + XMLName + '>' + LineEnding;
end;

procedure TEpiCustomBase.LoadFromXml(Root: TDOMNode);
begin
  // Do nothing - should be overridden in descendants.
end;

function TEpiCustomBase.GetOnChangeListCount: integer;
begin
  result := FOnChangeListCount;
end;

function TEpiCustomBase.GetOnChangeListCountIgnoreUpdate: integer;
begin
  result := FOnChangeListCountIgnoreUpdate;
end;

procedure TEpiCustomBase.DoChange(EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  i: Integer;
begin
  i := 0;
  while i < GetOnChangeListCountIgnoreUpdate do
  begin
    FOnChangeListIgnoreUpdate[i](Self, EventGroup, EventType, Data);
    inc(i);
  end;

  if ((EventGroup = eegCustomBase) and (EventType <> Word(ecceUpdate))) or
     (EventGroup <> eegCustomBase)
  then
    Modified := true;

  if FUpdateCount > 0 then exit;

  i := 0;
  while i < GetOnChangeListCount do
  begin
    FOnChangeList[i](Self, EventGroup, EventType, Data);
    inc(i);
  end;
end;

procedure TEpiCustomBase.BeginUpdate;
var
  i: Integer;
begin
  {$IFDEF EPI_CONSOLE_DEBUG}
  writeln('TEpiCustomBase.BeginUpdate: ' + ClassName);
  {$ENDIF EPI_CONSOLE_DEBUG}
  Inc(FUpdateCount);

  for i := 0 to ClassList.Count - 1 do
    TEpiCustomBase(ClassList[i]).BeginUpdate;
end;

procedure TEpiCustomBase.EndUpdate;
var
  i: Integer;
begin
  {$IFDEF EPI_CONSOLE_DEBUG}
  writeln('TEpiCustomBase.EndUpdate: ' + ClassName);
  {$ENDIF EPI_CONSOLE_DEBUG}
  for i := ClassList.Count - 1 downto 0 do
    TEpiCustomBase(ClassList[i]).EndUpdate;

  Dec(FUpdateCount);
  if (FUpdateCount < 0) or (FUpdateCount > 0) then
  begin
    if (FUpdateCount < 0) then
      FUpdateCount := 0;
    exit;
  end;

  Include(FState, ebsUpdating);
  DoChange(eegCustomBase, word(ecceUpdate), nil);
  Exclude(FState, ebsUpdating);
end;

procedure TEpiCustomBase.RegisterOnChangeHook(Event: TEpiChangeEvent;
  IgnoreUpdate: boolean);
begin
  if IgnoreUpdate then
  begin
    Inc(FOnChangeListCountIgnoreUpdate);
    ReAllocMem(FOnChangeListIgnoreUpdate, FOnChangeListCountIgnoreUpdate * SizeOf(TEpiChangeEvent));
    FOnChangeListIgnoreUpdate[FOnChangeListCountIgnoreUpdate-1] := Event;
  end else begin
    Inc(FOnChangeListCount);
    ReAllocMem(FOnChangeList, FOnChangeListCount * SizeOf(TEpiChangeEvent));
    FOnChangeList[FOnChangeListCount-1] := Event;
  end;
end;

procedure TEpiCustomBase.UnRegisterOnChangeHook(Event: TEpiChangeEvent);
var
  Idx: LongInt;
begin
  Idx := 0;
  while Idx <= FOnChangeListCountIgnoreUpdate -1 do
  begin
    if FOnChangeListIgnoreUpdate[Idx] = Event then
      break;
    Inc(Idx)
  end;
  if (Idx < FOnChangeListCountIgnoreUpdate) then
  begin
    dec(FOnChangeListCountIgnoreUpdate);
    if FOnChangeListCountIgnoreUpdate > Idx then
      System.Move(FOnChangeListIgnoreUpdate[Idx+1],FOnChangeListIgnoreUpdate[Idx],(FOnChangeListCountIgnoreUpdate-Idx)*SizeOf(TEpiChangeEvent));
    ReAllocMem(FOnChangeListIgnoreUpdate, FOnChangeListCountIgnoreUpdate*SizeOf(TEpiChangeEvent));
  end;

  Idx := 0;
  while Idx <= FOnChangeListCount -1 do
  begin
    if FOnChangeList[Idx] = Event then
      break;
    Inc(Idx)
  end;
  if Idx = FOnChangeListCount then exit;

  dec(FOnChangeListCount);
  if FOnChangeListCount > Idx then
    System.Move(FOnChangeList[Idx+1],FOnChangeList[Idx],(FOnChangeListCount-Idx)*SizeOf(TEpiChangeEvent));
  ReAllocMem(FOnChangeList, FOnChangeListCount*SizeOf(TEpiChangeEvent));
end;

procedure TEpiCustomBase.SetLanguage(const LangCode: string;
  const DefaultLanguage: boolean);
var
  i: Integer;
begin
  {$IFDEF EPI_CONSOLE_DEBUG}
  writeln(ClassName,'.SetLanguage: ', LangCode, ' (', DefaultLanguage, ')');
  {$ENDIF EPI_CONSOLE_DEBUG}
  if DefaultLanguage then
    FDefaultLang := LangCode;
  FCurrentLang := LangCode;

  for i := 0 to ClassList.Count - 1 do
    TEpiCustomBase(ClassList[i]).SetLanguage(LangCode, DefaultLanguage);
end;

function TEpiCustomBase.GetRootOwner: TEpiCustomBase;
var
  Obj: TEpiCustomBase;
begin
  Obj := Self;
  while Assigned(Obj) do
  begin
    if not Assigned(Obj.Owner) then
      Exit(Obj);
    Obj := Obj.Owner;
  end;
end;

procedure TEpiCustomBase.SetModified(const AValue: Boolean);
var
  i: Integer;
begin
  if FModified = AValue then exit;
  FModified := AValue;
  if Assigned(FOnModified) then
    FOnModified(Self);

  if AValue then
  begin
    if Assigned(Owner) then
      Owner.Modified := AValue;
  end else begin
    for i := 0 to ClassList.Count - 1 do
      TEpiCustomBase(ClassList[i]).Modified := AValue;
  end;
end;

procedure TEpiCustomBase.SetOnModified(const AValue: TNotifyEvent);
begin
  if FOnModified = AValue then exit;
  FOnModified := AValue;
end;

{ TEpiTranslatedText }

procedure TEpiTranslatedText.SetCurrentText(const AValue: string);
var
  Val: String;
begin
  if FCurrentText = AValue then exit;
  Val := FCurrentText;
  SetText(FCurrentLang, AValue);
  FCurrentText := AValue;
  DoChange(eegCustomBase, Word(ecceText), @Val);
end;

procedure TEpiTranslatedText.SetLanguage(const LangCode: string;
  const DefaultLanguage: boolean);
var
  Idx:  integer;
begin
  // First seek "new" language
  if FTextList.Find(LangCode, Idx) then
    FCurrentText := TString(FTextList.Objects[Idx]).Str
  // Fallback to default language
  else if FTextList.Find(FDefaultLang, Idx) then
    FCurrentText := TString(FTextList.Objects[Idx]).Str
  // If new default language does not exists create empty entry.
  else if DefaultLanguage then
  begin
    SetText(LangCode, '');
    FCurrentText := '';
  end;
  inherited SetLanguage(LangCode, DefaultLanguage);
end;

function TEpiTranslatedText.XMLName: string;
begin
  Result := FXMLName;
end;

constructor TEpiTranslatedText.Create(AOwner: TEpiCustomBase;
  const aXMLName: string);
begin
  inherited Create(AOwner);
  FTextList := TStringList.Create;
  FTextList.Sorted := true;
  FXMLName := aXMLName
end;

destructor TEpiTranslatedText.Destroy;
var
  i: Integer;
begin
  FXMLName := '';
  FCurrentText := '';
  for i := FTextList.Count - 1 downto 0 do
    FTextList.Objects[i].Free;
  FTextList.Clear;
  FTextList.Free;
  inherited Destroy;
end;

function TEpiTranslatedText.SaveToXml(Content: String; Lvl: integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FTextList.Count - 1 do
    if TString(FTextList.Objects[i]).Str <> '' then
      Result += Indent(Lvl) + '<' + XMLName + ' xml:lang="' + FTextList[i] + '">' +
                StringToXml(TString(FTextList.Objects[i]).Str) + '</' + XMLName + '>' + LineEnding;
end;

procedure TEpiTranslatedText.LoadFromXml(Root: TDOMNode);
var
  ElemList: TDOMNodeList;
  LangCode: String;
  Val: String;
  i: Integer;
begin
  // Root = <Containing Section>
  { eg.
    <Group> - this is root.
      <Name xml:lang="en">Group A</Name>
      <Name xml:lang="dk">Gruppe A</Name>
      ...
    </Group>
  }
  ElemList := TDOMElement(Root).GetElementsByTagName(XMLName);
  for i := 0 to ElemList.Count -1 do
  begin
    // Ugly hack to prevent looking at nodes that is not directly benieth the root.
    if ElemList[i].ParentNode <> Root then continue;
    LangCode := UTF8Encode(TDOMElement(ElemList[i]).AttribStrings['xml:lang']);
    Val := UTF8Encode(ElemList[i].TextContent);
    if LangCode = FCurrentLang then
      SetCurrentText(Val)
    else
      SetText(LangCode, Val);
  end;
  ElemList.Free;
end;

procedure TEpiTranslatedText.SetText( const LangCode: string;
  const AText: string);
var
  Idx: integer;
begin
  if FTextList.Find(LangCode, Idx) then
    TString(FTextList.Objects[Idx]).Str := AText
  else
    FTextList.AddObject(LangCode, TString.Create(AText));
end;

function TEpiTranslatedText.GetText(const LangCode: string): string;
var
  Idx: integer;
begin
  if FTextList.Find(LangCode, Idx) then
    Result := TString(FTextList.Objects[Idx]).Str
  else begin
    FTextList.Find(FDefaultLang, Idx);
    Result := TString(FTextList.Objects[Idx]).Str;
  end;
end;

{ TEpiCustomItem }

function TEpiCustomItem.GetId: string;
begin
  result := FId;
end;

procedure TEpiCustomItem.SetId(const AValue: string);
var
  Val: String;
begin
  if FId = AValue then exit;
  Val := FId;
  FId := AValue;
  DoChange(eegCustomBase, Word(ecceId), @Val);
end;

destructor TEpiCustomItem.Destroy;
begin
  FId := '';
  inherited Destroy;
end;

procedure TEpiCustomItem.LoadFromXml(Root: TDOMNode);
begin
  Id := TDOMElement(Root).AttribStrings[rsId];
end;

{ TEpiCustomNamedItem }
{
constructor TEpiCustomNamedItem.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FName := TEpiTranslatedText.Create(Self, rsName);
  RegisterClasses([FName]);
end;

destructor TEpiCustomNamedItem.Destroy;
begin
  FName.Free;
  inherited Destroy;
end;

procedure TEpiCustomNamedItem.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
  FName.LoadFromXml(Root);
end;
}
{ TEpiCustomControlItem }

function TEpiCustomControlItem.SaveToXml(Content: String; Lvl: integer
  ): string;
begin
  Content :=
    SaveNode(Lvl + 1, rsTop, Top) +
    SaveNode(Lvl + 1, rsLeft, Left) +
    Content;
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiCustomControlItem.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
  Top := LoadNodeInt(Root, rsTop);
  Left := LoadNodeInt(Root, rsLeft);
end;

procedure TEpiCustomControlItem.SetLeft(const AValue: Integer);
var
  Val: LongInt;
begin
  if Left = AValue then exit;
  Val := Left;
  FLeft := AValue;
  DoChange(eegCustomBase, Word(ecceSetLeft), @Val);
end;

procedure TEpiCustomControlItem.SetTop(const AValue: Integer);
var
  Val: LongInt;
begin
  if Top = AValue then exit;
  Val := Top;
  FTop := AValue;
  DoChange(eegCustomBase, Word(ecceSetTop), @Val);
end;

{ TEpiCustomList }

procedure TEpiCustomList.SetItemOwner(const AValue: boolean);
begin
  if FItemOwner = AValue then exit;
  FItemOwner := AValue;
end;

procedure TEpiCustomList.OnChangeHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  EpiSender: TEpiCustomItem absolute Sender;
begin
  if EventGroup <> eegCustomBase then exit;

  case TEpiCustomChangeEventType(EventType) of
    ecceDestroy: RemoveItem(EpiSender);
  end;
end;

constructor TEpiCustomList.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FList := TFPList.Create;
end;

function TEpiCustomList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TEpiCustomList.GetItems(Index: integer): TEpiCustomItem;
begin
  result := TEpiCustomItem(FList[Index]);
end;

procedure TEpiCustomList.SetItems(Index: integer; const AValue: TEpiCustomItem
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

class function TEpiCustomList.IdString: string;
begin
  Result := '';
end;

function TEpiCustomList.GetUniqueItemId(AClass: TEpiCustomItemClass): string;
var
  i: Integer;
begin
  i := Count;
  repeat
    result := AClass.IdString + IntToStr(i);
    Inc(i);
  until (not ItemExistsById(result));
end;

destructor TEpiCustomList.Destroy;
var
  F: TEpiCustomItem;
begin
  while FList.Count > 0 do
  begin
    // Using this unusual construct in destroying list items (when owned)
    // ensures that destroy notifications from Items is defered until after
    // the item is removed from the list.
    F := TEpiCustomItem(FList.Last);
    // Deleting is faster than removing...
    FList.Delete(FList.Count - 1);
    if ItemOwner then
      FreeAndNil(F);
  end;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TEpiCustomList.SaveToXml(Content: String; Lvl: integer): string;
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

function TEpiCustomList.NewItem(ItemClass: TEpiCustomItemClass
  ): TEpiCustomItem;
begin
  if Assigned(OnNewItemClass) then
    ItemClass := OnNewItemClass(Self, ItemClass);
  if not Assigned(ItemClass) then
    Exception.Create('');
  Result := ItemClass.Create(Self);
  Result.Id := GetUniqueItemId(ItemClass);
  AddItem(Result);
end;

procedure TEpiCustomList.AddItem(Item: TEpiCustomItem);
begin
  if ItemOwner then Item.FOwner := Self;
  FList.Add(Item);
  Item.RegisterOnChangeHook(@OnChangeHook, true);

  if ItemOwner then
  begin
    Item.SetLanguage(FDefaultLang, true);
    Item.SetLanguage(FCurrentLang, false);
  end;

  DoChange(eegCustomBase, Word(ecceAddItem), Item);
end;

procedure TEpiCustomList.RemoveItem(Item: TEpiCustomItem);
begin
  FList.Remove(Item);
  Item.UnRegisterOnChangeHook(@OnChangeHook);
  if ItemOwner then Item.FOwner := nil;
  DoChange(eegCustomBase, Word(ecceDelItem), Item);
end;

function TEpiCustomList.DeleteItem(Index: integer): TEpiCustomItem;
begin
  Result := TEpiCustomItem(FList[Index]);
  FList.Delete(Index);
  Result.UnRegisterOnChangeHook(@OnChangeHook);
  if ItemOwner then Result.FOwner := nil;
  DoChange(eegCustomBase, Word(ecceDelItem), Result);
end;

function TEpiCustomList.GetItemById(aId: string): TEpiCustomItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if TEpiCustomItem(FList[i]).Id = aId then
    begin
      Result := TEpiCustomItem(FList[i]);
      Exit;
    end;
  end;
end;

function TEpiCustomList.ItemExistsById(aId: string): boolean;
begin
  result := Assigned(GetItemById(aId));
end;

function TEpiCustomList.IndexOf(Item: TEpiCustomItem): integer;
begin
  result := FList.IndexOf(Item);
end;

procedure TEpiCustomList.BeginUpdate;
var
  i: Integer;
begin
  {$IFDEF EPI_CONSOLE_DEBUG}
  writeln('TEpiCustomList.BeginUpdate: ' + ClassName);
  {$ENDIF EPI_CONSOLE_DEBUG}
  inherited BeginUpdate;
  for i := 0 to Count - 1 do
    Items[i].BeginUpdate;
end;

procedure TEpiCustomList.EndUpdate;
var
  i: Integer;
begin
  {$IFDEF EPI_CONSOLE_DEBUG}
  writeln('TEpiCustomList.EndUpdate: ' + ClassName);
  {$ENDIF EPI_CONSOLE_DEBUG}
  for i := 0 to Count - 1 do
    Items[i].EndUpdate;
  inherited EndUpdate;
end;

procedure TEpiCustomList.SetLanguage(const LangCode: string;
  const DefaultLanguage: boolean);
var
  i: Integer;
begin
  inherited SetLanguage(LangCode, DefaultLanguage);
  for i := 0 to Count - 1 do
    Items[i].SetLanguage(LangCode, DefaultLanguage);
end;

procedure TEpiCustomList.SetModified(const AValue: Boolean);
var
  i: Integer;
begin
  inherited SetModified(AValue);
  if not AValue then
    for i := 0 to Count - 1 do
      Items[i].Modified := AValue;
end;

end.

