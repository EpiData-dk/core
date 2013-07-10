unit epicustombase;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, DCPrijndael, epidatafilestypes, typinfo,
  contnrs, LazMethodList;

const
  EPI_XML_DATAFILE_VERSION = 3;

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
    eegValueLabel,
    eegValueLabelSet,
    // epirelations.pas
    eegRelates
    );
  // ecce = Epi Custom Change Event
  TEpiCustomChangeEventType = (
    ecceDestroy, ecceUpdate, ecceName, ecceAddItem, ecceDelItem, ecceSetItem,
    ecceSetTop, ecceSetLeft, ecceText
  );
  TEpiChangeEvent = procedure(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer) of object;

  TEpiCustomBaseState = set of (ebsDestroying, ebsUpdating);

  TEpiCoreException = class (Exception);

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
    function   NodePath(Const Root: TDOMNode): string;
    procedure  RaiseErrorNode(const Root: TDOMNode; const NodeName: string);
    procedure  RaiseErrorAttr(const Root: TDOMNode; const AttrName: string);
    procedure  RaiseErrorMsg(const Root: TDOMNode; const Msg: string);
  protected
    function   StringToXml(const S: string): string;
    Function   Indent(Level: integer): string;
    function   ScrambleXml: boolean; virtual;

    { Check methods }
    function   NodeIsWhiteSpace(Const Node: TDomNode): boolean;
    procedure  CheckNode(const Node: TDOMNode; const NodeName: string); virtual;
    { Load methods }
    function   LoadNode(out Node: TDOMNode; const Root: TDOMNode;
      const NodeName: string; Fatal: boolean): boolean;
    function   LoadAttr(out Attr: TDOMAttr; const Root: TDOMNode;
      const AttrName: string; Fatal: boolean): boolean; overload;
    // Direct loading of node are always fatal, since they must return some value.
    function   LoadNodeInt(const Root: TDOMNode; Const NodeName: string; DefaultVal: EpiInteger = 0; Fatal: boolean = true): EpiInteger;
    function   LoadNodeFloat(const Root: TDOMNode; Const NodeName: string; DefaultVal: EpiFloat = 0; Fatal: boolean = true): EpiFloat;
    function   LoadNodeString(const Root: TDOMNode; Const NodeName: string; DefaultVal: EpiString = ''; Fatal: boolean = true): EpiString;
    function   LoadNodeDateTime(const Root: TDOMNode; Const NodeName: string; DefaultVal: EpiDateTime = 0; Fatal: boolean = true): EpiDateTime;
    function   LoadNodeBool(const Root: TDOMNode; Const NodeName: string; DefaultVal: Boolean = false; Fatal: boolean = true): boolean;
    // Loading attributes
    function   LoadAttrInt(const Root: TDOMNode; Const AttrName: string; DefaultVal: EpiInteger = 0; Fatal: Boolean = true): EpiInteger;
    function   LoadAttrEnum(const Root: TDOMNode; Const AttrName: string; TypeInfo: PTypeInfo; DefaultVal: String = ''; Fatal: Boolean = true): integer;
    function   LoadAttrFloat(const Root: TDOMNode; Const AttrName: string; DefaultVal: EpiFloat = 0; Fatal: Boolean = true): EpiFloat;
    function   LoadAttrString(const Root: TDOMNode; Const AttrName: string; DefaultVal: EpiString = ''; Fatal: Boolean = true): EpiString;
    function   LoadAttrDateTime(const Root: TDOMNode; Const AttrName: string; Const Format: string = ''; DefaultVal: EpiDateTime = 0; Fatal: Boolean = true): EpiDateTime; overload;
    function   LoadAttrBool(const Root: TDOMNode; Const AttrName: string; DefaultVal: Boolean = false; Fatal: Boolean = true): boolean;
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

    // Attributes save
  private
    // - used internally by SaveAttr
    function   SaveAttrRaw(const AttrName: string; const Val: string): string;
  protected
    function   SaveAttr(const AttrName: string; const Val: string): string; overload;
    function   SaveAttr(const AttrName: string; const Val: integer): string; overload;
    function   SaveAttr(const AttrName: string; const Val: extended): string; overload;
    function   SaveAttr(const AttrName: string; const Val: TDateTime): string; overload;
    function   SaveAttr(const AttrName: string; const Val: boolean): string; overload;
    function   SaveAttrEnum(const AttrName: string; const Val: integer; TypeInfo: PTypeInfo): string;
    function   SaveAttributesToXml: string; virtual;
  public
    function   XMLName: string; virtual;
    function   SaveToXml(Content: String; Lvl: integer): string; virtual;
    procedure  LoadFromXml(Root: TDOMNode); virtual;

    { Change-event hooks }
  private
    FOnChangeList: TMethodList;
    FOnChangeListIgnoreUpdate: TMethodList;
    FUpdateCount: Integer;
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
    property    CurrentLang: string read FCurrentLang;
    property    DefaultLang: string read FDefaultLang;

  { Class properties / inheritance }
  private
    FClassList: TFPList;
    FModified:  Boolean;
    FObjectData: PtrUInt;
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
    procedure   Assign(Const AEpiCustomBase: TEpiCustomBase); virtual;
    property    Owner: TEpiCustomBase read FOwner;
    property    RootOwner: TEpiCustomBase read GetRootOwner;
    property    State: TEpiCustomBaseState read FState;
    property    Modified: Boolean read FModified write SetModified;
    property    OnModified: TNotifyEvent read FOnModified write SetOnModified;

  { Cloning }
  protected
    // DoCloneCreate should only be overridden if a special constructor is used.
    function    DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase; virtual;
    // DoClone should normally be overridden, since this is where data should be copied.
    function    DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase = nil): TEpiCustomBase; virtual;
  public
    function    Clone: TEpiCustomBase;
    function    Clone(AOwner: TEpiCustomBase): TEpiCustomBase;
  end;
  {$static off}
  TEpiCustomBaseClass = class of TEpiCustomBase;

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
    constructor Create(AOwner: TEpiCustomBase; Const aXMLName: string); virtual;
    destructor  Destroy; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property    Text: string read FCurrentText write SetCurrentText;
    property    TextLang[LangCode: string]: string read GetText write SetText;
  { Cloning }
  protected
    function DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  end;

  { TEpiTranslatedTextWrapper }

  TEpiTranslatedTextWrapper = class(TEpiTranslatedText)
  private
    FNodeName: string;
  public
    constructor Create(AOwner: TEpiCustomBase; Const NodeName, TextName: string);
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
  { Cloning }
  protected
    function DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase; override;
  end;

  { TEpiCustomItem }

  TEpiCustomItem = class(TEpiCustomBase)
  protected
    FName: string;
    function    GetName: string; virtual;
    procedure   SetName(const AValue: string); virtual;
    function    SaveAttributesToXml: string; override;
    function    DoValidateRename(Const NewName: string): boolean; virtual;
    function    WriteNameToXml: boolean; virtual;
  public
    destructor  Destroy; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    ValidateRename(Const NewName: string; RenameOnSuccess: boolean): boolean; virtual;
    property    Name: string read GetName write SetName;
  {Cloning}
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  { CustomData }
  // CustomData is a custom property that can be used freely to store some data
  // along with the object. User added content is NEVER used by the internals of
  // Core.
  // In addition data will not be copied/assigned/freed etc., hence it is
  // entirely up to the user to keep track of it's use throught a program.
  private
    FCustomData: TFPObjectHashTable;
  public
    procedure AddCustomData(const Key: string; Const Obj: TObject);
    function  FindCustomData(const Key: string): TObject;
    function  RemoveCustomData(Const Key: string): TObject;
  end;
  TEpiCustomItemClass = class of TEpiCustomItem;

  { TEpiCustomControlItem }

  TEpiCustomControlItem = class(TEpiCustomItem)
  private
    FLeft: integer;
    FTop: integer;
  protected
    function   SaveAttributesToXml: string; override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    procedure  SetLeft(const AValue: Integer); virtual;
    procedure  SetTop(const AValue: Integer); virtual;
    procedure  Assign(const AEpiCustomBase: TEpiCustomBase); override;
  public
    property   Left: Integer read FLeft write SetLeft;
    property   Top: Integer read FTop write SetTop;
  {Cloning}
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  end;
  TEpiCustomControlItemClass = class of TEpiCustomControlItem;

  { TEpiCustomList }

  TEpiOnNewItemClass      = function(Sender: TEpiCustomList; DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass of object;
  TEpiValidateRenameEvent = function(Const NewName: string): boolean of object;
  TEpiPrefixEvent         = function: string of object;
  TEpiCustomList = class(TEpiCustomItem)
  private
    FItemOwner: boolean;
    FList: TFPList;
    procedure   SetItemOwner(const AValue: boolean);
    procedure   OnChangeHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure   RegisterItem(Item: TEpiCustomItem);
    procedure   UnRegisterItem(Item: TEpiCustomItem);
  protected
    constructor Create(AOwner: TEpiCustomBase); override;
    function    GetCount: Integer; virtual;
    function    GetItems(Index: integer): TEpiCustomItem; virtual;
    procedure   SetItems(Index: integer; const AValue: TEpiCustomItem); virtual;
    function    WriteNameToXml: boolean; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    property    List: TFPList read FList;
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
    property    Count: Integer read GetCount;
    property    Items[Index: integer]: TEpiCustomItem read GetItems write SetItems; default;
    property    ItemOwner: boolean read FItemOwner write SetItemOwner;
  { Naming and Validation }
  private
    FOnGetPrefix: TEpiPrefixEvent;
    FOnValidateRename: TEpiValidateRenameEvent;
    function  DoPrefix: string;
  protected
    function  Prefix: string; virtual;
  public
    function  GetUniqueItemName(AClass: TEpiCustomItemClass): string; virtual;
    function  ValidateRename(Const NewName: string; RenameOnSuccess: boolean): boolean; override;
    property  OnValidateRename: TEpiValidateRenameEvent read FOnValidateRename write FOnValidateRename;
    property  OnGetPrefix: TEpiPrefixEvent read FOnGetPrefix write FOnGetPrefix;
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
    procedure DoAssignList(Const EpiCustomList: TEpiCustomList); virtual;
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
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase =
       nil): TEpiCustomBase; override;
  end;

  { TEpiCustomControlItemList }

  TEpiCustomControlItemList = class(TEpiCustomList)
  private
    procedure ChangeHook(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
  protected
    procedure DoSort; override;
  public
    procedure InsertItem(const Index: integer; Item: TEpiCustomItem); override;
    function  DeleteItem(Index: integer): TEpiCustomItem; override;
  end;

{$I epixmlconstants.inc}

procedure BackupFormatSettings(NewFormatSettings: TFormatSettings); overload;
procedure BackupFormatSettings; overload;
procedure RestoreFormatSettings;

implementation

uses
  StrUtils, DCPsha256, XMLRead, epistringutils, episettings, epidocument,
  epidatafiles, androidutils;

var
  BackupDefaultFormatSettings: TFormatSettings;

procedure BackupFormatSettings(NewFormatSettings: TFormatSettings);
begin
  BackupDefaultFormatSettings := DefaultFormatSettings;
  DefaultFormatSettings := NewFormatSettings;
end;

procedure BackupFormatSettings;
begin
  BackupDefaultFormatSettings := DefaultFormatSettings;
end;

procedure RestoreFormatSettings;
begin
  DefaultFormatSettings := BackupDefaultFormatSettings;
end;

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
  Include(FState, ebsDestroying);
  DoChange(eegCustomBase, Word(ecceDestroy), nil);

  FClassList.Free;
  Freemem(FOnChangeList);
  Freemem(FOnChangeListIgnoreUpdate);
  inherited Destroy;
end;

procedure TEpiCustomBase.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgBase: TEpiCustomBase absolute AEpiCustomBase;
  i: Integer;
begin
  BeginUpdate;
  for i := 0 to OrgBase.ClassList.Count - 1 do
    TEpiCustomBase(ClassList[i]).Assign(TEpiCustomBase(OrgBase.ClassList[i]));
  EndUpdate;
end;

function TEpiCustomBase.DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase;
begin
  result := TEpiCustomBaseClass(Self.ClassType).Create(AOwner);
end;

function TEpiCustomBase.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
var
  OrgClass: TEpiCustomBase;
  NewClass: TEpiCustomBase;
  i: Integer;
begin
  Result := Dest;
  if not Assigned(Result) then
    Result := DoCloneCreate(AOwner);

  // TODO: OnChangeEvents - Copy or Not?

  for i := 0 to Result.ClassList.Count - 1 do
  begin
    NewClass := TEpiCustomBase(Result.ClassList[i]);
    OrgClass := TEpiCustomBase(ClassList[i]);

    OrgClass.DoClone(Result, NewClass);
  end;
end;

function TEpiCustomBase.Clone: TEpiCustomBase;
begin
  Result := DoClone(nil);
end;

function TEpiCustomBase.Clone(AOwner: TEpiCustomBase): TEpiCustomBase;
begin
  Result := DoClone(AOwner);
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

function TEpiCustomBase.NodePath(const Root: TDOMNode): string;
var
  R: TDOMNode;

  function GetId(Const Node: TDOMNode): string;
  begin
    Result := '';
    if (Node.HasAttributes) and (TDOMElement(Node).AttribStrings['id'] <> '') then
      Result := Result + '(' + TDOMElement(Node).AttribStrings['id'] + ')';
  end;

begin
  Result := Root.NodeName + GetId(Root);
  R := Root.ParentNode;
  while Assigned(R) do
  begin
    Result := R.NodeName + GetId(R) +  '->' + Result;
    R := R.ParentNode;
  end;
end;

procedure TEpiCustomBase.RaiseErrorNode(const Root: TDOMNode; const NodeName: string) ;
begin
  raise TEpiCoreException.Create('ERROR: A required XML tag was not found.' + LineEnding +
          Format('In section %s the tag "%s" was expected!', [NodePath(Root), NodeName]));
end;

procedure TEpiCustomBase.RaiseErrorAttr(const Root: TDOMNode; const AttrName: string
  );
begin
  raise TEpiCoreException.Create('ERROR: A required XML attribute was not found.' + LineEnding +
          Format('The tag %s expected an attribute with the name "%s"!', [NodePath(Root), AttrName]));
end;

procedure TEpiCustomBase.RaiseErrorMsg(const Root: TDOMNode; const Msg: string);
begin
  raise TEpiCoreException.CreateFmt('ERROR: An error occured reading tag "%s".' + LineEnding +
          '%s', [Root.NodeName, Msg]);
end;

function TEpiCustomBase.StringToXml(const S: string): string;
var
  i: Integer;
  T: string;
begin
  T := S;
  for i := 1 to Length(T) do
    if (Ord(T[i]) < 32) and
       (not (Ord(T[i]) in [10,13])) then
      T[i] := '?';
  result := StringsReplace(T,
   ['&',     '"',      '<',    '>',    ''''],
   ['&amp;', '&quot;', '&lt;', '&gt;', '&apos;'],
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

function TEpiCustomBase.NodeIsWhiteSpace(const Node: TDomNode): boolean;
var
  P: PWideChar;
begin
  result := false;
  if (Assigned(Node)) and
     (Node.NodeType = TEXT_NODE) then
  begin
    P := @Node.NodeValue[1];
    while P^ in [#10, #13, #32] do inc(p);
    result := (P^ = #0);
  end;
end;

procedure TEpiCustomBase.CheckNode(const Node: TDOMNode; const NodeName: string
  );
begin
  if Node.CompareName(NodeName) <> 0 then
    RaiseErrorNode(Node, NodeName);
end;

function TEpiCustomBase.LoadNode(out Node: TDOMNode; const Root: TDOMNode;
  const NodeName: string; Fatal: boolean): boolean;
begin
  result := true;

  Node := Root.FindNode(NodeName);
  if Assigned(Node) then exit;

  result := false;
  if not Fatal then exit;

  RaiseErrorNode(Root, NodeName);
end;

function TEpiCustomBase.LoadAttr(out Attr: TDOMAttr; const Root: TDOMNode;
  const AttrName: string; Fatal: boolean): boolean;
begin
  result := true;

  if not (Root is TDOMElement) then
    RaiseErrorMsg(Root, 'Root node is NOT a TDomElement. Please abort program!');

  Attr := TDOMElement(Root).GetAttributeNode(AttrName);
  if Assigned(Attr) then exit;

  result := false;
  if not Fatal then exit;

  RaiseErrorAttr(Root, AttrName);
end;

function TEpiCustomBase.LoadNodeInt(const Root: TDOMNode;
  const NodeName: string; DefaultVal: EpiInteger; Fatal: boolean): EpiInteger;
var
  Node: TDOMNode;
begin
  if LoadNode(Node, Root, NodeName, Fatal) then
    result := StrToInt64(Node.TextContent)
  else
    result := DefaultVal;
end;

function TEpiCustomBase.LoadNodeFloat(const Root: TDOMNode;
  const NodeName: string; DefaultVal: EpiFloat; Fatal: boolean): EpiFloat;
var
  Node: TDOMNode;
begin
  if LoadNode(Node, Root, NodeName, Fatal) then
  begin
    if (RootOwner is TEpiDocument) then
      BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
    result := StrToFloat(Node.TextContent);
    RestoreFormatSettings;
  end else
    result := DefaultVal;
end;

function TEpiCustomBase.LoadNodeString(const Root: TDOMNode;
  const NodeName: string; DefaultVal: EpiString; Fatal: boolean): EpiString;
var
  Node: TDOMNode;
begin
  if LoadNode(Node, Root, NodeName, Fatal) then
    result := UTF8Encode(Node.TextContent)
  else
    result := DefaultVal;
end;

function TEpiCustomBase.LoadNodeDateTime(const Root: TDOMNode;
  const NodeName: string; DefaultVal: EpiDateTime; Fatal: boolean): EpiDateTime;
var
  Node: TDOMNode;
begin
  if LoadNode(Node, Root, NodeName, Fatal) then
  begin
    if (RootOwner is TEpiDocument) then
      BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
    result := StrToDateTime(Node.TextContent);
    RestoreFormatSettings;
  end else
    result := DefaultVal;
end;

function TEpiCustomBase.LoadNodeBool(const Root: TDOMNode;
  const NodeName: string; DefaultVal: Boolean; Fatal: boolean): boolean;
var
  Node: TDOMNode;
begin
  if LoadNode(Node, Root, NodeName, Fatal) then
    result := WideLowerCase(Node.TextContent) = 'true'
  else
    result := DefaultVal;
end;

function TEpiCustomBase.LoadAttrInt(const Root: TDOMNode;
  const AttrName: string; DefaultVal: EpiInteger; Fatal: Boolean): EpiInteger;
var
  Attr: TDOMAttr;
begin
  ALogInfo('TEpiCustomBase.LoadAttrInt (1)');
  if LoadAttr(Attr, Root, AttrName, Fatal) then
    Result := StrToInt64(Attr.Value)
  else
    Result := DefaultVal;
end;

function TEpiCustomBase.LoadAttrEnum(const Root: TDOMNode;
  const AttrName: string; TypeInfo: PTypeInfo; DefaultVal: String;
  Fatal: Boolean): integer;
begin
  result := GetEnumValue(TypeInfo, LoadAttrString(Root, AttrName, DefaultVal, Fatal));
end;

function TEpiCustomBase.LoadAttrFloat(const Root: TDOMNode;
  const AttrName: string; DefaultVal: EpiFloat; Fatal: Boolean): EpiFloat;
var
  Attr: TDOMAttr;
begin
  if LoadAttr(Attr, Root, AttrName, Fatal) then
  begin
    if (RootOwner is TEpiDocument) then
      BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
    result := StrToFloat(Attr.Value);
    RestoreFormatSettings;
  end else
    Result := DefaultVal;
end;

function TEpiCustomBase.LoadAttrString(const Root: TDOMNode;
  const AttrName: string; DefaultVal: EpiString; Fatal: Boolean): EpiString;
var
  Attr: TDOMAttr;
begin
  if LoadAttr(Attr, Root, AttrName, Fatal) then
    Result := UTF8Encode(Attr.Value)
  else
    Result := DefaultVal;
end;

function TEpiCustomBase.LoadAttrDateTime(const Root: TDOMNode;
  const AttrName: string; const Format: string; DefaultVal: EpiDateTime;
  Fatal: Boolean): EpiDateTime;
var
  Attr: TDOMAttr;
begin
  if LoadAttr(Attr, Root, AttrName, Fatal) then
  begin
    if (RootOwner is TEpiDocument) then
      BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
    if Format <> '' then
      DefaultFormatSettings.ShortDateFormat := Format;
    Result := StrToDateTime(Attr.Value);
    if (RootOwner is TEpiDocument) then
      RestoreFormatSettings;
  end else
    Result := DefaultVal;
end;

function TEpiCustomBase.LoadAttrBool(const Root: TDOMNode;
  const AttrName: string; DefaultVal: Boolean; Fatal: Boolean): boolean;
var
  Attr: TDOMAttr;
begin
  if LoadAttr(Attr, Root, AttrName, Fatal) then
    result := WideLowerCase(Attr.Value) = 'true'
  else
    Result := DefaultVal;
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
  if (RootOwner is TEpiDocument) then
    BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
  result := SaveNode(Lvl, NodeName, FloatToStr(Val));
  RestoreFormatSettings;
end;

function TEpiCustomBase.SaveNode(const Lvl: integer; const NodeName: string;
  const Val: TDateTime): string;
begin
  if (RootOwner is TEpiDocument) then
  with TEpiDocument(RootOwner).XMLSettings do
  begin
    BackupFormatSettings(FormatSettings);
    result := SaveNode(Lvl, NodeName, FormatDateTime(FormatSettings.ShortDateFormat, Val));
    RestoreFormatSettings;
  end else
    result := SaveNode(Lvl, NodeName, FormatDateTime('YYYY/MM/DD HH:NN:SS', Val));
end;

function TEpiCustomBase.SaveNode(const Lvl: integer; const NodeName: string;
  const Val: boolean): string;
begin
  result := SaveNode(Lvl, NodeName, BoolToStr(Val, 'true', 'false'));
end;

// This function writes the raw content of Val and AttrName without further
// checking. If val need to be XML'ified it should go through SaveAttr(... val: string)
function TEpiCustomBase.SaveAttrRaw(const AttrName: string; const Val: string
  ): string;
begin
  result := Format(' %s="%s"', [AttrName, Val]);
end;

function TEpiCustomBase.SaveAttr(const AttrName: string; const Val: string
  ): string;
begin
  // Check that Val is not malformed (according to XML standard).
  result := SaveAttrRaw(AttrName, StringToXml(Val));
end;

function TEpiCustomBase.SaveAttr(const AttrName: string; const Val: integer
  ): string;
begin
  result := SaveAttrRaw(AttrName, IntToStr(Val));
end;

function TEpiCustomBase.SaveAttr(const AttrName: string; const Val: extended
  ): string;
begin
  result := SaveAttrRaw(AttrName, FloatToStr(Val));
end;

function TEpiCustomBase.SaveAttr(const AttrName: string; const Val: TDateTime
  ): string;
begin
  if (RootOwner is TEpiDocument) then
  with TEpiDocument(RootOwner).XMLSettings do
  begin
    BackupFormatSettings(FormatSettings);
    result := SaveAttr(AttrName, FormatDateTime(FormatSettings.ShortDateFormat, Val));
    RestoreFormatSettings;
  end else
    result := SaveAttrRaw(AttrName, FormatDateTime('YYYY/MM/DD HH:NN:SS', Val));
end;

function TEpiCustomBase.SaveAttr(const AttrName: string; const Val: boolean
  ): string;
begin
  result := SaveAttrRaw(AttrName, BoolToStr(Val, 'true', 'false'));
end;

function TEpiCustomBase.SaveAttrEnum(const AttrName: string;
  const Val: integer; TypeInfo: PTypeInfo): string;
begin
  result := SaveAttr(AttrName, GetEnumName(TypeInfo, Val));
end;

function TEpiCustomBase.SaveAttributesToXml: string;
begin
  result := '';
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

//  if ScrambleXml then
    //S := EnCrypt(S) + LineEnding;

  if S <> '' then
    Result :=
      Indent(Lvl) + '<' + XMLName + SaveAttributesToXml + '>' + LineEnding +
      S +
      Indent(Lvl) + '</' + XMLName + '>' + LineEnding
  else
    Result :=
      Indent(Lvl) + '<' + XMLName + SaveAttributesToXml + '/>' + LineEnding;
end;

procedure TEpiCustomBase.LoadFromXml(Root: TDOMNode);
begin
  // Do nothing - should be overridden in descendants.
end;

procedure TEpiCustomBase.DoChange(EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  i: Integer;
begin
  I := FOnChangeListIgnoreUpdate.Count;
  while FOnChangeListIgnoreUpdate.NextDownIndex(I) do
    TEpiChangeEvent(FOnChangeListIgnoreUpdate.Items[I])(Self, EventGroup, EventType, Data);

  if ((EventGroup = eegCustomBase) and (EventType <> Word(ecceUpdate))) or
     (EventGroup <> eegCustomBase)
  then
    Modified := true;

  if FUpdateCount > 0 then exit;

  I := FOnChangeList.Count;
  while FOnChangeList.NextDownIndex(I) do
    TEpiChangeEvent(FOnChangeList.Items[I])(Self, EventGroup, EventType, Data);
end;

procedure TEpiCustomBase.BeginUpdate;
var
  i: Integer;
begin
  Inc(FUpdateCount);

  for i := 0 to ClassList.Count - 1 do
    TEpiCustomBase(ClassList[i]).BeginUpdate;
end;

procedure TEpiCustomBase.EndUpdate;
var
  i: Integer;
begin
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
    if not Assigned(FOnChangeListIgnoreUpdate) then
      FOnChangeListIgnoreUpdate := TMethodList.Create;
    FOnChangeListIgnoreUpdate.Add(TMethod(Event), false);
  end else begin
    if not Assigned(FOnChangeList) then
      FOnChangeList := TMethodList.Create;
    FOnChangeList.Add(TMethod(Event), false);
  end;
end;

procedure TEpiCustomBase.UnRegisterOnChangeHook(Event: TEpiChangeEvent);
var
  Idx: LongInt;
begin
  FOnChangeListIgnoreUpdate.Remove(TMethod(Event));
  if FOnChangeListIgnoreUpdate.Count = 0 then
    FreeAndNil(FOnChangeListIgnoreUpdate);

  FOnChangeList.Remove(TMethod(Event));
  if FOnChangeList.Count = 0 then
    FreeAndNil(FOnChangeList);
end;

procedure TEpiCustomBase.SetLanguage(const LangCode: string;
  const DefaultLanguage: boolean);
var
  i: Integer;
begin
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
  if Assigned(Owner) then
    Owner.DoChange(eegCustomBase, Word(ecceText), @Val);
end;

procedure TEpiTranslatedText.SetLanguage(const LangCode: string;
  const DefaultLanguage: boolean);
var
  Idx:  integer;
  Val: String;
begin
  // First seek "new" language
  if FTextList.Find(LangCode, Idx) then
  begin
    Val := FCurrentText;
    FCurrentText := TString(FTextList.Objects[Idx]).Str;
    if Assigned(Owner) then
      Owner.DoChange(eegCustomBase, Word(ecceText), @Val);
  end
  // Fallback to default language
  else if (FTextList.Find(FDefaultLang, Idx)) and (not DefaultLanguage) then
  begin
    Val := FCurrentText;
    FCurrentText := TString(FTextList.Objects[Idx]).Str;
    if Assigned(Owner) then
      Owner.DoChange(eegCustomBase, Word(ecceText), @Val);
  end
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
    if (LangCode = FCurrentLang) or (LangCode = '') then
      SetCurrentText(Val)
    else
      SetText(LangCode, Val);
  end;
  ElemList.Free;
end;

procedure TEpiTranslatedText.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgText: TEpiTranslatedText absolute AEpiCustomBase;
  i: Integer;
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  FXMLName := OrgText.FXMLName;
  FCurrentText := OrgText.FCurrentText;
  for i := 0 to OrgText.FTextList.Count - 1 do
    FTextList.AddObject(OrgText.FTextList[i], TString.Create(TString(OrgText.FTextList.Objects[i]).Str));
  EndUpdate;
end;

function TEpiTranslatedText.DoCloneCreate(AOwner: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := TEpiTranslatedText.Create(AOwner, XMLName);
end;

function TEpiTranslatedText.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
var
  i: Integer;
begin
  Result := inherited DoClone(AOwner, Dest);
  with TEpiTranslatedText(Result) do
  begin
    FTextList.Clear;
    for i := 0 to Self.FTextList.Count - 1 do
      FTextList.AddObject(Self.FTextList[i], TString.Create(TString(Self.FTextList.Objects[i]).Str));

    FCurrentText := Self.FCurrentText;
  end;
end;

procedure TEpiTranslatedText.SetText( const LangCode: string;
  const AText: string);
var
  Idx: integer;
  Val: String;
begin
  Val := '';
  if FTextList.Find(LangCode, Idx) then
  begin
    Val := TString(FTextList.Objects[Idx]).Str;
    TString(FTextList.Objects[Idx]).Str := AText;
  end else
    FTextList.AddObject(LangCode, TString.Create(AText));
  if Assigned(Owner) then
    Owner.DoChange(eegCustomBase, Word(ecceText), @Val);
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

{ TEpiTranslatedTextWrapper }

constructor TEpiTranslatedTextWrapper.Create(AOwner: TEpiCustomBase;
  const NodeName, TextName: string);
begin
  inherited Create(AOwner, TextName);
  FNodeName := NodeName;
end;

function TEpiTranslatedTextWrapper.SaveToXml(Content: String; Lvl: integer
  ): string;
begin
  Result := inherited SaveToXml(Content, Lvl + 1);

  if Result <> '' then
    Result :=
      Indent(Lvl) + '<' + FNodeName + '>' + LineEnding +
      Result +
      Indent(Lvl) + '</' + FNodeName + '>' + LineEnding;
end;

procedure TEpiTranslatedTextWrapper.LoadFromXml(Root: TDOMNode);
var
  NRoot: TDOMNode;
begin
  // Root = Parent for FNodeName (since this is a wrapped object.
  if LoadNode(NRoot, Root, FNodeName, false) then
    inherited LoadFromXml(NRoot);
end;

function TEpiTranslatedTextWrapper.DoCloneCreate(AOwner: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := TEpiTranslatedTextWrapper.Create(AOwner, FNodeName, XMLName);
end;

{ TEpiCustomItem }

function TEpiCustomItem.GetName: string;
begin
  result := FName;
end;

procedure TEpiCustomItem.SetName(const AValue: string);
var
  Val: String;
begin
  if FName = AValue then exit;

  // Validate identifier
  if not DoValidateRename(AValue) then Exit;

  Val := FName;
  FName := AValue;
  DoChange(eegCustomBase, Word(ecceName), @Val);
end;

function TEpiCustomItem.SaveAttributesToXml: string;
begin
  Result := inherited SaveAttributesToXml;
  if WriteNameToXml then
    // We use name in program but present it as "id=...." in the XML.
    Result += SaveAttr(rsId, Name);
end;

function TEpiCustomItem.DoValidateRename(const NewName: string): boolean;
begin
  result := ValidateIdentifierUTF8(NewName);
  if (Owner is TEpiCustomList) then
    result := result and (TEpiCustomList(Owner).ValidateRename(NewName, false));
end;

function TEpiCustomItem.WriteNameToXml: boolean;
begin
  result := true;
end;

destructor TEpiCustomItem.Destroy;
begin
  FName := '';
  if Assigned(FCustomData) then
    FreeAndNil(FCustomData);
  inherited Destroy;
end;

procedure TEpiCustomItem.LoadFromXml(Root: TDOMNode);
var
  Attr: TDOMAttr;
begin
  if LoadAttr(Attr, Root, rsId, false) then
    FName := LoadAttrString(Root, rsId)
  else if WriteNameToXml then
    // This class was supposed to write an ID -> hence it also expects one! Error!
    RaiseErrorAttr(Root, rsId);
end;

function TEpiCustomItem.ValidateRename(const NewName: string;
  RenameOnSuccess: boolean): boolean;
begin
  if NewName = Name then exit(true);
  result := DoValidateRename(NewName);
end;

function TEpiCustomItem.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  TEpiCustomItem(Result).FName := FName;
end;

procedure TEpiCustomItem.AddCustomData(const Key: string; const Obj: TObject);
begin
  if not Assigned(FCustomData) then
    FCustomData := TFPObjectHashTable.Create(false);

  try
    FCustomData.Items[Key] := Obj;
  except
    raise Exception.Create('TEpiCustomItem: Duplicate CustomData - key=' + Key);
  end;
end;

function TEpiCustomItem.FindCustomData(const Key: string): TObject;
begin
  result := nil;
  if Assigned(FCustomData) then
    result := FCustomData.Items[Key];
end;

function TEpiCustomItem.RemoveCustomData(const Key: string): TObject;
begin
  Result := FindCustomData(Key);
  if not Assigned(Result) then exit;

  FCustomData.Delete(Key);

  if FCustomData.Count = 0 then
    FreeAndNil(FCustomData);
end;

{ TEpiCustomControlItem }

function TEpiCustomControlItem.SaveAttributesToXml: string;
begin
  Result :=
    inherited SaveAttributesToXml +
    SaveAttr(rsTop, Top) +
    SaveAttr(rsLeft, Left);
end;

procedure TEpiCustomControlItem.LoadFromXml(Root: TDOMNode);
begin
  inherited LoadFromXml(Root);
  Top := LoadAttrInt(Root, rsTop);
  Left := LoadAttrInt(Root, rsLeft);
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

procedure TEpiCustomControlItem.Assign(const AEpiCustomBase: TEpiCustomBase);
var
  OrgControlItem: TEpiCustomControlItem absolute AEpiCustomBase;
begin
  inherited Assign(AEpiCustomBase);
  BeginUpdate;
  if Top = 0 then
    Top := OrgControlItem.Top;
  if Left = 0 then
    Left := OrgControlItem.Left;
  EndUpdate;
end;

function TEpiCustomControlItem.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest);
  with TEpiCustomControlItem(Result) do
  begin
    FTop  := Self.FTop;
    FLeft := Self.FLeft;
  end;
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
    ecceAddItem: Sort;
    ecceDelItem: Sort;
    ecceUpdate:  Sort;
  end;
end;

procedure TEpiCustomList.RegisterItem(Item: TEpiCustomItem);
begin
  if ItemOwner then Item.FOwner := Self;
  Item.RegisterOnChangeHook(@OnChangeHook, true);

  if ItemOwner then
  begin
    Item.SetLanguage(FDefaultLang, true);
    Item.SetLanguage(FCurrentLang, false);
  end;

  DoChange(eegCustomBase, Word(ecceAddItem), Item);
  if Sorted then Sort;
end;

procedure TEpiCustomList.UnRegisterItem(Item: TEpiCustomItem);
begin
//  if not (ebsDestroying in Item.State) then
  Item.UnRegisterOnChangeHook(@OnChangeHook);

  if ItemOwner then Item.FOwner := nil;
  DoChange(eegCustomBase, Word(ecceDelItem), Item);
  if Sorted then Sort;
end;

constructor TEpiCustomList.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FList := TFPList.Create;
  FItemOwner := false;
  FSorted := false;
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

function TEpiCustomList.WriteNameToXml: boolean;
begin
  Result := false;
end;

procedure TEpiCustomList.LoadFromXml(Root: TDOMNode);
var
  NItem: TEpiCustomItem;
  Node: TDOMNode;
begin
  inherited LoadFromXml(Root);

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    // hack to skip whitespace nodes.
    while NodeIsWhiteSpace(Node) do
      Node := Node.NextSibling;
    if not Assigned(Node) then break;

    NItem := NewItem();
    CheckNode(Node, NItem.XMLName);
    NItem.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;
end;

function TEpiCustomList.GetUniqueItemName(AClass: TEpiCustomItemClass): string;
var
  i: Integer;
begin
  i := Count + 1;
  repeat
    result := DoPrefix + IntToStr(i);
    Inc(i);
  until ValidateRename(result, false);
end;

function TEpiCustomList.DoPrefix: string;
begin
  if Assigned(OnGetPrefix) then
    result := OnGetPrefix()
  else
    result := Prefix;
end;

function TEpiCustomList.Prefix: string;
begin
  result := 'id_';
end;

function TEpiCustomList.ValidateRename(const NewName: string;
  RenameOnSuccess: boolean): boolean;
begin
  result := not ItemExistsByName(NewName);
  if Assigned(OnValidateRename) then
    result := result and OnValidateRename(NewName);
end;

destructor TEpiCustomList.Destroy;
var
  F: TEpiCustomItem;
begin
  ClearAndFree;
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

procedure TEpiCustomList.Clear;
begin
  while Count > 0 do
    DeleteItem(Count - 1);
end;

procedure TEpiCustomList.ClearAndFree;
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
      FreeAndNil(F);
  end;
end;

function TEpiCustomList.ItemClass: TEpiCustomItemClass;
begin
  result := nil;
end;

function TEpiCustomList.NewItem(AItemClass: TEpiCustomItemClass
  ): TEpiCustomItem;
begin
  if not Assigned(AItemClass) then
    AItemClass := ItemClass;

  if Assigned(OnNewItemClass) then
    AItemClass := OnNewItemClass(Self, AItemClass);

  if not Assigned(AItemClass) then
    Exception.Create('TEpiCustomList: No ItemClass Defined!');

  Result := AItemClass.Create(Self);
  Result.Name := GetUniqueItemName(AItemClass);
  AddItem(Result);
end;

procedure TEpiCustomList.AddItem(Item: TEpiCustomItem);
begin
  // AddItem uses InsertItem internally, so only that method need overriding if needed.
  InsertItem(Count, Item);
end;

procedure TEpiCustomList.InsertItem(const Index: integer; Item: TEpiCustomItem
  );
begin
  if (not ValidateRename(Item.Name, false)) then
    raise TEpiCoreException.Create('Item "' + Item.Name + '" already exist in list');
  FList.Insert(Index, Item);
  RegisterItem(Item);
end;

procedure TEpiCustomList.RemoveItem(Item: TEpiCustomItem);
begin
  // RemoveItem uses DeleteItem internally, so only that method need overriding if needed.
  DeleteItem(FList.IndexOf(Item));
end;

function TEpiCustomList.DeleteItem(Index: integer): TEpiCustomItem;
begin
  Result := TEpiCustomItem(FList[Index]);
  FList.Delete(Index);
  UnRegisterItem(Result);
end;

function TEpiCustomList.GetItemByName(AName: string): TEpiCustomItem;
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

function TEpiCustomList.ItemExistsByName(AName: string): boolean;
begin
  result := Assigned(GetItemByName(AName));
end;

function TEpiCustomList.IndexOf(Item: TEpiCustomItem): integer;
begin
  result := FList.IndexOf(Item);
end;

procedure TEpiCustomList.BeginUpdate;
var
  i: Integer;
begin
  inherited BeginUpdate;
  for i := 0 to Count - 1 do
    Items[i].BeginUpdate;
end;

procedure TEpiCustomList.EndUpdate;
var
  i: Integer;
begin
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

procedure TEpiCustomList.DoAssignList(const EpiCustomList: TEpiCustomList);
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

procedure TEpiCustomList.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);

  if AEpiCustomBase is TEpiCustomList then
    DoAssignList(TEpiCustomList(AEpiCustomBase));
end;

procedure TEpiCustomList.SetSorted(AValue: boolean);
begin
  if FSorted = AValue then Exit;
  FSorted := AValue;
end;

procedure TEpiCustomList.DoSort;
begin
  if not Sorted then exit;

  if Assigned(FOnSort) then
    FList.Sort(FOnSort)
end;

procedure TEpiCustomList.Sort;
begin
  DoSort;
end;

function TEpiCustomList.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase
  ): TEpiCustomBase;
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
  Result := inherited DoClone(AOwner, Dest);

  // Set itemowner before copying the items - otherwise
  // owner is not set properly.
  TEpiCustomList(Result).FItemOwner := FItemOwner;
  for i := 0 to Count - 1 do
  begin
    NItem := TEpiCustomItem(Items[i].DoCloneCreate(Result));
    // Set a random name, otherwise calling Add will fail.
    // Name is correctly set in Items[i].DoClone...
    NItem.FName := GetRandomName;
    if TEpiCustomList(Result).IndexOf(NItem) = -1 then
      TEpiCustomList(Result).AddItem(NItem);
    Items[i].DoClone(Result, NItem);
  end;

  // Set Sorting last, otherwise each AddItem triggers a sort.
  with TEpiCustomList(Result) do
  begin
    FOnSort := Self.FOnSort;
    FSorted := Self.FSorted;
    Sort;
  end;
end;

{ TEpiCustomControlItemList }

function SortControlItems(Item1, Item2: Pointer): Integer;
var
  CI1: TEpiCustomControlItem absolute Item1;
  CI2: TEpiCustomControlItem absolute Item2;
begin
  // The same pointers is also the same object!
  if CI1.Equals(CI2) then exit(0);

  result := CI1.Top - CI2.Top;
  if result = 0 then
    result := CI1.Left - CI2.Left;

  // If two ControlItems are placed on eachother - the "highest" is the biggest pointer value.
  if result = 0 then
    result := Item1 - Item2;
end;

procedure TEpiCustomControlItemList.ChangeHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup <> eegCustomBase) then exit;

  case TEpiCustomChangeEventType(EventType) of
    ecceSetTop:  Sort;
    ecceSetLeft: Sort;
  end;
end;

procedure TEpiCustomControlItemList.DoSort;
begin
  if not Assigned(FOnSort) then
    FOnSort := @SortControlItems;

  inherited DoSort;

  if FOnSort = @SortControlItems then
    FOnSort := nil;
end;

procedure TEpiCustomControlItemList.InsertItem(const Index: integer;
  Item: TEpiCustomItem);
begin
  inherited InsertItem(Index, Item);
  Item.RegisterOnChangeHook(@ChangeHook, true);
end;

function TEpiCustomControlItemList.DeleteItem(Index: integer): TEpiCustomItem;
begin
  Result := inherited DeleteItem(Index);
  Result.UnRegisterOnChangeHook(@ChangeHook);
end;

end.

