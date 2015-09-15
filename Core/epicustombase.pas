unit epicustombase;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, epidatafilestypes, typinfo,
  contnrs, LazMethodList;

const
  EPI_XML_DATAFILE_VERSION = 4;
  {$IFNDEF RELEASE}
  EPI_XML_BRANCH_STRING = 'ADMIN';
  {$ENDIF}

type
  EEpiBadVersion  = class(Exception);
  EEpiExternalFileNoFound = class(Exception);

  TEpiCustomBase = class;
  TEpiCustomBaseClass = class of TEpiCustomBase;
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
    eegHeading,
    // epirange.pas
    eegRange,
    // epivaluelabels.pas
    eegValueLabel,
    eegValueLabelSet,
    // epirelations.pas
    eegRelations,
    // epirights.pas
    eegRights
    );

  // ecce = Epi Custom Change Event
  TEpiCustomChangeEventType = (
    ecceDestroy,
    ecceUpdate,
    ecceName,
    ecceAddItem,
    ecceDelItem,
    ecceSetItem,
    ecceSetTop,
    ecceSetLeft,
    ecceText,
    ecceReferenceDestroyed,
    ecceListMove
  );

  TEpiChangeEvent = procedure(
    Const Sender: TEpiCustomBase;     // Who is currently transmitting the event.
    Const Initiator: TEpiCustomBase;  // Who initiated the event
    EventGroup: TEpiEventGroup;       // Grouping of eventtypes
    EventType: Word;                  // Actual event type.
    Data: Pointer                     // Data associated with the event
  ) of object;

  TEpiCustomBaseState = set of (
    ebsDestroying,     // Set on BeforeDestruction when freeing an object
    ebsUpdating,       // UNKNOWN???
    ebsLoading         // Set on loading from XML file.
  );

  TEpiCoreException = class (Exception);

  { TEpiReferenceMap }

  TEpiReferenceMap = class
  private
    FObjectReferences: TStringList;
    FClassType: TList;
    FReferenceType: TByteArray;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFixupReference(
      Obj: TEpiCustomBase;                 // The object which has a reference that cannot be fulfilled
      EpiClassType: TEpiCustomBaseClass;   // The class type of the object. (should be used by Obj to check at what level of the class hierachy the reference should be fixed
      ReferenceType: Byte;                 // An identifier, indicating what reference (based on class-lvl) must be updated.
      Const ReferenceId: string);          // The reference identifier.
    procedure FixupReferences;
  end;

  TEpiCustomBase = class
  { Save/Load/XML functionality }
  private
    function   NodePath(Const Root: TDOMNode): string;
    procedure  RaiseErrorNode(const Root: TDOMNode; const NodeName: string);
    procedure  RaiseErrorAttr(const Root: TDOMNode; const AttrName: string);
    procedure  RaiseErrorMsg(const Root: TDOMNode; const Msg: string);
  protected
    { Check methods }
    function   NodeIsWhiteSpace(Const Node: TDomNode): boolean;
    procedure  CheckNode(const Node: TDOMNode; const NodeName: string); virtual;

  public
    function   XMLName: string; virtual;

  protected
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
  public
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); virtual;

  protected
    // Dom Attr
    procedure  SaveDomAttr(Const Node: TDomElement; Const Tag: String;
      Const Value: boolean); overload;
    procedure  SaveDomAttr(Const Node: TDomElement; Const Tag: String;
      Const Value: integer); overload;
    procedure  SaveDomAttr(Const Node: TDomElement; Const Tag: String;
      Const Value: extended); overload;
    procedure  SaveDomAttr(Const Node: TDomElement; Const Tag: String;
      Const Value: TDateTime); overload;
    procedure  SaveDomAttr(Const Node: TDomElement; Const Tag: String;
      Const Value: string); overload;
    procedure  SaveDomAttrEnum(Const Node: TDomElement; Const Tag: String;
      Const Value; TypeInfo: PTypeInfo);
    // Dom Text Content
    procedure  SaveTextContent(Const RootNode: TDOMElement; Const Tag: String;
      Const Value: String); overload;
    procedure  SaveTextContent(Const RootNode: TDOMElement; Const Tag: String;
      Const Value: Integer); overload;
    procedure  SaveTextContent(Const RootNode: TDOMElement; Const Tag: String;
      Const Value: Extended); overload;
    procedure  SaveTextContent(Const RootNode: TDOMElement; Const Tag: String;
      Const Value: TDateTime); overload;
    procedure  SaveTextContent(Const RootNode: TDOMElement; Const Tag: String;
      Const Value: Boolean); overload;
    // Save To Dom
    function   SaveToDom(RootDoc: TDOMDocument): TDOMElement; virtual;

    { Change-event hooks }
  private
    FOnChangeList: TMethodList;
    FOnChangeListIgnoreUpdate: TMethodList;
    FUpdateCount: Integer;
  protected
    procedure  DoChange(EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer); overload;
    { CONTRACT:
      All objects part of the an object-chain (owned->owner...) should override DoChange
      and perform actions related to the event being chained. All other objects that want
      to be notified of changes should do so using ChangeHooks!
      Eg.:
        1) If an TEpiCustomItem is destroyed, it will send an ecceDestroy event through the chain.
        2) The owning TEpiCustomList should (if it wants/needs to) handle this during DoChange.
        3) Any other objects not being part of this chain, eg. sibling TEpiCustomItem's should
           add a ChangeHook using RegisterOnChangeHook.
    }
    procedure  DoChange(Const Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer); virtual; overload;
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
    FOnModified: TNotifyEvent;
    FOwner:     TEpiCustomBase;
    FState:     TEpiCustomBaseState;
    function    GetRootOwner: TEpiCustomBase;
    procedure   SetOnModified(const AValue: TNotifyEvent);
  protected
    constructor Create(AOwner: TEpiCustomBase); virtual;
    procedure   SetModified(const AValue: Boolean); virtual;
    procedure   RegisterClasses(AClasses: Array of TEpiCustomBase); virtual;
    procedure   Assign(Const AEpiCustomBase: TEpiCustomBase); virtual;
    property    ClassList: TFPList read FClassList;
  public
    procedure   BeforeDestruction; override;
    destructor  Destroy; override;
    property    Owner: TEpiCustomBase read FOwner;
    property    RootOwner: TEpiCustomBase read GetRootOwner;
    property    State: TEpiCustomBaseState read FState;
    property    Modified: Boolean read FModified write SetModified;
    property    OnModified: TNotifyEvent read FOnModified write SetOnModified;

  { Fix References (relevant for Cloning and Loading) }
  protected
    // ReferenceType is a "private" identifier used by the object in order to
    // known what reference it should "fix".
    // ReferenceId is the identifier for the reference that needs to be fixed up.
    procedure   FixupReferences(EpiClassType: TEpiCustomBaseClass;
      ReferenceType: Byte;
      Const ReferenceId: string); virtual;

  { Cloning }
  protected
    // DoCloneCreate should only be overridden if a special constructor is used.
    function    DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase; virtual;
    // DoClone should normally be overridden, since this is where data should be copied.
    function    DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; virtual;
  public
    function    Clone: TEpiCustomBase;
    function    Clone(AOwner: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap = nil): TEpiCustomBase;

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
    procedure   Clear;
  public
    constructor Create(AOwner: TEpiCustomBase; Const aXMLName: string); virtual;
    destructor  Destroy; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property    Text: string read FCurrentText write SetCurrentText;
    property    TextLang[LangCode: string]: string read GetText write SetText;
  { Cloning }
  protected
    function DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase; override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;

  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  end;

  { TEpiTranslatedTextWrapper }

  TEpiTranslatedTextWrapper = class(TEpiTranslatedText)
  private
    FNodeName: string;
  public
    constructor Create(AOwner: TEpiCustomBase; Const NodeName, TextName: string);
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
  { Cloning }
  protected
    function DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase; override;
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  end;

  { TEpiCustomItem }
  TEpiCustomItem = class(TEpiCustomBase)
  protected
    FName: string;
    function    GetName: string; virtual;
    procedure   SetName(const AValue: string); virtual;
    function    DoValidateRename(Const NewName: string): boolean; virtual;
    function    WriteNameToXml: boolean; virtual;
  protected
    function    SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;

  { Observe Reference }
  private
    FReferenceList: TStringList;
    procedure ItemDestroyHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure RemoveReferece(Index: Integer);
  protected
    procedure ObserveReference(Item: TEpiCustomItem; PropertyName: shortstring);
    procedure ReferenceDestroyed(Item: TEpiCustomItem; PropertyName: shortstring); virtual;

  {Cloning}
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  public
    destructor  Destroy; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function    ValidateRename(Const NewName: string; RenameOnSuccess: boolean): boolean; virtual;
    property    Name: string read GetName write SetName;
  end;
  TEpiCustomItemClass = class of TEpiCustomItem;

  { TEpiCustomControlItem }

  TEpiCustomControlItem = class(TEpiCustomItem)
  private
    FLeft: integer;
    FTop: integer;
  protected
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure  SetLeft(const AValue: Integer); virtual;
    procedure  SetTop(const AValue: Integer); virtual;
    procedure  Assign(const AEpiCustomBase: TEpiCustomBase); override;
  protected
    function   SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    property   Left: Integer read FLeft write SetLeft;
    property   Top: Integer read FTop write SetTop;
  {Cloning}
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  end;
  TEpiCustomControlItemClass = class of TEpiCustomControlItem;

  TEpiCustomListEnumerator = class;
  TEpiOnNewItemClass      = function(Sender: TEpiCustomList; DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass of object;
  TEpiValidateRenameEvent = function(Const NewName: string): boolean of object;
  TEpiPrefixEvent         = function: string of object;

  { TEpiCustomList }

  TEpiCustomList = class(TEpiCustomItem)
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
  public
    constructor Create(AOwner: TEpiCustomBase); override;
  protected
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
    function    GetItemByName(Const AName: string): TEpiCustomItem; virtual;
    function    ItemExistsByName(AName: string): boolean; virtual;
    function    IndexOf(Item: TEpiCustomItem): integer; virtual;
    procedure   Move(Const CurIndex, NewIndex: Integer);
    function    GetEnumerator: TEpiCustomListEnumerator;
    property    Count: Integer read GetCount;
    property    Items[Index: integer]: TEpiCustomItem read GetItems write SetItems; default;
    property    ItemOwner: boolean read FItemOwner write SetItemOwner;
  { Naming and Validation }
  private
    FOnGetPrefix: TEpiPrefixEvent;
    FOnValidateRename: TEpiValidateRenameEvent;
    FUniqueNames: boolean;
  protected
    procedure SetUniqueNames(AValue: boolean); virtual;
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
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  end;
  TEpiCustomListClass = class of TEpiCustomList;

  { TEpiCustomListEnumerator }

  TEpiCustomListEnumerator = class
  private
    FCurrentIndex: Integer;
    FCustomList: TEpiCustomList;
    FListCount: Integer;
    procedure RaiseCountError;
    procedure CheckListCount;
  protected
    function GetCurrent: TEpiCustomItem; virtual;
  public
    constructor Create(CustomList: TEpiCustomList); virtual;
    function MoveNext: Boolean;
    property Current: TEpiCustomItem read GetCurrent;
  end;
  EEpiCustomListEnumeratorCountError = class(Exception);


  { TEpiCustomControlItemList }
  TEpiCustomControlItemListEnumerator = class;
  TEpiCustomControlItemList = class(TEpiCustomList)
  private
    procedure ChangeHook(Const Sender: TEpiCustomBase;
       Const Initiator: TEpiCustomBase;
       EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
  protected
    procedure DoSort; override;
    procedure DoChange(const Initiator: TEpiCustomBase;
       EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer); override;
       overload;
  public
    procedure InsertItem(const Index: integer; Item: TEpiCustomItem); override;
    function  DeleteItem(Index: integer): TEpiCustomItem; override;
    function GetEnumerator: TEpiCustomControlItemListEnumerator;
  end;

  { TEpiCustomControlItemListEnumerator }

  TEpiCustomControlItemListEnumerator = class(TEpiCustomListEnumerator)
  protected
    function GetCurrent: TEpiCustomControlItem; override;
  public
    property Current: TEpiCustomControlItem read GetCurrent;
  end;


{$I epixmlconstants.inc}

procedure BackupFormatSettings(NewFormatSettings: TFormatSettings); overload;
procedure BackupFormatSettings; overload;
procedure RestoreFormatSettings;

implementation

uses
  StrUtils, DCPsha256, laz2_XMLRead, epistringutils, episettings, epidocument,
  epidatafiles;

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

{ TEpiReferenceMap }

constructor TEpiReferenceMap.Create;
begin
  FClassType := TList.Create;
  FObjectReferences := TStringList.Create;
  FObjectReferences.Sorted := false;
end;

destructor TEpiReferenceMap.Destroy;
begin
  FClassType.Free;
  FObjectReferences.Free;
  inherited Destroy;
end;

procedure TEpiReferenceMap.AddFixupReference(Obj: TEpiCustomBase;
  EpiClassType: TEpiCustomBaseClass; ReferenceType: Byte;
  const ReferenceId: string);
var
  Idx: Integer;
begin
  if (Self = nil) then exit;

  Idx := FObjectReferences.AddObject(ReferenceId, Obj);
  FClassType.Add(EpiClassType);
  FReferenceType[Idx] := ReferenceType;
end;

procedure TEpiReferenceMap.FixupReferences;
var
  Obj: TEpiCustomBase;
  i: Integer;
begin
  if (Self = nil) then exit;

  for i := 0 to FObjectReferences.Count - 1 do
  begin
    Obj := TEpiCustomBase(FObjectReferences.Objects[i]);
    Obj.FixupReferences(
      TEpiCustomBaseClass(FClassType[i]),
      FReferenceType[i],
      FObjectReferences[i]);
  end;
end;


{ TEpiCustomBase }

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

procedure TEpiCustomBase.BeforeDestruction;
begin
  // Do the last Free notification to the event hooks.
  // - this allows for objects pointing the "self" to remove reference if needed.
  Include(FState, ebsDestroying);
  DoChange(eegCustomBase, Word(ecceDestroy), nil);

  inherited BeforeDestruction;
end;

destructor TEpiCustomBase.Destroy;
begin
  FClassList.Free;
  FOnChangeList.Free;
  FOnChangeListIgnoreUpdate.Free;
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

procedure TEpiCustomBase.FixupReferences(EpiClassType: TEpiCustomBaseClass;
  ReferenceType: Byte; const ReferenceId: string);
begin
  // Do nothing..
end;

function TEpiCustomBase.DoCloneCreate(AOwner: TEpiCustomBase): TEpiCustomBase;
begin
  result := TEpiCustomBaseClass(Self.ClassType).Create(AOwner);
end;

function TEpiCustomBase.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
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

    OrgClass.DoClone(Result, NewClass, ReferenceMap);
  end;
end;

function TEpiCustomBase.Clone: TEpiCustomBase;
var
  RefMap: TEpiReferenceMap;
begin
  RefMap := TEpiReferenceMap.Create;
  Result := DoClone(nil, nil, RefMap);
  RefMap.FixupReferences;
  RefMap.Free;
end;

function TEpiCustomBase.Clone(AOwner: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := DoClone(AOwner, nil, ReferenceMap);
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

function TEpiCustomBase.NodeIsWhiteSpace(const Node: TDomNode): boolean;
var
  P: PChar;
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
    result := Node.TextContent
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
  if LoadAttr(Attr, Root, AttrName, Fatal) then
    Result := StrToInt64(Attr.Value)
  else
    Result := DefaultVal;
end;

function TEpiCustomBase.LoadAttrEnum(const Root: TDOMNode;
  const AttrName: string; TypeInfo: PTypeInfo; DefaultVal: String;
  Fatal: Boolean): integer;
begin
  if TypeInfo^.Kind = tkSet then
    result := StringToSet(TypeInfo, LoadAttrString(Root, AttrName, DefaultVal, Fatal))
  else
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
    Result := Attr.Value
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
    result := LowerCase(Attr.Value) = 'true'
  else
    Result := DefaultVal;
end;

function TEpiCustomBase.XMLName: string;
begin
  result := ClassName;
end;

procedure TEpiCustomBase.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  // Do nothing - should be overridden in descendants.
end;

procedure TEpiCustomBase.SaveDomAttr(const Node: TDomElement;
  const Tag: String; const Value: string);
begin
  Node.SetAttribute(Tag, Value);
end;

procedure TEpiCustomBase.SaveDomAttrEnum(const Node: TDomElement;
  const Tag: String; const Value; TypeInfo: PTypeInfo);
var
  V: Byte;
  I: Integer;
begin
  if (TypeInfo^.Kind = tkSet) then
  begin
    I := Integer(Value);
    SaveDomAttr(Node, Tag, SetToString(TypeInfo, I, false));
  end else begin
    V := Byte(Value);
    SaveDomAttr(Node, Tag, GetEnumName(TypeInfo, V));
  end;
end;

procedure TEpiCustomBase.SaveDomAttr(const Node: TDomElement;
  const Tag: String; const Value: integer);
begin
  SaveDomAttr(Node, Tag, IntToStr(Value));
end;

procedure TEpiCustomBase.SaveDomAttr(const Node: TDomElement;
  const Tag: String; const Value: extended);
begin
  if (RootOwner is TEpiDocument) then
  with TEpiDocument(RootOwner).XMLSettings do
  begin
    BackupFormatSettings(FormatSettings);
    SaveDomAttr(Node, Tag, FloatToStr(Value));
    RestoreFormatSettings;
  end else
    SaveDomAttr(Node, Tag, FloatToStr(Value));
end;

procedure TEpiCustomBase.SaveDomAttr(const Node: TDomElement;
  const Tag: String; const Value: TDateTime);
begin
  if (RootOwner is TEpiDocument) then
  with TEpiDocument(RootOwner).XMLSettings do
  begin
    BackupFormatSettings(FormatSettings);
    SaveDomAttr(Node, Tag, FormatDateTime(FormatSettings.ShortDateFormat, Value));
    RestoreFormatSettings;
  end else
    SaveDomAttr(Node, Tag, FormatDateTime('YYYY/MM/DD HH:NN:SS', Value));
end;

procedure TEpiCustomBase.SaveDomAttr(const Node: TDomElement;
  const Tag: String; const Value: boolean);
begin
  SaveDomAttr(Node, Tag, BoolToStr(Value, 'true', 'false'));
end;

procedure TEpiCustomBase.SaveTextContent(const RootNode: TDOMElement;
  const Tag: String; const Value: String);
var
  Elem: TDOMElement;
begin
  Elem := RootNode.OwnerDocument.CreateElement(Tag);
  Elem.TextContent := Value;
  RootNode.AppendChild(Elem);
end;

procedure TEpiCustomBase.SaveTextContent(const RootNode: TDOMElement;
  const Tag: String; const Value: Integer);
begin
  SaveTextContent(RootNode, Tag, IntToStr(Value));
end;

procedure TEpiCustomBase.SaveTextContent(const RootNode: TDOMElement;
  const Tag: String; const Value: Extended);
begin
  if (RootOwner is TEpiDocument) then
    BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);

  SaveTextContent(RootNode, Tag, FloatToStr(Value));

  if (RootOwner is TEpiDocument) then
    RestoreFormatSettings;
end;

procedure TEpiCustomBase.SaveTextContent(const RootNode: TDOMElement;
  const Tag: String; const Value: TDateTime);
begin
  if (RootOwner is TEpiDocument) then
  with TEpiDocument(RootOwner).XMLSettings do
  begin
    BackupFormatSettings(FormatSettings);
    SaveTextContent(RootNode, Tag, FormatDateTime(FormatSettings.ShortDateFormat, Value));
    RestoreFormatSettings;
  end else
    SaveTextContent(RootNode, Tag, FormatDateTime('YYYY/MM/DD HH:NN:SS', Value));
end;

procedure TEpiCustomBase.SaveTextContent(const RootNode: TDOMElement;
  const Tag: String; const Value: Boolean);
begin
//  SaveTextContent(RootNode, Value, BoolToStr(Value, 'true', 'false'));
end;

function TEpiCustomBase.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
var
  Elem: TDOMElement;
  i: Integer;
begin
  Result := RootDoc.CreateElement(XMLName);

  for i := 0 to ClassList.Count - 1 do
  begin
    Elem := TEpiCustomBase(ClassList[i]).SaveToDom(RootDoc);
    if Assigned(Elem) then
      Result.AppendChild(Elem);
  end;
end;

procedure TEpiCustomBase.DoChange(EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  DoChange(Self, EventGroup, EventType, Data);
end;

procedure TEpiCustomBase.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  i: Integer;
  S: String;
  M: TEpiChangeEvent;
begin
  S := Self.ClassName;

  I := FOnChangeListIgnoreUpdate.Count;
  while FOnChangeListIgnoreUpdate.NextDownIndex(I) do
  begin
    M := TEpiChangeEvent(FOnChangeListIgnoreUpdate.Items[I]);
    M(Self, Initiator, EventGroup, EventType, Data);
  end;

  if ((EventGroup = eegCustomBase) and (EventType <> Word(ecceUpdate))) or
     (EventGroup <> eegCustomBase)
  then
    Modified := true;

  if FUpdateCount = 0 then
  begin
    I := FOnChangeList.Count;
    while FOnChangeList.NextDownIndex(I) do
    begin
      M := TEpiChangeEvent(FOnChangeList.Items[I]);
      M(Self, Initiator, EventGroup, EventType, Data);
    end;
  end;

  if Assigned(Owner) then
    Owner.DoChange(Initiator, EventGroup, EventType, Data);
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
    begin
      FOnChangeListIgnoreUpdate := TMethodList.Create;
      FOnChangeListIgnoreUpdate.AllowDuplicates := false;
    end;
    FOnChangeListIgnoreUpdate.Add(TMethod(Event), false);
  end else begin
    if not Assigned(FOnChangeList) then
    begin
      FOnChangeList := TMethodList.Create;
      FOnChangeList.AllowDuplicates := false;
    end;
    FOnChangeList.Add(TMethod(Event), false);
  end;
end;

procedure TEpiCustomBase.UnRegisterOnChangeHook(Event: TEpiChangeEvent);
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

procedure TEpiCustomBase.AddCustomData(const Key: string; const Obj: TObject);
begin
  if not Assigned(FCustomData) then
    FCustomData := TFPObjectHashTable.Create(false);

  try
    FCustomData.Items[Key] := Obj;
  except
    raise Exception.Create('TEpiCustomBase: Duplicate CustomData - key=' + Key);
  end;
end;

function TEpiCustomBase.FindCustomData(const Key: string): TObject;
begin
  result := nil;
  if Assigned(FCustomData) then
    result := FCustomData.Items[Key];
end;

function TEpiCustomBase.RemoveCustomData(const Key: string): TObject;
begin
  Result := FindCustomData(Key);
  if not Assigned(Result) then exit;

  FCustomData.Delete(Key);

  if FCustomData.Count = 0 then
    FreeAndNil(FCustomData);
end;

{ TEpiTranslatedText }

procedure TEpiTranslatedText.SetCurrentText(const AValue: string);
var
  Val: String;
begin
  if FCurrentText = AValue then exit;
  Val := FCurrentText;
  FCurrentText := AValue;
  SetText(FCurrentLang, AValue);
//  DoChange(eegCustomBase, Word(ecceText), @Val);
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
    DoChange(eegCustomBase, Word(ecceText), @Val);
  end
  // Fallback to default language
  else if (FTextList.Find(FDefaultLang, Idx)) and (not DefaultLanguage) then
  begin
    Val := FCurrentText;
    FCurrentText := TString(FTextList.Objects[Idx]).Str;
    DoChange(eegCustomBase, Word(ecceText), @Val);
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

procedure TEpiTranslatedText.Clear;
var
  i: Integer;
begin
  for i := FTextList.Count - 1 downto 0 do
    FTextList.Objects[i].Free;
  FTextList.Clear;
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
begin
  FXMLName := '';
  FCurrentText := '';

  Clear;

  FTextList.Free;
  inherited Destroy;
end;

procedure TEpiTranslatedText.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
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
    // Ugly hack to prevent looking at nodes that is not directly below the root.
    if ElemList[i].ParentNode <> Root then continue;

    LangCode := TDOMElement(ElemList[i]).AttribStrings['xml:lang'];
    Val := ElemList[i].TextContent;
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

  // Clear content before assign
  Clear;

  for i := 0 to OrgText.FTextList.Count - 1 do
    FTextList.AddObject(OrgText.FTextList[i], TString.Create(TString(OrgText.FTextList.Objects[i]).Str));
  EndUpdate;
end;

function TEpiTranslatedText.DoCloneCreate(AOwner: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := TEpiTranslatedText.Create(AOwner, XMLName);
end;

function TEpiTranslatedText.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
var
  i: Integer;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  with TEpiTranslatedText(Result) do
  begin
    Clear;

    for i := 0 to Self.FTextList.Count - 1 do
      FTextList.AddObject(Self.FTextList[i], TString.Create(TString(Self.FTextList.Objects[i]).Str));

    FCurrentText := Self.FCurrentText;
  end;
end;

function TEpiTranslatedText.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
var
  i: Integer;
  Elem: TDOMText;
begin
  Result := inherited SaveToDom(RootDoc);

  for i := 0 to FTextList.Count - 1 do
  begin
    if TString(FTextList.Objects[i]).Str <> '' then
    begin
      SaveDomAttr(Result, 'xml:lang', FTextList[i]);
      Result.TextContent := TString(FTextList.Objects[i]).Str;
    end;
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
  DoChange(eegCustomBase, Word(ecceText), @Val);
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

procedure TEpiTranslatedTextWrapper.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  NRoot: TDOMNode;
begin
  // Root = Parent for FNodeName (since this is a wrapped object.
  if LoadNode(NRoot, Root, FNodeName, false) then
    inherited LoadFromXml(NRoot, ReferenceMap);
end;

procedure TEpiTranslatedTextWrapper.Assign(const AEpiCustomBase: TEpiCustomBase
  );
begin
  inherited Assign(AEpiCustomBase);
  FNodeName := TEpiTranslatedTextWrapper(AEpiCustomBase).FNodeName;
end;

function TEpiTranslatedTextWrapper.DoCloneCreate(AOwner: TEpiCustomBase
  ): TEpiCustomBase;
begin
  Result := TEpiTranslatedTextWrapper.Create(AOwner, FNodeName, XMLName);
end;

function TEpiTranslatedTextWrapper.SaveToDom(RootDoc: TDOMDocument
  ): TDOMElement;
var
  Elem: TDOMElement;
  i: Integer;
  ReturnNil: Boolean;
begin
  ReturnNil := true;
  for i := 0 to FTextList.Count -1 do
    if TString(FTextList.Objects[i]).Str <> '' then
    begin
      ReturnNil := false;
      Break;
    end;

  if ReturnNil then
    Exit(nil);

  Elem := inherited SaveToDom(RootDoc);

  Result := RootDoc.CreateElement(FNodeName);
  Result.AppendChild(Elem);
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

function TEpiCustomItem.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  if WriteNameToXml then
    SaveDomAttr(Result, rsId, Name);
end;

destructor TEpiCustomItem.Destroy;
var
  i: Integer;
begin
  if Assigned(FReferenceList) then
  begin
    for i := FReferenceList.Count - 1 downto 0 do
      RemoveReferece(I);
  end;

  FName := '';
  inherited Destroy;
end;

procedure TEpiCustomItem.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Attr: TDOMAttr;
begin
  if LoadAttr(Attr, Root, rsId, false) then
    FName := LoadAttrString(Root, rsId)
  else if WriteNameToXml then
    // This class was supposed to write an ID -> hence it also expects one! Error!
    RaiseErrorAttr(Root, rsId);
end;

procedure TEpiCustomItem.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);

  BeginUpdate;
  Name := TEpiCustomItem(AEpiCustomBase).Name;
  EndUpdate;
end;

procedure TEpiCustomItem.ItemDestroyHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  Idx: Integer;
  S: shortstring;
  ItemProperty: Pointer;
  PItemProperty: Pointer;
begin
  if (EventGroup <> eegCustomBase) then exit;
  if (TEpiCustomChangeEventType(EventType) <> ecceDestroy) then exit;

  Idx := FReferenceList.IndexOfObject(Initiator);
  if (Idx = -1) then exit;

  S := FReferenceList[Idx];
  RemoveReferece(Idx);
  ReferenceDestroyed(TEpiCustomItem(Initiator), S);
end;

procedure TEpiCustomItem.RemoveReferece(Index: Integer);
var
  Item: TEpiCustomItem;
begin
  Item := TEpiCustomItem(FReferenceList.Objects[Index]);
  Item.UnRegisterOnChangeHook(@ItemDestroyHook);

  FReferenceList.Delete(Index);

  if FReferenceList.Count = 0 then
    FreeAndNil(FReferenceList);
end;

procedure TEpiCustomItem.ObserveReference(Item: TEpiCustomItem;
  PropertyName: shortstring);
begin
  if not Assigned(Item) then exit;

  if not Assigned(FReferenceList) then
    FReferenceList := TStringList.Create;

  FReferenceList.AddObject(PropertyName, Item);
  Item.RegisterOnChangeHook(@ItemDestroyHook, true);
end;

procedure TEpiCustomItem.ReferenceDestroyed(Item: TEpiCustomItem;
  PropertyName: shortstring);
begin
  Item.UnRegisterOnChangeHook(@ItemDestroyHook);
  DoChange(eegCustomBase, Word(ecceReferenceDestroyed), Item);
end;

function TEpiCustomItem.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  TEpiCustomItem(Result).FName := FName;
end;

function TEpiCustomItem.ValidateRename(const NewName: string;
  RenameOnSuccess: boolean): boolean;
begin
  if NewName = Name then exit(true);
  result := DoValidateRename(NewName);
end;

{ TEpiCustomControlItem }

procedure TEpiCustomControlItem.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);
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
  Top := OrgControlItem.Top;
  Left := OrgControlItem.Left;
  EndUpdate;
end;

function TEpiCustomControlItem.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  Result.SetAttribute(rsTop, IntToStr(Top));
  Result.SetAttribute(rsLeft, IntToStr(Left));
end;

function TEpiCustomControlItem.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
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

procedure TEpiCustomList.OnChangeHook(const Sender: TEpiCustomBase;
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

procedure TEpiCustomList.RegisterItem(Item: TEpiCustomItem);
begin
  if ItemOwner then
  begin
    Item.FOwner := Self;
    Item.SetLanguage(FDefaultLang, true);
    Item.SetLanguage(FCurrentLang, false);
  end else
    Item.RegisterOnChangeHook(@OnChangeHook, true);

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
  FUniqueNames := true;
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

procedure TEpiCustomList.LoadFromXml(Root: TDOMNode;
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

function TEpiCustomList.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
var
  i: Integer;
  Elem: TDOMElement;
begin
  if Count = 0 then
    Exit(nil);

  Result := inherited SaveToDom(RootDoc);

  for i := 0 to Count - 1 do
  begin
    Elem := Items[i].SaveToDom(RootDoc);
    if Assigned(Elem) then
      Result.AppendChild(Elem);
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
  // if unique names is not required, you can ALWAYS rename item.
  if (not UniqueNames) then
    Exit(true);

  result := not ItemExistsByName(NewName);
  if Assigned(OnValidateRename) then
    result := result and OnValidateRename(NewName);
end;

procedure TEpiCustomList.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited DoChange(Initiator, EventGroup, EventType, Data);
  OnChangeHook(Self, Initiator, EventGroup, EventType, Data);
end;

destructor TEpiCustomList.Destroy;
var
  F: TEpiCustomItem;
begin
  ClearAndFree;
  FreeAndNil(FList);
  inherited Destroy;
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
      F.Free;
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

function TEpiCustomList.GetItemByName(const AName: string): TEpiCustomItem;
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

procedure TEpiCustomList.Move(const CurIndex, NewIndex: Integer);
begin
  // Cannot move in a sorted list.
  // TODO: Create Exception!
  if Sorted then exit;

  FList.Move(CurIndex, NewIndex);
  DoChange(eegCustomBase, Word(ecceListMove), nil);
end;

function TEpiCustomList.GetEnumerator: TEpiCustomListEnumerator;
begin
  result := TEpiCustomListEnumerator.Create(Self);
end;

procedure TEpiCustomList.SetUniqueNames(AValue: boolean);
begin
  if FUniqueNames = AValue then Exit;
  FUniqueNames := AValue;
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

  // TESTING TRUE ASSIGN
  //  OnNewItemClass := EpiCustomList.OnNewItemClass;

  if EpiCustomList.Count > 0 then
  begin
    // TESTING
{    NItemClass := TEpiCustomItemClass(EpiCustomList[0].ClassType);
    for i := 0 to EpiCustomList.Count - 1 do
    begin
      Item := NewItem(NItemClass);
      Item.Assign(EpiCustomList[i]);
    end;}

    Clear;

    for Item in EpiCustomList do
      AddItem(Item);
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

function TEpiCustomList.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
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
  TEpiCustomList(Result).FItemOwner := FItemOwner;
  for i := 0 to Count - 1 do
  begin
    NItem := TEpiCustomItem(Items[i].DoCloneCreate(Result));
    // Set a random name, otherwise calling Add will fail.
    // Name is correctly set in Items[i].DoClone...
    NItem.FName := GetRandomName;
    if TEpiCustomList(Result).IndexOf(NItem) = -1 then
      TEpiCustomList(Result).AddItem(NItem);
    Items[i].DoClone(Result, NItem, ReferenceMap);
  end;

  // Set Sorting last, otherwise each AddItem triggers a sort.
  with TEpiCustomList(Result) do
  begin
    FOnSort := Self.FOnSort;
    FSorted := Self.FSorted;
    Sort;
  end;
end;

{ TEpiCustomListEnumerator }

function TEpiCustomListEnumerator.GetCurrent: TEpiCustomItem;
begin
  result := FCustomList[FCurrentIndex];
end;

procedure TEpiCustomListEnumerator.RaiseCountError;
begin
{  raise EEpiCustomListEnumeratorCountError.CreateFmt(
    'Enumeration error!' + LineEnding +
    'Expected %d items in list, but MoveNext found %d!' + LineEnding +
    'Deleting/Inserting items during iteration is NOT supported!',
    [FListCount, FCustomList.Count]
  );   }
end;

procedure TEpiCustomListEnumerator.CheckListCount;
begin
  if FCustomList.Count <> FListCount then
    RaiseCountError;
end;

constructor TEpiCustomListEnumerator.Create(CustomList: TEpiCustomList);
begin
  FCustomList := CustomList;
  FCurrentIndex := -1;
  FListCount := CustomList.Count;
end;

function TEpiCustomListEnumerator.MoveNext: Boolean;
begin
  CheckListCount;
  Inc(FCurrentIndex);
  Result := (FCurrentIndex < FCustomList.Count);
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

procedure TEpiCustomControlItemList.ChangeHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  EpiInitiator: TEpiCustomControlItem absolute Initiator;
begin
  if (EventGroup <> eegCustomBase) then exit;
  if not (Initiator is TEpiCustomControlItem) then exit;
  if IndexOf(EpiInitiator) = -1 then exit;

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

procedure TEpiCustomControlItemList.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited DoChange(Initiator, EventGroup, EventType, Data);
  ChangeHook(Self, Initiator, EventGroup, EventType, Data);
end;

procedure TEpiCustomControlItemList.InsertItem(const Index: integer;
  Item: TEpiCustomItem);
begin
  inherited InsertItem(Index, Item);
  if not ItemOwner then
    Item.RegisterOnChangeHook(@ChangeHook, true);
end;

function TEpiCustomControlItemList.DeleteItem(Index: integer): TEpiCustomItem;
begin
  Result := inherited DeleteItem(Index);
  Result.UnRegisterOnChangeHook(@ChangeHook);
end;

function TEpiCustomControlItemList.GetEnumerator: TEpiCustomControlItemListEnumerator;
begin
  result := TEpiCustomControlItemListEnumerator.Create(Self);
end;

{ TEpiCustomControlItemListEnumerator }

function TEpiCustomControlItemListEnumerator.GetCurrent: TEpiCustomControlItem;
begin
  result := TEpiCustomControlItem(inherited GetCurrent);
end;

end.

