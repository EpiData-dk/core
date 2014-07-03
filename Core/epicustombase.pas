unit epicustombase;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, DCPrijndael, epidatafilestypes, typinfo,
  contnrs, LazMethodList;

const
  EPI_XML_DATAFILE_VERSION = 3;
  {$IFNDEF RELEASE}
  EPI_XML_BRANCH_STRING = 'TRUNK';
  {$ENDIF}

type
  EEpiBadVersion  = class(Exception);
  EEpiExternalFileNoFound = class(Exception);

  TEpiCustomBase = class;
  TEpiCustomBaseClass = class of TEpiCustomBase;

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
    // epirange.pas
    eegRange,
    // epivaluelabels.pas
    eegValueLabel,
    eegValueLabelSet,
    // epirelations.pas
    eegRelations
    );

  // ecce = Epi Custom Change Event
  TEpiCustomChangeEventType = (
    ecceDestroy, ecceUpdate, ecceName, ecceAddItem, ecceDelItem, ecceSetItem,
    ecceSetTop, ecceSetLeft, ecceText, ecceReferenceDestroyed
  );

  TEpiChangeEvent = procedure(
    Const Sender: TEpiCustomBase;     // Who is currently transmitting the event.
    Const Initiator: TEpiCustomBase;  // Who initiated the event
    EventGroup: TEpiEventGroup;       // Grouping of eventtypes
    EventType: Word;                  // Actual event type.
    Data: Pointer                     // Data associated with the event
  ) of object;

  TEpiCustomBaseState = set of (ebsDestroying, ebsUpdating);

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
      Obj: TEpiCustomBase;                 // The object which has a reference which cannot be fulfilled
      EpiClassType: TEpiCustomBaseClass;   // The class type of the object. (should be used by Obj to chech at what level of the class hierachy the reference should be fixed
      ReferenceType: Byte;                 // An identifier, indicating what reference (based on class-lvl) must be updated.
      Const ReferenceId: string);          // The reference identifier.
    procedure FixupReferences;
  end;

//  {$static on}
  TEpiCustomBase = class
  { Scrambling }
  private
    FCrypter:   TDCP_rijndael;
    function    Get4ByteSalt: Integer;
  protected
    procedure   InitCrypt(Key: string);
    function    EnCrypt(Const S: string): string; overload;
    function    DeCrypt(Root: TDOMNode): TDOMNode; overload;
    function    DeCrypt(S: string): string; overload;
//    property    Crypter: TDCP_rijndael read FCrypter;   // DOES NOT WORK WITH FPC 2.4 - only from 2.5.1

  { Save/Load functionality }
  protected
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
    FObjectData: PtrUInt;
    FOnModified: TNotifyEvent;
    FState:     TEpiCustomBaseState;
    function    GetRootOwner: TEpiCustomBase;
    procedure   SetOnModified(const AValue: TNotifyEvent);
  protected
    FOwner:     TEpiCustomBase;
    constructor Create(AOwner: TEpiCustomBase); virtual;
    procedure   SetModified(const AValue: Boolean); virtual;
    procedure   RegisterClasses(AClasses: Array of TEpiCustomBase); virtual;
    property    ClassList: TFPList read FClassList;
  public
    procedure   BeforeDestruction; override;
    destructor  Destroy; override;
    procedure   Assign(Const AEpiCustomBase: TEpiCustomBase); virtual;
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
  end;
//  {$static off}

{$I epixmlconstants.inc}

procedure BackupFormatSettings(NewFormatSettings: TFormatSettings); overload;
procedure BackupFormatSettings; overload;
procedure RestoreFormatSettings;

implementation

uses
  StrUtils, DCPsha256, laz2_XMLRead, epistringutils, episettings;

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

procedure TEpiCustomBase.InitCrypt(Key: string);
begin
{  if not Assigned(FCrypter) then
  begin
    FCrypter := TDCP_rijndael.Create(nil);
    Randomize;
  end;

  FCrypter.InitStr(Key, TDCP_sha256);   }
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
{  Salt := Get4ByteSalt;
  Result := FCrypter.EncryptString(String(SaltStr) + S);

  FCrypter.Reset;}
end;

function TEpiCustomBase.DeCrypt(Root: TDOMNode): TDOMNode;
var
  St: TStringStream;
  XMLDoc: TDOMDocumentFragment;
  s: String;
  Node: TDOMNode;
begin
{  Result := nil;

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
  Result := XMLDoc;   }
end;

function TEpiCustomBase.DeCrypt(S: string): string;
begin
{  Result := FCrypter.DecryptString(S);
  FCrypter.Reset;
  Delete(Result, 1, 4); }
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
  result := DupeString('  ', Level);
end;

function TEpiCustomBase.ScrambleXml: boolean;
begin
  result := false;
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
 { if LoadNode(Node, Root, NodeName, Fatal) then
  begin
    if (RootOwner is TEpiDocument) then
      BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
    result := StrToFloat(Node.TextContent);
    RestoreFormatSettings;
  end else
    result := DefaultVal;   }
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
{  if LoadNode(Node, Root, NodeName, Fatal) then
  begin
    if (RootOwner is TEpiDocument) then
      BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
    result := StrToDateTime(Node.TextContent);
    RestoreFormatSettings;
  end else
    result := DefaultVal;  }
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
  result := GetEnumValue(TypeInfo, LoadAttrString(Root, AttrName, DefaultVal, Fatal));
end;

function TEpiCustomBase.LoadAttrFloat(const Root: TDOMNode;
  const AttrName: string; DefaultVal: EpiFloat; Fatal: Boolean): EpiFloat;
var
  Attr: TDOMAttr;
begin
 { if LoadAttr(Attr, Root, AttrName, Fatal) then
  begin
    if (RootOwner is TEpiDocument) then
      BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
    result := StrToFloat(Attr.Value);
    RestoreFormatSettings;
  end else
    Result := DefaultVal;    }
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
{  if LoadAttr(Attr, Root, AttrName, Fatal) then
  begin
    if (RootOwner is TEpiDocument) then
      BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
    if Format <> '' then
      DefaultFormatSettings.ShortDateFormat := Format;
    Result := StrToDateTime(Attr.Value);
    if (RootOwner is TEpiDocument) then
      RestoreFormatSettings;
  end else
    Result := DefaultVal;}
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
{  if (RootOwner is TEpiDocument) then
    BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);
  result := SaveNode(Lvl, NodeName, FloatToStr(Val));
  RestoreFormatSettings; }
end;

function TEpiCustomBase.SaveNode(const Lvl: integer; const NodeName: string;
  const Val: TDateTime): string;
begin
 { if (RootOwner is TEpiDocument) then
  with TEpiDocument(RootOwner).XMLSettings do
  begin
    BackupFormatSettings(FormatSettings);
    result := SaveNode(Lvl, NodeName, FormatDateTime(FormatSettings.ShortDateFormat, Val));
    RestoreFormatSettings;
  end else
    result := SaveNode(Lvl, NodeName, FormatDateTime('YYYY/MM/DD HH:NN:SS', Val)); }
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
{  if (RootOwner is TEpiDocument) then
  with TEpiDocument(RootOwner).XMLSettings do
  begin
    BackupFormatSettings(FormatSettings);
    result := SaveAttrRaw(AttrName, FloatToStr(Val));
    RestoreFormatSettings;
  end else
    result := SaveAttrRaw(AttrName, FloatToStr(Val));}
end;

function TEpiCustomBase.SaveAttr(const AttrName: string; const Val: TDateTime
  ): string;
begin
{  if (RootOwner is TEpiDocument) then
  with TEpiDocument(RootOwner).XMLSettings do
  begin
    BackupFormatSettings(FormatSettings);
    result := SaveAttr(AttrName, FormatDateTime(FormatSettings.ShortDateFormat, Val));
    RestoreFormatSettings;
  end else
    result := SaveAttrRaw(AttrName, FormatDateTime('YYYY/MM/DD HH:NN:SS', Val));    }
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
begin
  V := Byte(Value);
  SaveDomAttr(Node, Tag, GetEnumName(TypeInfo, V));
end;

procedure TEpiCustomBase.SaveDomAttr(const Node: TDomElement;
  const Tag: String; const Value: integer);
begin
  SaveDomAttr(Node, Tag, IntToStr(Value));
end;

procedure TEpiCustomBase.SaveDomAttr(const Node: TDomElement;
  const Tag: String; const Value: extended);
begin
{  if (RootOwner is TEpiDocument) then
  with TEpiDocument(RootOwner).XMLSettings do
  begin
    BackupFormatSettings(FormatSettings);
    SaveDomAttr(Node, Tag, FloatToStr(Value));
    RestoreFormatSettings;
  end else
    SaveDomAttr(Node, Tag, FloatToStr(Value));       }
end;

procedure TEpiCustomBase.SaveDomAttr(const Node: TDomElement;
  const Tag: String; const Value: TDateTime);
begin
{  if (RootOwner is TEpiDocument) then
  with TEpiDocument(RootOwner).XMLSettings do
  begin
    BackupFormatSettings(FormatSettings);
    SaveDomAttr(Node, Tag, FormatDateTime(FormatSettings.ShortDateFormat, Value));
    RestoreFormatSettings;
  end else
    SaveDomAttr(Node, Tag, FormatDateTime('YYYY/MM/DD HH:NN:SS', Value)); }
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
 { if (RootOwner is TEpiDocument) then
    BackupFormatSettings(TEpiDocument(RootOwner).XMLSettings.FormatSettings);

  SaveTextContent(RootNode, Tag, FloatToStr(Value));

  if (RootOwner is TEpiDocument) then
    RestoreFormatSettings;                }
end;

procedure TEpiCustomBase.SaveTextContent(const RootNode: TDOMElement;
  const Tag: String; const Value: TDateTime);
begin
{  if (RootOwner is TEpiDocument) then
  with TEpiDocument(RootOwner).XMLSettings do
  begin
    BackupFormatSettings(FormatSettings);
    SaveTextContent(RootNode, Tag, FormatDateTime(FormatSettings.ShortDateFormat, Value));
    RestoreFormatSettings;
  end else
    SaveTextContent(RootNode, Tag, FormatDateTime('YYYY/MM/DD HH:NN:SS', Value));}
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

end.

