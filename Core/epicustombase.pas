unit epicustombase;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, DCPrijndael;

type
  TEpiCustomItem = class;
  TEpiCustomList = class;
  TEpiCustomBase = class;

  { TEpiCustomBase }
  // eeg = Epi Event Group
  TEpiEventGroup = (
    // epicustomcase.pas
    eegCustomBase,
    // epidocument.pas
    eegDocument,
    // episettings.pas
    eegSetting,
    // epiadmin.pas
    eegAdmin,
    // epistudy.pas
    eegStudy,
    // epivaluelabels.pas
    eegValueLabels
    );
  // ecce = Epi Custom Change Event
  TEpiCustomChangeEventType = (
    ecceUpdate, ecceId, ecceName, ecceAddItem, ecceDelItem, ecceSetItem
  );
  TEpiChangeEvent = procedure(Sender: TObject; EventGroup: Word; EventType: Word; Data: Pointer) of object;

{$static on}
  TEpiCustomBase = class
  { Commen data and methods }
  private
    FOwner:     TEpiCustomBase;
    FCrypter:   TDCP_rijndael; static;
    function    Get4ByteSalt: Integer;
  protected
    constructor Create(AOwner: TEpiCustomBase); virtual;

  { Scrambling }
    procedure   InitCrypt(Key: string);
    function    EnCrypt(Const S: string): string; overload;
    function    EnCrypt(Const St: TStream): string; overload;
    function    DeCrypt(Root: TDOMNode): TDOMNode; overload;
    function    DeCrypt(S: string): string; overload;
//    property    Crypter: TDCP_rijndael read FCrypter;   // DOES NOT WORK WITH FPC 2.4 - only from 2.5.1
    property    Owner: TEpiCustomBase read FOwner;

  { Save/Load functionality }
  private
    procedure  RaiseError(const Root: TDOMNode; NodeName: string);
  protected
    function   StringToXml(const S: string): string;
    Function   Ins(Level: integer): string;

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

    { Save Methods }
    procedure  SaveStream(const St: TStream; const Constent: string);
    // Save list of different containers
    procedure  SaveClasses(const St: TStream; const Lvl: integer;
      const Classes: array of TEpiCustomBase; const NodeName: string); overload;
    // Save a list container
    procedure  SaveList(const St: TStream; const Lvl: integer;
      const List: TEpiCustomList; const NodeName: string;
      EnCryptList: boolean = false); overload;

    // Section Nodes w/o Id's (does not XML'ify content)
    function   SaveSection(const Lvl: integer; const NodeName: string;
      const Content: string): string; overload;
    function   SaveSection(const Lvl: integer; const NodeName: string;
      const Id: string; const Content: string): string; overload;

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
    procedure  SaveToStream(St: TStream; Lvl: integer); virtual;
    procedure  LoadFromXml(Root: TDOMNode); virtual;

    { Change-event hooks }
  private
    FOnChangeList: ^TEpiChangeEvent;
    FOnChangeListCount: Integer;
    FUpdateCount: Integer;
  protected
    procedure  DoChange(EventGroup: Word; EventType: Word; Data: Pointer); virtual;
  public
    procedure  BeginUpdate; virtual;
    procedure  EndUpdate; virtual;
    procedure  RegisterOnChangeHook(Event: TEpiChangeEvent); virtual;
    procedure  UnRegisterOnChangeHook(Event: TEpiChangeEvent); virtual;
  end;
{$static off}

  { TEpiCustomItem }

  TEpiCustomItem = class(TEpiCustomBase)
  protected
    constructor Create(AOwner: TEpiCustomBase); override;
  protected
    FId: string;
    FName: string;
    function  GetId: string; virtual;
    function  GetName: string; virtual;
    procedure SetId(const AValue: string); virtual;
    procedure SetName(const AValue: string); virtual;
    property  Id: string read GetId write SetId;
    property  Name: string read GetName write SetName;
  end;

  { TEpiCustomList }

  TEpiCustomList = class(TEpiCustomBase)
  private
    FItemOwner: boolean;
    FList: TFPList;
    procedure   SetItemOwner(const AValue: boolean);
  protected
    constructor Create(AOwner: TEpiCustomBase); override;
    function    GetCount: Integer; virtual;
    function    GetItems(Index: integer): TEpiCustomItem; virtual;
    procedure   SetItems(Index: integer; const AValue: TEpiCustomItem); virtual;
    property    List: TFPList read FList;
  public
    destructor  Destroy; override;
    procedure   AddItem(Item: TEpiCustomItem); virtual;
    procedure   RemoveItem(Item: TEpiCustomItem); virtual;
    procedure   DeleteItem(Index: integer); virtual;
    function    GetItemById(Id: string): TEpiCustomItem; virtual;
    function    GetItemByName(Name: string): TEpiCustomItem; virtual;
    function    IndexOf(Item: TEpiCustomItem): integer; virtual;
    property    Count: Integer read GetCount;
    property    Items[Index: integer]: TEpiCustomItem read GetItems write SetItems; default;
    property    ItemOwner: boolean read FItemOwner write SetItemOwner;

  { Change-event hooks overrides}
  public
    procedure  BeginUpdate; override;
    procedure  EndUpdate; override;
  end;

{$I epixmlconstants.inc}

implementation

uses
  StrUtils, DCPsha256, XMLRead;

{ TEpiCustomBase }

function TEpiCustomBase.Get4ByteSalt: Integer;
begin
  result := Random(maxLongint - 1) + 1;
end;

constructor TEpiCustomBase.Create(AOwner: TEpiCustomBase);
begin
  FOwner := AOwner;
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

function TEpiCustomBase.EnCrypt(const St: TStream): string;
var
  TmpSt: TStringStream;
  Salt: Integer;
  SaltStr: Array[0..3] of Char absolute Salt;
begin
  Salt := Get4ByteSalt;

  St.Position := 0;
  TmpSt := TStringStream.Create(String(SaltStr));
  TmpSt.Position := TmpSt.Size;
  TmpSt.CopyFrom(St, St.Size);
  TmpSt.Position := 0;

  Result := FCrypter.EncryptString(TStringStream(TmpSt).DataString);
  FCrypter.Reset;
  TmpSt.Free;
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

function TEpiCustomBase.Ins(Level: integer): string;
begin
  result := DupeString(' ', Level);
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

procedure TEpiCustomBase.SaveStream(const St: TStream; const Constent: string);
begin
  St.Write(Constent[1], Length(Constent));
end;

procedure TEpiCustomBase.SaveClasses(const St: TStream; const Lvl: integer;
  const Classes: array of TEpiCustomBase; const NodeName: string);
var
  i: Integer;
begin
  SaveStream(St, Ins(Lvl) + '<' + NodeName + '>' + LineEnding);
  for i := Low(Classes) to High(Classes) do
    Classes[i].SaveToStream(St, Lvl + 1);
  SaveStream(St, Ins(Lvl) + '</' + NodeName + '>' + LineEnding);
end;

procedure TEpiCustomBase.SaveList(const St: TStream; const Lvl: integer;
  const List: TEpiCustomList; const NodeName: string; EnCryptList: boolean);
var
  i: Integer;
  TmpSt: TStream;
  S: String;
begin
  SaveStream(St, Ins(Lvl) + '<' + NodeName + '>' + LineEnding);

  if EncryptList then
    TmpSt := TMemoryStream.Create
  else
    TmpSt := St;

  for i := 0 to List.Count - 1 do
    List.Items[i].SaveToStream(TmpSt, Lvl + 1);

  if EncryptList then
  begin
    S := EnCrypt(TmpSt);
    SaveStream(St, S);
    TmpSt.Free;
  end;

  SaveStream(St, Ins(Lvl) + '</' + NodeName + '>' + LineEnding);
end;

function TEpiCustomBase.SaveSection(const Lvl: integer; const NodeName: string;
  const Content: string): string;
begin
  Result :=
    Ins(Lvl) + '<' + NodeName + '>' + LineEnding +
       Content +
    Ins(Lvl) + '</' + NodeName + '>' + LineEnding;
end;

function TEpiCustomBase.SaveSection(const Lvl: integer; const NodeName: string;
  const Id: string; const Content: string): string;
begin
  Result :=
    Ins(Lvl) + '<' + NodeName + ' id="' + Id + '">' + LineEnding +
       Content +
    Ins(Lvl) + '</' + NodeName + '>' + LineEnding;
end;

function TEpiCustomBase.SaveNode(const Lvl: integer; const NodeName: string;
  const Val: string): string;
begin
  Result :=
    Ins(Lvl) + '<' + NodeName + '>' + StringToXml(Val) + '</' + NodeName + '>' +
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
  result := SaveNode(Lvl, NodeName, DateTimeToStr(Val));
end;

function TEpiCustomBase.SaveNode(const Lvl: integer; const NodeName: string;
  const Val: boolean): string;
begin
  result := SaveNode(Lvl, NodeName, BoolToStr(Val, 'true', 'false'));
end;

procedure TEpiCustomBase.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
begin
  // Base template - should be overridden in descendants.
  S := Ins(LvL) + '<' + ClassName + '>Not Implemented Yet</' + ClassName + '>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiCustomBase.LoadFromXml(Root: TDOMNode);
begin
  // Do nothing - should be overridden in descendants.
end;

procedure TEpiCustomBase.DoChange(EventGroup: word; EventType: word; Data: Pointer);
var
  i: Integer;
begin
  if FUpdateCount > 0 then exit;

  for i := 0 to FOnChangeListCount - 1 do
    FOnChangeList[i](Self, EventGroup, EventType, Data);
end;

procedure TEpiCustomBase.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TEpiCustomBase.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount < 0) or (FUpdateCount > 0) then
  begin
    if (FUpdateCount < 0) then
      FUpdateCount := 0;
    exit;
  end;
  DoChange(word(eegCustomBase), word(ecceUpdate), nil);
end;

procedure TEpiCustomBase.RegisterOnChangeHook(Event: TEpiChangeEvent);
begin
  Inc(FOnChangeListCount);
  ReAllocMem(FOnChangeList, FOnChangeListCount * SizeOf(TEpiChangeEvent));
  FOnChangeList[FOnChangeListCount-1] := Event
end;

procedure TEpiCustomBase.UnRegisterOnChangeHook(Event: TEpiChangeEvent);
var
  Idx: LongInt;
begin
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

{ TEpiCustomItem }

constructor TEpiCustomItem.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

function TEpiCustomItem.GetId: string;
begin
  result := FId;
end;

function TEpiCustomItem.GetName: string;
begin
  result := FName;
end;

procedure TEpiCustomItem.SetId(const AValue: string);
var
  Val: String;
begin
  if FId = AValue then exit;
  Val := FId;
  FId := AValue;
  DoChange(Word(eegCustomBase), Word(ecceId), @Val);
end;

procedure TEpiCustomItem.SetName(const AValue: string);
var
  Val: String;
begin
  if FName = AValue then exit;
  Val := FName;
  FName := AValue;
  DoChange(Word(eegCustomBase), Word(ecceName), @Val);
end;

procedure TEpiCustomList.SetItemOwner(const AValue: boolean);
begin
  if FItemOwner = AValue then exit;
  FItemOwner := AValue;
end;

{ TEpiCustomList }

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
  if FList[Index] = Pointer(AValue) then exit;
  Val := FList[Index];
  FList[Index] := AValue;
  DoChange(Word(eegCustomBase), Word(ecceSetItem), Val);
end;

destructor TEpiCustomList.Destroy;
var
  F: TEpiCustomItem;
begin
  while FList.Count > 0 do
  begin
    if ItemOwner then
    begin
      F := TEpiCustomItem(FList.Last);
      FreeAndNil(F);
    end;
    FList.Delete(FList.Count - 1);
  end;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TEpiCustomList.AddItem(Item: TEpiCustomItem);
begin
  FList.Add(Item);
  DoChange(Word(eegCustomBase), Word(ecceAddItem), nil);
end;

procedure TEpiCustomList.RemoveItem(Item: TEpiCustomItem);
begin
  FList.Remove(Item);
  DoChange(Word(eegCustomBase), Word(ecceDelItem), Item);
end;

procedure TEpiCustomList.DeleteItem(Index: integer);
var
  Val: Pointer;
begin
  Val := FList[Index];
  FList.Delete(Index);
  DoChange(Word(eegCustomBase), Word(ecceDelItem), Val);
end;

function TEpiCustomList.GetItemById(Id: string): TEpiCustomItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if TEpiCustomItem(FList[i]).Id = Id then
    begin
      Result := TEpiCustomItem(FList[i]);
      Exit;
    end;
  end;
end;

function TEpiCustomList.GetItemByName(Name: string): TEpiCustomItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if TEpiCustomItem(FList[i]).Name = Name then
    begin
      Result := TEpiCustomItem(FList[i]);
      Exit;
    end;
  end;
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

end.

