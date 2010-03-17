unit epiadmin;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustomclass, episettings, DOM;

type
  TEpiAdmin = class;
  TEpiUsers = class;
  TEpiUser = class;
  TEpiGroups = class;
  TEpiGroup = class;

  TEpiAdminRight = (
    // Data access
    arCreate, arRead, arUpdate, arDelete, arVerify,
    // User/Admin access
    arStructure, arTranslate, arUsers, arPassword
  );
  TEpiAdminRights = set of TEpiAdminRight;

  TEpiAdminEventType = (
    // Generic event.
    aeUpdate,
    // User related events:
    aeUserAdd, aeUserRemove,
    aeUserSetId, aeUserSetLogin, aeUserSetName, aeUserSetPassword,
      aeUserSetGroup,
    // Group related events:
    aeAddGroup, aeRemoveGroup,
    aeGroupSetId, aeGroupSetName, aeGroupSetRights
    );

  TEpiAdminEvent = procedure(Sender: TObject; EventType: TEpiAdminEventType; Data: Pointer) of object;

  { TEpiCustomAdmin }

  TEpiCustomAdmin = class(TEpiCustomClass)
  private
    // OnChange-hook privates
    FOnChangeList: ^TEpiAdminEvent;
    FOnChangeListCount: Integer;
    FUpdateCount: Integer;
    procedure  DoChange(Event: TEpiAdminEventType; Data: Pointer);
  public
    // OnChange-hook methods
    procedure  BeginUpdate; virtual;
    procedure  EndUpdate; virtual;
    procedure  RegisterOnChangeHook(Event: TEpiAdminEvent); virtual;
    procedure  UnRegisterOnChangeHook(Event: TEpiAdminEvent); virtual;
  end;

  { TEpiAdmin }

  TEpiAdmin = class(TEpiCustomAdmin)
  private
    FGroups: TEpiGroups;
    FUsers: TEpiUsers;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  BeginUpdate; override;
    procedure  EndUpdate; override;
    procedure  SaveToStream(St: TStream; Lvl: integer);
    procedure  LoadFromXml(Root: TDOMNode; const Setting: TEpiSettings);
    Property   Users: TEpiUsers read FUsers;
    Property   Groups: TEpiGroups read FGroups;
  end;

  { TEpiUsers }

  TEpiUsers = class(TEpiCustomAdmin)
  private
    FAdmin: TEpiAdmin;
    FList: TFPList;
    function GetCount: integer;
    function GetUser(Index: integer): TEpiUser;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer);
    procedure  LoadFromXml(Root: TDOMNode; const Setting: TEpiSettings);
    Property   User[Index: integer]: TEpiUser read GetUser; default;
    Property   Count: integer read GetCount;
    Property   Admin: TEpiAdmin read FAdmin;
  end;

  { TEpiUser }

  TEpiUser = class(TEpiCustomAdmin)
  private
    FExpireDate: TDateTime;
    FGroup: TEpiGroup;
    FId: string;
    FLastLogin: TDateTime;
    FLogin: string;
    FName: string;
    FPassword: string;
    procedure SetExpireDate(const AValue: TDateTime);
    procedure SetGroup(const AValue: TEpiGroup);
    procedure SetId(const AValue: string);
    procedure SetLastLogin(const AValue: TDateTime);
    procedure SetLogin(const AValue: string);
    procedure SetName(const AValue: string);
    procedure SetPassword(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer);
    procedure  LoadFromXml(Root: TDOMNode; const Setting: TEpiSettings);
    // Unscrambles data:
    Property   Id: string read FId write SetId;
    Property   Login: string read FLogin write SetLogin;
    Property   Password: string read FPassword write SetPassword;
    // Scrambled data (in UserInfo section):
    Property   Name: string read FName write SetName;
    Property   Group: TEpiGroup read FGroup write SetGroup;
    Property   LastLogin: TDateTime read FLastLogin write SetLastLogin;
    property   ExpireDate: TDateTime read FExpireDate write SetExpireDate;
  end;

  { TEpiGroups }

  TEpiGroups = class
  private
    FAdmin: TEpiAdmin;
    FList: TFPList;
    function GetCount: integer;
    function GetGroup(Index: integer): TEpiGroup;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer);
    function   GroupById(const Id: string): TEpiGroup;
    Property   Group[Index: integer]: TEpiGroup read GetGroup; default;
    Property   Count: integer read GetCount;
    Property   Admin: TEpiAdmin read FAdmin;
  end;

  { TEpiGroup }

  TEpiGroup = class(TEpiCustomAdmin)
  private
    FId: string;
    FName: string;
    FRights: TEpiAdminRights;
    procedure SetId(const AValue: string);
    procedure SetName(const AValue: string);
    procedure SetRights(const AValue: TEpiAdminRights);
  public
    constructor Create;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer);
    Property   Id: string read FId write SetId;
    Property   Name: string read FName write SetName;
    Property   Rights: TEpiAdminRights read FRights write SetRights;
  end;

implementation

uses
  epistringutils, epidataglobals;

{ TEpiCustomAdmin }

procedure TEpiCustomAdmin.DoChange(Event: TEpiAdminEventType; Data: Pointer);
var
  i: Integer;
begin
  if FUpdateCount > 0 then exit;

  for i := 0 to FOnChangeListCount - 1 do
    FOnChangeList[i](Self, Event, Data);
end;

procedure TEpiCustomAdmin.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TEpiCustomAdmin.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount < 0) or (FUpdateCount > 0) then
  begin
    if (FUpdateCount < 0) then
      FUpdateCount := 0;
    exit;
  end;
  DoChange(aeUpdate, nil);
end;

procedure TEpiCustomAdmin.RegisterOnChangeHook(Event: TEpiAdminEvent);
begin
  Inc(FOnChangeListCount);
  ReAllocMem(FOnChangeList, FOnChangeListCount * SizeOf(TEpiAdminEvent));
  FOnChangeList[FOnChangeListCount-1] := Event
end;

procedure TEpiCustomAdmin.UnRegisterOnChangeHook(Event: TEpiAdminEvent);
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
    System.Move(FOnChangeList[Idx+1],FOnChangeList[Idx],(FOnChangeListCount-Idx)*SizeOf(TEpiAdminEvent));
  ReAllocMem(FOnChangeList, FOnChangeListCount*SizeOf(TEpiAdminEvent));
end;

{ TEpiAdmin }

constructor TEpiAdmin.Create;
begin
  FUsers := TEpiUsers.Create;
  FUsers.FAdmin := Self;

  FGroups := TEpiGroups.Create;
  FGroups.FAdmin := Self;
end;

destructor TEpiAdmin.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiAdmin.BeginUpdate;
var
  i: Integer;
begin
  inherited BeginUpdate;
  for i := 0 to Groups.Count -1 do
    Groups[i].BeginUpdate;
  for i := 0 to Users.Count -1 do
    Users[i].BeginUpdate;
end;

procedure TEpiAdmin.EndUpdate;
var
  i: Integer;
begin
  for i := 0 to Groups.Count -1 do
    Groups[i].EndUpdate;
  for i := 0 to Users.Count -1 do
    Users[i].EndUpdate;
  inherited EndUpdate;
end;

procedure TEpiAdmin.SaveToStream(St: TStream; Lvl: integer);
var
  TmpStr: String;
begin
  if Groups.Count = 0 then
    Exit;

  TmpStr :=
    Ins(Lvl) + '<Admin>' + LineEnding;
  St.Write(TmpStr[1], Length(TmpStr));

  Groups.SaveToStream(St, Lvl + 1);
  Users.SaveToStream(St, Lvl + 1);

  TmpStr :=
    Ins(Lvl) + '</Admin>' + LineEnding;
  St.Write(TmpStr[1], Length(TmpStr));
end;

procedure TEpiAdmin.LoadFromXml(Root: TDOMNode; const Setting: TEpiSettings);
var
  Node: TDOMNode;
begin
  // Root = <Admin>

  // First load user if scrambled (we need to obtain passwords)
  if Setting.Scrambled then
  begin
    Node := Root.FindNode('Users');
    Users.LoadFromXml(Node, Setting);
  end;



  // Load groups first
  Node := Root.FindNode('Groups');
  Users.LoadFromXml(Node, Setting);

  // Then load users (perhaps to complete user info).
  Node := Root.FindNode('Users');
  Users.LoadFromXml(Node, Setting);
end;

{ TEpiUsers }

function TEpiUsers.GetCount: integer;
begin
  result := FList.Count;
end;

function TEpiUsers.GetUser(Index: integer): TEpiUser;
begin
  result := TEpiUser(FList[Index]);
end;

constructor TEpiUsers.Create;
begin
  FList := TFPList.Create;
end;

destructor TEpiUsers.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TEpiUsers.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
  i: Integer;
begin
  if Count = 0 then
    exit;

  S :=
    Ins(Lvl) + '<Users>' + LineEnding;
  St.Write(S[1], Length(S));

  for i := 0 to Count - 1 do
    User[i].SaveToStream(St, Lvl + 1);

  S :=
    Ins(Lvl) + '</Users>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiUsers.LoadFromXml(Root: TDOMNode; const Setting: TEpiSettings);
var
  Node: TDOMNode;
  NewUser: TEpiUser;
begin
  // Root = <Users>

  // Load all basic user info before requesting user for login.

  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    // TODO : ErrorMessage
    if Node.CompareName('User') <> 0 then
      ReportXmlError(EPI_XML_TAG_MISSING, 0, '', []);

    NewUser := TEpiUser.Create;
    NewUser.Login := Node.FindNode('Login').TextContent;
    NewUser.Password := Node.FindNode('Password').TextContent;

    if not Setting.Scrambled then
    begin
      NewUser.Group := Admin.Groups.GroupById(UTF8Encode(Node.FindNode('GroupId').TextContent));
      NewUser.LastLogin := StrToDateTime(Node.FindNode('LastLogin').TextContent);
      NewUser.ExpireDate := StrToDateTime(Node.FindNode('ExpireDate').TextContent);
      NewUser.Name := UTF8Encode(Node.FindNode('Name').TextContent);
    end;

    Node := Node.NextSibling;
  end;

  if not Setting.Scrambled then exit;
end;

{ TEpiUser }

procedure TEpiUser.SetGroup(const AValue: TEpiGroup);
var
  Val: TEpiGroup;
begin
  if FGroup = AValue then exit;
  Val := FGroup;
  FGroup := AValue;
  DoChange(aeUserSetGroup, Val);
end;

procedure TEpiUser.SetExpireDate(const AValue: TDateTime);
begin
  if FExpireDate = AValue then exit;
  FExpireDate := AValue;
end;

procedure TEpiUser.SetId(const AValue: string);
var
  Val: String;
begin
  if FId = AValue then exit;
  Val := FId;
  FId := AValue;
  DoChange(aeUserSetId, @Val);
end;

procedure TEpiUser.SetLastLogin(const AValue: TDateTime);
begin
  if FLastLogin = AValue then exit;
  FLastLogin := AValue;
end;

procedure TEpiUser.SetLogin(const AValue: string);
var
  Val: String;
begin
  if FLogin = AValue then exit;
  Val := FLogin;
  FLogin := AValue;
  DoChange(aeUserSetLogin, @Val);
end;

procedure TEpiUser.SetName(const AValue: string);
var
  Val: String;
begin
  if FName = AValue then exit;
  Val := FName;
  FName := AValue;
  DoChange(aeUserSetName, @Val);
end;

procedure TEpiUser.SetPassword(const AValue: string);
var
  Val: String;
begin
  if FPassword = AValue then exit;
  Val := FPassword;
  FPassword := AValue;
  DoChange(aeUserSetPassword, @Val);
end;

constructor TEpiUser.Create;
begin
  FGroup := nil;
end;

destructor TEpiUser.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiUser.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
begin
  S :=
    Ins(Lvl)     + '<User id="' + Id + '">' +
    Ins(Lvl + 1) + '<Login>' + Login + '</Login>' + LineEnding +
    Ins(Lvl + 1) + '<Name>' + Name + '</Name>' + LineEnding +
    Ins(Lvl + 1) + '<Password>' + Password + '</Password>' + LineEnding +
    Ins(Lvl + 1) + '<GroupId>' + Group.Id + '</GroupId>' + LineEnding +
    Ins(Lvl)     + '</User>'  + LineEnding;
end;

procedure TEpiUser.LoadFromXml(Root: TDOMNode; const Setting: TEpiSettings);
begin

end;

{ TEpiGroups }

function TEpiGroups.GetCount: integer;
begin
  result := FList.Count;
end;

function TEpiGroups.GetGroup(Index: integer): TEpiGroup;
begin
  Result := TEpiGroup(FList[Index]);
end;

constructor TEpiGroups.Create;
begin
  FList := TFPList.Create;
end;

destructor TEpiGroups.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TEpiGroups.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
  i: Integer;
begin
  if Count = 0 then
    exit;

  S :=
    Ins(Lvl) + '<Groups>' + LineEnding;
  St.Write(S[1], Length(S));

  for i := 0 to Count - 1 do
    Group[i].SaveToStream(St, Lvl + 1);

  S :=
    Ins(Lvl) + '</Groups>' + LineEnding;
  St.Write(S[1], Length(S));
end;

function TEpiGroups.GroupById(const Id: string): TEpiGroup;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count -1 do
    if Group[i].Id = Id then
      result := Group[i];
end;

{ TEpiGroup }

procedure TEpiGroup.SetId(const AValue: string);
var
  Val: String;
begin
  if FId = AValue then exit;
  Val := FId;
  FId := AValue;
  DoChange(aeGroupSetId, @Val);
end;

procedure TEpiGroup.SetName(const AValue: string);
var
  Val: String;
begin
  if FName = AValue then exit;
  Val := FName;
  FName := AValue;
  DoChange(aeGroupSetName, @Val);
end;

procedure TEpiGroup.SetRights(const AValue: TEpiAdminRights);
var
  Val: TEpiAdminRights;
begin
  if FRights = AValue then exit;
  Val := FRights;
  FRights := AValue;
  DoChange(aeGroupSetRights, @Val);
end;

constructor TEpiGroup.Create;
begin

end;

destructor TEpiGroup.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiGroup.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
begin
  S :=
    Ins(Lvl) + '<Group id="' + Id + '">' + LineEnding +
    Ins(Lvl + 1) + '<Name>' + Name + '</Name>' + LineEnding +
//TODO : Save Group Rights...    Ins(Lvl + 1) + '<Rights>' + WriteStr(Rights) + '</Rights>' + LineEnding +
    Ins(Lvl) + '</Group>' + LineEnding;
  St.Write(S[1], Length(S));
end;

end.

