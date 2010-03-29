unit epiadmin;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustomclass, episettings, DOM, epidatatypes;

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
    function   GetSettings: TEpiSettings;
    procedure  DoChange(Event: TEpiAdminEventType; Data: Pointer);
  protected
    property   Settings: TEpiSettings read GetSettings;
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
    FOnPassword: TRequestPasswordEvent;
    FUsers: TEpiUsers;
    function   RequestPassword: Boolean;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer);
    procedure  LoadFromXml(Root: TDOMNode); override;
    Property   Users: TEpiUsers read FUsers;
    Property   Groups: TEpiGroups read FGroups;
    property   OnPassword:  TRequestPasswordEvent read FOnPassword write FOnPassword;
  public
    // User / Group related functions.
    function   NewUser: TEpiUser;
    function   NewGroup: TEpiGroup;
  public
    // OnChange-hook methods
    procedure  BeginUpdate; override;
    procedure  EndUpdate; override;
  end;

  { TEpiUsers }

  TEpiUsers = class(TEpiCustomAdmin)
  private
    FAdmin: TEpiAdmin;
    FList: TFPList;
    function GetCount: integer;
    function GetUser(Index: integer): TEpiUser;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
    function   GetUserByLogin(const Login: string): TEpiUser;
    function   IndexOf(const Login: string): Integer;
    procedure  SaveToStream(St: TStream; Lvl: integer);
    procedure  PreLoadFromXml(Root: TDOMNode);
    procedure  LoadFromXml(Root: TDOMNode); override;
    procedure  AddUser(AUser: TEpiUser);
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
    // Master password as stored in file:
    // - Base64( AES ( ClearTextPassword ))
    FMasterPassword: string;
    FName: string;
    // Users password as stored in file:
    // - '$' + Base64(Salt) + '$' + Base64( SHA1 ( Salt + ClearTextPassword + Login ))
    FPassword: string;
    // a 4-byte string used for scrambling the password.
    // - is reset every time the user changes password (even if it is the same password).
    // - this gives approx. 2^32 different ways to store the same password.
    FSalt: string;
    function GetAdmin: TEpiAdmin;
    procedure SetExpireDate(const AValue: TDateTime);
    procedure SetGroup(const AValue: TEpiGroup);
    procedure SetId(const AValue: string);
    procedure SetLastLogin(const AValue: TDateTime);
    procedure SetLogin(const AValue: string);
    procedure SetMasterPassword(const AValue: string);
    procedure SetName(const AValue: string);
    procedure SetPassword(const AValue: string);
  protected
    property  Salt: string read FSalt;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer);
    procedure  LoadFromXml(Root: TDOMNode); override;
    property   Admin: TEpiAdmin read GetAdmin;
    // ====== DATA =======
    // Unscrambles data:
    Property   Id: string read FId write SetId;
    Property   Login: string read FLogin write SetLogin;
    Property   Password: string read FPassword write SetPassword;
    Property   MasterPassword: string read FMasterPassword write SetMasterPassword;
    // Scrambled data (in UserInfo section):
    Property   Name: string read FName write SetName;
    Property   Group: TEpiGroup read FGroup write SetGroup;
    Property   LastLogin: TDateTime read FLastLogin write SetLastLogin;
    property   ExpireDate: TDateTime read FExpireDate write SetExpireDate;
  end;

  { TEpiGroups }

  TEpiGroups = class(TEpiCustomAdmin)
  private
    FAdmin: TEpiAdmin;
    FList: TFPList;
    function GetCount: integer;
    function GetGroup(Index: integer): TEpiGroup;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer);
    procedure  LoadFromXml(Root: TDOMNode); override;
    function   GroupById(const Id: string): TEpiGroup;
    procedure  AddGroup(AGroup: TEpiGroup);
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
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer);
    procedure  LoadFromXml(Root: TDOMNode); override;
    Property   Id: string read FId write SetId;
    Property   Name: string read FName write SetName;
    Property   Rights: TEpiAdminRights read FRights write SetRights;
  end;

implementation

uses
  epistringutils, epidataglobals, DCPsha256, DCPsha1, DCPbase64, DCPrijndael,
  XMLRead, epidocument, epixmlutils;

type
  TEpiCustomAdminClass = class of TEpiCustomAdmin;

function GetSHA1Base64EncodedStr(const Key: string): string;
var
  Sha1: TDCP_sha1;
  Digest: string;
begin
  SetLength(Digest, 20);
  Sha1 := TDCP_sha1.Create(nil);
  Sha1.Init;
  Sha1.UpdateStr(Key);
  Sha1.Final(Digest[1]);
  result := Base64EncodeStr(Digest);
  Sha1.Free;
end;

{ TEpiCustomAdmin }

function TEpiCustomAdmin.GetSettings: TEpiSettings;
var
  Cl: TEpiCustomAdminClass;
begin
  if Self is TEpiAdmin then
    result := TEpiDocument(Owner).Settings;
  if (Self is TEpiUsers) or (Self is TEpiGroups) then
    result := TEpiDocument(TEpiAdmin(Owner).Owner).Settings;
end;

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

function TEpiAdmin.RequestPassword: Boolean;
var
  Login, Password: string;
  TheUser: TEpiUser;
begin
  result := false;

  if not Assigned(OnPassword) then exit;

  OnPassword(Self.Owner, Login, Password);

  TheUser := Users.GetUserByLogin(Login);
  if not Assigned(TheUser) then exit;

  result := '$' + Base64EncodeStr(TheUser.Salt) + '$' + GetSHA1Base64EncodedStr(TheUser.Salt + Password + Login) = TheUser.Password;
  if not result then exit;

  InitScrambler(TheUser.Salt + Password + Login);
  Settings.MasterPassword := DeScramble(TheUser.MasterPassword);

  InitScrambler(Settings.MasterPassword);
end;

constructor TEpiAdmin.Create(AOwner: TObject);
begin
  inherited;

  FUsers := TEpiUsers.Create(self);
  FUsers.FAdmin := Self;

  FGroups := TEpiGroups.Create(self);
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

  InitScrambler(Settings.MasterPassword);
  Groups.SaveToStream(St, Lvl + 1);
  Users.SaveToStream(St, Lvl + 1);

  TmpStr :=
    Ins(Lvl) + '</Admin>' + LineEnding;
  St.Write(TmpStr[1], Length(TmpStr));
end;

procedure TEpiAdmin.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  login, password: string;
  I: Integer;
begin
  // Root = <Admin>

  // First load user if scrambled (we need to obtain passwords)
  if Settings.Scrambled then
  begin
    Node := Root.FindNode('Users');
    Users.PreLoadFromXml(Node);

    I := 0;
    repeat
      inc(i);
    until RequestPassword or (I >= 3);
  end;

  // Load groups
  Node := Root.FindNode('Groups');
  Groups.LoadFromXml(Node);

  // Then load users (perhaps to complete user info).
  Node := Root.FindNode('Users');
  Users.LoadFromXml(Node);
end;

function TEpiAdmin.NewUser: TEpiUser;
begin
  result := TEpiUser.Create(Users);
  Users.AddUser(result);
end;

function TEpiAdmin.NewGroup: TEpiGroup;
begin
  result := TEpiGroup.Create(Groups);
  Groups.AddGroup(result);
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

constructor TEpiUsers.Create(AOwner: TObject);
begin
  inherited;
  FList := TFPList.Create;
end;

destructor TEpiUsers.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TEpiUsers.GetUserByLogin(const Login: string): TEpiUser;
var
  Idx: LongInt;
begin
  Result := nil;
  Idx := IndexOf(Login);
  if Idx >= 0 then
    Result := User[Idx];
end;

function TEpiUsers.IndexOf(const Login: string): Integer;
begin
  for result := 0 to Count - 1 do
  begin
    if User[result].Login = Login then
      exit;
  end;
  Result := -1;
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

procedure TEpiUsers.PreLoadFromXml(Root: TDOMNode);
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

    NewUser := TEpiUser.Create(Self);
    NewUser.Login := UTF8Encode(Node.FindNode('Login').TextContent);
    // Set password directly here, since the SetPassword method hash'es it and reencrypts the master password.
    NewUser.FPassword := Node.FindNode('Password').TextContent;
    NewUser.FSalt := Base64DecodeStr(ExtractStrBetween(NewUser.FPassword, '$', '$'));
    NewUser.MasterPassword := Node.FindNode('MasterPassword').TextContent;
    AddUser(NewUser);

    Node := Node.NextSibling;
  end;
end;

procedure TEpiUsers.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NewUser: TEpiUser;
begin
  // Root = <Users>
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    // TODO : ErrorMessage
    if Node.CompareName('User') <> 0 then
      ReportXmlError(EPI_XML_TAG_MISSING, 0, '', []);

    NewUser := GetUserByLogin(UTF8Encode(Node.FindNode('Login').TextContent));
    if not Assigned(NewUser) then
    begin
      NewUser := TEpiUser.Create(Self);
      NewUser.Login := UTF8Encode(Node.FindNode('Login').TextContent);
      NewUser.Password := UTF8Encode(Node.FindNode('Password').TextContent);
      NewUser.MasterPassword := UTF8Encode(Node.FindNode('MasterPassword').TextContent);
    end;
    NewUser.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;
end;

procedure TEpiUsers.AddUser(AUser: TEpiUser);
begin
  if not Assigned(AUser) then exit;

  FList.Add(AUser);
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

function TEpiUser.GetAdmin: TEpiAdmin;
begin
  result := TEpiAdmin(TEpiUsers(Owner).Owner);
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

procedure TEpiUser.SetMasterPassword(const AValue: string);
begin
  if FMasterPassword = AValue then exit;
  FMasterPassword := AValue;
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
  SaltInt: LongInt;
  SaltByte: array[0..3] of char absolute SaltInt;
begin
  SaltInt := (Random(maxLongint - 1) + 1) or $80000000;
  FSalt := String(SaltByte);

  // Sha1 the new password and Base64 it..
  FPassword := '$' + Base64EncodeStr(Salt) + '$' + GetSHA1Base64EncodedStr(Salt + AValue + Login);

  // Scramble master password with own key.
  InitScrambler(Salt + AValue + Login);
  MasterPassword := EnScramble(Admin.Settings.MasterPassword);
  InitScrambler(Admin.Settings.MasterPassword);

  DoChange(aeUserSetPassword, nil);
end;

constructor TEpiUser.Create(AOwner: TObject);
begin
  inherited;

  FGroup := nil;
end;

destructor TEpiUser.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiUser.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
  TmpSt: TStringStream;
begin
  S :=
    Ins(Lvl)     + '<User id="' + Id + '">' + LineEnding +
    Ins(Lvl + 1) + '<Login>' + Login + '</Login>' + LineEnding +
    Ins(Lvl + 1) + '<Password>' + Password + '</Password>' + LineEnding;
  St.Write(S[1], Length(S));

  if Admin.Settings.Scrambled then
  begin
    S :=Ins(Lvl + 1) + '<MasterPassword>' + MasterPassword + '</MasterPassword>' + LineEnding;
    St.Write(S[1], Length(S));
  end;

  S :=
    Ins(Lvl + 1) + '<Name>' + Name + '</Name>' + LineEnding +
    Ins(Lvl + 1) + '<GroupId>' + Group.Id + '</GroupId>' + LineEnding +
    Ins(Lvl + 1) + '<LastLogin>' + DateTimeToStr(LastLogin) + '</LastLogin>' + LineEnding +
    Ins(Lvl + 1) + '<ExpireDate>' + DateTimeToStr(ExpireDate) + '</ExpireDate>' + LineEnding;
  if Admin.Settings.Scrambled then
  begin
    TmpSt := TStringStream.Create(S);
    S := EnScramble(TmpSt) + LineEnding;
    St.Write(S[1], Length(S));
    TmpSt.Free;
  end;

  S :=
    Ins(Lvl) + '</User>'  + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiUser.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NewRoot: TDOMNode;
  s: String;
begin
  // Root = <User>
  // Remember that login, password and masterpassword have already been
  // read by now... only scrambled things need to be obtained now.

  Id := TDOMElement(Root).GetAttribute('id');

  if Admin.Settings.Scrambled then
    NewRoot := DeScramble(Root)
  else
    NewRoot := Root;

  // Name
  Node := NewRoot.FindNode('Name');
  Name := UTF8Encode(Node.TextContent);

  // Last login
  Node := NewRoot.FindNode('LastLogin');
  LastLogin := StrToDateTime(Node.TextContent);

  // Expire Date
  Node := NewRoot.FindNode('ExpireDate');
  ExpireDate := StrToDateTime(Node.TextContent);

  // Group
  Node := NewRoot.FindNode('GroupId');
  Group := Admin.Groups.GroupById(UTF8Encode(Node.TextContent));
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

constructor TEpiGroups.Create(AOwner: TObject);
begin
  inherited;

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
  TempSt: TStream;
begin
  if Count = 0 then
    exit;

  S :=
    Ins(Lvl) + '<Groups>' + LineEnding;
  St.Write(S[1], Length(S));

  if Admin.Settings.Scrambled then
    TempSt := TStringStream.Create('')
  else
    TempSt := St;

  for i := 0 to Count - 1 do
    Group[i].SaveToStream(TempSt, Lvl + 1);

  if Admin.Settings.Scrambled then
  begin
    S := EnScramble(TempSt) + LineEnding;
    St.Write(S[1], Length(S));
    TempSt.Free;
  end;

  S :=
    Ins(Lvl) + '</Groups>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiGroups.LoadFromXml(Root: TDOMNode);
var
  NewRoot: TDOMNode;
  NewGroup: TEpiGroup;
  Node: TDOMNode;
begin
  // Root = <Groups>

  // If file is scrambles, then we first need to descramble (using master password)
  // and then read xml structure.

  InitScrambler(Admin.Settings.MasterPassword);
  if Admin.Settings.Scrambled then
    NewRoot := DeScramble(Root)
  else
    NewRoot := Root;

  Node := NewRoot.FirstChild;
  while Assigned(Node) do
  begin
    if Node.CompareName('Group') <> 0 then
      ReportXmlError(EPI_XML_TAG_MISSING, 0, '', []); // TODO : Errormessage

    NewGroup := TEpiGroup.Create(Self);
    NewGroup.LoadFromXml(Node);
    AddGroup(NewGroup);

    Node := Node.NextSibling;
  end;

  if Settings.Scrambled then
    NewRoot.Free;
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

procedure TEpiGroups.AddGroup(AGroup: TEpiGroup);
begin
  if not Assigned(AGroup) then exit;

  FList.Add(AGroup);
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
    Ins(Lvl + 1) + '<Rights>' + IntToStr(LongInt(Rights)) + '</Rights>' + LineEnding +
    Ins(Lvl) + '</Group>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiGroup.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
begin
  // Root = <Group>
  Id := TDOMElement(Root).GetAttribute('id');

  // Name:
  Node := Root.FindNode('Name');
  Name := UTF8Encode(Node.TextContent);

  // Rights:
  Node := Root.FindNode('Rights');
  Rights := TEpiAdminRights(StrToInt(Node.TextContent));
end;

end.

