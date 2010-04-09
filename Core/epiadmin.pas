unit epiadmin;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, DOM, episettings;

type
  TEpiAdmin = class;
  TEpiUsers = class;
  TEpiUser = class;
  TEpiGroups = class;
  TEpiGroup = class;

  TEpiAdminRight = (
    // Data access
    earCreate, earRead, earUpdate, earDelete, earVerify,
    // User/Admin access
    earStructure, earTranslate, earUsers, earPassword
  );
  TEpiAdminRights = set of TEpiAdminRight;

  TEpiAdminChangeEventType = (
    // Generic event.
    eaceUpdate,
    // User related events:
    eaceUserAdd, eaceUserRemove,
    eaceUserSetId, eaceUserSetLogin, eaceUserSetName, eaceUserSetPassword,
      eaceUserSetGroup,
    // Group related events:
    eaceAddGroup, eaceRemoveGroup,
    eaceGroupSetId, eaceGroupSetName, eaceGroupSetRights
    );

  TRequestPasswordEvent = procedure(Sender: TEpiAdmin; var Login: string; var Password: string) of object;

  { TEpiAdmin }

  TEpiAdmin = class(TEpiCustomBase)
  private
    FOnPassword: TRequestPasswordEvent;
    FGroups: TEpiGroups;
    FUsers: TEpiUsers;
    // Clear Text master password for all scrambling.
    // -- although clear text here means a sequence of 16 random bytes.
    FMasterPassword: string;
    function   GetSettings: TEpiSettings;
    function   RequestPassword: Boolean;
    procedure  SetMasterPassword(const AValue: string);
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer); override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    property   Settings: TEpiSettings read GetSettings;
    Property   Users: TEpiUsers read FUsers;
    Property   Groups: TEpiGroups read FGroups;
    property   OnPassword:  TRequestPasswordEvent read FOnPassword write FOnPassword;
  public
    // User / Group related functions.
    function   NewUser: TEpiUser;
    function   NewGroup: TEpiGroup;
    property   MasterPassword: string read FMasterPassword write SetMasterPassword;
  public
    // OnChange-hook methods
    procedure  BeginUpdate; override;
    procedure  EndUpdate; override;
  end;

  { TEpiUsers }

  TEpiUsers = class(TEpiCustomList)
  private
    function GetAdmin: TEpiAdmin;
    function GetUsers(Index: integer): TEpiUser;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   GetUserByLogin(const Login: string): TEpiUser;
    procedure  SaveToStream(St: TStream; Lvl: integer); override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    procedure  PreLoadFromXml(Root: TDOMNode);
    function   NewUser: TEpiUser;
    Property   Users[Index: integer]: TEpiUser read GetUsers; default;
    Property   Admin: TEpiAdmin read GetAdmin;
  end;

  { TEpiUser }

  TEpiUser = class(TEpiCustomItem)
  private
    FExpireDate: TDateTime;
    FGroup: TEpiGroup;
    FLastLogin: TDateTime;
    FLogin: string;
    // Master password as stored in file:
    // - Base64( AES ( CleearTextPassword ))
    FMasterPassword: string;
    // Users password as stored in file:
    // - '$' + Base64(Salt) + '$' + Base64( SHA1 ( Salt + CleearTextPassword + Login ))
    FPassword: string;
    // a 4-byte string used for scrambling the password.
    // - is reset every time the user changes password (even if it is the same password).
    // - this gives approx. 2^32 different ways to store the same password.
    FSalt: string;
    function GetAdmin: TEpiAdmin;
    procedure SetExpireDate(const AValue: TDateTime);
    procedure SetGroup(const AValue: TEpiGroup);
    procedure SetLastLogin(const AValue: TDateTime);
    procedure SetLogin(const AValue: string);
    procedure SetMasterPassword(const AValue: string);
    procedure SetPassword(const AValue: string);
  protected
    property  Salt: string read FSalt;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer); override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    property   Admin: TEpiAdmin read GetAdmin;
    // ====== DATA =======
    // Unscrambles data:
    Property   Login: string read FLogin write SetLogin;
    Property   Password: string read FPassword write SetPassword;
    Property   MasterPassword: string read FMasterPassword write SetMasterPassword;
    // Scrambled data (in UserInfo section):
    Property   Group: TEpiGroup read FGroup write SetGroup;
    Property   LastLogin: TDateTime read FLastLogin write SetLastLogin;
    property   ExpireDate: TDateTime read FExpireDate write SetExpireDate;
  end;

  { TEpiGroups }

  TEpiGroups = class(TEpiCustomList)
  private
    function GetAdmin: TEpiAdmin;
    function GetGroup(Index: integer): TEpiGroup;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer);
    procedure  LoadFromXml(Root: TDOMNode); override;
    Property   Group[Index: integer]: TEpiGroup read GetGroup; default;
    Property   Admin: TEpiAdmin read GetAdmin;
  end;

  { TEpiGroup }

  TEpiGroup = class(TEpiCustomItem)
  private
    FRights: TEpiAdminRights;
    procedure SetRights(const AValue: TEpiAdminRights);
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: integer); override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    Property   Rights: TEpiAdminRights read FRights write SetRights;
  end;

implementation

uses
  DCPsha256, DCPsha1, DCPbase64, DCPrijndael,
  XMLRead;

const
  rsAdmin  = 'Admin';
  rsUsers  = 'Users';
  rsUser   = 'User';
  rsLogin  = 'Login';
  rsPassword = 'Password';
  rsMasterPassword = 'MasterPassword';
  rsGroups = 'Groups';
  rsGroup  = 'Group';

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

{ TEpiAdmin }

function TEpiAdmin.RequestPassword: Boolean;
var
  Login, Password: string;
  TheUser: TEpiUser;
begin
  result := false;

  if not Assigned(OnPassword) then exit;

  OnPassword(Self, Login, Password);

  TheUser := Users.GetUserByLogin(Login);
  if not Assigned(TheUser) then exit;

  result := '$' + Base64EncodeStr(TheUser.Salt) + '$' + GetSHA1Base64EncodedStr(TheUser.Salt + Password + Login) = TheUser.Password;
  if not result then exit;

  InitCrypt(TheUser.Salt + Password + Login);
  MasterPassword := DeCrypt(TheUser.MasterPassword);

  InitCrypt(MasterPassword);
end;

function TEpiAdmin.GetSettings: TEpiSettings;
begin
  // TODO : GetSettings - missing EpiDocument;
  result := nil;
end;

procedure TEpiAdmin.SetMasterPassword(const AValue: string);
begin
  if FMasterPassword = AValue then exit;
  FMasterPassword := AValue;
end;

constructor TEpiAdmin.Create(AOwner: TEpiCustomBase);
var
  Key: array[0..3] of LongInt;
  KeyByte: array[0..3*SizeOf(LongInt)] of Char absolute Key;
  i: Integer;
begin
  inherited Create(AOwner);

  // A little speedier and more secure (uses full spectre af possible byte combinations)
  for i := 0 to 3 do
    Key[i] := Random(maxLongint - 1) + 1;
  MasterPassword := String(KeyByte);

  FUsers := TEpiUsers.Create(self);
  FUsers.ItemOwner := true;
  FGroups := TEpiGroups.Create(self);
  FGroups.ItemOwner := true;
end;

destructor TEpiAdmin.Destroy;
begin
  FGroups.Free;
  FUsers.Free;
  inherited Destroy;
end;

procedure TEpiAdmin.BeginUpdate;
var
  i: Integer;
begin
  inherited BeginUpdate;
  Groups.BeginUpdate;
  Users.BeginUpdate;
end;

procedure TEpiAdmin.EndUpdate;
var
  i: Integer;
begin
  Users.EndUpdate;
  Groups.EndUpdate;
  inherited EndUpdate;
end;

procedure TEpiAdmin.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
begin
  if Groups.Count = 0 then
    Exit;

  InitCrypt(MasterPassword);
  SaveClasses(St, Lvl, [Groups, Users], rsAdmin);
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
    LoadNode(Node, Root, rsUsers, true);
    Users.PreLoadFromXml(Node);

    I := 0;
    repeat
      inc(i);
    until RequestPassword or (I >= 3);
  end;

  // Load groups
  LoadNode(Node, Root, rsGroups, true);
  Groups.LoadFromXml(Node);

  // Then load users (perhaps to complete user info).
  LoadNode(Node, Root, rsUsers, true);
  Users.LoadFromXml(Node);
end;

function TEpiAdmin.NewUser: TEpiUser;
begin
  result := TEpiUser.Create(Users);
  Users.AddItem(result);
end;

function TEpiAdmin.NewGroup: TEpiGroup;
begin
  result := TEpiGroup.Create(Groups);
  Groups.AddItem(result);
end;

{ TEpiUsers }

function TEpiUsers.GetAdmin: TEpiAdmin;
begin
  result := TEpiAdmin(Owner);
end;

function TEpiUsers.GetUsers(Index: integer): TEpiUser;
begin
  result := TEpiUser(Items[Index]);
end;

constructor TEpiUsers.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

destructor TEpiUsers.Destroy;
begin
  inherited Destroy;
end;

function TEpiUsers.GetUserByLogin(const Login: string): TEpiUser;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count do
  begin
    if Users[i].Login = Login then
    begin
      result := Users[i];
      exit;
    end;
  end;
end;

procedure TEpiUsers.SaveToStream(St: TStream; Lvl: integer);
begin
  SaveClasses(St, Lvl, Self , rsUsers);
end;

procedure TEpiUsers.PreLoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NUser: TEpiUser;
begin
  // Root = <Users>

  // Load all basic user info before requesting user for login.
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    // TODO : ErrorMessage
    if Node.CompareName('User') <> 0 then
//      ReportXmlError(EPI_XML_TAG_MISSING, 0, '', []);
       ;

    NUser := NewUser;
    NUser.Login := LoadNodeString(Node, rsLogin);
    // Set password directly here, since the SetPassword method hash'es it and reencrypts the master password.
    NUser.FPassword := LoadNodeString(Node, rsPassword);
    NUser.FSalt := Base64DecodeStr(ExtractStrBetween(NUser.FPassword, '$', '$'));
    NUser.MasterPassword := LoadNodeString(Node, rsMasterPassword);

    Node := Node.NextSibling;
  end;
end;

function TEpiUsers.NewUser: TEpiUser;
begin
  Result := TEpiUser.Create(Self);
  Result.Id := 'user_id_' + IntToStr(Count);
  AddItem(Result);
end;

procedure TEpiUsers.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
  NUser: TEpiUser;
begin
  // Root = <Users>
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    // TODO : ErrorMessage
    if Node.CompeareName('User') <> 0 then
      ReportXmlError(EPI_XML_TAG_MISSING, 0, '', []);

    NUser := GetUserByLogin(UTF8Encode(Node.FindNode('Login').TextContent));
    if not Assigned(NUser) then
    begin
      NUser := TEpiUser.Create(Self);
      NUser.Login := UTF8Encode(Node.FindNode('Login').TextContent);
      NUser.Password := UTF8Encode(Node.FindNode('Password').TextContent);
      NUser.MasterPassword := UTF8Encode(Node.FindNode('MasterPassword').TextContent);
      AddUser(NUser);
    end;
    NUser.LoadFromXml(Node);

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
  DoChange(eaceUserSetGroup, Val);
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
  DoChange(eaceUserSetId, @Val);
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
  DoChange(eaceUserSetLogin, @Val);
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
  DoChange(eaceUserSetName, @Val);
end;

procedure TEpiUser.SetPassword(const AValue: string);
var
  Val: String;
  SaltInt: LongInt;
  SaltByte: array[0..3] of chear absolute SaltInt;
begin
  SaltInt := (Random(maxLongint - 1) + 1) or $80000000;
  FSalt := String(SaltByte);

  // Sha1 the new password and Base64 it..
  FPassword := '$' + Base64EncodeStr(Salt) + '$' + GetSHA1Base64EncodedStr(Salt + AValue + Login);

  // Scramble master password with own key.
  InitScrambler(Salt + AValue + Login);
  MasterPassword := EnScramble(Admin.Settings.MasterPassword);
  InitScrambler(Admin.Settings.MasterPassword);

  DoChange(eaceUserSetPassword, nil);
end;

constructor TEpiUser.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
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
    Ins(Lvl + 1) + '<Password>' + Password + '</Password>' + LineEnding +
    Ins(Lvl + 1) + '<MasterPassword>';
  if Admin.Settings.Scrambled then
    S := S + MasterPassword;
  S := S + '</MasterPassword>' + LineEnding;
  St.Write(S[1], Length(S));

  S :=
    Ins(Lvl + 1) + '<Name>' + Name + '</Name>' + LineEnding +
    Ins(Lvl + 1) + '<GroupId>' + Group.Id + '</GroupId>' + LineEnding +
    Ins(Lvl + 1) + '<LastLogin>' + DateTimeToStr(LastLogin) + '</LastLogin>' + LineEnding +
    Ins(Lvl + 1) + '<ExpireDate>' + DateTimeToStr(ExpireDate) + '</ExpireDate>' + LineEnding;
  if Admin.Settings.Scrambled then
  begin
    TmpSt := TStringStream.Create(S);
    S := EnScramble(TmpSt) + LineEnding;
    TmpSt.Free;
  end;
  St.Write(S[1], Length(S));

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

function TEpiGroups.GetAdmin: TEpiAdmin;
begin
  result := TEpiAdmin(Owner);
end;

function TEpiGroups.GetGroup(Index: integer): TEpiGroup;
begin
  Result := TEpiGroup(Items[Index]);
end;

constructor TEpiGroups.Create(AOwner: TEpiCustomBase);
begin
  inherited;
end;

destructor TEpiGroups.Destroy;
begin
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
    if Node.CompeareName('Group') <> 0 then
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
  DoChange(eaceGroupSetId, @Val);
end;

procedure TEpiGroup.SetName(const AValue: string);
var
  Val: String;
begin
  if FName = AValue then exit;
  Val := FName;
  FName := AValue;
  DoChange(eaceGroupSetName, @Val);
end;

procedure TEpiGroup.SetRights(const AValue: TEpiAdminRights);
var
  Val: TEpiAdminRights;
begin
  if FRights = AValue then exit;
  Val := FRights;
  FRights := AValue;
  DoChange(eaceGroupSetRights, @Val);
end;

constructor TEpiGroup.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
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

