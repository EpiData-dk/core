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
    // User related events:
    eaceUserSetLogin,
    eaceUserSetPassword, eaceUserSetGroup,
    eaceUserSetExpireDate,eaceUserSetLastLogin,
    // Group related events:
    eaceGroupSetRights
  );

  TRequestPasswordEvent = procedure(Sender: TObject; var Login: string; var Password: string) of object;

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
    function   XMLName: string; override;
    function   SaveToXml(Content: String; Lvl: integer): string; override;
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
    function XMLName: string; override;
    function   GetUserByLogin(const Login: string): TEpiUser;
    procedure  LoadFromXml(Root: TDOMNode); override;
    procedure  PreLoadFromXml(Root: TDOMNode);
    function   NewUser: TEpiUser;
    Property   Users[Index: integer]: TEpiUser read GetUsers;
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
    FName: string;
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
    procedure SetName(const AValue: string);
    procedure SetPassword(const AValue: string);
  protected
    class function IdString: string; override;
    property  Salt: string read FSalt;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function XMLName: string; override;
    function   SaveToXml(Content: String; Lvl: integer): string; override;
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
    property   Name: string read FName write SetName;
  public
    Property   Id;
  end;

  { TEpiGroups }

  TEpiGroups = class(TEpiCustomList)
  private
    function    GetAdmin: TEpiAdmin;
    function    GetGroup(Index: integer): TEpiGroup;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    ScrambleXml: boolean; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    function    NewGroup: TEpiGroup;
    Property    Group[Index: integer]: TEpiGroup read GetGroup; default;
    Property    Admin: TEpiAdmin read GetAdmin;
  end;

  { TEpiGroup }

  TEpiGroup = class(TEpiCustomItem)
  private
    FName: TEpiTranslatedText;
    FRights: TEpiAdminRights;
    procedure SetRights(const AValue: TEpiAdminRights);
  protected
    class function IdString: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    function   SaveToXml(Content: String; Lvl: integer): string; override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    property   Name: TEpiTranslatedText read FName;
    Property   Rights: TEpiAdminRights read FRights write SetRights;
  end;

implementation

uses
  DCPsha1, DCPbase64, epistringutils,
  XMLRead, epidocument;

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
  result := TEpiDocument(Owner).Settings;
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

  RegisterClasses([Groups, Users]);
end;

destructor TEpiAdmin.Destroy;
begin
  FGroups.Free;
  FUsers.Free;
  inherited Destroy;
end;

function TEpiAdmin.XMLName: string;
begin
  Result := rsAdmin;
end;

function TEpiAdmin.SaveToXml(Content: String; Lvl: integer): string;
begin
  InitCrypt(MasterPassword);
  result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiAdmin.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
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
  result := Users.NewUser;
end;

function TEpiAdmin.NewGroup: TEpiGroup;
begin
  result := Groups.NewGroup;
end;

procedure TEpiAdmin.BeginUpdate;
begin
  inherited BeginUpdate;
end;

procedure TEpiAdmin.EndUpdate;
begin
  inherited EndUpdate;
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
  for i := 0 to Count - 1 do
  begin
    if Users[i].Login = Login then
    begin
      result := Users[i];
      exit;
    end;
  end;
end;

function TEpiUsers.XMLName: string;
begin
  result := rsUsers;
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
    CheckNode(Node, rsUser);

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
  Result := TEpiUser(NewItem(TEpiUser));
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
    CheckNode(Node, rsUser);

    NUser := GetUserByLogin(LoadNodeString(Node, rsLogin));
    if not Assigned(NUser) then
    begin
      NUser := NewUser;
      NUser.Login := LoadNodeString(Node, rsLogin);
      NUser.FPassword := LoadNodeString(Node, rsPassword);
      NUser.MasterPassword := LoadNodeString(Node, rsMasterPassword);
    end;
    NUser.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;
end;

{ TEpiUser }

procedure TEpiUser.SetGroup(const AValue: TEpiGroup);
var
  Val: TEpiGroup;
begin
  if FGroup = AValue then exit;
  Val := FGroup;
  FGroup := AValue;
  DoChange(eegAdmin, Word(eaceUserSetGroup), Val);
end;

procedure TEpiUser.SetExpireDate(const AValue: TDateTime);
var
  Val: TDateTime;
begin
  if FExpireDate = AValue then exit;
  Val := FExpireDate;
  FExpireDate := AValue;
  DoChange(eegAdmin, Word(eaceUserSetExpireDate), @Val);
end;

function TEpiUser.GetAdmin: TEpiAdmin;
begin
  result := TEpiAdmin(TEpiUsers(Owner).Owner);
end;

procedure TEpiUser.SetLastLogin(const AValue: TDateTime);
var
  Val: TDateTime;
begin
  if FLastLogin = AValue then exit;
  Val := FLastLogin;
  FLastLogin := AValue;
  DoChange(eegAdmin, Word(eaceUserSetLastLogin), @Val);
end;

procedure TEpiUser.SetLogin(const AValue: string);
var
  Val: String;
begin
  if FLogin = AValue then exit;
  Val := FLogin;
  FLogin := AValue;
  DoChange(eegAdmin, Word(eaceUserSetLogin), @Val);
end;

procedure TEpiUser.SetMasterPassword(const AValue: string);
begin
  if FMasterPassword = AValue then exit;
  FMasterPassword := AValue;
end;

procedure TEpiUser.SetName(const AValue: string);
begin
  if FName = AValue then exit;
  FName := AValue;
end;

procedure TEpiUser.SetPassword(const AValue: string);
var
  SaltInt: LongInt;
  SaltByte: array[0..3] of char absolute SaltInt;
begin
  SaltInt := (Random(maxLongint - 1) + 1) or $80000000;
  FSalt := String(SaltByte);

  // Sha1 the new password and Base64 it..
  FPassword := '$' + Base64EncodeStr(Salt) + '$' + GetSHA1Base64EncodedStr(Salt + AValue + Login);

  // Scramble master password with own key.
  InitCrypt(Salt + AValue + Login);
  MasterPassword := EnCrypt(Admin.MasterPassword);
  InitCrypt(Admin.MasterPassword);

  DoChange(eegAdmin, Word(eaceUserSetPassword), nil);
end;

class function TEpiUser.IdString: string;
begin
  Result := 'user_id_';
end;

constructor TEpiUser.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FGroup := nil;
end;

destructor TEpiUser.Destroy;
begin
  FLogin := '';
  FMasterPassword := '';
  FPassword := '';
  FSalt := '';
  inherited Destroy;
end;

function TEpiUser.XMLName: string;
begin
  result := rsUser;
end;

function TEpiUser.SaveToXml(Content: String; Lvl: integer): string;
var
  S: String;
begin
  Inc(Lvl);
  Content :=
    SaveNode(Lvl, rsLogin, Login) +
    SaveNode(Lvl, rsPassword, Password) +
    SaveNode(Lvl, rsMasterPassword, BoolToStr(Admin.Settings.Scrambled, MasterPassword, ''));

  // TODO : NAME MUST NOT BE SAVED UNSCRAMBLED!!!
  S :=
    SaveNode(Lvl, rsName, Name) +
    SaveNode(Lvl, rsGroupId, Group.Id) +
    SaveNode(Lvl, rsLastLogin, LastLogin) +
    SaveNode(Lvl, rsExpireDate, ExpireDate);
  if Admin.Settings.Scrambled then
    S := EnCrypt(S);

  Dec(Lvl);
  Result := inherited SaveToXml(Content + S, Lvl);
end;

procedure TEpiUser.LoadFromXml(Root: TDOMNode);
var
  NewRoot: TDOMNode;
begin
  // Root = <User>
  // Remember that login, password and masterpassword have already been
  // read by now... only scrambled things need to be obtained now.
  inherited LoadFromXml(Root);

  if Admin.Settings.Scrambled then
    NewRoot := DeCrypt(Root)
  else
    NewRoot := Root;

  Name       := LoadNodeString(NewRoot, rsName);
  LastLogin  := LoadNodeDateTime(NewRoot, rsLastLogin);
  ExpireDate := LoadNodeDateTime(NewRoot, rsExpireDate);
  Group      := TEpiGroup(Admin.Groups.GetItemById(LoadNodeString(NewRoot, rsGroupId)));

  if Admin.Settings.Scrambled then
    NewRoot.Free;
end;

{ TEpiGroups }

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
  inherited Create(AOwner);
end;

destructor TEpiGroups.Destroy;
begin
  inherited Destroy;
end;

function TEpiGroups.XMLName: string;
begin
  Result := rsGroups;
end;

function TEpiGroups.ScrambleXml: boolean;
begin
  Result := Admin.Settings.Scrambled;
end;

procedure TEpiGroups.LoadFromXml(Root: TDOMNode);
var
  NewRoot: TDOMNode;
  NGroup: TEpiGroup;
  Node: TDOMNode;
begin
  // Root = <Groups>

  // If file is scrambles, then we first need to descramble (using master password)
  // and then read xml structure.
  if Admin.Settings.Scrambled then
    NewRoot := DeCrypt(Root)
  else
    NewRoot := Root;

  Node := NewRoot.FirstChild;
  while Assigned(Node) do
  begin
    CheckNode(Node, rsGroup);

    NGroup := NewGroup;
    NGroup.LoadFromXml(Node);

    Node := Node.NextSibling;
  end;

  if Admin.Settings.Scrambled then
    NewRoot.Free;
end;

function TEpiGroups.NewGroup: TEpiGroup;
begin
  Result := TEpiGroup(NewItem(TEpiGroup));
end;

{ TEpiGroup }

procedure TEpiGroup.SetRights(const AValue: TEpiAdminRights);
var
  Val: TEpiAdminRights;
begin
  if FRights = AValue then exit;
  Val := FRights;
  FRights := AValue;
  DoChange(eegAdmin, Word(eaceGroupSetRights), @Val);
end;

class function TEpiGroup.IdString: string;
begin
  Result := 'group_id_';
end;

constructor TEpiGroup.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);

  FName := TEpiTranslatedText.Create(Self, rsName);
  RegisterClasses([Name]);
end;

destructor TEpiGroup.Destroy;
begin
  FName.Free;
  inherited Destroy;
end;

function TEpiGroup.XMLName: string;
begin
  Result := rsGroup;
end;

function TEpiGroup.SaveToXml(Content: String; Lvl: integer): string;
begin
  Content :=
    SaveNode(Lvl + 1, rsRights, LongInt(Rights));
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiGroup.LoadFromXml(Root: TDOMNode);
begin
  // Root = <Group>
  inherited LoadFromXml(Root);

  // If no name present, TEpiTranslatedText will take care of it.
  Name.LoadFromXml(Root);
  Rights := TEpiAdminRights(LoadNodeInt(Root, rsRights));
end;

end.

