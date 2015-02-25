unit epiadmin;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, Laz2_DOM, DCPrijndael;

type
  TEpiAdmin = class;
  TEpiUsers = class;
  TEpiUser = class;
  TEpiGroups = class;
  TEpiGroup = class;

  EEpiBadPassword = class(Exception);

  TEpiManagerRight = (
    // User/Admin access
    earStructure, earTranslate, earUsers, earPassword
  );
  TEpiManagerRights = set of TEpiManagerRight;

const
  EpiManagerRightCaptions: array[TEpiManagerRight] of string =
    (
      'Edit Struncture',
      'Translate project',
      'Manage users',
      'Reset password'
    );


type
  TEpiAdminChangeEventType = (
    // User related events:
    eaceUserSetFullName,
    eaceUserSetPassword,   eaceUserSetGroup,
    eaceUserSetExpireDate, eaceUserSetLastLogin,
    // Group related events:
    eaceGroupSetManageRights
  );

  TEpiRequestPasswordType = (
    erpSinglePassword,          // Only a valid password is required - login is ignored.
    erpUserLogin                // Project is managed by a user/password and both are requred.
  );

  TEpiRequestPasswordResponse = (
     rprAskOnFail,              // If login/password failed send request again.
     rprStopOnFail              // If login/password failed stop loading.
  );

  TRequestPasswordEvent = function(
    Sender: TObject;
    RequestType: TEpiRequestPasswordType;
    RequestNo: Integer;
    var Login: string;
    var Password: string
  ): TEpiRequestPasswordResponse of object;

  { TEpiAdmin }

  TEpiAdmin = class(TEpiCustomBase)
  private
    FGroups: TEpiGroups;
    FOnPassWord: TRequestPasswordEvent;
    FUsers: TEpiUsers;
    // Clear Text master password for all scrambling.
    // -- although clear text here means a sequence of 16 random bytes.
    FMasterPassword: string;
    function   DoRequestPassword: Boolean;
    procedure  SetMasterPassword(const AValue: string);

  { Encrypt / Decrypt methods for user handling }
  private
    FCrypter: TDCP_rijndael;
  protected
    function   Encrypt(Const Key, Data: String): String;  //encrypts a value with given key, using the Rijndael crytp engine.
    function   Decrypt(Const Key, Data: String): String;  //decrypts a value with given key, using the Rijndael crytp engine.
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    Property   Users: TEpiUsers read FUsers;
    Property   Groups: TEpiGroups read FGroups;
  public
    // User / Group related functions.
    function   NewUser: TEpiUser;
    function   NewGroup: TEpiGroup;
    function   RequestPassword(Const RepeatCount: Byte): Boolean;
    property   MasterPassword: string read FMasterPassword write SetMasterPassword;
    property   OnPassWord: TRequestPasswordEvent read FOnPassWord write FOnPassWord;
  public
    // OnChange-hook methods
    procedure  BeginUpdate; override;
    procedure  EndUpdate; override;
  {Cloning}
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  end;

  TEpiUsersEnumerator = class;

  { TEpiUsers }

  TEpiUsers = class(TEpiCustomList)
  private
    function GetAdmin: TEpiAdmin;
    function GetUsers(Index: integer): TEpiUser;
  protected
    function Prefix: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    function   GetUserByLogin(const Login: string): TEpiUser;
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function   PreLoadFromXml(Root: TDOMNode): Boolean;
    procedure  PreSaveToDom(RootDoc: TDOMDocument; Root: TDOMNode);
    function   NewUser: TEpiUser;
    function   GetEnumerator: TEpiUsersEnumerator;
    Property   Users[Index: integer]: TEpiUser read GetUsers; default;
    Property   Admin: TEpiAdmin read GetAdmin;
  end;

  { TEpiUser }

  TEpiUser = class(TEpiCustomItem)
  private
    FGroups: TEpiGroups;
    FExpireDate: TDateTime;
    FLastLogin: TDateTime;
    FFullName: string;
    // Master password as stored in file:
    // - Base64( AES ( CleearTextPassword ))
    FMasterPassword: string;
    // Users password as stored in file:
    // - '$' + Base64(Salt) + '$' + Base64( SHA1 ( Salt + ClearTextPassword + Login ))
    FPassword: string;
    // a 4-byte string used for scrambling the password.
    // - is reset every time the user changes password (even if it is the same password).
    // - this gives approx. 2^32 different ways to store the same password.
    FSalt: string;
    FLogin: string;
    function GetAdmin: TEpiAdmin;
    function GetLogin: string;
    procedure SetExpireDate(const AValue: TDateTime);
    procedure SetFullName(const AValue: string);
    procedure SetLastLogin(const AValue: TDateTime);
    procedure SetLogin(AValue: string);
    procedure SetMasterPassword(const AValue: string);
    procedure SetPassword(const AValue: string);
  protected
    property  Salt: string read FSalt;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
    procedure FixupReferences(EpiClassType: TEpiCustomBaseClass;
      ReferenceType: Byte; const ReferenceId: string); override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function   SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
    property   Admin: TEpiAdmin read GetAdmin;
    // ====== DATA =======
    // Unscrambled data:
    Property   Login: string read GetLogin write SetLogin;
    Property   Password: string read FPassword write SetPassword;
    Property   MasterPassword: string read FMasterPassword write SetMasterPassword;
    // Scrambled data:
    Property   Groups: TEpiGroups read FGroups;
    Property   LastLogin: TDateTime read FLastLogin write SetLastLogin;
    property   ExpireDate: TDateTime read FExpireDate write SetExpireDate;
    property   FullName: string read FFullName write SetFullName;
  end;

  { TEpiUsersEnumerator }

  TEpiUsersEnumerator = class(TEpiCustomListEnumerator)
  protected
    function GetCurrent: TEpiUser; override;
  public
    property Current: TEpiUser read Getcurrent;
  end;

  TEpiGroupsEnumerator = class;

  { TEpiGroups }

  TEpiGroups = class(TEpiCustomList)
  private
    function    GetAdmin: TEpiAdmin;
    function    GetGroup(Index: integer): TEpiGroup;
  protected
    function    Prefix: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    NewGroup: TEpiGroup;
    function    ItemClass: TEpiCustomItemClass; override;
    function    GetEnumerator: TEpiGroupsEnumerator;
    Property    Group[Index: integer]: TEpiGroup read GetGroup; default;
    Property    Admin: TEpiAdmin read GetAdmin;
  end;

  { TEpiGroup }

  TEpiGroup = class(TEpiCustomItem)
  private
    FCaption: TEpiTranslatedTextWrapper;
    FManageRights: TEpiManagerRights;
    procedure SetManageRights(const AValue: TEpiManagerRights);
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    property   Caption: TEpiTranslatedTextWrapper read FCaption;
    Property   ManageRights: TEpiManagerRights read FManageRights write SetManageRights;
  end;

  { TEpiGroupsEnumerator }

  TEpiGroupsEnumerator = class(TEpiCustomListEnumerator)
  protected
    function GetCurrent: TEpiGroup; override;
  public
    property Current: TEpiGroup read GetCurrent;
  end;


implementation

uses
  DCPbase64, DCPsha256, epistringutils, epimiscutils;

{ TEpiUsersEnumerator }

function TEpiUsersEnumerator.GetCurrent: TEpiUser;
begin
  Result := TEpiUser(inherited GetCurrent);
end;

{ TEpiGroupsEnumerator }

function TEpiGroupsEnumerator.GetCurrent: TEpiGroup;
begin
  result := TEpiGroup(inherited GetCurrent);
end;

{ TEpiAdmin }

function TEpiAdmin.DoRequestPassword: Boolean;
var
  Login, Password: string;
  TheUser: TEpiUser;
  Key: String;
  Res: TEpiRequestPasswordResponse;
  Count: Integer;
begin
  result := false;

  if not Assigned(OnPassword) then exit;

  Count := 1;
  repeat
    Res := OnPassword(Self, erpUserLogin, Count, Login, Password);
    Inc(Count);

    TheUser := Users.GetUserByLogin(Login);
    if not Assigned(TheUser) then exit;

    result := '$' + Base64EncodeStr(TheUser.Salt) + '$' + StrToSHA1Base64(TheUser.Salt + Password + Login) = TheUser.Password;
  until (Result) or (Res = rprStopOnFail);

  Key := TheUser.Salt + Password + Login;
  MasterPassword := Decrypt(Key, TheUser.MasterPassword);
end;

procedure TEpiAdmin.SetMasterPassword(const AValue: string);
begin
  if FMasterPassword = AValue then exit;
  FMasterPassword := AValue;
end;

function TEpiAdmin.Encrypt(const Key, Data: String): String;
begin
  FCrypter.InitStr(Key, TDCP_sha256);
  Result := FCrypter.EncryptString(Data);
end;

function TEpiAdmin.Decrypt(const Key, Data: String): String;
begin
  FCrypter.InitStr(Key, TDCP_sha256);
  Result := FCrypter.DecryptString(Data);
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

  FCrypter := TDCP_rijndael.Create(nil);

  RegisterClasses([Groups, Users]);
end;

destructor TEpiAdmin.Destroy;
begin
  FCrypter.Free;
  FGroups.Free;
  FUsers.Free;
  inherited Destroy;
end;

function TEpiAdmin.XMLName: string;
begin
  Result := rsAdmin;
end;

procedure TEpiAdmin.LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
  I: Integer;
begin
  // Root = <Admin>

  // Load groups
  if LoadNode(Node, Root, rsGroups, false) then
    Groups.LoadFromXml(Node, ReferenceMap);

  // Then load users
  LoadNode(Node, Root, rsUsers, true);
  Users.LoadFromXml(Node, ReferenceMap);
end;

function TEpiAdmin.NewUser: TEpiUser;
begin
  result := Users.NewUser;
end;

function TEpiAdmin.NewGroup: TEpiGroup;
begin
  result := Groups.NewGroup;
end;

function TEpiAdmin.RequestPassword(const RepeatCount: Byte): Boolean;
var
  i: Integer;
begin
  i := 1;
  result := false;

  while (not Result) and
        (i <= RepeatCount)
  do
    begin
      Result := DoRequestPassword;
      Inc(i);
    end;
end;

procedure TEpiAdmin.BeginUpdate;
begin
  inherited BeginUpdate;
end;

procedure TEpiAdmin.EndUpdate;
begin
  inherited EndUpdate;
end;

function TEpiAdmin.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  TEpiAdmin(Result).FMasterPassword := FMasterPassword;
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

function TEpiUsers.Prefix: string;
begin
  Result := 'User';
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

function TEpiUsers.PreLoadFromXml(Root: TDOMNode): Boolean;
var
  Node: TDOMNode;
  NUser: TEpiUser;
begin
  // Root = <Users>
  Result := false;

  // Load all basic user info before requesting user for login.
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    // hack to skip whitespace nodes.
    while NodeIsWhiteSpace(Node) do
      Node := Node.NextSibling;
    if not Assigned(Node) then break;

    CheckNode(Node, rsUser);

    NUser := NewUser;
    NUser.Login := LoadAttrString(Node, rsName);
    // Set password directly here, since the SetPassword method hash'es it and reencrypts the master password.
    NUser.FPassword := LoadAttrString(Node, rsPassword);
    NUser.FSalt := Base64DecodeStr(ExtractStrBetween(NUser.FPassword, '$', '$'));
    NUser.MasterPassword := LoadAttrString(Node, rsMasterPassword);

    Node := Node.NextSibling;
  end;

  result := Admin.DoRequestPassword;
end;

procedure TEpiUsers.PreSaveToDom(RootDoc: TDOMDocument; Root: TDOMNode);
var
  UsersNode: TDOMElement;
  UserNode: TDOMElement;
  i: Integer;
begin
  // Root = <EpiData>
  UsersNode := RootDoc.CreateElement(rsUsers);

  // for User in Self do
  for i := 0 to Count - 1 do
  begin
    UserNode := RootDoc.CreateElement(rsUser);

    SaveDomAttr(UserNode, rsName,           Users[i].Login);
    SaveDomAttr(UserNode, rsPassword,       Users[i].Password);
    SaveDomAttr(UserNode, rsMasterPassword, Users[i].MasterPassword);

    UsersNode.AppendChild(UserNode);
  end;

  Root.InsertBefore(UsersNode, Root.FirstChild);
end;

function TEpiUsers.NewUser: TEpiUser;
begin
  Result := TEpiUser(NewItem(TEpiUser));
end;

function TEpiUsers.GetEnumerator: TEpiUsersEnumerator;
begin
  Result := TEpiUsersEnumerator.Create(Self);
end;

procedure TEpiUsers.LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
  NUser: TEpiUser;
begin
  // No inherited loading, since users should exists at this point in loading.

  // Root = <Users>
  Node := Root.FirstChild;
  while Assigned(Node) do
  begin
    // hack to skip whitespace nodes.
    while NodeIsWhiteSpace(Node) do
      Node := Node.NextSibling;
    if not Assigned(Node) then break;

    CheckNode(Node, rsUser);

    NUser := GetUserByLogin(LoadAttrString(Node, rsId));
    if not Assigned(NUser) then
    begin
      // This situation should only occur if someone delete the XML line with
      // login/pw that should have been loaded during PreLoadFromXml;
      NUser := NewUser;
      NUser.Login := LoadAttrString(Node, rsId);
      NUser.FPassword := LoadAttrString(Node, rsPassword);
      NUser.MasterPassword := LoadAttrString(Node, rsMasterPassword);
    end;
    NUser.LoadFromXml(Node, ReferenceMap);

    Node := Node.NextSibling;
  end;
end;

{ TEpiUser }

procedure TEpiUser.SetExpireDate(const AValue: TDateTime);
var
  Val: TDateTime;
begin
  if FExpireDate = AValue then exit;
  Val := FExpireDate;
  FExpireDate := AValue;
  DoChange(eegAdmin, Word(eaceUserSetExpireDate), @Val);
end;

procedure TEpiUser.SetFullName(const AValue: string);
var
  Val: String;
begin
  if FFullName = AValue then exit;
  Val := FFullName;
  FFullName := AValue;
  DoChange(eegAdmin, Word(eaceUserSetFullName), @Val);
end;

function TEpiUser.GetAdmin: TEpiAdmin;
begin
  result := TEpiAdmin(TEpiUsers(Owner).Owner);
end;

function TEpiUser.GetLogin: string;
begin
  result := FLogin;
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

procedure TEpiUser.SetLogin(AValue: string);
begin
  // TODO: changing login name should trigger a whole lot of things:
  // 1: Validating that no other user has the same login
  // 2: Recompute password hash
  // 3: Recomputer master password hash
  Name := AValue;
  FLogin := Name;
end;

procedure TEpiUser.SetMasterPassword(const AValue: string);
begin
  if FMasterPassword = AValue then exit;
  FMasterPassword := AValue;
end;

procedure TEpiUser.SetPassword(const AValue: string);
var
  SaltInt: LongInt;
  SaltByte: array[0..3] of char absolute SaltInt;
  Key: String;
begin
  SaltInt := (Random(maxLongint - 1) + 1) or $80000000;  // Must have highest bit set.
  FSalt := String(SaltByte);

  // Sha1 the new password and Base64 it..
  FPassword := '$' + Base64EncodeStr(Salt) + '$' + StrToSHA1Base64(Salt + AValue + Login);

  // Scramble master password with own key.
  Key := Salt + AValue + Login;
  MasterPassword := Admin.Encrypt(Key, Admin.MasterPassword);

  DoChange(eegAdmin, Word(eaceUserSetPassword), nil);
end;

function TEpiUser.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  with TEpiUser(Result) do
  begin
//    FGroup      := TEpiGroup(Admin.Groups.GetItemByName(Self.FGroup.Name));
    FExpireDate := Self.FExpireDate;
    FLastLogin  := Self.FLastLogin;
    FFullName   := Self.FFullName;
    FMasterPassword := Self.FMasterPassword;
    FPassword       := Self.FPassword;
    FSalt           := Self.FSalt;
  end;
end;

procedure TEpiUser.FixupReferences(EpiClassType: TEpiCustomBaseClass;
  ReferenceType: Byte; const ReferenceId: string);
var
  GroupList: TStrings;
  S: String;
begin
  inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);

  if (EpiClassType <> TEpiUser) then exit;
  if (ReferenceType <> 0) then exit;

  // ReferenceId is the comma delimited list of group id's
  GroupList := TStringList.Create;
  SplitString(ReferenceId, GroupList, [',']);

  for S in GroupList do
    Groups.AddItem(Admin.Groups.GetItemByName(S));
end;

constructor TEpiUser.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FGroups := TEpiGroups.Create(Self);
  FGroups.ItemOwner := false;
  FFullName := '';
  FMasterPassword := '';
  FPassword := '';
  FSalt := '';
  FExpireDate := 0;
  FLastLogin := 0;
end;

destructor TEpiUser.Destroy;
begin
  FFullName := '';
  FMasterPassword := '';
  FPassword := '';
  FSalt := '';
  FGroups.Free;
  inherited Destroy;
end;

function TEpiUser.XMLName: string;
begin
  result := rsUser;
end;

procedure TEpiUser.LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
begin
  // Root = <User>
  // Remember that login, password and masterpassword have already been
  // read by now... only scrambled things need to be obtained now.
  inherited LoadFromXml(Root, ReferenceMap);

  FullName   := LoadNodeString(Root, rsFullName);
  LastLogin  := LoadAttrDateTime(Root, rsLastLogin, '', 0, false);
  ExpireDate := LoadAttrDateTime(Root, rsExpireDate, '', 0, false);

  if LoadNode(Node, Root, rsGroupRefs, false) then
    ReferenceMap.AddFixupReference(Self,TEpiUser, 0, LoadNodeString(Root, rsGroupRefs));
end;

function TEpiUser.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
var
  S: String;
  G: TEpiGroup;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveTextContent(Result, rsFullName, FullName);

  if (LastLogin > 0) then
    SaveDomAttr(Result, rsLastLogin, LastLogin);

  if (ExpireDate > 0) then
    SaveDomAttr(Result, rsExpireDate, ExpireDate);

  if Groups.Count > 0 then
  begin
    S := '';
    for G in Groups do
      S := S + G.Name + ',';
    Delete(S, Length(S), 1);

    SaveTextContent(Result, rsGroupRefs, S);
  end;
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

function TEpiGroups.Prefix: string;
begin
  Result := 'group_';
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

function TEpiGroups.NewGroup: TEpiGroup;
begin
  Result := TEpiGroup(NewItem(TEpiGroup));
end;

function TEpiGroups.ItemClass: TEpiCustomItemClass;
begin
  Result := TEpiGroup;
end;

function TEpiGroups.GetEnumerator: TEpiGroupsEnumerator;
begin
  result := TEpiGroupsEnumerator.Create(Self);
end;

{ TEpiGroup }

procedure TEpiGroup.SetManageRights(const AValue: TEpiManagerRights);
var
  Val: TEpiManagerRights;
begin
  if FManageRights = AValue then exit;
  Val := FManageRights;
  FManageRights := AValue;
  DoChange(eegAdmin, Word(eaceGroupSetManageRights), @Val);
end;

function TEpiGroup.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  TEpiGroup(Result).FManageRights := FManageRights;
end;

function TEpiGroup.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttrEnum(Result, rsManageRights, ManageRights, TypeInfo(TEpiManagerRights));
end;

constructor TEpiGroup.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);

  FCaption := TEpiTranslatedTextWrapper.Create(Self, rsCaption, rsText);
  RegisterClasses([Caption]);
end;

destructor TEpiGroup.Destroy;
begin
  FCaption.Free;
  inherited Destroy;
end;

function TEpiGroup.XMLName: string;
begin
  Result := rsGroup;
end;

procedure TEpiGroup.LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap);
begin
  // Root = <Group>
  inherited LoadFromXml(Root, ReferenceMap);

  // If no name present, TEpiTranslatedText will take care of it.
  Caption.LoadFromXml(Root, ReferenceMap);
  ManageRights := TEpiManagerRights(LoadAttrEnum(Root, rsManageRights, TypeInfo(TEpiManagerRights), '', false));
end;

end.

