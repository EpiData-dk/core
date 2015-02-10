unit epiadmin;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, Laz2_DOM, episettings, DCPrijndael;

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

  TEpiAdminChangeEventType = (
    // User related events:
    eaceUserSetFirstName, eaceUserSetLastName,
    eaceUserSetPassword, eaceUserSetGroup,
    eaceUserSetExpireDate,eaceUserSetLastLogin,
    // Group related events:
    eaceGroupSetManageRights
  );

  TRequestPasswordEvent = procedure(Sender: TObject; var Login: string; var Password: string) of object;

  { TEpiAdmin }

  TEpiAdmin = class(TEpiCustomBase)
  private
    FGroups: TEpiGroups;
    FOnPassWord: TRequestPasswordEvent;
    FUsers: TEpiUsers;
    // Clear Text master password for all scrambling.
    // -- although clear text here means a sequence of 16 random bytes.
    FMasterPassword: string;
    function   GetSettings: TEpiXMLSettings;
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
//    property   Settings: TEpiXMLSettings read GetSettings;
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
    procedure  PreLoadFromXml(Root: TDOMNode);
    procedure  PreSaveToDom(RootDoc: TDOMDocument; Root: TDOMNode);
    function   NewUser: TEpiUser;
    Property   Users[Index: integer]: TEpiUser read GetUsers;
    Property   Admin: TEpiAdmin read GetAdmin;
  end;

  { TEpiUser }

  TEpiUser = class(TEpiCustomItem)
  private
    FGroup: TEpiGroup;
    FExpireDate: TDateTime;
    FLastLogin: TDateTime;
    FFirstName: string;
    FLastName: string;
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
    procedure SetFirstName(const AValue: string);
    procedure SetGroup(const AValue: TEpiGroup);
    procedure SetLastLogin(const AValue: TDateTime);
    procedure SetLastName(const AValue: string);
    procedure SetLogin(AValue: string);
    procedure SetMasterPassword(const AValue: string);
    procedure SetPassword(const AValue: string);
  protected
    property  Salt: string read FSalt;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
//    function   SaveToXml(Content: String; Lvl: integer): string; override;
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function   SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
    property   Admin: TEpiAdmin read GetAdmin;
    // ====== DATA =======
    // Unscrambled data:
    Property   Login: string read GetLogin write SetLogin;
    Property   Password: string read FPassword write SetPassword;
    Property   MasterPassword: string read FMasterPassword write SetMasterPassword;
    // Scrambled data (in UserInfo section):
    Property   Group: TEpiGroup read FGroup write SetGroup;
    Property   LastLogin: TDateTime read FLastLogin write SetLastLogin;
    property   ExpireDate: TDateTime read FExpireDate write SetExpireDate;
    property   FirstName: string read FFirstName write SetFirstName;
    property   LastName: string read FLastName write SetLastName;
  end;

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
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    property   Caption: TEpiTranslatedTextWrapper read FCaption;
    Property   ManageRights: TEpiManagerRights read FManageRights write SetManageRights;
  end;


implementation

uses
  DCPbase64, DCPsha256, epistringutils, epidocument, epimiscutils;

{ TEpiAdmin }

function TEpiAdmin.DoRequestPassword: Boolean;
var
  Login, Password: string;
  TheUser: TEpiUser;
  Key: String;
begin
  result := false;

  if not Assigned(OnPassword) then exit;

  OnPassword(Self, Login, Password);

  TheUser := Users.GetUserByLogin(Login);
  if not Assigned(TheUser) then exit;

  result := '$' + Base64EncodeStr(TheUser.Salt) + '$' + StrToSHA1Base64(TheUser.Salt + Password + Login) = TheUser.Password;
  if not result then exit;

  Key := TheUser.Salt + Password + Login;
  MasterPassword := Decrypt(Key, TheUser.MasterPassword);
end;

function TEpiAdmin.GetSettings: TEpiXMLSettings;
begin
  // TODO : GetSettings - missing EpiDocument;
  result := TEpiDocument(Owner).XMLSettings;
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

  Admin.DoRequestPassword;
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

procedure TEpiUser.SetFirstName(const AValue: string);
var
  Val: String;
begin
  if FFirstName = AValue then exit;
  Val := FFirstName;
  FFirstName := AValue;
  DoChange(eegAdmin, Word(eaceUserSetFirstName), @Val);
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

procedure TEpiUser.SetLastName(const AValue: string);
var
  Val: String;
begin
  if FLastName = AValue then exit;
  Val := FLastName;
  FLastName := AValue;
  DoChange(eegAdmin, Word(eaceUserSetLastName), @Val);
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
    FGroup      := TEpiGroup(Admin.Groups.GetItemByName(Self.FGroup.Name));
    FExpireDate := Self.FExpireDate;
    FLastLogin  := Self.FLastLogin;
    FFirstName  := Self.FFirstName;
    FLastName   := Self.FLastName;
    FMasterPassword := Self.FMasterPassword;
    FPassword       := Self.FPassword;
    FSalt           := Self.FSalt;
  end;
end;

constructor TEpiUser.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FGroup := nil;
end;

destructor TEpiUser.Destroy;
begin
  FFirstName := '';
  FLastName := '';
  FMasterPassword := '';
  FPassword := '';
  FSalt := '';
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

  FirstName  := LoadNodeString(Root, rsFirstName);
  LastName   := LoadNodeString(Root, rsLastName);
  LastLogin  := LoadNodeDateTime(Root, rsLastLogin);
  ExpireDate := LoadNodeDateTime(Root, rsExpireDate);

  if LoadNode(Node, Root, rsGroupId, false) then
    ;
//  Group      := TEpiGroup(Admin.Groups.GetItemByName(LoadNodeString(NewRoot, rsGroupId)));
end;

function TEpiUser.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveTextContent(Result, rsFirstName, FirstName);
  SaveTextContent(Result, rsLastName,  LastName);
  SaveTextContent(Result, rsLastLogin, LastLogin);
  SaveTextContent(Result, rsExpireDate, ExpireDate);
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
  Result := 'Group';
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

{ TEpiGroup }

procedure TEpiGroup.SetManageRights(const AValue: TEpiManagerRights);
var
  Val: TEpiManagerRights;
begin
  if FManageRights = AValue then exit;
  Val := FManageRights;
  FManageRights := AValue;
//  DoChange(eegAdmin, Word(eaceGroupSetRights), @Val);
end;

function TEpiGroup.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  TEpiGroup(Result).FManageRights := FManageRights;
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
//  Rights := TEpiManagerRights(LoadAttrInt(Root, rsRights));
end;

end.

