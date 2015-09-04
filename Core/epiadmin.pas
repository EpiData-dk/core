unit epiadmin;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, Laz2_DOM, DCPrijndael, epicustomrelations;

type
  TEpiAdmin = class;
  TEpiUsers = class;
  TEpiUser = class;
  TEpiGroups = class;
  TEpiGroup = class;
  TEpiGroupRelation     = class;
  TEpiGroupRelationList = class;

  EEpiBadPassword = class(Exception);
  EEpiPasswordCanceled = class(Exception);

  // Rights in manager
  TEpiManagerRight = (
    // Project Content Design:
    earDefineProject,           // Structural CRUD in Datafile, Valuelabels, Study Info, etc. related to project management but not data.
    earTranslate,               // May change all TEpiTranslatedText objects

    // Assign Project Rights:
    earGroups,                  // CRUD for Groups based on EpiData RBAC model AND Assign groups to sections!

    // User Management:
    earUsers,                   // CRUD for Users based on EpiData RBAC model.
    earPassword,                // May change password for users.

    // Data Access:
    earExtentendedData,         // Export, pack
    earViewData                 // Can view Data
  );
  TEpiManagerRights = set of TEpiManagerRight;

const
  EpiManagerRightCaptions: array[TEpiManagerRight] of string =
    (
      '&Define Project',
      '&Translate Project',
      'Manage &Groups',
      'Manage &Users',
      'Reset &Password',
      '&Extended Data',
      '&View Data'
    );

  EpiManagerRightCaptionsShort: array[TEpiManagerRight] of string =
    ( 'D',
      'T',
      'G',
      'U',
      'P',
      'E',
      'V'
    );



  EpiManageRightFirst = earDefineProject;
  EpiManageRightLast  = earViewData;

  EpiAllManageRights: TEpiManagerRights =
    [EpiManageRightFirst..EpiManageRightLast];


type

  TEpiAdminChangeEventType = (
    // User related events:
    eaceUserSetFullName,
    eaceUserSetPassword,   eaceUserSetGroup,
    eaceUserSetExpireDate, eaceUserSetLastLogin,
    // Group related events:
    eaceGroupSetManageRights,
    // Admin related events:
    eaceAdminLoginSuccessfull,       // Data: TEpiUser = the authenticated user.
    eaceAdminIncorrectUserName,      // Data: string   = the incorrect login name
    eaceAdminIncorrectPassword,      // Data: string   = the incorrect login name
    eaceAdminIncorrectNewPassword    // Data: TEpiUser = the authenticated user.
  );

  TEpiRequestPasswordType = (
    erpSinglePassword,          // Only a valid password is required - login is ignored.
    erpUserLogin,               // Project is managed by a user/password and both are requred.
    erpNewPassword              // The authorized user needs a new password
  );

  TEpiRequestPasswordResult = (
    prSuccess,      // Asking user for password succeeded (correct username/password combo)
    prFailed,       // Asking user for password failed    (incorrect username/password combo)
    prCanceled      // The user canceled the login
  );

  TEpiRequestPasswordResponse = (
     rprAskOnFail,              // If login/password/new password failed send request again.
     rprStopOnFail,             // If login/password/new password failed stop loading.
     rprCanceled                 // The user cancled at the password form.
  );

  TRequestPasswordEvent = function(
    Sender: TObject;
    RequestType: TEpiRequestPasswordType;
    RequestNo: Integer;
    var Login: string;
    var Password: string
  ): TEpiRequestPasswordResponse of object;

  TEpiUserAuthorizedEvent = procedure(
    Sender: TEpiAdmin;
    User:   TEpiUser
  ) of object;


  { TEpiAdmin }

  TEpiAdmin = class(TEpiCustomBase)
  private
    FAdminsGroup: TEpiGroup;
    FAdminRelation: TEpiGroupRelation;
    FAdminRelations: TEpiGroupRelationList; // An internal only relationslist. Needed to have ValidateRename work correctly for GroupRelations.
    FOnPassWord: TRequestPasswordEvent;
    FOnUserAuthorized: TEpiUserAuthorizedEvent;
    FUsers: TEpiUsers;
    FGroups: TEpiGroups;
    // Clear Text master password for all scrambling.
    // -- although clear text here means a sequence of 16 random bytes.
    FMasterPassword: string;
    function   DoRequestPassword: TEpiRequestPasswordResult;
    procedure  SetMasterPassword(const AValue: string);
    procedure  DoUserAuthorized(Const User: TEpiUser);

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
    procedure FixupReferences(EpiClassType: TEpiCustomBaseClass;
  ReferenceType: Byte; const ReferenceId: string); override;
    property   Users: TEpiUsers read FUsers;
    property   Groups: TEpiGroups read FGroups;
    property   Admins: TEpiGroup read FAdminsGroup;
    property   AdminRelation: TEpiGroupRelation read FAdminRelation;
  public
    // User / Group related functions.
    function   NewUser: TEpiUser;
    function   NewGroup: TEpiGroup;
    procedure  ResetAll;
//    function   RequestPassword(Const RepeatCount: Byte): TRequestPasswordResult;
    property   MasterPassword: string read FMasterPassword write SetMasterPassword;
    property   OnPassWord: TRequestPasswordEvent read FOnPassWord write FOnPassWord;
    property   OnUserAuthorized: TEpiUserAuthorizedEvent read FOnUserAuthorized write FOnUserAuthorized;

  { Load/Save }
  protected
    function   SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;

  {Cloning}
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;

  { Meta data information }
  private
    FCreated: TDateTime;
    function GetGroupEdited: TDateTime;
    function GetUserEdited: TDateTime;
  public
    property Created: TDateTime read FCreated write FCreated;
    property GroupEdited: TDateTime read GetGroupEdited;
    property UserEdited: TDateTime read GetUserEdited;
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
    function   PreLoadFromXml(Root: TDOMNode): TEpiRequestPasswordResult;
    procedure  PreSaveToDom(RootDoc: TDOMDocument; Root: TDOMNode);
    function   NewUser: TEpiUser;
    function   GetEnumerator: TEpiUsersEnumerator;
    Property   Users[Index: integer]: TEpiUser read GetUsers; default;
    Property   Admin: TEpiAdmin read GetAdmin;
  end;

  { TEpiUser }

  TEpiUser = class(TEpiCustomItem)
  private
    FCreated: TDateTime;
    FGroups: TEpiGroups;
    FExpireDate: TDateTime;
    FLastLogin: TDateTime;
    FFullName: string;
    // Master password as stored in file:
    // - Base64( AES ( CleearTextPassword ))
    FMasterPassword: string;
    FModified: TDateTime;
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
    procedure DoChange(const Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer); override;
      overload;
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
    property   Created: TDateTime read FCreated;
    property   Modified: TDateTime read FModified;
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
    function    HasRights(Const ManagerRights: TEpiManagerRights): Boolean;
    Property    Group[Index: integer]: TEpiGroup read GetGroup; default;
    Property    Admin: TEpiAdmin read GetAdmin;
  end;

  { TEpiGroup }

  TEpiGroup = class(TEpiCustomItem)
  private
    FCaption: TEpiTranslatedTextWrapper;
    FCreated: TDateTime;
    FModified: TDateTime;
    FManageRights: TEpiManagerRights;
    FUsers: TEpiUsers;
    procedure SetManageRights(const AValue: TEpiManagerRights);
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
    procedure DoChange(const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer); override; overload;
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    property   Caption: TEpiTranslatedTextWrapper read FCaption;
    property   ManageRights: TEpiManagerRights read FManageRights write SetManageRights;
    // Special for Users - not saved/loaded to XML. This is done in TEpiUser.
    property   Users: TEpiUsers read FUsers;
    property   Created: TDateTime read FCreated;
    property   Modified: TDateTime read FModified;
  end;

  { TEpiGroupsEnumerator }

  TEpiGroupsEnumerator = class(TEpiCustomListEnumerator)
  protected
    function GetCurrent: TEpiGroup; override;
  public
    property Current: TEpiGroup read GetCurrent;
  end;

  TEpiGroupRelationChangeEvent = (
    egrceSetGroup
  );

  { TEpiGroupRelation }

  TEpiGroupRelation = class(TEpiCustomRelationItem)
  private
    FGroup: TEpiGroup;
    function GetGroupRelation(const Index: integer): TEpiGroupRelation;
    function GetGroupRelations: TEpiGroupRelationList;
    function GetParentRelation: TEpiGroupRelation;
    procedure SetGroup(AValue: TEpiGroup);
  protected
    class function GetRelationListClass: TEpiCustomRelationListClass; override;
    procedure ReferenceDestroyed(Item: TEpiCustomItem; PropertyName: shortstring
      ); override;
    procedure FixupReferences(EpiClassType: TEpiCustomBaseClass;
       ReferenceType: Byte; const ReferenceId: string); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      RefenceMap: TEpiReferenceMap): TEpiCustomBase; override;
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    procedure LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function XMLName: string; override;
    property Group: TEpiGroup read FGroup write SetGroup;
    property GroupRelation[Const Index: integer]: TEpiGroupRelation read GetGroupRelation; default;
    property GroupRelations: TEpiGroupRelationList read GetGroupRelations;
    property ParentRelation: TEpiGroupRelation read GetParentRelation;
  end;

  TEpiGroupRelationListEnumerator = class;

  { TEpiGroupRelationList }

  TEpiGroupRelationList = class(TEpiCustomRelationItemList)
  private
    function GetGroupRelation(const Index: Integer): TEpiGroupRelation;
  protected
    function Prefix: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    function XMLName: string; override;
    function NewGroupRelation: TEpiGroupRelation;
    function ItemClass: TEpiCustomItemClass; override;
    function GetEnumerator: TEpiGroupRelationListEnumerator;
    property GroupRelation[Const Index: Integer]: TEpiGroupRelation read GetGroupRelation; default;
  end;

  { TEpiGroupRelationListEnumerator }

  TEpiGroupRelationListEnumerator = class(TEpiCustomRelationItemListEnumerator)
  protected
    function GetCurrent: TEpiGroupRelation; override;
  public
    property Current: TEpiGroupRelation read GetCurrent;
  end;

implementation

uses
  DCPbase64, DCPsha256, epistringutils, epimiscutils, epidocument,
  math;

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

function TEpiAdmin.DoRequestPassword: TEpiRequestPasswordResult;
var
  Login, Password, NewPassword: string;
  TheUser: TEpiUser;
  Key: String;
  Res: TEpiRequestPasswordResponse;
  Count: Integer;
begin
  result := prFailed;

  if not Assigned(OnPassword) then exit;
  Login := '';
  Password := '';
  NewPassword := '';

  Count := 1;
  repeat
    // Send the password request to the program (reciever)
    Res := OnPassword(Self, erpUserLogin, Count, Login, Password);

    // The user canceled the login
    if (Res = rprCanceled) then
      begin
        Result := prCanceled;
        Break;
      end;

    // Increate the try counter
    Inc(Count);

    // Find user
    TheUser := Users.GetUserByLogin(Login);
    if not Assigned(TheUser) then
      begin
        // Login did not exists - send an event abount it.
        DoChange(eegAdmin, Word(eaceAdminIncorrectUserName), @Login);
        Continue;
      end;

    if ('$' + Base64EncodeStr(TheUser.Salt) + '$' + StrToSHA1Base64(TheUser.Salt + Password + Login) = TheUser.Password) then
      Result := prSuccess
    else begin
      Result := prFailed;
       DoChange(eegAdmin, Word(eaceAdminIncorrectPassword), @Login);
    end;
  until (Result = prSuccess) or (Res = rprStopOnFail);

  if (Result = prSuccess) and
     (TheUser.ExpireDate <> 0) and
     (Now >= TheUser.ExpireDate)
  then
    begin
      Count := 1;
      repeat
        Res := OnPassword(Self, erpNewPassword, Count, Login, NewPassword);

        // The user canceled the login
        if (Res = rprCanceled) then
          begin
            Result := prCanceled;
            Break;
          end;

        if (NewPassword <> Password) then
          Result := prSuccess
        else begin
          Result := prFailed;
          DoChange(eegAdmin, Word(eaceAdminIncorrectNewPassword), TheUser);
        end;

      until (Result = prSuccess) or (Res = rprStopOnFail);
    end;

  if (Result = prSuccess)
  then
    begin
      TheUser.LastLogin := Now;
      DoUserAuthorized(TheUser);
      DoChange(eegAdmin, Word(eaceAdminLoginSuccessfull), TheUser);

      Key := TheUser.Salt + Password + Login;
      MasterPassword := Decrypt(Key, TheUser.MasterPassword);
    end;
end;

procedure TEpiAdmin.SetMasterPassword(const AValue: string);
begin
  if FMasterPassword = AValue then exit;
  FMasterPassword := AValue;
end;

procedure TEpiAdmin.DoUserAuthorized(const User: TEpiUser);
begin
  if Assigned(OnUserAuthorized) then
    OnUserAuthorized(Self, User);
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
  FUsers.Name := 'TEpiAdmin.Users';

  FGroups := TEpiGroups.Create(self);
  FGroups.ItemOwner := true;
  FGroups.Name := 'TEpiAdmin.Groups';

  FAdminRelations := TEpiGroupRelationList.Create(Self);
  FAdminRelations.ItemOwner := true;

  ResetAll;

  {
  FAdminsGroup := FGroups.NewGroup;
  FAdminsGroup.ManageRights := EpiAllManageRights;
  FAdminsGroup.Caption.TextLang['en'] := 'Admins';
  FAdminsGroup.Name := 'admins_group';

  FAdminRelations := TEpiGroupRelationList.Create(Self);
  FAdminRelations.ItemOwner := true;

  FAdminRelation := FAdminRelations.NewGroupRelation;
  FAdminRelation.Group := FAdminsGroup;  }

  FCrypter := TDCP_rijndael.Create(nil);

  RegisterClasses([Users, Groups]);
end;

destructor TEpiAdmin.Destroy;
begin
  FCrypter.Free;
  FAdminsGroup.Free;
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
begin
  // Since the Admin group is autocreated we remove it during load.
  FreeAndNil(FAdminRelation);
  FreeAndNil(FAdminsGroup);

  inherited LoadFromXml(Root, ReferenceMap);
  // Root = <Admin>

  // Load groups
  if LoadNode(Node, Root, rsGroups, false) then
    Groups.LoadFromXml(Node, ReferenceMap);

  // Then load users
  if LoadNode(Node, Root, rsUsers, false) then
    Users.LoadFromXml(Node, ReferenceMap);

  // finally load the relationships
  LoadNode(Node, Root, 'GroupRelation', true);

  FAdminRelation := FAdminRelations.NewGroupRelation;
  AdminRelation.LoadFromXml(Node, ReferenceMap);

  // During fixup, set the correct admingroup!
  ReferenceMap.AddFixupReference(Self, TEpiAdmin, 0, '');
end;

procedure TEpiAdmin.FixupReferences(EpiClassType: TEpiCustomBaseClass;
  ReferenceType: Byte; const ReferenceId: string);
begin
  if EpiClassType = TEpiAdmin then
    case ReferenceType of
      0: begin
           FAdminsGroup := AdminRelation.Group;
           // Always reset the mangerights, since these may change on XML version change.
           // and the ADMIN group must ALWAYS have all management rights.
           FAdminsGroup.FManageRights := EpiAllManageRights;
         end;
    end
  else
    inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);
end;

function TEpiAdmin.NewUser: TEpiUser;
begin
  result := Users.NewUser;
end;

function TEpiAdmin.NewGroup: TEpiGroup;
begin
  result := Groups.NewGroup;
end;

procedure TEpiAdmin.ResetAll;
begin
  Users.ClearAndFree;
  Groups.ClearAndFree;

  FAdminsGroup := FGroups.NewGroup;
  FAdminsGroup.ManageRights := EpiAllManageRights;
  FAdminsGroup.Caption.TextLang['en'] := 'Admins';
  FAdminsGroup.Name := 'admins_group';

  FAdminRelation := FAdminRelations.NewGroupRelation;
  FAdminRelation.Group := FAdminsGroup;
end;

function TEpiAdmin.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
var
  Elem: TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  Elem := AdminRelation.SaveToDom(RootDoc);
  if Assigned(Elem) then
    Result.AppendChild(Elem);
end;

function TEpiAdmin.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  TEpiAdmin(Result).FMasterPassword := FMasterPassword;
end;

function TEpiAdmin.GetGroupEdited: TDateTime;
var
  G: TEpiGroup;
begin
  Result := 0;
  for G in Groups do
    Result := Max(Result, G.Modified);
end;

function TEpiAdmin.GetUserEdited: TDateTime;
var
  U: TEpiUser;
begin
  Result := 0;
  for U in Users do
    Result := Max(Result, U.Modified);
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

function TEpiUsers.PreLoadFromXml(Root: TDOMNode): TEpiRequestPasswordResult;
var
  Node: TDOMNode;
  NUser: TEpiUser;
begin
  // Root = <Users>
  Result := prFailed;

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
var
  S: String;
  G: TEpiGroup;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  with TEpiUser(Result) do
  begin
    FExpireDate := Self.FExpireDate;
    FLastLogin  := Self.FLastLogin;
    FFullName   := Self.FFullName;
    FMasterPassword := Self.FMasterPassword;
    FPassword       := Self.FPassword;
    FSalt           := Self.FSalt;
  end;

  S := '';
  for G in Groups do
    S := S + ',' + G.Name;

  if S <> '' then
    Delete(S,1,1);

  ReferenceMap.AddFixupReference(Self, TEpiUser, 0, S);
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

  GroupList.Free;
end;

procedure TEpiUser.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited DoChange(Initiator, EventGroup, EventType, Data);

  // Use the Dochange to catch add/remove in the Groups container, which
  // in turn should be added/removed from the Group.Users container.
  if (ebsDestroying in State) then exit;

  if (Initiator = Self) or (Initiator = Groups) then
    FModified := Now;

  if (Initiator <> Groups) then exit;
  if (EventGroup <> eegCustomBase) then exit;
  if not (TEpiCustomChangeEventType(EventType) in [ecceAddItem, ecceDelItem]) then exit;


  case TEpiCustomChangeEventType(EventType) of
    ecceAddItem:
      TEpiGroup(Data).Users.AddItem(Self);
    ecceDelItem:
      TEpiGroup(Data).Users.RemoveItem(Self);
  end;
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
  FCreated := Now;
  FModified := FCreated;
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

  FFullName   := LoadNodeString(Root, rsFullName);
  FLastLogin  := LoadAttrDateTime(Root, rsLastLogin, '', 0, false);
  FExpireDate := LoadAttrDateTime(Root, rsExpireDate, '', 0, false);

  FCreated    := LoadAttrDateTime(Root, rsCreatedAttr, '', Now, false);
  FModified   := LoadAttrDateTime(Root, rsModifiedAttr, '', Now, false);

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

  SaveDomAttr(Result, rsCreatedAttr, Created);
  SaveDomAttr(Result, rsModifiedAttr, Modified);

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

function TEpiGroups.HasRights(const ManagerRights: TEpiManagerRights): Boolean;
var
  Grp: TEpiGroup;
  RightsNeeded: TEpiManagerRights;
  Right: TEpiManagerRight;
begin
  RightsNeeded := ManagerRights;

  // Walk through all groups, and each groups right to see if the list of
  // managerights are present.
  // Do this by removing a right found in a group from the list of needed rights
  // and if this list is empty by the end of the loop, then all rights were found.
  for Grp in Self do
  begin
    for Right in Grp.ManageRights do
      if Right in RightsNeeded then
        Exclude(RightsNeeded, Right);

    if (RightsNeeded = []) then
      break;
  end;

  Result := (RightsNeeded = []);
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

procedure TEpiGroup.DoChange(const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  inherited DoChange(Initiator, EventGroup, EventType, Data);

  if (Initiator = Self) or (Initiator = FCaption) then
    FModified := Now;
end;

function TEpiGroup.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttrEnum(Result, rsManageRights, ManageRights, TypeInfo(TEpiManagerRights));
  SaveDomAttr(Result, rsCreatedAttr, Created);
  SaveDomAttr(Result, rsModifiedAttr, Modified);
end;

constructor TEpiGroup.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);

  FCaption  := TEpiTranslatedTextWrapper.Create(Self, rsCaption, rsText);
  FCreated  := Now;
  FModified := FCreated;

  FUsers    := TEpiUsers.Create(self);
  FUsers.ItemOwner := false;
  FUsers.Sorted := false;

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

  FCreated    := LoadAttrDateTime(Root, rsCreatedAttr, '', Now, false);
  FModified   := LoadAttrDateTime(Root, rsModifiedAttr, '', Now, false);
end;

{ TEpiGroupRelation }

procedure TEpiGroupRelation.SetGroup(AValue: TEpiGroup);
begin
  if FGroup = AValue then Exit;
  FGroup := AValue;

  ObserveReference(FGroup, 'Group');
  DoChange(eegGroupRelations, Word(egrceSetGroup), Group);
end;

class function TEpiGroupRelation.GetRelationListClass: TEpiCustomRelationListClass;
begin
  result := TEpiGroupRelationList;
end;

function TEpiGroupRelation.GetGroupRelation(const Index: integer
  ): TEpiGroupRelation;
begin
  result := TEpiGroupRelation(RelationList.Items[Index]);
end;

function TEpiGroupRelation.GetGroupRelations: TEpiGroupRelationList;
begin
  result := TEpiGroupRelationList(RelationList);
end;

function TEpiGroupRelation.GetParentRelation: TEpiGroupRelation;
begin
  result := nil;

  if (Assigned(Owner)) and
     (Owner is TEpiGroupRelationList) and
     (Assigned(Owner.Owner)) and
     (Owner.Owner is TEpiGroupRelation)
  then
    Result := TEpiGroupRelation(Owner.Owner);
end;

procedure TEpiGroupRelation.ReferenceDestroyed(Item: TEpiCustomItem;
  PropertyName: shortstring);
begin
  case PropertyName of
    'Group': FGroup := nil;
  end;

  inherited ReferenceDestroyed(Item, PropertyName);
end;

procedure TEpiGroupRelation.FixupReferences(EpiClassType: TEpiCustomBaseClass;
  ReferenceType: Byte; const ReferenceId: string);
begin
  if (EpiClassType = TEpiGroupRelation)
  then
    begin
      case ReferenceType of
        0: // Datafile
          Group := TEpiGroup(TEpiDocument(RootOwner).Admin.Groups.GetItemByName(ReferenceId));
      end;
    end
  else
    inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);
end;

function TEpiGroupRelation.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; RefenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, RefenceMap);

  if Assigned(Group) then
    RefenceMap.AddFixupReference(Result, TEpiGroupRelation, 0, Group.Name);
end;

function TEpiGroupRelation.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, rsGroupRef, Group.Name);
end;

procedure TEpiGroupRelation.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
begin
  inherited LoadFromXml(Root, ReferenceMap);

  ReferenceMap.AddFixupReference(Self, TEpiGroupRelation, 0, LoadAttrString(Root, rsGroupRef));

  if LoadNode(Node, Root, 'GroupRelations', false) then
    RelationList.LoadFromXml(Node, ReferenceMap);
end;

function TEpiGroupRelation.XMLName: string;
begin
  Result := 'GroupRelation';
end;

{ TEpiGroupRelationList }

function TEpiGroupRelationList.GetGroupRelation(const Index: Integer
  ): TEpiGroupRelation;
begin
  result := TEpiGroupRelation(Items[Index]);
end;

function TEpiGroupRelationList.Prefix: string;
begin
  Result := 'grouprelation_id_';
end;

constructor TEpiGroupRelationList.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

function TEpiGroupRelationList.XMLName: string;
begin
  Result := 'GroupRelations';
end;

function TEpiGroupRelationList.NewGroupRelation: TEpiGroupRelation;
begin
  Result := TEpiGroupRelation(NewItem());
end;

function TEpiGroupRelationList.ItemClass: TEpiCustomItemClass;
begin
  Result := TEpiGroupRelation;
end;

function TEpiGroupRelationList.GetEnumerator: TEpiGroupRelationListEnumerator;
begin
  result := TEpiGroupRelationListEnumerator.Create(Self);
end;

{ TEpiGroupRelationListEnumerator }

function TEpiGroupRelationListEnumerator.GetCurrent: TEpiGroupRelation;
begin
  Result := TEpiGroupRelation(inherited GetCurrent);
end;

end.

