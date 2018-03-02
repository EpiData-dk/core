unit epiadmin;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, Laz2_DOM, DCPrijndael, epicustomrelations,
  epidatafilestypes, epi_rsa;

type
  TEpiAdmin = class;
  TEpiUsers = class;
  TEpiUser = class;
  TEpiGroups = class;
  TEpiGroup = class;
  TEpiGroupRelation     = class;
  TEpiGroupRelationList = class;

  EEpiAdminException = class(Exception);
  EEpiBadPassword = class(EEpiAdminException);
  EEpiPasswordCanceled = class(EEpiAdminException);
  EEpiTooManyFailedLogins = class(EEpiAdminException);
  EEpiUserExpired = class(EEpiAdminException);

  // Rights in manager
  TEpiManagerRight = (
    // Project Content Design:
    earDefineProject,           // Structural CRUD in Datafile, Valuelabels, Study Info, etc. related to project management but not data.
    earPrepareDoubleEntry,      // Single right for allowing prepare double entry
    earTranslate,               // May change all TEpiTranslatedText objects

    // Assign Project Rights:
    earGroups,                  // CRUD for Groups based on EpiData RBAC model AND Assign GroupRights to datafiles.

    // User Management:
    earUsers,                   // CRUD for Users based on EpiData RBAC model.
    earPassword,                // May change password for users.

    // Data Access:
    earExport,                  // Export project AND Import project into other projects.
    earExtentendedData,         // Pac, Append
    earReport                   // Can run reports.
  );
  TEpiManagerRights = set of TEpiManagerRight;

const
  EpiManagerRightCaptions: array[TEpiManagerRight] of string =
    (
      '&Define Project',
      'Prepare D&ouble Entry',
      '&Translate Project',
      'Manage &Groups',
      'Manage &Users',
      'Reset &Password',
      'Import / &Export Data',
      'E&xtended Data',
      '&Reports'
    );

  EpiManagerRightCaptionsShort: array[TEpiManagerRight] of string =
    (
      'D',
      'De',
      'T',
      'G',
      'U',
      'P',
      'E',
      'X',
      'R'
    );

  EpiManagerRightHint: array[TEpiManagerRight] of string =
    (
      'Can manage fields/sections/heading, Valuelabels, Define Key.' + LineEnding +
        'In general everything related to the structure of the project.',
      'Can use the "Prepare Double Entry" tool',
      'Allow the user to translate the captions of the project (Field/Section/Heading caption)' + LineEnding +
        'Label part of the Valuelabels, etc.',
      'Can add/delete new groups as a sub-group of users own group(s) and assign' + LineEnding +
        'Manager/EntryClient Rights',
      'Can add/delete other users in users own group or sub-groups',
      'Can reset password for ALL users EXCEPT for the Admin group',
      'Can export projects and allow import into other projects.',
      'Can use the Append and Pack data tools',
      'Can run reports'
    );


  EpiManageRightFirst = earDefineProject;
  EpiManageRightLast  = earReport;

  EpiAllManageRights: TEpiManagerRights =
    [EpiManageRightFirst..EpiManageRightLast];


type

  TEpiAdminChangeEventType = (
    // User related events:
    eaceUserSetFullName,
    eaceUserSetPassword,
    eaceUserSetLastLogin,
    eaceUserSetNotes,
    eaceUserSetExpireDate,
    // Group related events:
    eaceGroupSetManageRights,
    // Admin related events:
    eaceAdminInitializing,           // Data: nil  (Send right after initialinzing internal structures)
    eaceAdminResetting,              // Data: nil  (Send right before all users/groups are freed!)
    eaceAdminLoginSuccessfull,       // Data: TEpiUser = the authenticated user.
    eaceAdminIncorrectUserName,      // Data: string   = the incorrect login name
    eaceAdminIncorrectPassword,      // Data: string   = the incorrect login name
    eaceAdminIncorrectNewPassword,   // Data: TEpiUser = the authenticated user.
    eaceAdminUserExpired,            // Data: TEpiUser = the expired user account
    eaceAdminBlockedLogin,           // No data
    eaceAdminDayBetweenPassword      // Data: integer = old value
  );

  TEpiRequestPasswordType = (
    erpSinglePassword,               // Only a valid password is required - login is ignored.
    erpUserLogin,                    // Project is managed by a user/password and both are requred.
    erpNewPassword                   // The authorized user needs a new password , return '' (empty string) if the two passwords do not match
  );

  TEpiRequestPasswordResult = (
    prSuccess,                       // Asking user for password succeeded (correct username/password combo)
    prFailed,                        // Asking user for password failed    (incorrect username/password combo)
    prUserExpired,                   // The user account has expired
    prCanceled                       // The user canceled the login
  );

  TEpiRequestPasswordResponse = (
     rprAskOnFail,                   // If login/password/new password failed send request again.
     rprStopOnFail,                  // If login/password/new password failed stop loading.
     rprCanceled                     // The user cancled at the password form.
  );

  TRequestPasswordEvent = function(
    Sender: TObject;
    RequestType: TEpiRequestPasswordType;
    RequestNo: Integer;
    var Login: UTF8String;
    var Password: UTF8String
  ): TEpiRequestPasswordResponse of object;

  TEpiUserAuthorizedEvent = procedure(
    Sender: TEpiAdmin;
    User:   TEpiUser
  ) of object;


  { TEpiAdmin }

  TEpiAdmin = class(TEpiCustomBase)
  private
    FDaysBetweenPasswordChange: integer;
    FInitialized: Boolean;
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
    function   DoRequestPassword(FailLogger: TEpiCustomBase; ReferenceMap: TEpiReferenceMap; DocumentNode: TDOMNode = nil): TEpiRequestPasswordResult;
    procedure  SetMasterPassword(const AValue: string);
    procedure  DoUserAuthorized(Const User: TEpiUser);
    procedure SetDaysBetweenPasswordChange(AValue: integer);

  { Encrypt / Decrypt methods for user handling }
  private
    FAuthorizedUser: TEpiUser;
    FCrypter: TDCP_rijndael;
    FRSA:     TEpiRSA;
  protected
    function   Encrypt(Const Key, Data: String): String;  //encrypts a value with given key, using the Rijndael crytp engine.
    function   Decrypt(Const Key, Data: String): String;  //decrypts a value with given key, using the Rijndael crytp engine.
  public
    property   RSA: TEpiRSA read FRSA;

  { Class stuff }
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function   LoadCrypto(Root: TDOMNode; ReferenceMap: TEpiReferenceMap; FailLogger: TEpiCustomBase): TEpiRequestPasswordResult;
    function   SaveCrypto(RootDoc: TDOMDocument): TDOMElement;
    procedure  FixupReferences(EpiClassType: TEpiCustomBaseClass; ReferenceType: Byte; const ReferenceId: string); override;
    property   AuthorizedUser: TEpiUser read FAuthorizedUser;
    property   Users: TEpiUsers read FUsers;
    property   Groups: TEpiGroups read FGroups;
    property   Admins: TEpiGroup read FAdminsGroup;
    property   AdminRelation: TEpiGroupRelation read FAdminRelation;
    // Version 6: If set to 0 then no forced password changed should happen
    property   DaysBetweenPasswordChange: integer read FDaysBetweenPasswordChange write SetDaysBetweenPasswordChange;
  public
    // User / Group related functions.
    function   NewUser: TEpiUser;
    function   NewGroup: TEpiGroup;
    procedure  ResetAll;
    procedure  InitAdmin;
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
    property Initialized: Boolean read FInitialized;
  end;

  TEpiUsersEnumerator = class;

  { TEpiUsers }

  TEpiUsers = class(TEpiCustomList)
  private
    function GetAdmin: TEpiAdmin;
    function GetUsers(Index: integer): TEpiUser;
    procedure  PreLoadUsers(Root: TDOMNode);
    function   PreSaveUsers(RootDoc: TDOMDocument): TDOMElement;
  protected
    function Prefix: string; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    function   GetUserByLogin(const Login: string): TEpiUser;
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
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
    FFullName: UTF8String;
    FLastPWChange: TDateTime;
    // Master password as stored in file:
    // - Base64( AES ( ClearTextPassword ))
    FMasterPassword: string;
    FModified: TDateTime;
    // Users password as stored in file:
    // - '$' + Base64(Salt) + '$' + Base64( SHA1 ( Salt + ClearTextPassword + Login ))
    FPassword: string;
    // a 4-byte string used for scrambling the password.
    // - is reset every time the user changes password (even if it is the same password).
    // - this gives approx. 2^32 different ways to store the same password.
    FSalt: UTF8String;
    FLogin: UTF8String;
    FNotes: UTF8String;
    function GetAdmin: TEpiAdmin;
    function GetLogin: UTF8String;
    procedure SetExpireDate(const AValue: TDateTime);
    procedure SetFullName(const AValue: UTF8String);
    procedure SetLastLogin(const AValue: TDateTime);
    procedure SetLogin(AValue: UTF8String);
    procedure SetPassword(const AValue: UTF8String);
    procedure SetNotes(AValue: UTF8String);
  protected
    property  Salt: UTF8String read FSalt;
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
    Property   Login: UTF8String read GetLogin write SetLogin;
    Property   Password: UTF8String read FPassword write SetPassword;
    Property   MasterPassword: string read FMasterPassword;
    // Scrambled data:
    Property   Groups: TEpiGroups read FGroups;
    Property   LastLogin: TDateTime read FLastLogin write SetLastLogin;
    Property   LastPWChange: TDateTime read FLastPWChange;
    property   ExpireDate: TDateTime read FExpireDate write SetExpireDate;
    property   FullName: UTF8String read FFullName write SetFullName;
    property   Notes: UTF8String read FNotes write SetNotes;
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

  { TEpiGroupRelation }

  TEpiGroupRelation = class(TEpiCustomRelationItem)
  private
    FGroup: TEpiGroup;
    function GetGroupRelation(const Index: integer): TEpiGroupRelation;
    function GetGroupRelations: TEpiGroupRelationList;
    function GetParentRelation: TEpiGroupRelation;
    procedure SetGroup(AValue: TEpiGroup);
    procedure GroupHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
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
  DCPbase64, DCPsha256, epistringutils, epimiscutils, epidocument, epilogger,
  math, epiglobals, laz2_XMLRead;

type

  { TEpiAdminGroups }

  TEpiAdminGroups = class(TEpiGroups)
  protected
    function NewItemLoad(const AName: EpiString; AItemClass: TEpiCustomItemClass
      = nil): TEpiCustomItem; override;
  end;

  { TEpiAdminGroup }

  TEpiAdminGroup = class(TEpiGroup)
  public
    constructor Create(AOwner: TEpiCustomBase); override;
  end;

  { TEpiAdminRelation }

  TEpiAdminRelation = class(TEpiGroupRelation)
  public
    constructor Create(AOwner: TEpiCustomBase); override;
  end;

{ TEpiAdminGroups }

function TEpiAdminGroups.NewItemLoad(const AName: EpiString;
  AItemClass: TEpiCustomItemClass): TEpiCustomItem;
begin
  if AName = EpiAdminGroupName then
    AItemClass := TEpiAdminGroup;

  Result := inherited NewItemLoad(AName, AItemClass);
end;

{ TEpiAdminsGroup }

constructor TEpiAdminGroup.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FProtectedItem := true;
end;

{ TEpiAdminRelation }

constructor TEpiAdminRelation.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FProtectedItem := true;
end;

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

function TEpiAdmin.DoRequestPassword(FailLogger: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap; DocumentNode: TDOMNode
  ): TEpiRequestPasswordResult;
var
  Login, Password, NewPassword: UTF8String;
  TheUser: TEpiUser;
  Key: String;
  Res: TEpiRequestPasswordResponse;
  Count: Integer;
  Node: TDOMNode;
  SS: TStringStream;
  MS: TMemoryStream;
  DeCrypter: TDCP_rijndael;
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

    // Increment the try counter
    Inc(Count);

    // Find user
    TheUser := Users.GetUserByLogin(Login);
    if not Assigned(TheUser) then
      begin
        // Login did not exists - send an event abount it.
        DoChange(eegAdmin, Word(eaceAdminIncorrectUserName), @Login);
        if TEpiFailedLogger(FailLogger).TooManyFailedLogins(EpiAdminLoginAttemps, EpiAdminLoginInterval) then
          begin
            DoChange(eegAdmin, word(eaceAdminBlockedLogin), nil);
            raise EEpiTooManyFailedLogins.Create(rsTooManyFailedAttemps);
          end;
        Continue;
      end;

    if ('$' + Base64EncodeStr(TheUser.Salt) + '$' + StrToSHA1Base64(TheUser.Salt + Password + Login) = TheUser.Password) then
      Result := prSuccess
    else
      begin
        Result := prFailed;
        DoChange(eegAdmin, Word(eaceAdminIncorrectPassword), @Login);

        if TEpiFailedLogger(FailLogger).TooManyFailedLogins(EpiAdminLoginAttemps, EpiAdminLoginInterval) then
          begin
            DoChange(eegAdmin, word(eaceAdminBlockedLogin), nil);
            raise EEpiTooManyFailedLogins.Create(rsTooManyFailedAttemps);
          end;
      end;
  until (Result = prSuccess) or (Res = rprStopOnFail);

  if (Result <> prSuccess) then
    Exit;

  Key := TheUser.Salt + Password + Login;
  MasterPassword := Decrypt(Key, TheUser.MasterPassword);

  // Load and decrypt the rest
  if (TEpiDocument(RootOwner).Version >= 6) and
     (Assigned(DocumentNode))
  then
    begin
      {$IFNDEF EPI_ADMIN_NOCRYPT_LOAD}
      LoadNode(Node, DocumentNode, rsEncrypted, true);

      SS := TStringStream.Create(Base64DecodeStr(Node.TextContent));
      MS := TMemoryStream.Create;

      DeCrypter := TDCP_rijndael.Create(nil);
      DeCrypter.InitStr(MasterPassword, TDCP_sha256);
      DeCrypter.DecryptStream(SS, MS, SS.Size);

      MS.Position := 0;
      ReadXMLFragment(DocumentNode, MS, [xrfPreserveWhiteSpace]);
      {$ENDIF}

      // This should load the whole Admin, including users etc...
      if LoadNode(Node, DocumentNode, rsAdmin, false) then
        LoadFromXml(Node, ReferenceMap);

      // At this point we now have access to both DaysBetweenPasswordChange + TEpiUser.LastLogin and TEpiUser.ExpireDate
      // this is the time to check for new password or expired accounts.
      // - check if the user is expired:
      if (TheUser.ExpireDate > 0) and
         (Now > TheUser.ExpireDate)
      then
        begin
          Result := prUserExpired;
          DoChange(eegAdmin, Word(eaceAdminUserExpired), TheUser);
          Exit;
        end;

      // - check if the password needs to change
      if (DaysBetweenPasswordChange > 0) and
         (Now > TheUser.LastPWChange + DaysBetweenPasswordChange) then
        begin
          Count := 1;
          repeat
            Res := OnPassword(Self, erpNewPassword, Count, Login, NewPassword);
            Inc(Count);

            // The user canceled the login
            if (Res = rprCanceled) then
              begin
                Result := prCanceled;
                Break;
              end;

            if (NewPassword <> '') and
               (NewPassword <> Password)
            then
              Result := prSuccess
            else
              begin
                Result := prFailed;
                DoChange(eegAdmin, Word(eaceAdminIncorrectNewPassword), TheUser);
              end;

          until (Result = prSuccess) or (Res = rprStopOnFail);
        end;
    end;

  if (Result = prSuccess) then
    begin
      TheUser.LastLogin := Now;
      DoUserAuthorized(TheUser);
      DoChange(eegAdmin, Word(eaceAdminLoginSuccessfull), TheUser);

      if (NewPassword <> '') then
        TheUser.Password := NewPassword;
    end;
end;

procedure TEpiAdmin.SetMasterPassword(const AValue: string);
begin
  if FMasterPassword = AValue then exit;
  FMasterPassword := AValue;
end;

procedure TEpiAdmin.DoUserAuthorized(const User: TEpiUser);
begin
  FAuthorizedUser := User;
  if Assigned(OnUserAuthorized) then
    OnUserAuthorized(Self, User);
end;

procedure TEpiAdmin.SetDaysBetweenPasswordChange(AValue: integer);
var
  Val: Integer;
begin
  if FDaysBetweenPasswordChange = AValue then Exit;
  Val := FDaysBetweenPasswordChange;
  FDaysBetweenPasswordChange := AValue;
  DoChange(eegAdmin, Word(eaceAdminDayBetweenPassword), @Val);
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
  FInitialized := false;

  for i := 0 to 3 do
    Key[i] := Random(maxLongint - 1) + 1;
  MasterPassword := String(KeyByte);

  FUsers := TEpiUsers.Create(self);
  FUsers.ItemOwner := true;
  FUsers.Name := 'TEpiAdmin_Users';

  FGroups := TEpiAdminGroups.Create(self);
  FGroups.ItemOwner := true;
  FGroups.Name := 'TEpiAdmin_Groups';

  FAdminRelations := TEpiGroupRelationList.Create(Self);
  FAdminRelations.ItemOwner := true;

  FCrypter := TDCP_rijndael.Create(nil);
  FRSA     := TEpiRSA.Create;

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
  S: EpiString;
  PWRes: TEpiRequestPasswordResponse;
  Count: Integer;
  FRes: TEpiRequestPasswordResult;
  Login, NewPassword: UTF8String;
begin
  // Root = <Admin>
  inherited LoadFromXml(Root, ReferenceMap);

  // Version 6:
  FDaysBetweenPasswordChange := LoadAttrInt(Root, rsDaysBetweemPw, 90, false);

  // Load groups
  if LoadNode(Node, Root, rsGroups, false) then
    Groups.LoadFromXml(Node, ReferenceMap);

  // Then load users
  if LoadNode(Node, Root, rsUsers, false) then
    Users.LoadFromXml(Node, ReferenceMap);

  // finally load the relationships
  LoadNode(Node, Root, 'GroupRelation', true);

  // Load the always existing Admin relation.
  FAdminRelation := TEpiGroupRelation(FAdminRelations.NewItem(TEpiAdminRelation));   //NewGroupRelation;
  AdminRelation.LoadFromXml(Node, ReferenceMap);

  // Now load the private certificate (in case we need to decrypt failed logins)
  S := LoadNodeString(Root, 'PrivCert');
  if (S <> '') then
    FRSA.PrivateKey := S;

  // During fixup, set the correct admingroup!
  ReferenceMap.AddFixupReference(Self, TEpiAdmin, 0, '');

  // Admin is only loaded from XML if it truely exists - hence it is initialized
  FInitialized := true;
end;

function TEpiAdmin.LoadCrypto(Root: TDOMNode; ReferenceMap: TEpiReferenceMap;
  FailLogger: TEpiCustomBase): TEpiRequestPasswordResult;
var
  Node: TDOMNode;
  S: EpiString;
begin
  // Root = <Crypto>
  if LoadNode(Node, Root, rsUsers, True) then
    Users.PreLoadUsers(Node);

  S := LoadNodeString(Root, 'PubCert', '', false);
  if (S <> '') then
    FRSA.PublicKey := S;

  DoChange(eegAdmin, Word(eaceAdminInitializing), nil);
  Result := DoRequestPassword(FailLogger, ReferenceMap, Root.OwnerDocument.DocumentElement);
end;

function TEpiAdmin.SaveCrypto(RootDoc: TDOMDocument): TDOMElement;
begin
  result := RootDoc.CreateElement(rsCrypto);

  Result.AppendChild(Users.PreSaveUsers(RootDoc));
  SaveTextContent(Result, 'PubCert', FRSA.PublicKey);
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
      1: begin
           FAdminRelation := FAdminRelations[0];
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
  DoChange(eegAdmin, Word(eaceAdminResetting), nil);

  Users.ClearAndFree;
  Groups.ClearAndFree;
  FAdminsGroup := nil;
  FAdminRelation := nil;
  FInitialized := false;
end;

procedure TEpiAdmin.InitAdmin;
begin
  if Assigned(FAdminsGroup) then exit;

  FAdminsGroup := TEpiAdminGroup.Create(nil);
  FAdminsGroup.ManageRights := EpiAllManageRights;
  FAdminsGroup.Caption.TextLang['en'] := 'Admins';
  FAdminsGroup.Name := EpiAdminGroupName;
  FGroups.AddItem(FAdminsGroup);

  FAdminRelation := TEpiGroupRelation(FAdminRelations.NewItem(TEpiAdminRelation)); //FAdminRelations.NewGroupRelation;
  FAdminRelation.Group := FAdminsGroup;

  FRSA.GenerateKeys();
  DoChange(eegAdmin, word(eaceAdminInitializing), nil);

  FInitialized := true;
end;

function TEpiAdmin.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
var
  Elem: TDOMElement;
  S: String;
begin
  if Users.Count = 0 then
    Exit(nil);

  Result := inherited SaveToDom(RootDoc);

  // Version 6:
  SaveDomAttr(Result, rsDaysBetweemPw, DaysBetweenPasswordChange);

  Elem := AdminRelation.SaveToDom(RootDoc);
  if Assigned(Elem) then
    Result.AppendChild(Elem);

  SaveTextContent(Result, 'PrivCert', FRSA.PrivateKey);
end;

function TEpiAdmin.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  With TEpiAdmin(Result) do
    begin
      Self.FAdminRelations.DoClone(Result, FAdminRelations, ReferenceMap);
      FDaysBetweenPasswordChange := Self.FDaysBetweenPasswordChange;
      FMasterPassword := Self.FMasterPassword;
      FRSA.PrivateKey := Self.FRSA.PrivateKey;
      FRSA.PublicKey  := Self.FRSA.PublicKey;
    end;

  if Assigned(FAdminsGroup) then
    begin
      ReferenceMap.AddFixupReference(Result, TEpiAdmin, 1, '');
      ReferenceMap.AddFixupReference(Result, TEpiAdmin, 0, '');
    end;
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

procedure TEpiUsers.PreLoadUsers(Root: TDOMNode);
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
    NUser.FMasterPassword := LoadAttrString(Node, rsMasterPassword);

    Node := Node.NextSibling;
  end;
end;

function TEpiUsers.PreSaveUsers(RootDoc: TDOMDocument): TDOMElement;
var
  UserNode: TDOMElement;
  i: Integer;
  User: TEpiUser;
begin
  Result := RootDoc.CreateElement(rsUsers);

  for User in Self do
    begin
      UserNode := RootDoc.CreateElement(rsUser);

      SaveDomAttr(UserNode, rsName,           User.Login);
      SaveDomAttr(UserNode, rsPassword,       User.Password);
      SaveDomAttr(UserNode, rsMasterPassword, User.MasterPassword);

      Result.AppendChild(UserNode);
    end;
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
      // login/pw that should have been loaded during PreLoadUsers;
      NUser := NewUser;
      NUser.Login := LoadAttrString(Node, rsId);
      NUser.FPassword := LoadAttrString(Node, rsPassword);
      NUser.FMasterPassword := LoadAttrString(Node, rsMasterPassword);
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

procedure TEpiUser.SetFullName(const AValue: UTF8String);
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

function TEpiUser.GetLogin: UTF8String;
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

procedure TEpiUser.SetLogin(AValue: UTF8String);
begin
  // TODO: changing login name should trigger a whole lot of things:
  // 1: Validating that no other user has the same login
  // 2: Recompute password hash
  // 3: Recomputer master password hash
  Name := AValue;
  FLogin := Name;
end;

procedure TEpiUser.SetPassword(const AValue: UTF8String);
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
  FMasterPassword := Admin.Encrypt(Key, Admin.MasterPassword);

  FLastPWChange := Now;
  DoChange(eegAdmin, Word(eaceUserSetPassword), nil);
end;

procedure TEpiUser.SetNotes(AValue: UTF8String);
var
  Val: String;
begin
  if FNotes = AValue then Exit;
  Val := FNotes;
  FNotes := AValue;
  DoChange(eegAdmin, Word(eaceUserSetNotes), @Val);
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
    FCreated        := Self.FCreated;
    FModified       := Self.FModified;
    FLastPWChange   := Self.FLastPWChange;
    FExpireDate     := Self.FExpireDate;
    FLastLogin      := Self.FLastLogin;
    FFullName       := Self.FFullName;
    FLogin          := Self.FLogin;
    FMasterPassword := Self.FMasterPassword;
    FPassword       := Self.FPassword;
    FSalt           := Self.FSalt;
    FNotes          := Self.FNotes;
  end;

  S := '';
  for G in Groups do
    S := S + ',' + G.Name;

  if S <> '' then
    Delete(S,1,1);

  ReferenceMap.AddFixupReference(Result, TEpiUser, 0, S);
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
  FLastLogin := MinDateTime;
  FCreated := Now;
  FLastPWChange := Now;
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

  FFullName   := LoadNodeString(Root, rsFullName, '', false);
  FNotes      := LoadNodeString(Root, rsNotes, '', false);

  // When loading the authenticated user, the LastLogin is set immediately, hence
  // this user should not have loaded the last-login information from the XML.
  if (FLastLogin = MinDateTime) then
    FLastLogin  := LoadAttrDateTime(Root, rsLastLogin, '', 0, false);
  FExpireDate := LoadAttrDateTime(Root, rsExpireDate, '', 0, false);

  // Version 6: (if last pw change wasn't set, then set it to today)
  FLastPWChange := LoadAttrDateTime(Root, rsLastPasswordChange, '', 0, false);
  if (FLastPWChange = 0) then
    FLastPWChange := Now();

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

  if (FullName <> '') then
    SaveTextContent(Result, rsFullName, FullName);
  if (Notes <> '') then
    SaveTextContent(Result, rsNotes, Notes);

  if (LastLogin > 0) then
    SaveDomAttr(Result, rsLastLogin, LastLogin);

  if (ExpireDate > 0) then
    SaveDomAttr(Result, rsExpireDate, ExpireDate);

  SaveDomAttr(Result, rsCreatedAttr, Created);
  SaveDomAttr(Result, rsModifiedAttr, Modified);
  SaveDomAttr(Result, rsLastPasswordChange, FLastPWChange);

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
  with TEpiGroup(Result) do
  begin
    FManageRights := Self.FManageRights;
    FCreated      := Self.FCreated;
    FModified     := Self.FModified;
  end;
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

  FCreated    := LoadAttrDateTime(Root, rsCreatedAttr, '', Now);
  FModified   := LoadAttrDateTime(Root, rsModifiedAttr, '', Now, false);
end;

{ TEpiGroupRelation }

procedure TEpiGroupRelation.SetGroup(AValue: TEpiGroup);
begin
  if FGroup = AValue then Exit;

  if Assigned(Group) then
    Group.UnRegisterOnChangeHook(@GroupHook);

  FGroup := AValue;

  if Assigned(Group) then
    Group.RegisterOnChangeHook(@GroupHook, true);

  ObserveReference(FGroup, 'Group');
  DoSendAssignObjectChangeEvent('Group', Group);
end;

procedure TEpiGroupRelation.GroupHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  GR: TEpiGroupRelation;
  RightsDiff: TEpiManagerRights;
begin
  if (Initiator <> Group) then exit;
  if (EventGroup <> eegAdmin) then exit;
  if (TEpiAdminChangeEventType(EventType) <> eaceGroupSetManageRights) then Exit;

  // if there is a change in the rights for this group, we need to remove
  // the rights from child groups that was removed from this group.
  // We should not add managerrights, since this is a job for programs to
  // implement.
  RightsDiff := TEpiManagerRights(Data^) - Group.ManageRights;
  if RightsDiff <> [] then
    for GR in GroupRelations do
      GR.Group.ManageRights := GR.Group.ManageRights - RightsDiff;
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
    'Group': Group := nil;
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

