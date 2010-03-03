unit epiadmin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TEpiAdmin = class;
  TEpiUsers = class;
  TEpiUser = class;
  TEpiGroups = class;
  TEpiGroup = class;

  TEpiAdminRight = (erManage, erRead, erWrite);
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

  TEpiCustomAdmin = class
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
    Property   Users: TEpiUsers read FUsers;
    Property   Groups: TEpiGroups read FGroups;
  end;

  { TEpiUsers }

  TEpiUsers = class
  private
    FList: TFPList;
    function GetCount: integer;
    function GetUser(Index: integer): TEpiUser;
  public
    constructor Create;
    destructor Destroy; override;
    Property   User[Index: integer]: TEpiUser read GetUser; default;
    Property   Count: integer read GetCount;
  end;

  { TEpiUser }

  TEpiUser = class(TEpiCustomAdmin)
  private
    FGroup: TEpiGroup;
    FId: string;
    FLogin: string;
    FName: string;
    FPassword: string;
    procedure SetGroup(const AValue: TEpiGroup);
    procedure SetId(const AValue: string);
    procedure SetLogin(const AValue: string);
    procedure SetName(const AValue: string);
    procedure SetPassword(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    Property   Id: string read FId write SetId;
    Property   Login: string read FLogin write SetLogin;
    Property   Name: string read FName write SetName;
    Property   Password: string read FPassword write SetPassword;
    Property   Group: TEpiGroup read FGroup write SetGroup;
  end;

  { TEpiGroups }

  TEpiGroups = class
  private
    FList: TFPList;
    function GetCount: integer;
    function GetGroup(Index: integer): TEpiGroup;
  public
    constructor Create;
    destructor Destroy; override;
    Property   Group[Index: integer]: TEpiGroup read GetGroup; default;
    Property   Count: integer read GetCount;
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
    Property   Id: string read FId write SetId;
    Property   Name: string read FName write SetName;
    Property   Rights: TEpiAdminRights read FRights write SetRights;
  end;

implementation

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

procedure TEpiUser.SetId(const AValue: string);
var
  Val: String;
begin
  if FId = AValue then exit;
  Val := FId;
  FId := AValue;
  DoChange(aeUserSetId, @Val);
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

end.

