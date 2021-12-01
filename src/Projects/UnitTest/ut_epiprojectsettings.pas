unit ut_epiprojectsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, epicustombase, epidocument, episettings;

type

  { TUnitTest_EpiProjectSettings }

  TUnitTest_EpiProjectSettings = class(TTestCase)
  private
    FCallBacks: integer;
    FEpiDoc: TEpiDocument;
    FSettings: TEpiProjectSettings;
    procedure ChangeEvent(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  public
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
  published
    procedure ShowFieldBorders;
    procedure ShowFieldNames;
    procedure BackupInterval;
    procedure BackupOnShutdown;
    procedure AutoIncStartValue;
    procedure ChangeEventCheck;
  end;

implementation

const
  BACKUP_INTERVAL     = 99;
  AUTOINC_START_VALUE = 99;
  CALLBACK_CALLS = Word(High(TEpiProjectSettingChangeEvent)) + 1;


procedure TUnitTest_EpiProjectSettings.ChangeEvent(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if EventGroup <> eegProjectSettings then exit;

  case TEpiProjectSettingChangeEvent(EventType) of
    epceFieldName:      Inc(FCallBacks);
    epceFieldBorder:    Inc(FCallBacks);
    epceBackupInterval: Inc(FCallBacks);
    epceBackupShutdown: Inc(FCallBacks);
    epceAutoIncStart:   Inc(FCallBacks);
  end;
end;

procedure TUnitTest_EpiProjectSettings.SetUpOnce;
begin
  inherited SetUpOnce;
  FEpiDoc := TEpiDocument.Create('en');
  FSettings := FEpiDoc.ProjectSettings;
  FSettings.RegisterOnChangeHook(@ChangeEvent);
end;

procedure TUnitTest_EpiProjectSettings.TearDownOnce;
begin
  FSettings.UnRegisterOnChangeHook(@ChangeEvent);
  FEpiDoc.Free;
  inherited TearDownOnce;
end;

procedure TUnitTest_EpiProjectSettings.ShowFieldBorders;
var
  Val: Boolean;
begin
  Val := FSettings.ShowFieldBorders;
  FSettings.ShowFieldBorders := not Val;
  CheckNotEquals(Val, FSettings.ShowFieldBorders, 'ShowFieldBorders = Val');
end;

procedure TUnitTest_EpiProjectSettings.ShowFieldNames;
var
  Val: Boolean;
begin
  Val := FSettings.ShowFieldNames;
  FSettings.ShowFieldNames := not Val;
  CheckNotEquals(Val, FSettings.ShowFieldNames, 'ShowFieldNames = Val');
end;

procedure TUnitTest_EpiProjectSettings.BackupInterval;
begin
  FSettings.BackupInterval := BACKUP_INTERVAL;
  CheckEquals(BACKUP_INTERVAL, FSettings.BackupInterval, 'BackupInterval <> BACKUP_INTERVAL');
end;

procedure TUnitTest_EpiProjectSettings.BackupOnShutdown;
var
  Val: Boolean;
begin
  Val := FSettings.BackupOnShutdown;
  FSettings.BackupOnShutdown := not Val;
  CheckNotEquals(Val, FSettings.BackupOnShutdown, 'ShowFieldBorders = val');
end;

procedure TUnitTest_EpiProjectSettings.AutoIncStartValue;
begin
  FSettings.AutoIncStartValue := AUTOINC_START_VALUE;
  CheckEquals(AUTOINC_START_VALUE, FSettings.AutoIncStartValue, 'ShowFieldBorders <> AUTOINC_START_VALUE');
end;

procedure TUnitTest_EpiProjectSettings.ChangeEventCheck;
begin
  CheckEquals(CALLBACK_CALLS, FCallBacks, 'Did not make enough callbacks through ChangeEvent!');
end;

initialization

  RegisterTest(TUnitTest_EpiProjectSettings.Suite);
end.

