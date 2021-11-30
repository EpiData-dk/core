unit ut_epixmlsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, epicustombase, epidocument, episettings;

type

  { TUnitTest_EpiProjectSettings }

  { TUnitTest_EpiXMLSettings }

  TUnitTest_EpiXMLSettings = class(TTestCase)
  private
    FCallBacks: integer;
    FEpiDoc: TEpiDocument;
    FSettings: TEpiXMLSettings;
    procedure ChangeEvent(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  public
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
  published
    procedure DateSeparator;
    procedure TimeSeparator;
    procedure DecimalSeparator;
    procedure Scrambled;
    procedure ChangeEventCheck;
  end;

implementation

const
  DATESEP = '!';
  TIMESEP = '=';
  DECSEP  = '_';
  SCRAMBLED_CONST = true;
  CALLBACK_CALLS = Word(High(TEpiSettingChangeEvent)) + 1;

{ TUnitTest_EpiXMLSettings }

procedure TUnitTest_EpiXMLSettings.ChangeEvent(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if EventGroup <> eegXMLSetting then exit;

  case TEpiSettingChangeEvent(EventType) of
    esceDateSep:  Inc(FCallBacks);
    esceTimeSep:  Inc(FCallBacks);
    esceDecSep:   Inc(FCallBacks);
    esceScramble: Inc(FCallBacks);
  end;
end;

procedure TUnitTest_EpiXMLSettings.SetUpOnce;
begin
  inherited SetUpOnce;
  FEpiDoc := TEpiDocument.Create('en');
  FSettings := FEpiDoc.XMLSettings;
  FSettings.RegisterOnChangeHook(@ChangeEvent);
end;

procedure TUnitTest_EpiXMLSettings.TearDownOnce;
begin
  FSettings.UnRegisterOnChangeHook(@ChangeEvent);
  FEpiDoc.Free;
  inherited TearDownOnce;
end;

procedure TUnitTest_EpiXMLSettings.DateSeparator;
begin
  FSettings.DateSeparator := DATESEP;
  CheckEquals(DATESEP, FSettings.DateSeparator, 'DateSepartor <> ' + DATESEP);
end;

procedure TUnitTest_EpiXMLSettings.TimeSeparator;
begin
  FSettings.TimeSeparator := TIMESEP;
  CheckEquals(TIMESEP, FSettings.TimeSeparator, 'TimeSeparator <> ' + TIMESEP);
end;

procedure TUnitTest_EpiXMLSettings.DecimalSeparator;
begin
  FSettings.DecimalSeparator := DECSEP;
  CheckEquals(DECSEP, FSettings.DecimalSeparator, 'DecimalSeparator <> ' + DECSEP);
end;

procedure TUnitTest_EpiXMLSettings.Scrambled;
begin
  FSettings.Scrambled := SCRAMBLED_CONST;
  CheckEquals(SCRAMBLED_CONST, FSettings.Scrambled, 'Scrambled <> ' + BoolToStr(SCRAMBLED_CONST));
end;

procedure TUnitTest_EpiXMLSettings.ChangeEventCheck;
begin
  CheckEquals(CALLBACK_CALLS, FCallBacks, 'Did not make enough callbacks through ChangeEvent!');
end;

initialization
  RegisterTest(TUnitTest_EpiXMLSettings.Suite);

end.

