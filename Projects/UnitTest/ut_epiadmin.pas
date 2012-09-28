unit ut_epiadmin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, epicustombase, epidocument, epiadmin;

type

  { TUnitTest_EpiProjectSettings }

  TUnitTest_EpiProjectSettings = class(TTestCase)
  private
    FCallBacks: integer;
    FEpiDoc: TEpiDocument;
    FAdmins: TEpiAdmin;
    procedure ChangeEvent(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  public
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
  published
    // TODO : Implement Unit Test when Admin is implemented/used.
    procedure EmptyCheck;
  end;

implementation

const
  CALLBACK_CALLS = Word(High(TEpiAdminChangeEventType)) + 1;

{ TUnitTest_EpiProjectSettings }

procedure TUnitTest_EpiProjectSettings.ChangeEvent(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  //
end;

procedure TUnitTest_EpiProjectSettings.SetUpOnce;
begin
  inherited SetUpOnce;
end;

procedure TUnitTest_EpiProjectSettings.TearDownOnce;
begin
  inherited TearDownOnce;
end;

procedure TUnitTest_EpiProjectSettings.EmptyCheck;
begin
  Check(true, 'Just Do An Empty Check');
end;

initialization
 RegisterTest(TUnitTest_EpiProjectSettings.Suite);

end.

