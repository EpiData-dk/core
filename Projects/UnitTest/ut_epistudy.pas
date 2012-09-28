unit ut_epistudy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, epicustombase, epidocument, epistudy;

type

  { TUnitTest_EpiStudy }

  TUnitTest_EpiStudy = class(TTestCase)
  private
    FCallBacks: integer;
    FEpiDoc: TEpiDocument;
    FStudy: TEpiStudy;
    procedure ChangeEvent(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  public
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
  published
    procedure AbstractText;
    procedure Author;
    procedure Agency;
    procedure Citations;
//    procedure Created;
    procedure Funding;
    procedure GeographicalCoverage;
    procedure Identifier;
    procedure ModifiedDate;
    procedure Notes;
    procedure Publisher;
    procedure Purpose;
    procedure Population;
    procedure Rights;
    procedure TimeCoverage;
    procedure Title;
    procedure Keywords;
    procedure Version;
    procedure ChangeEventCheck;
  end;

implementation

const
  ABSTRACT_TEXT = 'Abstract Text';
  AUTHOR_TEXT = 'Author Text';
  AGENCY_TEXT = 'Agency Text';
  CITATIONS_TEXT = 'Citations Text';
  FUNDING_TEXT = 'Funding Text';
  GEOGRAPHICALCOVERAGE_TEXT = 'GeographicalCoverage Text';
  IDENTIFIER_TEXT = 'Identifier Text';
  NOTES_TEXT = 'Notes Text';
  PUBLISHER_TEXT = 'Publisher Text';
  PURPOSE_TEXT = 'Purpose Text';
  POPULATION_TEXT = 'Population Text';
  RIGHTS_TEXT = 'Rights Text';
  TIMECOVERAGE_TEXT = 'TimeCoverage Text';
  TITLE_TEXT = 'Title Text';
  KEYWORDS_TEXT = 'Keywords,Text';
  VERSION_TEXT = 'Version Text';                      // 10 = TEpiAbstractText callbacks
  CALLBACK_CALLS = Word(High(TEpiStudyChangeEvent)) + 10 + 1;

{ TUnitTest_EpiStudy }

procedure TUnitTest_EpiStudy.ChangeEvent(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup = eegCustomBase) and
     (TEpiCustomChangeEventType(EventType) = ecceText)
  then
    Inc(FCallBacks);

  if EventGroup = eegStudy then
  case TEpiStudyChangeEvent(EventType) of
    esceAgency:       Inc(FCallBacks);
    esceAuthor:       Inc(FCallBacks);
    esceIdentifier:   Inc(FCallBacks);
    esceKeywords:     Inc(FCallBacks);
    esceModifiedDate: Inc(FCallBacks);
    esceNotes:        Inc(FCallBacks);
    esceVersion:      Inc(FCallBacks);
  end;
end;

procedure TUnitTest_EpiStudy.SetUpOnce;
begin
  inherited SetUpOnce;
  FEpiDoc := TEpiDocument.Create('');
  FStudy := FEpiDoc.Study;
  FStudy.RegisterOnChangeHook(@ChangeEvent);
end;

procedure TUnitTest_EpiStudy.TearDownOnce;
begin
  FStudy.UnRegisterOnChangeHook(@ChangeEvent);
  FEpiDoc.Free;
  inherited TearDownOnce;
end;

procedure TUnitTest_EpiStudy.AbstractText;
var
  Val: String;
begin
  Val := FStudy.AbstractText.Text;
  FStudy.AbstractText.Text := ABSTRACT_TEXT;
  CheckNotEquals(Val, FStudy.AbstractText.Text);
end;

procedure TUnitTest_EpiStudy.Author;
var
  Val: String;
begin
  Val := FStudy.Author;
  FStudy.Author := AUTHOR_TEXT;
  CheckNotEquals(Val, FStudy.Author);
end;

procedure TUnitTest_EpiStudy.Agency;
var
  Val: String;
begin
  Val := FStudy.Agency;
  FStudy.Agency := AGENCY_TEXT;
  CheckNotEquals(Val, FStudy.Agency);
end;

procedure TUnitTest_EpiStudy.Citations;
var
  Val: String;
begin
  Val := FStudy.Citations.Text;
  FStudy.Citations.Text := CITATIONS_TEXT;
  CheckNotEquals(Val, FStudy.Citations.Text);
end;
{
procedure TUnitTest_EpiStudy.Created;
begin
  // TODO : A test for Created?
end;
}
procedure TUnitTest_EpiStudy.Funding;
var
  Val: String;
begin
  Val := FStudy.Funding.Text;
  FStudy.Funding.Text := FUNDING_TEXT;
  CheckNotEquals(Val, FStudy.Funding.Text);
end;

procedure TUnitTest_EpiStudy.GeographicalCoverage;
var
  Val: String;
begin
  Val := FStudy.GeographicalCoverage.Text;
  FStudy.GeographicalCoverage.Text := GEOGRAPHICALCOVERAGE_TEXT;
  CheckNotEquals(Val, FStudy.GeographicalCoverage.Text);
end;

procedure TUnitTest_EpiStudy.Identifier;
var
  Val: String;
begin
  Val := FStudy.Identifier;
  FStudy.Identifier := IDENTIFIER_TEXT;
  CheckNotEquals(Val, FStudy.Identifier);
end;

procedure TUnitTest_EpiStudy.ModifiedDate;
var
  Val: TDateTime;
begin
  Val := FStudy.ModifiedDate;
  FStudy.ModifiedDate := Val - 1;
  CheckNotEquals(Val, FStudy.ModifiedDate);
end;

procedure TUnitTest_EpiStudy.Notes;
var
  Val: String;
begin
  Val := FStudy.Notes;
  FStudy.Notes := NOTES_TEXT;
  CheckNotEquals(Val, FStudy.Notes);
end;

procedure TUnitTest_EpiStudy.Publisher;
var
  Val: String;
begin
  Val := FStudy.Publisher.Text;
  FStudy.Publisher.Text := PUBLISHER_TEXT;
  CheckNotEquals(Val, FStudy.Publisher.Text);
end;

procedure TUnitTest_EpiStudy.Purpose;
var
  Val: String;
begin
  Val := FStudy.Purpose.Text;
  FStudy.Purpose.Text := PURPOSE_TEXT;
  CheckNotEquals(Val, FStudy.Purpose.Text);
end;

procedure TUnitTest_EpiStudy.Population;
var
  Val: String;
begin
  Val := FStudy.Population.Text;
  FStudy.Population.Text := POPULATION_TEXT;
  CheckNotEquals(Val, FStudy.Population.Text);
end;

procedure TUnitTest_EpiStudy.Rights;
var
  Val: String;
begin
  Val := FStudy.Rights.Text;
  FStudy.Rights.Text := RIGHTS_TEXT;
  CheckNotEquals(Val, FStudy.Rights.Text);
end;

procedure TUnitTest_EpiStudy.TimeCoverage;
var
  Val: String;
begin
  Val := FStudy.TimeCoverage.Text;
  FStudy.TimeCoverage.Text := TIMECOVERAGE_TEXT;
  CheckNotEquals(Val, FStudy.TimeCoverage.Text);
end;

procedure TUnitTest_EpiStudy.Title;
var
  Val: String;
begin
  Val := FStudy.Title.Text;
  FStudy.Title.Text := TITLE_TEXT;
  CheckNotEquals(Val, FStudy.Title.Text);
end;

procedure TUnitTest_EpiStudy.Keywords;
var
  Val: String;
begin
  Val := FStudy.Keywords;
  FStudy.Keywords := KEYWORDS_TEXT;
  CheckNotEquals(Val, FStudy.Keywords);
end;

procedure TUnitTest_EpiStudy.Version;
var
  Val: String;
begin
  Val := FStudy.Version;
  FStudy.Version := VERSION_TEXT;
  CheckNotEquals(Val, FStudy.Version);
end;

procedure TUnitTest_EpiStudy.ChangeEventCheck;
begin
  CheckEquals(CALLBACK_CALLS, FCallBacks);
end;

initialization
  RegisterTest(TUnitTest_EpiStudy.Suite);

end.

