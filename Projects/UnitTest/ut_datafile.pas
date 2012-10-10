unit ut_datafile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, epicustombase, epidocument, epidatafiles;

type

  { TUnitTest_EpiStudy }

  { TUnitTest_EpiDataFiles }

  TUnitTest_EpiDataFiles = class(TTestCase)
  private
    FCallBacks: integer;
    FEpiDoc: TEpiDocument;
    FDataFiles: TEpiDataFiles;
    FDataFile: TEpiDataFile;
    procedure ChangeEvent(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  private
    {Support}
    F1: TEpiField;
    F2: TEpiField;
    F3: TEpiField;
    // Sets up 3 fields with values 0.., 0.0..., A... (Size of Datafile)!
    procedure Setup3Fields;
    procedure TearDown3Fields;
  public
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
  published
    procedure NewField;
    procedure NewSection;
    procedure NewHeading;
    procedure NewRecords;
    procedure Pack;
    procedure ExchangeRecords;
    procedure SortRecords;
    procedure MainSection;
    procedure Sections;
    procedure Section;
    procedure Fields;
    procedure Field;
    procedure ControlItems;
    procedure ControlItem;
    procedure Headings;
    procedure Heading;
    procedure ValueLabels;
    procedure ValueLabelSet;
    procedure Size;
    procedure DeletedCount;
    procedure VerifiedCount;
    procedure Caption;
    procedure Version;
    procedure Notes;
    procedure KeyFields;
  end;

implementation

uses
  epidatafilestypes, epidatafileutils, epitools_integritycheck;

const
  DATAFILE_CAPTION_TEXT = 'Datafile Caption Text';
  DATAFILE_VERSION_TEXT = 'Datafile Version Text';
  DATAFILE_NOTES_TEXT = 'Datafile Notes Text';

{ TUnitTest_EpiDataFiles }

procedure TUnitTest_EpiDataFiles.ChangeEvent(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin

end;

procedure TUnitTest_EpiDataFiles.Setup3Fields;
var
  i: Integer;
begin
  F1 := FDataFile.NewField(ftInteger);
  F2 := FDataFile.NewField(ftFloat);
  F3 := FDataFile.NewField(ftString);

  for i := 0 to FDataFile.Size - 1 do
  begin
    F1.AsInteger[i] := i;
    F2.AsFloat[i] := i * 0.1;
    F3.AsString[i] := Char(i + Ord('A'));
  end;
end;

procedure TUnitTest_EpiDataFiles.TearDown3Fields;
begin
  F1.Free;
  F2.Free;
  F3.Free;
end;

procedure TUnitTest_EpiDataFiles.SetUpOnce;
begin
  inherited SetUpOnce;
  FEpiDoc := TEpiDocument.Create('en');
  FDataFiles := FEpiDoc.DataFiles;
  FDataFile := FDataFiles.NewDataFile;
end;

procedure TUnitTest_EpiDataFiles.TearDownOnce;
begin
  FEpiDoc.Free;
  inherited TearDownOnce;
end;

procedure TUnitTest_EpiDataFiles.NewField;
var
  F: TEpiField;
  S: string;
  ft: TEpiFieldType;
begin
  for ft in TEpiFieldType do
  begin
    WriteStr(S, ft);
    F := FDataFile.NewField(ft);
    CheckNotNull(F, 'F is null: ' + S);
    CheckIs(F, FieldClassFromFieldType(ft), 'F is not ' + S);
    F.Free;
  end;
end;

procedure TUnitTest_EpiDataFiles.NewSection;
var
  S: TEpiSection;
begin
  S := FDataFile.NewSection;
  CheckNotNull(S, 'NewSection is null');
  S.Free;
end;

procedure TUnitTest_EpiDataFiles.NewHeading;
var
  H: TEpiHeading;
begin
  H := FDataFile.NewHeading;
  CheckNotNull(H, 'NewHeading is null');
  H.Free;
end;

procedure TUnitTest_EpiDataFiles.NewRecords;
var
  Sz: Integer;
begin
  FDataFile.NewRecords();
  CheckEquals(1, FDataFile.Size, 'DataSize <> 1');

  FDataFile.NewRecords(10);
  CheckEquals(11, FDataFile.Size, 'DataSize <> 11');
end;

procedure TUnitTest_EpiDataFiles.Pack;
var
  i: Integer;
begin
  FDataFile.Size := 10;

  Setup3Fields;
  for i := 0 to FDataFile.Size - 1 do
    FDataFile.Deleted[i] := (i mod 3) = 0;

  FDataFile.Pack;
  CheckEquals(6, FDataFile.Size, 'Pack failed: Size');
  CheckEquals(1, F1.AsInteger[0], 'Pack failed: F1[0]');
  CheckEquals(4, F1.AsInteger[2], 'Pack failed: F1[2]');
  CheckEquals(8, F1.AsInteger[5], 'Pack failed: F1[5]');
  CheckEquals(0.1, F2.AsFloat[0], 'Pack failed: F2[0]');
  CheckEquals(0.4, F2.AsFloat[2], 'Pack failed: F2[2]');
  CheckEquals(0.8, F2.AsFloat[5], 'Pack failed: F2[5]');
  CheckEquals('B', F3.AsString[0], 'Pack failed: F3[0]');
  CheckEquals('E', F3.AsString[2], 'Pack failed: F3[2]');
  CheckEquals('I', F3.AsString[5], 'Pack failed: F3[5]');

  TearDown3Fields;
  FDataFile.Size := 0;
end;

procedure TUnitTest_EpiDataFiles.ExchangeRecords;
begin
  FDataFile.Size := 10;
  Setup3Fields;

  FDataFile.ExchangeRecords(2, 6);
  CheckEquals(0, F1.AsInteger[0], 'ExchangeRecords failed: F1[0]');
  CheckEquals(6, F1.AsInteger[2], 'ExchangeRecords failed: F1[2]');
  CheckEquals(7, F1.AsInteger[7], 'ExchangeRecords failed: F1[7]');
  CheckEquals(0.0, F2.AsFloat[0], 'ExchangeRecords failed: F2[0]');
  CheckEquals(0.6, F2.AsFloat[2], 'ExchangeRecords failed: F2[2]');
  CheckEquals(0.7, F2.AsFloat[7], 'ExchangeRecords failed: F2[7]');
  CheckEquals('A', F3.AsString[0], 'ExchangeRecords failed: F3[0]');
  CheckEquals('G', F3.AsString[2], 'ExchangeRecords failed: F3[2]');
  CheckEquals('H', F3.AsString[7], 'ExchangeRecords failed: F3[7]');

  TearDown3Fields;
  FDataFile.Size := 0;
end;

procedure TUnitTest_EpiDataFiles.SortRecords;
var
  F: TEpiField;
  i: Integer;
const
  Delta = 0.0000000001;
begin
  FDataFile.Size := 10;
  Setup3Fields;

  F := TEpiField.CreateField(nil, ftInteger);
  F.Size := FDataFile.Size;
  for i := 0 to 9 do
    F.AsInteger[i] := 10 - i;

  FDataFile.SortRecords(F);
  CheckEquals(9, F1.AsInteger[0], 'SortRecords failed: F1[0]');
  CheckEquals(7, F1.AsInteger[2], 'SortRecords failed: F1[2]');
  CheckEquals(2, F1.AsInteger[7], 'SortRecords failed: F1[7]');
  CheckEquals(0.9, F2.AsFloat[0], Delta, 'SortRecords failed: F2[0]');
  CheckEquals(0.7, F2.AsFloat[2], Delta, 'SortRecords failed: F2[2]');
  CheckEquals(0.2, F2.AsFloat[7], Delta, 'SortRecords failed: F2[7]');
  CheckEquals('J', F3.AsString[0], 'SortRecords failed: F3[0]');
  CheckEquals('H', F3.AsString[2], 'SortRecords failed: F3[2]');
  CheckEquals('C', F3.AsString[7], 'SortRecords failed: F3[7]');

  F.Free;
  TearDown3Fields;
  FDataFile.Size := 0;
end;

procedure TUnitTest_EpiDataFiles.MainSection;
begin
  // TODO : Check MainSection
  Check(true);
end;

procedure TUnitTest_EpiDataFiles.Sections;
begin
  // TODO : Check Sections
  Check(true);
end;

procedure TUnitTest_EpiDataFiles.Section;
begin
  // TODO : Check Section
  Check(true);
end;

procedure TUnitTest_EpiDataFiles.Fields;
begin
  // TODO : Check Fields
  Check(true);
end;

procedure TUnitTest_EpiDataFiles.Field;
begin
  // TODO : Check Field
  Check(true);
end;

procedure TUnitTest_EpiDataFiles.ControlItems;
begin
  // TODO : Check ControlItems
  Check(true);
end;

procedure TUnitTest_EpiDataFiles.ControlItem;
begin
  // TODO : Check ControlItem
  Check(true);
end;

procedure TUnitTest_EpiDataFiles.Headings;
begin
  // TODO : Check Headings
  Check(true);
end;

procedure TUnitTest_EpiDataFiles.Heading;
begin
  // TODO : Check Heading
  Check(true);
end;

procedure TUnitTest_EpiDataFiles.ValueLabels;
begin
  // TODO : Check ValueLabels
  Check(true);
end;

procedure TUnitTest_EpiDataFiles.ValueLabelSet;
begin
  // TODO : Check ValueLabelSet
  Check(true);
end;

procedure TUnitTest_EpiDataFiles.Size;
var
  Sz: Integer;
begin
  Sz := FDataFile.Size;
  CheckEquals(0, Sz, 'FDataFile Size <> 0');

  // Check that fields increase in size WITH datafile increase.
  Setup3Fields;
  FDataFile.Size := 10;
  CheckEquals(10, FDataFile.Size, 'FDataFile Size <> 10');
  CheckEquals(10, F1.Size, 'F1.Size <> 10');
  CheckEquals(10, F2.Size, 'F2.Size <> 10');
  CheckEquals(10, F3.Size, 'F3.Size <> 10');
  TearDown3Fields;

  // Check that field get same SIZE as existing datafile.size
  Setup3Fields;
  CheckEquals(10, F1.Size, 'F1.Size <> 10');
  CheckEquals(10, F2.Size, 'F2.Size <> 10');
  CheckEquals(10, F3.Size, 'F3.Size <> 10');

  // Check that fields decrease in size WITH datafile increase.
  FDataFile.Size := 0;
  CheckEquals(0, Sz, 'FDataFile Size <> 0');
  CheckEquals(0, F1.Size, 'F1.Size <> 0');
  CheckEquals(0, F2.Size, 'F2.Size <> 0');
  CheckEquals(0, F3.Size, 'F3.Size <> 0');

  TearDown3Fields;
end;

procedure TUnitTest_EpiDataFiles.DeletedCount;
var
  i: Integer;
begin
  FDataFile.Size := 10;

  Setup3Fields;
  for i := 0 to FDataFile.Size - 1 do
    FDataFile.Deleted[i] := (i mod 3) = 0;

  CheckEquals(4, FDataFile.DeletedCount, 'DeletedCount: Before pack count <> 4');
  FDataFile.Pack;
  CheckEquals(0, FDataFile.DeletedCount, 'DeletedCount: After pack count <> 0');

  TearDown3Fields;
  FDataFile.Size := 0;
end;

procedure TUnitTest_EpiDataFiles.VerifiedCount;
var
  i: Integer;
begin
  FDataFile.Size := 10;

  Setup3Fields;
  for i := 0 to FDataFile.Size - 1 do
    FDataFile.Verified[i] := (i mod 3) = 0;

  CheckEquals(4, FDataFile.VerifiedCount, 'VerifiedCount: count <> 4');

  TearDown3Fields;
  FDataFile.Size := 0;
end;

procedure TUnitTest_EpiDataFiles.Caption;
var
  S: String;
begin
  S := FDataFile.Caption.Text;
  FDataFile.Caption.Text := DATAFILE_CAPTION_TEXT;
  CheckNotEquals(S, FDataFile.Caption.Text, 'DataFile Caption Text Equals!');
end;

procedure TUnitTest_EpiDataFiles.Version;
var
  S: String;
begin
  S := FDataFile.Version;
  FDataFile.Version := DATAFILE_VERSION_TEXT;
  CheckNotEquals(S, FDataFile.Version, 'DataFile Version Text Equals!');
end;

procedure TUnitTest_EpiDataFiles.Notes;
var
  S: String;
begin
  S := FDataFile.Notes.Text;
  FDataFile.Notes.Text := DATAFILE_NOTES_TEXT;
  CheckNotEquals(S, FDataFile.Notes.Text, 'DataFile Notes Text Equals!');
end;

procedure TUnitTest_EpiDataFiles.KeyFields;
var
  i: Integer;
  KF: TEpiFields;
  IT: TEpiIntegrityChecker;
  FailedRecs: TBoundArray;
  FailedVals: TBoundArray;
begin
  FDataFile.Size := 10;

  Setup3Fields;
  KF := FDataFile.KeyFields;
  KF.AddItem(F1);

  // F1 contains 1..10
  IT := TEpiIntegrityChecker.Create;
  CheckTrue(IT.IndexIntegrity(FDataFile, FailedRecs, FailedVals), 'IndexIntegrityCheck - 1');

  // F1 contains 1, 1, 2, 2 ... 5, 5 => should fail!
  for i := 0 to FDataFile.Size -1 do
    F1.AsInteger[i] := F1.AsInteger[i] div 2;
  CheckFalse(IT.IndexIntegrity(FDataFile, FailedRecs, FailedVals), 'IndexIntegrityCheck - 2');

  IT.Free;
  TearDown3Fields;
  FDataFile.Size := 0;
end;

initialization
  RegisterTest(TUnitTest_EpiDataFiles.Suite);

end.

