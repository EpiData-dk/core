unit epireport_report_projectheading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base,
  epidocument, epirelations, epicustombase;

type

  EEpiReportProjectHeader = class(EEpiReportBaseException);

  { TEpiReportProjectHeader }

  TEpiReportProjectHeader = class(TEpiReportBase)
  private
    FDocument: TEpiDocument;
    FFilename: string;
    FFileNo: integer;
    FCurrentRowNo: Integer;
    procedure KeysTable(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
    procedure DataFormsTable(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
  protected
    procedure DoSanityCheck; override;
  public
    procedure RunReport; override;
    property Document: TEpiDocument read FDocument write FDocument;
    property Filename: string read FFilename write FFilename;
    property FileNo: integer read FFileNo write FFileNo;
  end;

implementation

uses
  epireport_types, math, epidatafiles, strutils, epimiscutils;

resourcestring
  SEpiReportProjectHeaderNoDocument = 'EpiReport: No document assigned to project header.';
  SEpiReportProjectHeaderNoFilename = 'EpiReport: No filename for project header.';

{ TEpiReportProjectHeader }

procedure TEpiReportProjectHeader.KeysTable(const Relation: TEpiMasterRelation;
  const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
  Data: Pointer);
var
  DF: TEpiDataFile;
  F: TEpiField;
  S: String;
  ColNo: Integer;
begin
  S := '';
  if Depth > 0 then
    begin
      S := DupeString('-', Depth) + ' ';
    end;

  with Relation.Datafile do
  begin
    ColNo := 0;

    // Caption:
    DoTableCell(PostInc(ColNo), FCurrentRowNo, S + Caption.Text);


    // Relation:
    if Document.Relations.IsMultiLeveled then
    begin
       if Relation.InheritsFrom(TEpiDetailRelation)
       then
         if (TEpiDetailRelation(Relation).MaxRecordCount > 0) then
           S := '1:' + IntToStr(TEpiDetailRelation(Relation).MaxRecordCount)
         else
           S := '1:' + char($E2) + char($88) + char($9E)             // unicode infinity symbol (UTF-8 encoded)
       else
         S := ' - ';
       DoTableCell(PostInc(ColNo), FCurrentRowNo, S);
    end;

    // Key:
    S := '';
    DF := Relation.Datafile;
    for F in DF.KeyFields do
      S := S + ' + (' + F.Name + ':' + F.Question.Text + ')';
    Delete(S, 1, 3);

    DoTableCell(PostInc(ColNo), FCurrentRowNo, S);
  end;
  Inc(FCurrentRowNo);
end;

procedure TEpiReportProjectHeader.DataFormsTable(
  const Relation: TEpiMasterRelation; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean; Data: Pointer);
var
  S: String;
begin
  S := '';
  if Depth > 0 then
    begin
      S := DupeString('-', Depth) + ' ';
    end;

  with Relation.Datafile do
  begin
    DoTableCell(0, FCurrentRowNo, S + Caption.Text);
    DoTableCell(1, FCurrentRowNo, DateTimeToStr(Created));
    DoTableCell(2, FCurrentRowNo, DateTimeToStr(StructureModifiedDate));
    DoTableCell(3, FCurrentRowNo, DateTimeToStr(RecModifiedDate));
    DoTableCell(4, FCurrentRowNo, IntToStr(Sections.Count));
    DoTableCell(5, FCurrentRowNo, IntToStr(Fields.Count));
    DoTableCell(6, FCurrentRowNo, IntToStr(Size));
    DoTableCell(7, FCurrentRowNo, IntToStr(DeletedCount));
  end;
  Inc(FCurrentRowNo);
end;

procedure TEpiReportProjectHeader.DoSanityCheck;
begin
  inherited DoSanityCheck;

  if not Assigned(FDocument) then
    DoError(EEpiReportProjectHeader, SEpiReportProjectHeaderNoDocument);
  if FFilename = '' then
    DoError(EEpiReportProjectHeader, SEpiReportProjectHeaderNoFilename);
end;

procedure TEpiReportProjectHeader.RunReport;
var
  i: Integer;
  S: String;
  j: Integer;
  LastEdit: TDateTime;
  OrderedDataFiles: TEpiDataFiles;
  List: TEpiDatafileRelationList;
  Relations: TEpiDatafileRelationList;
  ColCount: Integer;
  MultiLevel: Boolean;
  ColNo: Integer;

begin
  inherited RunReport;

  LastEdit := Document.Study.ModifiedDate;
  for i := 0 to Document.DataFiles.Count -1 do
  with Document.DataFiles[i] do
    LastEdit := Max(LastEdit, Max(RecModifiedDate, StructureModifiedDate));

  DoTableHeader('File ' + IntToStr(FileNo) + ': ' + {ExtractFileName(}Filename{)}, 2, 5, []);
  DoTableCell(0, 0, 'Title');
  DoTableCell(0, 1, 'Created');
  DoTableCell(0, 2, 'Last Edited');
  DoTableCell(0, 3, 'Version');
  DoTableCell(0, 4, 'Cycle');
  DoTableCell(1, 0, Document.Study.Title.Text);
  DoTableCell(1, 1, DateTimeToStr(Document.Study.Created));
  DoTableCell(1, 2, DateTimeToStr(LastEdit));
  DoTableCell(1, 3, Document.Study.Version, tcaLeftAdjust);
  DoTableCell(1, 4, IntToStr(Document.CycleNo), tcaLeftAdjust);
  S :=
    'Backup on shutdown: ' + BoolToStr(Document.ProjectSettings.BackupOnShutdown, 'yes', 'no') + LineEnding +
    'Encrypted data: '     + BoolToStr(Document.PassWord <> '',                   'yes', 'no');
  DoTableFooter(Trim(S));

  DoLineText('');


  DoTableHeader('Dataforms:', 9, Document.DataFiles.Count +1 { OrderedDataFiles.Count + 1});
  // Header row:
  DoTableCell(0, 0, 'Caption');
  DoTableCell(1, 0, 'Created');
  DoTableCell(2, 0, 'Structure Edited');
  DoTableCell(3, 0, 'Data Edited');
  DoTableCell(4, 0, 'Sections');
  DoTableCell(5, 0, 'Fields');
  DoTableCell(6, 0, 'Records');
  DoTableCell(7, 0, 'Deleted');

  FCurrentRowNo := 1;
  Document.Relations.OrderedWalk(@DataFormsTable);
  DoTableFooter('');

  DoLineText('');


  MultiLevel := Document.Relations.IsMultiLeveled;
  ColCount := 2;
  if MultiLevel then
    Inc(ColCount);

  DoTableHeader('', ColCount, Document.DataFiles.Count + 1);

  ColNo := 0;
  DoTableCell(PostInc(ColNo), 0, 'Caption');
  if MultiLevel then
    DoTableCell(PostInc(ColNo), 0, 'Relation');
  DoTableCell(PostInc(ColNo), 0, 'Fields in key');

  FCurrentRowNo := 1;
  Document.Relations.OrderedWalk(@KeysTable);
  DoTableFooter('');

{  for i := 0 to OrderedDataFiles.Count -1 do
  with OrderedDataFiles[i] do
  begin
    if KeyFields.Count = 0 then continue;

    DoHeading('Key for "' + Caption.Text + '"');
    for j := 0 to KeyFields.Count -1 do
      DoLineText(KeyFields[j].Name + ' - ' + KeyFields[j].Question.Text);
    DoLineText('');
  end;

  OrderedDataFiles.Free;  }
end;

end.

