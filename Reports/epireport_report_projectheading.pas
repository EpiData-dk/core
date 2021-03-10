unit epireport_report_projectheading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_generator_base,
  epidocument, epidatafilerelations, epicustombase;

type

  EEpiReportProjectHeader = class(EEpiReportBaseException);

  TEpiReportProjectHeaderOption = (
    erpoShowRelationsTable,
    erpoShowKeyTables
  );
  TEpiReportProjectHeaderOptions = set of TEpiReportProjectHeaderOption;

  { TEpiReportProjectHeader }

  TEpiReportProjectHeader = class(TEpiReportBase)
  private
    FDocument: TEpiDocument;
    FFilename: string;
    FFileNo: integer;
    FCurrentRowNo: Integer;
    FOptions: TEpiReportProjectHeaderOptions;
    procedure KeysTable(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
    procedure DataFormsTable(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
  protected
    procedure DoSanityCheck; override;
  public
    constructor Create(ReportGenerator: TEpiReportGeneratorBase); override; overload;
    procedure RunReport; override;
    property Document: TEpiDocument read FDocument write FDocument;
    property Filename: string read FFilename write FFilename;
    property FileNo: integer read FFileNo write FFileNo;
    property Options: TEpiReportProjectHeaderOptions read FOptions write FOptions;
  end;

implementation

uses
  epireport_types, math, epidatafiles, strutils, epimiscutils, epidatafilerelations_helper,
  epicustomlist_helper;

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
  if Relation.ProtectedItem then exit;

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
  if Relation.ProtectedItem then exit;

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

constructor TEpiReportProjectHeader.Create(
  ReportGenerator: TEpiReportGeneratorBase);
begin
  inherited Create(ReportGenerator);
  FOptions := [erpoShowKeyTables, erpoShowRelationsTable];
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

  DoTableHeader('File ' + IntToStr(FileNo) + ': ' + {ExtractFileName(}Filename{)}, 2, 4, []);
  DoTableCell(0, 0, 'Title');
  DoTableCell(0, 1, 'Created');
  DoTableCell(0, 2, 'Last Edited');
  DoTableCell(0, 3, 'Cycle');
  DoTableCell(1, 0, Document.Study.Title.Text);
  DoTableCell(1, 1, DateTimeToStr(Document.Study.Created));
  DoTableCell(1, 2, DateTimeToStr(LastEdit));
  DoTableCell(1, 3, IntToStr(Document.CycleNo), tcaLeftAdjust);

  S := 'Backup on shutdown: ' + BoolToStr(Document.ProjectSettings.BackupOnShutdown, 'yes', 'no') + LineEnding +
       'Email on shutdown: ' + BoolToStr(Document.ProjectSettings.EmailOnShutdown, 'yes', 'no') + LineEnding +
       'Project Encryption: ';

  if (Document.Admin.Initialized) then
      S += 'extended access' + LineEnding +
           'Length of Password period: ' + IntToStr(Document.Admin.DaysBetweenPasswordChange)
  else if (Document.PassWord <> '') then
    S += 'simple password (data only)'
  else
    S += 'none';

  DoTableFooter(Trim(S));

  DoLineText('');


  if (erpoShowRelationsTable in Options) then
    begin
      DoTableHeader('Dataforms:', 9, Document.DataFiles.UnprotectedCount + 1 { OrderedDataFiles.Count + 1});
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
    end;


  if (erpoShowKeyTables in Options) then
    begin
      MultiLevel := Document.Relations.IsMultiLeveled;
      ColCount := 2;
      if MultiLevel then
        Inc(ColCount);

      DoTableHeader('', ColCount, Document.DataFiles.UnprotectedCount + 1);

      ColNo := 0;
      DoTableCell(PostInc(ColNo), 0, 'Caption');
      if MultiLevel then
        DoTableCell(PostInc(ColNo), 0, 'Relation');
      DoTableCell(PostInc(ColNo), 0, 'Fields in key');

      FCurrentRowNo := 1;
      Document.Relations.OrderedWalk(@KeysTable);
      DoTableFooter('');
    end;
end;

end.

