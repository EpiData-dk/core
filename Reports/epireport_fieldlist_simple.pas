unit epireport_fieldlist_simple;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_types, epireport_htmlgenerator,
  epidocument, epidatafiles;
       {
type

  { TEpiReportSimpleFieldList }
  TEpiReportSimpleFieldList = class(TEpiReportBase)
  private
    FEpiDataFiles: TEpiDataFiles;
    FSortType:  TEpiReportFieldListSortType;
    procedure   PrintDatafile(Const DataFile: TEpiDataFile);
  public
    constructor Create(const AEpiDocument: TEpiDocument;
      Const ASortType: TEpiReportFieldListSortType = stCreation);
    procedure   RunReport; override;
    property    EpiDataFiles: TEpiDataFiles read FEpiDataFiles;
    property    SortType: TEpiReportFieldListSortType read FSortType write FSortType;
  end;

  { TEpiReportSimpleFieldListHtml }

  TEpiReportSimpleFieldListHtml = class(TEpiReportSimpleFieldList)
  private
    FHtmlGenerator: TEpiReportHTMLGenerator;
    FCompleteHtml: Boolean;
  protected
    function GetReportText: string; override;
  public
    constructor Create(const AEpiDocument: TEpiDocument;
      Const ASortType: TEpiReportFieldListSortType = stCreation;
      Const CompleteHtml: boolean = false);
    destructor Destroy; override;
    procedure RunReport; override;
    property HtmlGenerator: TEpiReportHTMLGenerator read FHtmlGenerator;
  end;
         }

implementation
       {
uses
  epimiscutils;

{ TEpiReportSimpleFieldList }

function FieldNameSort(Item1, Item2: Pointer): integer;
var
  Field1: TEpiField absolute Item1;
  Field2: TEpiField absolute Item2;
begin
  result:= AnsiCompareStr(Field1.Name, Field2.Name);
end;

function EntryFlowSort(Item1, Item2: Pointer): integer;
var
  Field1: TEpiField absolute Item1;
  Field2: TEpiField absolute Item2;
  MainSection: TEpiSection;

  function SectionTop(Const F: TEpiField): integer;
  begin
    result := F.Section.Top{ + F.Top};
  end;
  function SectionLeft(Const F: TEpiField): integer;
  begin
    result := F.Section.Left{ + F.Left};
  end;

begin
  if (Field1.Section = Field2.Section) then
  begin
    Result := (Field1.Top - Field2.Top);
    if Result = 0 then
      Result := (Field1.Left - Field2.Left);
    Exit;
  end;

  MainSection := Field1.DataFile.MainSection;

  // Cross section comparison.
  if (Field1.Section <> MainSection) and (Field2.Section <> MainSection) then
  begin
    result := SectionTop(Field1) - SectionTop(Field2);
    if result = 0 then
      Result := SectionLeft(Field1) - SectionLeft(Field2);
    Exit;
  end;

  // Main <-> Section comparison
  if (Field1.Section = MainSection) then
  begin
    result := Field1.Top - SectionTop(Field2);
    if Result = 0 then
      result := Field1.Left - SectionLeft(Field2);
    exit;
  end;

  // Section <-> Main comparison
  if (Field2.Section = MainSection) then
  begin
    result := SectionTop(Field1) - Field2.Top;
    if Result = 0 then
      result := SectionLeft(Field1) - Field2.Left;
  end;
end;

procedure TEpiReportSimpleFieldList.PrintDatafile(const DataFile: TEpiDataFile);
var
  i: Integer;
  FieldList: TList;
begin
  DoTableHeader(DataFile.Caption.Text, 4, DataFile.Fields.Count + 1);
  DoTableCell(0, 0, 'Name');
  DoTableCell(1, 0, 'Type');
  DoTableCell(2, 0, 'ValueLabel');
  DoTableCell(3, 0, 'Question');


  FieldList := TList.Create;
  for i := 0 to DataFile.Fields.Count - 1 do FieldList.Add(DataFile.Field[i]);
  case SortType of
    stCreation:  ; // Do nothing...
    stFieldName: FieldList.Sort(@FieldNameSort);
    stEntryFlow: FieldList.Sort(@EntryFlowSort);
  end;

  for i := 0 to FieldList.Count - 1 do
  with TEpiField(FieldList[i]) do
  begin
    DoTableCell(0, i+1, Name);
    DoTableCell(1, i+1, EpiTypeNames[FieldType]);
    if Assigned(ValueLabelSet) then
      DoTableCell(2, i+1, ValueLabelSet.Name)
    else
      DoTableCell(2, i+1, '');
    DoTableCell(3, i+1, Question.Text);
  end;
  DoTableFooter('');
end;

constructor TEpiReportSimpleFieldList.Create(const AEpiDocument: TEpiDocument;
  const ASortType: TEpiReportFieldListSortType);
begin
  inherited Create(AEpiDocument);
  FEpiDataFiles := AEpiDocument.DataFiles;
  FSortType := ASortType;
end;

procedure TEpiReportSimpleFieldList.RunReport;
var
  i: Integer;
begin
  inherited RunReport;

  for i := 0 to EpiDataFiles.Count - 1 do
    PrintDatafile(EpiDataFiles[i]);
end;

{ TEpiReportSimpleFieldListHtml }

function TEpiReportSimpleFieldListHtml.GetReportText: string;
begin
  Result := FHtmlGenerator.GetReportText;
end;

constructor TEpiReportSimpleFieldListHtml.Create(
  const AEpiDocument: TEpiDocument;
  const ASortType: TEpiReportFieldListSortType; const CompleteHtml: boolean);
begin
  inherited Create(AEpiDocument, ASortType);
  FHtmlGenerator := TEpiReportHTMLGenerator.Create(Self);
  FCompleteHtml := CompleteHtml;
end;

destructor TEpiReportSimpleFieldListHtml.Destroy;
begin
  FHtmlGenerator.Free;
  inherited Destroy;
end;

procedure TEpiReportSimpleFieldListHtml.RunReport;
begin
  if FCompleteHtml then
    FHtmlGenerator.InitHtml('List of fields');

  inherited RunReport;

  if FCompleteHtml then
    FHtmlGenerator.CloseHtml;
end;
                              }
end.

