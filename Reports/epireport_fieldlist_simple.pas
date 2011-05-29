unit epireport_fieldlist_simple;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_htmlgenerator,
  epidocument, epidatafiles;

type

  { TEpiReportSimpleFieldList }

  TEpiReportSimpleFieldList = class(TEpiReportBase)
  private
    FEpiDataFiles: TEpiDataFiles;
    procedure   PrintDatafile(Const DataFile: TEpiDataFile);
  public
    constructor Create(const AEpiDocument: TEpiDocument); override;
    procedure   RunReport; override;
    property    EpiDataFiles: TEpiDataFiles read FEpiDataFiles;
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
      Const CompleteHtml: boolean);
    destructor Destroy; override;
    procedure RunReport; override;
  end;


implementation

uses
  epimiscutils;

{ TEpiReportSimpleFieldList }

procedure TEpiReportSimpleFieldList.PrintDatafile(const DataFile: TEpiDataFile
  );
var
  i: Integer;
begin
  DoTableHeader(DataFile.Caption.Text, 4, DataFile.Fields.Count + 1);
  DoTableCell(0, 0, 'Name');
  DoTableCell(1, 0, 'Type');
  DoTableCell(2, 0, 'ValueLabel');
  DoTableCell(3, 0, 'Question');

  for i := 0 to DataFile.Fields.Count - 1 do
  with DataFile.Field[i] do
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

constructor TEpiReportSimpleFieldList.Create(const AEpiDocument: TEpiDocument);
begin
  inherited Create(AEpiDocument);
  FEpiDataFiles := AEpiDocument.DataFiles;
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
  const AEpiDocument: TEpiDocument; const CompleteHtml: boolean);
begin
  inherited Create(AEpiDocument);
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
    FHtmlGenerator.InitHtml;

  inherited RunReport;

  if FCompleteHtml then
    FHtmlGenerator.CloseHtml;
end;

end.

