unit epireport_valuelabels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_htmlgenerator,
  epidocument, epivaluelabels;

type

  { TEpiReportValueLabels }

  TEpiReportValueLabels = class(TEpiReportBase)
  private
    FEpiValueLabels: TEpiValueLabelSets;
    procedure   PrintValueLabelSet(Const VLSet: TEpiValueLabelSet);
  public
    constructor Create(const AEpiDocument: TEpiDocument); override;
    procedure   RunReport; override;
    property    EpiValueLabels: TEpiValueLabelSets read FEpiValueLabels;
  end;

  { TEpiReportValueLabelsHtml }

  TEpiReportValueLabelsHtml = class(TEpiReportValueLabels)
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

{ TEpiReportValueLabels }

procedure TEpiReportValueLabels.PrintValueLabelSet(
  const VLSet: TEpiValueLabelSet);
var
  i: Integer;
begin
  DoTableHeader(VLSet.Name + ': (' + EpiTypeNames[VLSet.LabelType] + ')', 3, VLSet.Count + 1);

  DoTableCell(0, 0, 'Category');
  DoTableCell(1, 0, 'Label');
  DoTableCell(2, 0, 'Missing');

  for i := 0 to VLSet.Count - 1 do
  with VLSet[i] do
  begin
    DoTableCell(0, i+1, ValueAsString);
    DoTableCell(1, i+1, TheLabel.Text);
    DoTableCell(2, i+1, BoolToStr(IsMissingValue, 'yes', ''));
  end;
  DoTableFooter('');
end;

constructor TEpiReportValueLabels.Create(const AEpiDocument: TEpiDocument);
begin
  inherited Create(AEpiDocument);
  FEpiValueLabels := AEpiDocument.ValueLabelSets;
end;

procedure TEpiReportValueLabels.RunReport;
var
  LabelList: TStringList;
  i: Integer;
begin
  inherited RunReport;

  LabelList := TStringList.Create;
  LabelList.Sorted := true;
  for i := 0 to EpiValueLabels.Count - 1 do
    LabelList.AddObject(EpiValueLabels[i].Name, EpiValueLabels[i]);

  DoSection('ValueLabels');

  for i := 0 to LabelList.Count - 1 do
    PrintValueLabelSet(TEpiValueLabelSet(LabelList.Objects[i]));
end;

{ TEpiReportValueLabelsHtml }

function TEpiReportValueLabelsHtml.GetReportText: string;
begin
  Result := FHtmlGenerator.GetReportText;
end;

constructor TEpiReportValueLabelsHtml.Create(const AEpiDocument: TEpiDocument;
  const CompleteHtml: boolean);
begin
  inherited Create(AEpiDocument);
  FHtmlGenerator := TEpiReportHTMLGenerator.Create(Self);
  FCompleteHtml := CompleteHtml;
end;

destructor TEpiReportValueLabelsHtml.Destroy;
begin
  FHtmlGenerator.Free;
  inherited Destroy;
end;

procedure TEpiReportValueLabelsHtml.RunReport;
begin
  if FCompleteHtml then
    FHtmlGenerator.InitHtml;

  inherited RunReport;

  if FCompleteHtml then
    FHtmlGenerator.CloseHtml;
end;

end.

