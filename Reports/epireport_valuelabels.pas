unit epireport_valuelabels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_generator_html,
  epidocument, epivaluelabels;

const
  SEpiReportValueLabelsNoValueLabelSets = 'No EpiValueLabels assigned';

type
  EEpiReportValueLabelsException = class(Exception);

  { TEpiReportValueLabels }

  TEpiReportValueLabels = class(TEpiReportBase)
  private
    FEpiValueLabels: TEpiValueLabelSets;
    procedure   PrintValueLabelSet(Const VLSet: TEpiValueLabelSet);
  protected
    procedure DoSanityCheck; override;
  public
    procedure   RunReport; override;
    property    EpiValueLabels: TEpiValueLabelSets read FEpiValueLabels write FEpiValueLabels;
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

procedure TEpiReportValueLabels.DoSanityCheck;
begin
  inherited DoSanityCheck;

  if not Assigned(FEpiValueLabels) then
    DoError(EEpiReportValueLabelsException, SEpiReportValueLabelsNoValueLabelSets);
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

end.

