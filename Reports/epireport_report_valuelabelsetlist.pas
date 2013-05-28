unit epireport_report_valuelabelsetlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base,
  epivaluelabels;

type

  EEpiReportValueLabelSetList = class(EEpiReportBaseException);

  { TEpiReportValueLabelSetList }

  TEpiReportValueLabelSetList = class(TEpiReportBase)
  private
    FValueLabelSet: TEpiValueLabelSet;
  protected
    procedure DoSanityCheck; override;
  public
    procedure RunReport; override;
    property ValueLabelSet: TEpiValueLabelSet read FValueLabelSet write FValueLabelSet;
  end;

implementation

uses
  epimiscutils, epireport_types;

resourcestring
  SEpiReportValueLabelSetListNoValueLabelSet = 'EpiReport: No valuelabelset assigned for valuelabelset list.';

{ TEpiReportValueLabelSetList }

procedure TEpiReportValueLabelSetList.DoSanityCheck;
begin
  inherited DoSanityCheck;

  if not Assigned(FValueLabelSet) then
    DoError(EEpiReportValueLabelSetList, SEpiReportValueLabelSetListNoValueLabelSet);
end;

procedure TEpiReportValueLabelSetList.RunReport;
var
  i: Integer;
begin
  inherited RunReport;

  DoTableHeader(
//    'Value label: ' + ValueLabelSet.Name + ': (' + EpiTypeNames[ValueLabelSet.LabelType] + ')',
    '',
    3,
    ValueLabelSet.Count + 1
  );

  DoTableCell(0, 0, 'Value');
  DoTableCell(1, 0, 'Label');
  DoTableCell(2, 0, 'Missing (M), set: ' + ValueLabelSet.Name, tcaLeftAdjust);

  for i := 0 to ValueLabelSet.Count - 1 do
  with ValueLabelSet[i] do
  begin
    DoTableCell(0, i+1, ValueAsString, tcaLeftAdjust);
    DoTableCell(1, i+1, TheLabel.Text);
    DoTableCell(2, i+1, BoolToStr(IsMissingValue, 'M', ''), tcaCenter);
  end;
  DoTableFooter('');
end;

end.

