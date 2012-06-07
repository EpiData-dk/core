unit epireport_base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_generator_base;

const
  SEpiReportBaseNoGenerator = 'No ReportGenerator Assigned To Report';

type
  EEpiReportBaseException = class(Exception);

  { TEpiReportBase }

  TEpiReportBase = class
  private
    FReportGenerator: TEpiReportGeneratorBase;
    procedure DoError(Const Msg: string);
  protected
    function GetReportText: string; virtual;
    procedure DoTableHeader(Const Text: string;
      Const ColCount, RowCount: integer);
    procedure DoTableFooter(Const Text: string);
    procedure DoTableCell(Const Col, Row: Integer; Const Text: string);
    procedure DoSection(Const Text: string);
    procedure DoHeading(Const Text: string);
    procedure DoLineText(Const Text: string);
  public
    constructor Create(ReportGeneratorClass: TEpiReportGeneratorBaseClass); virtual;
    procedure RunReport; virtual;
    property ReportText: string read GetReportText;
  end;

implementation

procedure TEpiReportBase.DoError(const Msg: string);
begin
  raise EEpiReportBaseException.Create(Msg);
end;

function TEpiReportBase.GetReportText: string;
begin
  result := FReportGenerator.GetReportText;
end;

procedure TEpiReportBase.DoTableHeader(const Text: string; const ColCount,
  RowCount: integer);
begin
  FReportGenerator.TableHeader(Text, ColCount, RowCount);
end;

procedure TEpiReportBase.DoTableFooter(Const Text: string);
begin
  FReportGenerator.TableFooter(Text);
end;

procedure TEpiReportBase.DoTableCell(Const Col, Row: Integer; Const Text: string);
begin
  FReportGenerator.TableCell(Text, Col, Row);
end;

procedure TEpiReportBase.DoSection(Const Text: string);
begin
  FReportGenerator.Section(Text);
end;

procedure TEpiReportBase.DoHeading(const Text: string);
begin
  FReportGenerator.Heading(Text);
end;

procedure TEpiReportBase.DoLineText(Const Text: string);
begin
  FReportGenerator.Line(Text);
end;

constructor TEpiReportBase.Create(
  ReportGeneratorClass: TEpiReportGeneratorBaseClass);
begin
  FReportGenerator := ReportGeneratorClass.Create;
end;

procedure TEpiReportBase.RunReport;
begin
  if not Assigned(FReportGenerator) then
    DoError(SEpiReportBaseNoGenerator);
end;

end.

