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
    procedure SanityCheck;
  protected
    procedure DoError(EC: ExceptClass; Const Msg: string);
    procedure DoSanityCheck; virtual;
    function GetReportText: string; virtual;
    procedure DoTableHeader(Const Text: string;
      Const ColCount, RowCount: integer);
    procedure DoTableFooter(Const Text: string);
    procedure DoTableCell(Const Col, Row: Integer; Const Text: string);
    procedure DoSection(Const Text: string);
    procedure DoHeading(Const Text: string);
    procedure DoLineText(Const Text: string);
  public
    // This constructor automaticall call the constructor with an instance of the
    // assigned ReportGeneratorClass.
    constructor Create(ReportGeneratorClass: TEpiReportGeneratorBaseClass); virtual; overload;
    // This constructor may/should be used if the same generator is used across
    // multiple reports. In addition this is the contructor sub-classes should override
    // since it is called from the other constructor.
    constructor Create(ReportGenerator: TEpiReportGeneratorBase); virtual; overload;
    procedure RunReport; virtual;
    property ReportText: string read GetReportText;
  end;

implementation

procedure TEpiReportBase.DoError(EC: ExceptClass; const Msg: string);
begin
  raise EC.Create(Msg);
end;

procedure TEpiReportBase.SanityCheck;
begin
  DoSanityCheck;
end;

procedure TEpiReportBase.DoSanityCheck;
begin
  if not Assigned(FReportGenerator) then
    DoError(EEpiReportBaseException, SEpiReportBaseNoGenerator);
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
  Create(ReportGeneratorClass.Create);
end;

constructor TEpiReportBase.Create(ReportGenerator: TEpiReportGeneratorBase);
begin
  FReportGenerator := ReportGenerator;
end;

procedure TEpiReportBase.RunReport;
begin
  SanityCheck;
end;

end.

