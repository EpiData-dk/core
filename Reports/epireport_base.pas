unit epireport_base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_generator_base, epireport_types;

resourcestring
  SEpiReportBaseNoGenerator = 'No ReportGenerator Assigned To Report';

type
  EEpiReportBaseException = class(Exception);

  { TEpiReportBase }

  TEpiReportBase = class
  private
    procedure SanityCheck;
  protected
    FReportGenerator: TEpiReportGeneratorBase;
    procedure DoError(EC: ExceptClass; Const Msg: string);
    procedure DoSanityCheck; virtual;
    function GetReportText: string; virtual;
    procedure DoTableHeader(Const Text: string;
      Const ColCount, RowCount: integer;
      Const TableOptions: TEpiReportGeneratorTableHeaderOptionSet  = [thoRowHeader]
      );
    procedure DoTableFooter(Const Text: string);
    procedure DoTableCell(Const Col, Row: Integer; Const Text: string;
      CellAdjust: TEpiReportGeneratorTableCellAdjustment = tcaAutoAdjust);
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
  RowCount: integer; const TableOptions: TEpiReportGeneratorTableHeaderOptionSet
  );
begin
  FReportGenerator.TableHeader(Text, ColCount, RowCount, TableOptions);
end;

procedure TEpiReportBase.DoTableFooter(const Text: string);
begin
  FReportGenerator.TableFooter(Text);
end;

procedure TEpiReportBase.DoTableCell(const Col, Row: Integer;
  const Text: string; CellAdjust: TEpiReportGeneratorTableCellAdjustment);
begin
  FReportGenerator.TableCell(Text, Col, Row, CellAdjust);
end;

procedure TEpiReportBase.DoSection(const Text: string);
begin
  FReportGenerator.Section(Text);
end;

procedure TEpiReportBase.DoHeading(const Text: string);
begin
  FReportGenerator.Heading(Text);
end;

procedure TEpiReportBase.DoLineText(const Text: string);
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

