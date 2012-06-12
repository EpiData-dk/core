unit epireport_generator_base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  SEpiReportGeneratorBaseInTableError = 'Cannot write %s while in table!';
  SEpiReportGeneratorBaseNotInTableError = 'Cannot write %s while not in table!';
  SEpiReportGeneratorBaseIndexError = 'Index out of bound for table! Col: %d; Row: %d';

type
  EEpiReportGeneratorBaseException = class(Exception);

  { TEpiReportGeneratorBase }

  TEpiReportGeneratorBase = class
  private
    FReportText: TStringList;
    InTable: Boolean;
    FRowCount: Integer;
    FColCount: Integer;

  { MISC }
  protected
    procedure AddLine(Const Txt: string);
    procedure DoError(Const Msg: string);
    property  RowCount: Integer read FRowCount;
    property  ColCount: Integer read FColCount;

  { Inherited methods }
  public
    // Lines
    procedure Section(Const Text: string); virtual;
    procedure Heading(Const Text: string); virtual;
    procedure Line(Const Text: string); virtual;

    // Table
    procedure TableHeader(Const Text: string;
      Const AColCount, ARowCount: Integer); virtual;
    procedure TableFooter(Const Text: string); virtual;
    procedure TableCell(Const Text: string;
      Const Col, Row: Integer); virtual;

    // Generator static output
    procedure   StartReport(Const Title: string); virtual; abstract;
    procedure   EndReport; virtual; abstract;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    function    GetReportText: string;
  end;
  TEpiReportGeneratorBaseClass = class of TEpiReportGeneratorBase;

implementation

{ TEpiReportGeneratorBase }

procedure TEpiReportGeneratorBase.AddLine(const Txt: string);
begin
  FReportText.Add(Txt);
end;

procedure TEpiReportGeneratorBase.DoError(const Msg: string);
begin
  Raise EEpiReportGeneratorBaseException.Create(Msg);
end;

procedure TEpiReportGeneratorBase.Section(const Text: string);
begin
  if InTable then
    DoError(Format(SEpiReportGeneratorBaseInTableError, ['Section']));
end;

procedure TEpiReportGeneratorBase.Heading(const Text: string);
begin
  if InTable then
    DoError(Format(SEpiReportGeneratorBaseInTableError, ['Heading']));
end;

procedure TEpiReportGeneratorBase.Line(const Text: string);
begin
  if InTable then
    DoError(Format(SEpiReportGeneratorBaseInTableError, ['Line']));
end;

procedure TEpiReportGeneratorBase.TableHeader(const Text: string;
  const AColCount, ARowCount: Integer);
begin
  if InTable then
    DoError(Format(SEpiReportGeneratorBaseInTableError, ['TableHeader']));
  InTable := true;
  FRowCount := ARowCount;
  FColCount := AColCount;
end;

procedure TEpiReportGeneratorBase.TableFooter(const Text: string);
begin
  if not InTable then
    DoError(Format(SEpiReportGeneratorBaseNotInTableError, ['TableFooter']));
  InTable := false;
end;

procedure TEpiReportGeneratorBase.TableCell(const Text: string; const Col,
  Row: Integer);
begin
  if not InTable then
    DoError(Format(SEpiReportGeneratorBaseNotInTableError, ['TableCell']));

  if (Col < 0) or (Col > (ColCount-1)) or
     (Row < 0) or (Row > (RowCount-1)) then
    DoError(Format(SEpiReportGeneratorBaseIndexError, [Col,Row]));
end;

constructor TEpiReportGeneratorBase.Create;
begin
  FReportText := TStringList.Create;
end;

destructor TEpiReportGeneratorBase.Destroy;
begin
  FReportText.Free;
  inherited Destroy;
end;

function TEpiReportGeneratorBase.GetReportText: string;
begin
  result := FReportText.Text;
end;

end.

