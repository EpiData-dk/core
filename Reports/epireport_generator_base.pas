unit epireport_generator_base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_types;

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
    FTableCellOptions: Array of TEpiReportGeneratorTableCellOptionSet;
    function GetTableCellOptions(const Col, Row: Integer
      ): TEpiReportGeneratorTableCellOptionSet;
    procedure CheckInTableCell(Const Col, Row: Integer);
    procedure SetTableCellOptions(const Col, Row: Integer;
      AValue: TEpiReportGeneratorTableCellOptionSet);

  { MISC }
  protected
    FHeaderOptions: TEpiReportGeneratorTableHeaderOptionSet;
    procedure AddLine(Const Txt: string);
    procedure DoError(Const Msg: string);
    property  RowCount: Integer read FRowCount;
    property  ColCount: Integer read FColCount;
    property  TableCellOptions[Const Col, Row: Integer]: TEpiReportGeneratorTableCellOptionSet read GetTableCellOptions write SetTableCellOptions;

  { Inherited methods }
  public
    // Lines
    procedure Section(Const Text: string); virtual;
    procedure Heading(Const Text: string); virtual;
    procedure Line(Const Text: string); virtual;

    // Table
    procedure TableHeader(Const Text: string;
      Const AColCount, ARowCount: Integer;
      Const HeaderOptions: TEpiReportGeneratorTableHeaderOptionSet = [thoRowHeader]); virtual;
    procedure TableFooter(Const Text: string); virtual;
    procedure TableCell(Const Text: string;
      Const Col, Row: Integer;
      Const CellAdjust: TEpiReportGeneratorTableCellAdjustment = tcaAutoAdjust;
      Const CellOptions: TEpiReportGeneratorTableCellOptionSet = []
      ); virtual;

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

function TEpiReportGeneratorBase.GetTableCellOptions(const Col, Row: Integer
  ): TEpiReportGeneratorTableCellOptionSet;
begin
  CheckInTableCell(Col, Row);
  Result := FTableCellOptions[ColCount * Row + Col];
end;

procedure TEpiReportGeneratorBase.CheckInTableCell(const Col, Row: Integer);
begin
  if not InTable then
    DoError(Format(SEpiReportGeneratorBaseNotInTableError, ['TableCell']));

  if (Col < 0) or (Col > (ColCount-1)) or
     (Row < 0) or (Row > (RowCount-1)) then
    DoError(Format(SEpiReportGeneratorBaseIndexError, [Col,Row]));
end;

procedure TEpiReportGeneratorBase.SetTableCellOptions(const Col, Row: Integer;
  AValue: TEpiReportGeneratorTableCellOptionSet);
begin
  CheckInTableCell(Col, Row);
  FTableCellOptions[ColCount * Row + Col] := AValue;
end;

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
  const AColCount, ARowCount: Integer;
  const HeaderOptions: TEpiReportGeneratorTableHeaderOptionSet);
var
  i: Integer;
begin
  if InTable then
    DoError(Format(SEpiReportGeneratorBaseInTableError, ['TableHeader']));
  InTable := true;
  FRowCount := ARowCount;
  FColCount := AColCount;
  SetLength(FTableCellOptions, FRowCount * FColCount);

  FHeaderOptions := HeaderOptions;

  // Automatically insert cell options if first row is header
  if thoRowHeader in FHeaderOptions then
    for i := 0 to FColCount - 1 do
      TableCellOptions[i, 0] := TableCellOptions[i, 0] + [tcoTopBorder, tcoBottomBorder];

  // Automatically insert cell options if first col is header
  if thoColHeader in FHeaderOptions then
    for i := 0 to FRowCount - 1 do
      TableCellOptions[0, i] := TableCellOptions[0, i] + [tcoLeftBorder, tcoRightBorder];
end;

procedure TEpiReportGeneratorBase.TableFooter(const Text: string);
begin
  if not InTable then
    DoError(Format(SEpiReportGeneratorBaseNotInTableError, ['TableFooter']));
  InTable := false;
  SetLength(FTableCellOptions, 0);
end;

procedure TEpiReportGeneratorBase.TableCell(const Text: string; const Col,
  Row: Integer; const CellAdjust: TEpiReportGeneratorTableCellAdjustment;
  const CellOptions: TEpiReportGeneratorTableCellOptionSet);
begin
  CheckInTableCell(Col, Row);

  TableCellOptions[Col, Row] := TableCellOptions[Col, Row] + CellOptions;
end;

constructor TEpiReportGeneratorBase.Create;
begin
  FReportText := TStringList.Create;
  FHeaderOptions := [];
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

