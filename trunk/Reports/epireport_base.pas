unit epireport_base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument;

type
  TEpiReportBase = class;

  // Lines
  TEpiReportSection     = procedure(Sender: TEpiReportBase; Const Text: string) of object;
  TEpiReportHeading     = procedure(Sender: TEpiReportBase; Const Text: string) of object;
  TEpiReportLineText    = procedure(Sender: TEpiReportBase; Const Text: string) of object;

  // Table
  TEpiReportTableHeader = procedure(Sender: TEpiReportBase; Const Text: string;
    Const ColCount, RowCount: Integer) of object;
  TEpiReportTableFooter = procedure(Sender: TEpiReportBase; Const Text: string) of object;
  TEpiReportTableCell   = procedure(Sender: TEpiReportBase; Const Text: string;
    Const Col, Row: Integer) of object;


  { TEpiReportBase }

  TEpiReportBase = class
  private
    FEpiDocument: TEpiDocument;
    FOnHeading: TEpiReportHeading;
    FOnLineText: TEpiReportLineText;
    FOnSection: TEpiReportSection;
    FOnTableCell: TEpiReportTableCell;
    FOnTableFooter: TEpiReportTableFooter;
    FOnTableHeader: TEpiReportTableHeader;
    procedure SetOnHeading(const AValue: TEpiReportHeading);
    procedure SetOnLineText(const AValue: TEpiReportLineText);
    procedure SetOnSection(const AValue: TEpiReportSection);
    procedure SetOnTableCell(const AValue: TEpiReportTableCell);
    procedure SetOnTableFooter(const AValue: TEpiReportTableFooter);
    procedure SetOnTableHeader(const AValue: TEpiReportTableHeader);
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
    constructor Create(Const AEpiDocument: TEpiDocument); virtual;
    procedure RunReport; virtual;
    property ReportText: string read GetReportText;
    property OnTableHeader: TEpiReportTableHeader read FOnTableHeader write SetOnTableHeader;
    property OnTableFooter: TEpiReportTableFooter read FOnTableFooter write SetOnTableFooter;
    property OnTableCell:   TEpiReportTableCell read FOnTableCell write SetOnTableCell;
    property OnSection:     TEpiReportSection read FOnSection write SetOnSection;
    property OnHeading:     TEpiReportHeading read FOnHeading write SetOnHeading;
    property OnLineText:    TEpiReportLineText read FOnLineText write SetOnLineText;
    property EpiDocument:   TEpiDocument read FEpiDocument;
  end;

implementation

{ TEpiReportBase }

procedure TEpiReportBase.SetOnHeading(const AValue: TEpiReportHeading);
begin
  if FOnHeading = AValue then exit;
  FOnHeading := AValue;
end;

procedure TEpiReportBase.SetOnLineText(const AValue: TEpiReportLineText);
begin
  if FOnLineText = AValue then exit;
  FOnLineText := AValue;
end;

procedure TEpiReportBase.SetOnSection(const AValue: TEpiReportSection);
begin
  if FOnSection = AValue then exit;
  FOnSection := AValue;
end;

procedure TEpiReportBase.SetOnTableCell(const AValue: TEpiReportTableCell);
begin
  if FOnTableCell = AValue then exit;
  FOnTableCell := AValue;
end;

procedure TEpiReportBase.SetOnTableFooter(const AValue: TEpiReportTableFooter);
begin
  if FOnTableFooter = AValue then exit;
  FOnTableFooter := AValue;
end;

procedure TEpiReportBase.SetOnTableHeader(const AValue: TEpiReportTableHeader);
begin
  if FOnTableHeader = AValue then exit;
  FOnTableHeader := AValue;
end;

function TEpiReportBase.GetReportText: string;
begin
  // Empty result - must be implemented in sub-classes.
  result := '';
end;

procedure TEpiReportBase.DoTableHeader(const Text: string; const ColCount,
  RowCount: integer);
begin
  if Assigned(FOnTableHeader) then
    FOnTableHeader(Self, Text, ColCount, RowCount);
end;

procedure TEpiReportBase.DoTableFooter(Const Text: string);
begin
  if Assigned(OnTableFooter) then
    OnTableFooter(Self, Text);
end;

procedure TEpiReportBase.DoTableCell(Const Col, Row: Integer; Const Text: string);
begin
  if Assigned(FOnTableCell) then
    FOnTableCell(Self, Text, Col, Row);
end;

procedure TEpiReportBase.DoSection(Const Text: string);
begin
  if Assigned(OnSection) then
    OnSection(Self, Text);
end;

procedure TEpiReportBase.DoHeading(const Text: string);
begin
  if Assigned(OnHeading) then
    OnHeading(Self, Text);
end;

procedure TEpiReportBase.DoLineText(Const Text: string);
begin
  if Assigned(OnLineText) then
    OnLineText(Self, Text);
end;

constructor TEpiReportBase.Create(const AEpiDocument: TEpiDocument);
begin
  FEpiDocument := AEpiDocument;
end;

procedure TEpiReportBase.RunReport;
begin
  // Do nothing, should be overriden.
end;

end.

