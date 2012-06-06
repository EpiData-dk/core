unit epireport_txtgenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base;

type

  { TEpiReportTXTGenerator }

  TEpiReportTXTGenerator = class
  private
    FReport: TEpiReportBase;
    FReportText: TStringList;
    FTableList:  TStringList;
    InTable: Boolean;
    FRowCount: Integer;
    FColCount: Integer;

    // Misc
    procedure InternalInit;
    procedure AddLine(Const Txt: string);

    // Lines
    procedure Section(Sender: TEpiReportBase; Const Text: string);
    procedure Heading(Sender: TEpiReportBase; Const Text: string);
    procedure Line(Sender: TEpiReportBase; Const Text: string);

    // Table
    procedure TableHeader(Sender: TEpiReportBase; Const Text: string;
      Const ColCount, RowCount: Integer);
    procedure TableFooter(Sender: TEpiReportBase; Const Text: string);
    procedure TableCell(Sender: TEpiReportBase; Const Text: string;
      Const Col, Row: Integer);
  public
    constructor Create(Const Report: TEpiReportBase);
    destructor  Destroy; override;
    function    GetReportText: string;
  end;

implementation

uses
  strutils, math, LazUTF8;

{ TEpiReportTXTGenerator }

procedure TEpiReportTXTGenerator.InternalInit;
begin
  FReportText := TStringList.Create;
  with FReport do
  begin
    OnHeading := @Heading;
    OnLineText := @Line;
    OnSection := @Section;
    OnTableCell := @TableCell;
    OnTableFooter := @TableFooter;
    OnTableHeader := @TableHeader;
  end;
end;

procedure TEpiReportTXTGenerator.AddLine(const Txt: string);
begin
  FReportText.Add(Txt);
end;

procedure TEpiReportTXTGenerator.Section(Sender: TEpiReportBase;
  const Text: string);
begin
  AddLine(DupeString('=', Length(Text)));
  AddLine(Text);
  AddLine(DupeString('=', Length(Text)));
end;

procedure TEpiReportTXTGenerator.Heading(Sender: TEpiReportBase;
  const Text: string);
begin
  AddLine(DupeString('-', Length(Text)));
  AddLine(Text);
  AddLine(DupeString('-', Length(Text)));
end;

procedure TEpiReportTXTGenerator.Line(Sender: TEpiReportBase; const Text: string
  );
begin
  AddLine(Text);
end;

procedure TEpiReportTXTGenerator.TableHeader(Sender: TEpiReportBase;
  const Text: string; const ColCount, RowCount: Integer);
var
  i: Integer;
begin
  if InTable then
    raise Exception.Create('TEpiReportHTMLGenerator: Previous table not closed!');

  FColCount := ColCount;
  FRowCount := RowCount;
  InTable   := true;

  FTableList := TStringList.Create;
  FTableList.Capacity := (FColCount * FRowCount) + 2;
  for i := 0 to FTableList.Capacity -1 do
    FTableList.Add('');
  FTableList[0] := Text;
end;

procedure TEpiReportTXTGenerator.TableFooter(Sender: TEpiReportBase;
  const Text: string);
var
  ColWidths: TBoundArray;
  ColWidthTotal: Integer;
  Txt: String;
  i: Integer;
  j: Integer;
  Idx: Integer;
  Value: Double;
begin
  FTableList[1] := Text;

  // Find max with of each column.
  SetLength(ColWidths, FColCount);
  for i := 2 to FTableList.Count - 1 do
  begin
    Idx := ((i - 2) mod FColCount);
    ColWidths[Idx] := Math.Max(ColWidths[Idx], UTF8Length(FTableList[i]));
  end;

  ColWidthTotal := 0;
  for i := 0 to FColCount - 1 do
    ColWidthTotal += ColWidths[i];

  ColWidthTotal := Math.Max(ColWidthTotal, UTF8Length(FTableList[0])) +
    // for adding " | " to sides and in between cells.
    ((FColCount - 1) * 3) +
    // For adding "| " and " |" in beginning and end of cells
    4;

  // Table header
  AddLine(DupeString('-', ColWidthTotal));
  i := (ColWidthTotal - UTF8Length(FTableList[0])) div 2 - 2;
  AddLine('| ' + DupeString(' ', i) + FTableList[0] + DupeString(' ', i) + ' |');
  AddLine(DupeString('-', ColWidthTotal));

  // Table cells
  for i := 0 to FRowCount - 1 do
  begin
    Txt := '|';
    for j := 0 to FColCount - 1 do
    begin
      Idx := (FColCount * i) + j + 2;

      // First row : TODO

      Txt += ' ';
      // If data is number then right-adjust.
      if TryStrToFloat(FTableList[Idx], Value) then
        Txt += Format('%' + IntToStr(ColWidths[j]) + 's', [FTableList[Idx]])
      else
        Txt += Format('%-' + IntToStr(ColWidths[j]) + 's', [FTableList[Idx]]);
      Txt += ' |';
    end;
    AddLine(Txt);
    AddLine(DupeString('-', ColWidthTotal));
  end;
  // Table footer
  AddLine(FTableList[1]);

  FTableList.Free;
  InTable := false;
end;

procedure TEpiReportTXTGenerator.TableCell(Sender: TEpiReportBase;
  const Text: string; const Col, Row: Integer);
var
  Idx: Integer;
begin
  if not InTable then
    Raise Exception.Create('TEpiReportTXTGenerator: Table not initialised');

  if (Col < 0) or (Col > (FColCount-1)) or
     (Row < 0) or (Row > (FRowCount-1)) then
    Raise Exception.Create('TEpiReportTXTGenerator: Index out of bound for table! ' + Format('Col: %d; Row: %d', [Col,Row]));

  Idx := (FColCount * Row) + Col + 2;
  FTableList[Idx] := Text;
end;

constructor TEpiReportTXTGenerator.Create(const Report: TEpiReportBase);
begin
  if not Assigned(Report) then
    Raise Exception.Create('TEpiReportTXTGenerator: Report cannot be nil!');
  FReport := Report;
  InternalInit;
end;

destructor TEpiReportTXTGenerator.Destroy;
begin
  FReportText.Free;
  inherited Destroy;
end;

function TEpiReportTXTGenerator.GetReportText: string;
begin
  result := FReportText.Text;
end;

end.

