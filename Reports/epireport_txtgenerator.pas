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
    function  CenterText(Const Txt: string; Width: Integer): string;
    function  LineFromLines(var Lines: string): string;

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

function TEpiReportTXTGenerator.CenterText(const Txt: string; Width: Integer
  ): string;
var
  L: PtrInt;
  D: Integer;
  Count: Integer;
begin
  L := UTF8Length(Txt);
  if L >= Width then
  begin
    Result := Txt;
    Exit;
  end;

  // D indicates if an extra space is required at the end of text.
  D := 0;
  if (odd(Width) and not odd(L)) then Inc(D);
  if (not odd(Width) and odd(L)) then Inc(D);

  Count := (Width - UTF8Length(Txt)) div 2;
  Result :=
    // Prefix
    DupeString(' ', Count) +
    // Text
    Txt +
    // Postfix (with possible extra space)
    DupeString(' ', Count + D);
end;

function TEpiReportTXTGenerator.LineFromLines(var Lines: string): string;
var
  p: Integer;
begin
  p := Pos(LineEnding, Lines);
  if p = 0 then
  begin
    result:=Lines;
    Lines:='';
  end else begin
    Result := Copy(Lines, 1, p-1);
    delete(Lines, 1, (p - 1) + Length(LineEnding));
  end;
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
  S: String;
  T: String;
  HasMoreText: Boolean;
  W: PtrInt;
begin
  FTableList[1] := Text;

  // Find max with of each column.
  SetLength(ColWidths, FColCount);
  for i := 2 to FTableList.Count - 1 do
  begin
    Idx := ((i - 2) mod FColCount);

    // TODO : Handle multiple lines in one cell.
    Txt := FTableList[i];
    while Length(Txt) > 0 do
    begin
      S := LineFromLines(Txt);
      ColWidths[Idx] := Math.Max(ColWidths[Idx], UTF8Length(S));
    end;
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
  AddLine('| ' + CenterText(FTableList[0], ColWidthTotal - 4) + ' |');
  AddLine(DupeString('-', ColWidthTotal));

  // Table - first row:
  Txt := '| ';
  for i := 0 to FColCount - 1 do
    Txt += CenterText(FTableList[i+2], ColWidths[i]) + ' | ';
  TrimRight(Txt);
  AddLine(Txt);
  AddLine(DupeString('-', ColWidthTotal));

  // Table cells
  for i := 1 to FRowCount - 1 do
  begin
    HasMoreText := true;
    while HasMoreText do
    begin
      Txt := '|';
      HasMoreText := false;
      for j := 0 to FColCount - 1 do
      begin
        Idx := (FColCount * i) + j + 2;

        Txt += ' ';

        T := FTableList[Idx];
        S := LineFromLines(T);
        FTableList[Idx] := T;

        // If data is number then right-adjust.
        W := ColWidths[j] - UTF8Length(S);
        if TryStrToFloat(S, Value) then
          Txt += DupeString(' ', W) + S
        else
          Txt += S + DupeString(' ', W);
        Txt += ' |';

        HasMoreText := HasMoreText or (Length(T) > 0);
      end;
      AddLine(Txt);
    end;
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

