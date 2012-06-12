unit epireport_generator_txt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_generator_base;

type

  { TEpiReportTXTGenerator }

  TEpiReportTXTGenerator = class(TEpiReportGeneratorBase)
  private
    FTableList:  TStringList;
    function  CenterText(Const Txt: string; Width: Integer): string;
    function  LineFromLines(var Lines: string): string;

  public
    // Lines
    procedure Section(Const Text: string); override;
    procedure Heading(Const Text: string); override;
    procedure Line(Const Text: string); override;

    // Table
    procedure TableHeader(Const Text: string; Const AColCount, ARowCount: Integer); override;
    procedure TableFooter(Const Text: string); override;
    procedure TableCell(Const Text: string; Const Col, Row: Integer); override;

    procedure StartReport(Const Title: string); override;
    procedure EndReport; override;
  end;

implementation

uses
  strutils, math, LazUTF8;

{ TEpiReportTXTGenerator }

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

procedure TEpiReportTXTGenerator.Section(const Text: string);
begin
  inherited Section(Text);
  AddLine(DupeString('=', Length(Text)));
  AddLine(Text);
  AddLine(DupeString('=', Length(Text)));
end;

procedure TEpiReportTXTGenerator.Heading(const Text: string);
begin
  inherited Heading(Text);
  AddLine(DupeString('-', Length(Text)));
  AddLine(Text);
  AddLine(DupeString('-', Length(Text)));
end;

procedure TEpiReportTXTGenerator.Line(const Text: string);
begin
  inherited Line(Text);
  AddLine(Text);
end;

procedure TEpiReportTXTGenerator.TableHeader(const Text: string;
  const AColCount, ARowCount: Integer);
var
  i: Integer;
begin
  inherited TableHeader(Text, AColCount, ARowCount);

  FTableList := TStringList.Create;
  FTableList.Capacity := (ColCount * RowCount) + 2;
  for i := 0 to FTableList.Capacity -1 do
    FTableList.Add('');
  FTableList[0] := Text;
end;

procedure TEpiReportTXTGenerator.TableFooter(const Text: string);
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
  inherited TableFooter(Text);
  FTableList[1] := Text;

  // Find max with of each column.
  SetLength(ColWidths, ColCount);
  for i := 2 to FTableList.Count - 1 do
  begin
    Idx := ((i - 2) mod ColCount);

    // TODO : Handle multiple lines in one cell.
    Txt := FTableList[i];
    while Length(Txt) > 0 do
    begin
      S := LineFromLines(Txt);
      ColWidths[Idx] := Math.Max(ColWidths[Idx], UTF8Length(S));
    end;
  end;

  ColWidthTotal := 0;
  for i := 0 to ColCount - 1 do
    ColWidthTotal += ColWidths[i];

  ColWidthTotal := Math.Max(ColWidthTotal, UTF8Length(FTableList[0])) +
    // for adding " | " to sides and in between cells.
    ((ColCount - 1) * 3) +
    // For adding "| " and " |" in beginning and end of cells
    4;

  // Table header
  if Length(FTableList[0]) > 0 then
  begin
    // Do not write an empty header... looks goofy :)
    AddLine(DupeString('-', ColWidthTotal));
    AddLine('| ' + CenterText(FTableList[0], ColWidthTotal - 4) + ' |');
  end;
  AddLine(DupeString('-', ColWidthTotal));

  // Table - first row:
  Txt := '| ';
  for i := 0 to ColCount - 1 do
    Txt += CenterText(FTableList[i+2], ColWidths[i]) + ' | ';
  TrimRight(Txt);
  AddLine(Txt);
  AddLine(DupeString('-', ColWidthTotal));

  // Table cells
  for i := 1 to RowCount - 1 do
  begin
    HasMoreText := true;
    while HasMoreText do
    begin
      Txt := '|';
      HasMoreText := false;
      for j := 0 to ColCount - 1 do
      begin
        Idx := (ColCount * i) + j + 2;

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
end;

procedure TEpiReportTXTGenerator.TableCell(const Text: string; const Col,
  Row: Integer);
var
  Idx: Integer;
begin
  inherited TableCell(Text, Col, Row);

  Idx := (ColCount * Row) + Col + 2;
  FTableList[Idx] := Text;
end;

procedure TEpiReportTXTGenerator.StartReport(const Title: string);
begin
  // Do nothing
end;

procedure TEpiReportTXTGenerator.EndReport;
begin
  // Do nothing
end;

end.

