unit epireport_generator_txt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_generator_base, epireport_types;

type

  { TEpiReportTXTGenerator }

  TEpiReportTXTGenerator = class(TEpiReportGeneratorBase)
  private
    FTableList:  TStringList;
    function  CenterText(Const Txt: string; Width: Integer): string;
    function  LeftAdjustText(Const Txt: string; Width: Integer): string;
    function  RightAdjustText(Const Txt: string; Width: Integer): string;
    function  LineFromLines(var Lines: string): string;
    function  GetCellAdjust(Idx: Integer): TEpiReportGeneratorTableCellAdjustment;
  public
    // Lines
    procedure Section(Const Text: string); override;
    procedure Heading(Const Text: string); override;
    procedure Line(Const Text: string); override;

    // Table
    procedure TableHeader(const Text: string; const AColCount, ARowCount: Integer;
      const HeaderOptions: TEpiReportGeneratorTableHeaderOptionSet = [
      thoRowHeader]); override;
    procedure TableFooter(Const Text: string); override;
    procedure TableCell(const Text: string; const Col, Row: Integer;
      const CellAdjust: TEpiReportGeneratorTableCellAdjustment = tcaAutoAdjust;
      Const CellOptions: TEpiReportGeneratorTableCellOptionSet = []
      ); override;

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

function TEpiReportTXTGenerator.LeftAdjustText(const Txt: string; Width: Integer
  ): string;
begin
  Result := Txt + DupeString(' ', Width - UTF8Length(Txt));
end;

function TEpiReportTXTGenerator.RightAdjustText(const Txt: string;
  Width: Integer): string;
begin
  Result := DupeString(' ', Width - UTF8Length(Txt)) + Txt;
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

function TEpiReportTXTGenerator.GetCellAdjust(Idx: Integer
  ): TEpiReportGeneratorTableCellAdjustment;
begin
  Result := TEpiReportGeneratorTableCellAdjustment(Integer(PtrInt(FTableList.Objects[Idx])));
end;

procedure TEpiReportTXTGenerator.Section(const Text: string);
var
  T: String;
  L: Integer;
begin
  inherited Section(Text);

  T := Text;
  L := 0;
  while Length(T) > 0 do
    L := Max(L, UTF8Length(LineFromLines(T)));

  AddLine(DupeString('=', L));
  AddLine(Text);
  AddLine(DupeString('=', L));
end;

procedure TEpiReportTXTGenerator.Heading(const Text: string);
var
  T: String;
  L: Integer;
begin
  inherited Heading(Text);

  T := Text;
  L := 0;
  while Length(T) > 0 do
    L := Max(L, UTF8Length(LineFromLines(T)));

  AddLine(DupeString('-', L));
  AddLine(Text);
  AddLine(DupeString('-', L));
end;

procedure TEpiReportTXTGenerator.Line(const Text: string);
begin
  inherited Line(Text);
  AddLine(Text);
end;

procedure TEpiReportTXTGenerator.TableHeader(const Text: string;
  const AColCount, ARowCount: Integer;
  const HeaderOptions: TEpiReportGeneratorTableHeaderOptionSet);
var
  i: Integer;
begin
  inherited TableHeader(Text, AColCount, ARowCount, HeaderOptions);

  // Txt tables ALWAYS start with a line...
  for i := 0 to AColCount - 1 do
    TableCellOptions[i, 0] := TableCellOptions[i, 0] + [tcoTopBorder];

  // Txt tables ALWAYS ends with a line...
  for i := 0 to AColCount - 1 do
    TableCellOptions[i, RowCount - 1] := TableCellOptions[i, RowCount - 1] + [tcoBottomBorder];

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
  StartRow: Integer;


  function GetAdjacentCellBorder(Col, Row: Integer;
    Const Border: TEpiReportGeneratorTableCellOptions): TEpiReportGeneratorTableCellOptionSet;
  begin
    result := [];

    if (Col = 0) and (Border = tcoLeftBorder) then exit;
    if (Col = (ColCount - 1)) and (Border = tcoRightBorder) then exit;
    if (Row = 0) and (Border = tcoTopBorder) then exit;
    if (Row = (RowCount - 1)) and (Border = tcoBottomBorder) then exit;

    case Border of
      tcoTopBorder:    Dec(Row);
      tcoBottomBorder: Inc(Row);
      tcoLeftBorder:   Dec(Col);
      tcoRightBorder:  Inc(Col);
    end;
    Result := TableCellOptions[Col, Row];
  end;

begin
  FTableList[1] := Text;

  // Find max with of each column.
  SetLength(ColWidths, ColCount);
  for i := 2 to FTableList.Count - 1 do
  begin
    Idx := ((i - 2) mod ColCount);

    // Handle multiple lines in one cell.
    Txt := FTableList[i];
    while Length(Txt) > 0 do
    begin
      S := LineFromLines(Txt);
      ColWidths[Idx] := Math.Max(ColWidths[Idx], UTF8Length(S));
    end;
  end;

{  for i := 0 to ColCount - 1 do
    for j := 0 to RowCount - 1 do
    begin
      if tcoLeftBorder in TableCellOptions[i, j] then
        ColWidths[i] := ColWidths[i] + 1;

      if tcoRightBorder in TableCellOptions[i, j] then
        ColWidths[i] := ColWidths[i] + 1;

      if [tcoRightBorder, tcoLeftBorder]*TableCellOptions[i, j] <> [] then
        break;
    end;      }

  ColWidthTotal := 0;
  for i := 0 to ColCount - 1 do
    ColWidthTotal += ColWidths[i];

  ColWidthTotal := Math.Max(ColWidthTotal, UTF8Length(FTableList[0])) + (ColCount - 1);

  // Table header
  // Do not write an empty header... looks goofy :)
  if Length(FTableList[0]) > 0 then
    AddLine(FTableList[0]);

  // Table cells
  for i := 0 to RowCount - 1 do
  begin
    Txt := '';

    // Create top border!
    for j := 0 to ColCount - 1 do
    begin
      S := ' ';
      if (tcoTopBorder in TableCellOptions[j, i]) and
         (not (tcoBottomBorder in GetAdjacentCellBorder(j, i, tcoTopBorder)))
      then
        S := '-';

      Txt += DupeString(S, ColWidths[j]);
      if j < (ColCount - 1) then
        Txt += S;
    end;

    if Trim(txt) <> '' then
      AddLine(Txt);

    HasMoreText := true;
    while HasMoreText do
    begin
      Txt := '';

      // Accumulate text in Each Cell.
      HasMoreText := false;
      for j := 0 to ColCount - 1 do
      begin
        Idx := (ColCount * i) + j + 2;

 {       // Left border!
        if (tcoLeftBorder in TableCellOptions[j, i]) and
           (not (tcoRightBorder in GetAdjacentCellBorder(j, i, tcoLeftBorder)))
        then
          Txt += '| ';     }

        T := FTableList[Idx];
        S := LineFromLines(T);
        FTableList[Idx] := T;

        case GetCellAdjust(Idx) of
          tcaAutoAdjust:
            // If data is number (or missing) then right-adjust.
            if (TryStrToFloat(S, Value)) or
               (S = '.')
            then
              Txt += RightAdjustText(S, ColWidths[j])
            else
              Txt += LeftAdjustText(S, ColWidths[j]);
          tcaLeftAdjust:
            Txt += LeftAdjustText(S, ColWidths[j]);
          tcaCenter:
            Txt += CenterText(S, ColWidths[j]);
          tcaRightAdjust:
            Txt += RightAdjustText(S, ColWidths[j]);
        end;

 {       // Right border
        if (tcoRightBorder in TableCellOptions[j, i])
        then
          Txt += '| '
        else          }
          Txt += ' ';

        HasMoreText := HasMoreText or (Length(T) > 0);
      end;
      AddLine(Txt);
    end;

    // Create bottom border!
    Txt := '';
    for j := 0 to ColCount - 1 do
    begin
      S := ' ';
      if (tcoBottomBorder in TableCellOptions[j, i])
      then
        S := '-';

      Txt += DupeString(S, ColWidths[j]);
      if j < (ColCount - 1) then
        Txt += S;
    end;

    if Trim(txt) <> '' then
      AddLine(Txt);
  end;

  // Table footer
  if FTableList[1] <> '' then
    AddLine(FTableList[1]);

  FTableList.Free;
  inherited TableFooter(Text);
end;

procedure TEpiReportTXTGenerator.TableCell(const Text: string; const Col,
  Row: Integer; const CellAdjust: TEpiReportGeneratorTableCellAdjustment;
  const CellOptions: TEpiReportGeneratorTableCellOptionSet);
var
  Idx: Integer;
begin
  inherited TableCell(Text, Col, Row, CellAdjust, CellOptions);

  Idx := (ColCount * Row) + Col + 2;
  FTableList[Idx] := Text;
  FTableList.Objects[Idx] := TObject(PtrInt(Integer(CellAdjust)));
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

