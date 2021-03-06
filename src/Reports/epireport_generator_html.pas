unit epireport_generator_html;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_generator_base, epireport_types;

type

  { TEpiReportHTMLGenerator }

  TEpiReportHTMLGenerator = class(TEpiReportGeneratorBase)
  private
    FTableList: TStringList;
    function  GetCellAdjust(Idx: Integer): TEpiReportGeneratorTableCellAdjustment;
  public
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


    procedure StartReport(const Title: string); override;
    procedure EndReport; override;
  end;

function  HtmlString(Const Txt: String): String;

implementation

uses
  strutils;

function HtmlString(const Txt: String): String;
var
  S: String;
  i: Integer;
begin
  S := Txt;
  for i := 1 to Length(S) do
    if (Ord(S[i]) < 32) and
       (not (Ord(S[i]) in [10,13])) then
      S[i] := '?';
  Result := StringsReplace(S,
   [LineEnding, '&',     '"',      '<',    '>',    ''''],
   ['<br>',     '&amp;', '&quot;', '&lt;', '&gt;', '&apos;'],
   [rfReplaceAll]);

  if Result = '' then
    Result := '&nbsp;';
end;

{ TEpiReportHTMLGenerator }

function TEpiReportHTMLGenerator.GetCellAdjust(Idx: Integer
  ): TEpiReportGeneratorTableCellAdjustment;
begin
  Result := TEpiReportGeneratorTableCellAdjustment(Integer(PtrInt(FTableList.Objects[Idx])));
end;

procedure TEpiReportHTMLGenerator.Section(const Text: string);
begin
  inherited Section(Text);
  AddLine('<h1>' + HtmlString(Text) + '</h1>');
end;

procedure TEpiReportHTMLGenerator.Heading(const Text: string);
begin
  inherited Heading(Text);
  AddLine('<h2>' + HtmlString(Text) + '</h2>');
end;

procedure TEpiReportHTMLGenerator.Line(const Text: string);
begin
  inherited Line(Text);
  AddLine(HtmlString(Text) + '<br>');
end;

procedure TEpiReportHTMLGenerator.TableHeader(const Text: string;
  const AColCount, ARowCount: Integer;
  const HeaderOptions: TEpiReportGeneratorTableHeaderOptionSet);
var
  i: Integer;
begin
  inherited TableHeader(Text, AColCount, ARowCount, HeaderOptions);

  FTableList := TStringList.Create;
  FTableList.Capacity := (ColCount * RowCount) + 2;
  for i := 0 to FTableList.Capacity -1 do
    FTableList.Add('');
  FTableList[0] := Text;
end;

procedure TEpiReportHTMLGenerator.TableFooter(const Text: string);
var
  S: String;
  i: Integer;
  j: Integer;
  Idx: Integer;
begin
  inherited TableFooter(Text);
  FTableList[1] := Text;

  AddLine('<TABLE cellspacing=0 class=simple>');
  AddLine('<CAPTION class=caption>' + HtmlString(FTableList[0]) + '</CAPTION>');

  for i := 0 to RowCount - 1 do
  begin
    S := '<TR>';

    for j := 0 to ColCount - 1 do
    begin
      Idx := (ColCount * i) + j + 2;

      if (i = 0) and (thoRowHeader in FHeaderOptions) then
        S += '<TD class=firstrow>'
      else if (j = 0) and (thoColHeader in FHeaderOptions) then
        S += '<TD class=firstcol>'
      else
        case GetCellAdjust(Idx) of
          tcaAutoAdjust:
            S += '<TD class=cell>';
          tcaLeftAdjust:
            S += '<TD class=cell style="text-align: left">';
          tcaCenter:
            S += '<TD class=cell style="text-align: center">';
          tcaRightAdjust:
            S += '<TD class=cell style="text-align: right">';
        end;

      if FTableList[Idx] <> '' then
        S += HtmlString(FTableList[Idx]) + '</TD>'
      else
        S += '&nbsp;</TD>';
    end;

    S += '</TR>';
    AddLine(S);
  end;

  if FTableList[1] <> '' then
  begin
    AddLine('<TR>');
    AddLine('<TD class=cellfoot colspan=' + IntToStr(ColCount) + '>' +
            HtmlString(FTableList[1]) + '</TD>');
    AddLine('</TR>');
  end;
  AddLine('</TABLE>');
end;

procedure TEpiReportHTMLGenerator.TableCell(const Text: string; const Col,
  Row: Integer; const CellAdjust: TEpiReportGeneratorTableCellAdjustment;
  const CellOptions: TEpiReportGeneratorTableCellOptionSet);
var
  Idx: Integer;
begin
  inherited TableCell(Text, Col, Row, CellAdjust);

  Idx := (ColCount * Row) + Col + 2;
  FTableList[Idx] := Text;
  FTableList.Objects[Idx] := TObject(PtrInt(Integer(CellAdjust)));
end;

procedure TEpiReportHTMLGenerator.StartReport(const Title: string);
var
  Txt: String;
begin
  Txt :=
    '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
    '<HTML>' + LineEnding +
    '<Head>' + LineEnding +
    '' + LineEnding +
    '<meta name="Copyright" content="EpiData Association, Denmark">' + LineEnding +
    '<meta name="No_Payment" content="EpiData Analysis is freeware">' + LineEnding +
    '<meta name="Update_from:" content="Http://www.epidata.dk">' + LineEnding +
    '<meta name="Disclaimer" content="Http://www.epidata.dk/disclaim.htm">' + LineEnding +
    '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">' + LineEnding +
    '' + LineEnding +
    '<STYLE type="text/css">' + LineEnding +

    // Stylesheet
    '<!--' + LineEnding +
    '  .body {color: black; background-color: white;  font-size: 1.0em; font-weight: normal}' + LineEnding +
    '' + LineEnding +
    '   p {color: black ;font-size: 1.0em; font-family: proportional,monospace; font-weight: normal; margin: 0em }' + LineEnding +
    '  h1 {color: blue; font-size: 1.25em; font-family: proportional,monospace; font-weight: bold}' + LineEnding +
    '  h2 {color: blue; font-size: 1.20em; font-family: proportional,monospace; font-weight: bold}' + LineEnding +
    '  h3 {color: blue; font-size: 1.15em; font-family: proportional,monospace; font-weight: bold}' + LineEnding +
    '  .small {color: black; font-size: 0.85em; font-family: proportional,monospace; font-weight: normal}' + LineEnding +
    '' + LineEnding +
    '  .warning {color: black; font-size: 0.85em; font-weight: normal; font-family: monospace}' + LineEnding +
    '  .info {color: green; font-size: 1.0em; font-weight: normal; font-family: monospace ;}' + LineEnding +
    '  .error {color: red; font-family: monospace}' + LineEnding +
    '' + LineEnding +
    'table.simple  {color: black; font-size: 1.0em; font-family: proportional; font-weight: normal; border-left: solid 2px black; border-right: solid 2px black; border-bottom: solid 2px black; border-spacing: 0; margin-top: 1.25cm; }' + LineEnding +
    'table.simple th,' + LineEnding +
    'table.simple tr {padding: 0.2em}' + LineEnding +
    'table.simple td {text-align: right; vertical-align: top; padding: 0.2em}' + LineEnding +
    'table.simple .cell {text-align: left; vertical-align: top; padding: 0.2em;}' + LineEnding +
    '' + LineEnding +
    'table.simple .cellfoot {border-top: solid 2px black; font-size: 0.8em; text-align: left;}' + LineEnding +
    'table.simple .caption {font-size: 1.1em; font-weight: bold; border-bottom: 2px solid black; text-align: center;}' + LineEnding +
    '' + LineEnding +
    'table.simple .firstrow {font-weight: bold; text-align: center; padding-right: 0.4em }' + LineEnding +
    'table.simple .firstcol {font-weight: bold; text-align: right; padding-right: 0.4em}' + LineEnding +
    '' + LineEnding +
    '/* EpiData Reporting Minimalistic style sheet - white background' + LineEnding +
    '   v1.0' + LineEnding +
    '   Use the design table.system as a template for a new design. To be safe, define all styles for a design.' + LineEnding +
    '   Note that a style followed by a comma will take the attributes at the end of the group, so do not sort this file.' + LineEnding +
    '*/' + LineEnding +
    '-->' + LineEnding +

    '</STYLE>' + LineEnding +
    '<TITLE>' + HtmlString(Title) + '</TITLE>' + LineEnding +
    '</HEAD>' + LineEnding +
    '<BODY class=body>' + LineEnding;
  AddLine(Txt);
end;

procedure TEpiReportHTMLGenerator.EndReport;
begin
  AddLine('</BODY>' + LineEnding +
          '</HTML>');
end;

end.

