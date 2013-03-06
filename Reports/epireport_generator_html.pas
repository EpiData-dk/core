unit epireport_generator_html;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_generator_base;

type

  { TEpiReportHTMLGenerator }

  TEpiReportHTMLGenerator = class(TEpiReportGeneratorBase)
  public
    procedure Section(Const Text: string); override;
    procedure Heading(Const Text: string); override;
    procedure Line(Const Text: string); override;

    // Table
    procedure TableHeader(Const Text: string; Const AColCount, ARowCount: Integer); override;
    procedure TableFooter(Const Text: string); override;
    procedure TableCell(Const Text: string; Const Col, Row: Integer); override;


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
   ['<br>',   '&amp;', '&quot;', '&lt;', '&gt;', '&apos;'],
   [rfReplaceAll]);

  if Result = '' then
    Result := '&nbsp;';
end;

{ TEpiReportHTMLGenerator }

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
  const AColCount, ARowCount: Integer);
begin
  inherited TableHeader(Text, AColCount, ARowCount);

  AddLine('<TABLE cellspacing=0 class=simple>');
  AddLine('<CAPTION class=caption>' + HtmlString(Text) + '</CAPTION>');
end;

procedure TEpiReportHTMLGenerator.TableFooter(const Text: string);
begin
  inherited TableFooter(Text);

  AddLine('</TABLE>');
end;

procedure TEpiReportHTMLGenerator.TableCell(const Text: string; const Col,
  Row: Integer);
var
  S: String;
begin
  inherited TableCell(Text, Col, Row);

  S := '';
  if (Col = 0) then
    S += '<TR>';

  if Row = 0 then
    S += '<TD class=firstrow>'
  else if (Col = 0) then
    S += '<TD class=firstcol>'
  else
    S += '<TD class=cell>';

  S += HtmlString(Text) + '</TD>';


  if (Col = (ColCount - 1)) then
    S += '</TR>';

  AddLine(S);
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
    'table.simple  {color: black; font-size: 1.0em; font-family: proportional; font-weight: normal; border-left: solid 2px black; border-right: solid 2px black; border-spacing: 0; margin-top: 1.25cm; }' + LineEnding +
    'table.simple th,' + LineEnding +
    'table.simple tr {padding: 0.2em}' + LineEnding +
    'table.simple td {border-bottom: 2px solid black; text-align: right; vertical-align: top; padding: 0.2em}' + LineEnding +
    'table.simple .cell {text-align: left; vertical-align: top; padding: 0.2em;}' + LineEnding +
    '' + LineEnding +
    'table.simple .cellfoot {font-size: 0.8em; text-align: left;}' + LineEnding +
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

