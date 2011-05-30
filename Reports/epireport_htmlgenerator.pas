unit epireport_htmlgenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base;

type

  { TEpiReportHTMLGenerator }

  TEpiReportHTMLGenerator = class
  private
    FReport: TEpiReportBase;
    FReportText: TStringList;
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
    procedure   InitHtml;
    procedure   CloseHtml;
  public
    class function HtmlHeader: string;
    class function HtmlFooter: string;
  end;

implementation

{ TEpiReportHTMLGenerator }

procedure TEpiReportHTMLGenerator.InternalInit;
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

procedure TEpiReportHTMLGenerator.AddLine(const Txt: string);
begin
  FReportText.Add(Txt);
end;

procedure TEpiReportHTMLGenerator.Section(Sender: TEpiReportBase;
  const Text: string);
begin
  AddLine('<h1>' + Text + '</h1>');
end;

procedure TEpiReportHTMLGenerator.Heading(Sender: TEpiReportBase;
  const Text: string);
begin
  AddLine('<h2>' + Text + '</h2>');
end;

procedure TEpiReportHTMLGenerator.Line(Sender: TEpiReportBase;
  const Text: string);
begin
  AddLine(Text + '<br>');
end;

procedure TEpiReportHTMLGenerator.TableHeader(Sender: TEpiReportBase;
  const Text: string; const ColCount, RowCount: Integer);
begin
  if InTable then
    raise Exception.Create('TEpiReportHTMLGenerator: Previous table not closed!');

  FColCount := ColCount;
  FRowCount := RowCount;

  AddLine('<TABLE cellspacing=0 class=simple>');
//  if Text <> '' then
    AddLine('<CAPTION class=caption>' + Text + '</CAPTION>');
end;

procedure TEpiReportHTMLGenerator.TableFooter(Sender: TEpiReportBase;
  const Text: string);
begin
  AddLine('</TABLE>');
end;

procedure TEpiReportHTMLGenerator.TableCell(Sender: TEpiReportBase;
  const Text: string; const Col, Row: Integer);
var
  S: String;
begin
  if (Col < 0) or (Col > (FColCount-1)) or
     (Row < 0) or (Row > (FRowCount-1)) then
    Raise Exception.Create('TEpiReportValueLabelsHtml: Index out of bound for table! ' + Format('Col: %d; Row: %d', [Col,Row]));

  S := '';
  if (Col = 0) then
    S += '<TR>';

  if Row = 0 then
    S += '<TD class=firstrow>'
  else if (Col = 0) then
    S += '<TD class=firstcol>'
  else
    S += '<TD class=cell>';

  S += Text + '</TD>';


  if (Col = (FColCount - 1)) then
    S += '</TR>';

  AddLine(S);
end;

constructor TEpiReportHTMLGenerator.Create(const Report: TEpiReportBase);
begin
  if not Assigned(Report) then
    Raise Exception.Create('TEpiReportHTMLGenerator: Report cannot be nil!');
  FReport := Report;
  FReportText := TStringList.Create;
  InternalInit;
end;

destructor TEpiReportHTMLGenerator.Destroy;
begin
  FReportText.Free;
  inherited Destroy;
end;

function TEpiReportHTMLGenerator.GetReportText: string;
begin
  result := FReportText.Text;
end;

procedure TEpiReportHTMLGenerator.InitHtml;
begin
  AddLine(HtmlHeader);
end;

procedure TEpiReportHTMLGenerator.CloseHtml;
begin
  AddLine(HtmlFooter);
end;

class function TEpiReportHTMLGenerator.HtmlHeader: string;
begin
  result :=
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
    'table.simple  {color: black; font-size: 1.0em; font-family: proportional; font-weight: normal; border-left: none; border-right: none; border-bottom: solid 2px red; border-spacing: 0; margin-top: 1.25cm; }' + LineEnding +
    'table.simple th,' + LineEnding +
    'table.simple tr {border-bottom: double 2px black; padding: 0.2em}' + LineEnding +
    'table.simple td {text-align: right; vertical-align: top; padding: 0.2em}' + LineEnding +
    'table.simple .cell {text-align: right; vertical-align: top; padding: 0.2em;}' + LineEnding +
    '' + LineEnding +
    'table.simple .cellfoot {font-size: 0.8em; border-top: solid 2.0px; text-align: left;}' + LineEnding +
    'table.simple .caption {font-size: 1.1em; font-weight: bold; border-bottom: solid 2.0px; text-align: left;}' + LineEnding +
    '' + LineEnding +
    'table.simple .firstrow {font-weight: bold; text-align: center;padding-right: 0.4em }' + LineEnding +
    'table.simple .firstcol {font-weight: bold; text-align: right; padding-right: 0.4em}' + LineEnding +
    '' + LineEnding +
    '/* EpiData Reporting Minimalistic style sheet - white background' + LineEnding +
    '   v1.0' + LineEnding +
    '   Use the design table.system as a template for a new design. To be safe, define all styles for a design.' + LineEnding +
    '   Note that a style followed by a comma will take the attributes at the end of the group, so do not sort this file.' + LineEnding +
    '*/' + LineEnding +
    '-->' + LineEnding +
    '</STYLE>' + LineEnding +
    '' + LineEnding +
    '' + LineEnding +
    '<TITLE>Codebook</TITLE>' + LineEnding +
    '</HEAD>' + LineEnding +
    '<BODY class=body>' + LineEnding;
end;

class function TEpiReportHTMLGenerator.HtmlFooter: string;
begin
  result :=
    '</BODY>' + LineEnding +
    '</HTML>';
end;

end.

