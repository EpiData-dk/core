unit epireport_valuelabels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epidocument,
  epivaluelabels;

type

  { TEpiReportValueLabels }

  TEpiReportValueLabels = class(TEpiReportBase)
  private
    FEpiValueLabels: TEpiValueLabelSets;
    procedure PrintValueLabelSet(Const VLSet: TEpiValueLabelSet);
  public
    constructor Create(const AEpiDocument: TEpiDocument); override;
    procedure   RunReport; override;
    property    EpiValueLabels: TEpiValueLabelSets read FEpiValueLabels;
  end;

  { TEpiReportValueLabelsHtml }

  TEpiReportValueLabelsHtml = class(TEpiReportValueLabels)
  private
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
  protected
    function GetReportText: string; override;
  public
    constructor Create(const AEpiDocument: TEpiDocument); override;
    procedure RunReport; override;
  end;

implementation

uses
  epimiscutils;

{ TEpiReportValueLabels }

procedure TEpiReportValueLabels.PrintValueLabelSet(
  const VLSet: TEpiValueLabelSet);
var
  i: Integer;
begin
  DoHeading(VLSet.Name + ': (' + EpiTypeNames[VLSet.LabelType] + ')');

  DoTableHeader('', 3, VLSet.Count + 1);

  DoTableCell(0, 0, 'Category');
  DoTableCell(1, 0, 'Label');
  DoTableCell(2, 0, 'Missing');

  for i := 0 to VLSet.Count - 1 do
  with VLSet[i] do
  begin
    DoTableCell(0, i+1, ValueAsString);
    DoTableCell(1, i+1, TheLabel.Text);
    DoTableCell(2, i+1, BoolToStr(IsMissingValue, 'yes', ''));
  end;
  DoTableFooter('');
end;

constructor TEpiReportValueLabels.Create(const AEpiDocument: TEpiDocument);
begin
  inherited Create(AEpiDocument);
  FEpiValueLabels := AEpiDocument.ValueLabelSets;
end;

procedure TEpiReportValueLabels.RunReport;
var
  LabelList: TStringList;
var
  i: Integer;
begin
  inherited RunReport;

  LabelList := TStringList.Create;
  LabelList.Sorted := true;
  for i := 0 to EpiValueLabels.Count - 1 do
    LabelList.AddObject(EpiValueLabels[i].Name, EpiValueLabels[i]);

  DoSection('ValueLabels');

  for i := 0 to LabelList.Count - 1 do
    PrintValueLabelSet(TEpiValueLabelSet(LabelList.Objects[i]));
end;

{ TEpiReportValueLabelsHtml }

procedure TEpiReportValueLabelsHtml.InternalInit;
begin
  FReportText := TStringList.Create;
  OnHeading := @Heading;
  OnLineText := @Line;
  OnSection := @Section;
  OnTableCell := @TableCell;
  OnTableFooter := @TableFooter;
  OnTableHeader := @TableHeader;
end;

procedure TEpiReportValueLabelsHtml.AddLine(const Txt: string);
begin
  FReportText.Add(Txt);
end;

procedure TEpiReportValueLabelsHtml.Section(Sender: TEpiReportBase;
  const Text: string);
begin
  AddLine('<h1>' + Text + '</h1>');
end;

procedure TEpiReportValueLabelsHtml.Heading(Sender: TEpiReportBase;
  const Text: string);
begin
  AddLine('<h2>' + Text + '</h2>');
end;

procedure TEpiReportValueLabelsHtml.Line(Sender: TEpiReportBase;
  const Text: string);
begin
  AddLine(Text + '<br>');
end;

procedure TEpiReportValueLabelsHtml.TableHeader(Sender: TEpiReportBase;
  const Text: string; const ColCount, RowCount: Integer);
begin
  if InTable then
    raise Exception.Create('TEpiReportValueLabelsHtml: Previous table not closed!');

  FColCount := ColCount;
  FRowCount := RowCount;

  AddLine('<TABLE cellspacing=0 class=simple>');
  if Text <> '' then
    AddLine('<CAPTION class=caption>' + Text + '</CAPTION>');
end;

procedure TEpiReportValueLabelsHtml.TableFooter(Sender: TEpiReportBase;
  const Text: string);
begin
  AddLine('</TABLE>');
end;

procedure TEpiReportValueLabelsHtml.TableCell(Sender: TEpiReportBase;
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

function TEpiReportValueLabelsHtml.GetReportText: string;
begin
  Result := FReportText.Text;
end;

constructor TEpiReportValueLabelsHtml.Create(const AEpiDocument: TEpiDocument);
begin
  inherited Create(AEpiDocument);
  InternalInit;
end;

procedure TEpiReportValueLabelsHtml.RunReport;
begin
  // HTML HEADING
  AddLine('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">');
  AddLine('<HTML>');
//  AddLine('<!-- This file generated by xxxxxxxxxxxxxx       -> Epidata Analysis EpiData Analysis V2.2.1.171  -->');
  AddLine('<Head>');
  AddLine('');
//  AddLine('<meta name="author" content="EpiData Analysis V2.2.1.171 ">');
  AddLine('<meta name="Copyright" content="EpiData Association, Denmark">');
  AddLine('<meta name="No_Payment" content="EpiData Analysis is freeware">');
  AddLine('<meta name="Update_from:" content="Http://www.epidata.dk">');
  AddLine('<meta name="Disclaimer" content="Http://www.epidata.dk/disclaim.htm">');
  AddLine('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">');
  AddLine('');

  // STYLESHEET
  AddLine('<STYLE type="text/css">');
  AddLine('<!--');
  AddLine('  .body {color: black; background-color: white;  font-size: 1.0em; font-weight: normal}');
  AddLine('');
  AddLine('   p {color: black ;font-size: 1.0em; font-family: proportional,monospace; font-weight: normal; margin: 0em }');
  AddLine('  h1 {color: blue; font-size: 1.25em; font-family: proportional,monospace; font-weight: bold}');
  AddLine('  h2 {color: blue; font-size: 1.20em; font-family: proportional,monospace; font-weight: bold}');
  AddLine('  h3 {color: blue; font-size: 1.15em; font-family: proportional,monospace; font-weight: bold}');
  AddLine('  .small {color: black; font-size: 0.85em; font-family: proportional,monospace; font-weight: normal}');
  AddLine('');
  AddLine('  .warning {color: black; font-size: 0.85em; font-weight: normal; font-family: monospace}');
  AddLine('  .info {color: green; font-size: 1.0em; font-weight: normal; font-family: monospace ;}');
  AddLine('  .error {color: red; font-family: monospace}');
  AddLine('');
  AddLine('table.simple  {color: black; font-size: 1.0em; font-family: proportional; font-weight: normal; border-left: none; border-right: none; border-bottom: solid 2px black; border-spacing: 0; margin-top: 8px; }');
  AddLine('table.simple th,');
  AddLine('table.simple tr {border-bottom: double 2px black; padding: 0.2em}');
  AddLine('table.simple td {text-align: right; vertical-align: top; padding: 0.7em}');
  AddLine('table.simple .cell {text-align: right; vertical-align: top; padding: 0.7em}');
  AddLine('');
  AddLine('table.simple .cellfoot {font-size: 0.8em; border-top: solid 2.0px; text-align: left;}');
  AddLine('table.simple .caption {font-size: 1.1em; font-weight: bold; border-bottom: solid 1.5px; text-align: center;}');
  AddLine('');
  AddLine('table.simple .firstrow {font-weight: bold; text-align: left; }');
  AddLine('table.simple .firstcol {font-weight: bold; text-align: left; }');
  AddLine('');
  AddLine('/* EpiData Reporting Minimalistic style sheet - white background');
  AddLine('   v1.0');
  AddLine('   Use the design table.system as a template for a new design. To be safe, define all styles for a design.');
  AddLine('   Note that a style followed by a comma will take the attributes at the end of the group, so do not sort this file.');
  AddLine('*/');
  AddLine('-->');
  AddLine('</STYLE>');
  AddLine('');
  AddLine('');

  // TITLE & BODY:
  AddLine('<TITLE>Codebook</TITLE>');
  AddLine('</HEAD>');
  AddLine('<BODY class=body>');
  AddLine('');

  inherited RunReport;

  AddLine('</BODY>');
  AddLine('</HTML>')
end;

end.

