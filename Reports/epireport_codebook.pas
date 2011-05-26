unit epireport_codebook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epidocument;

type

  { TEpiReportCodeBook }

  TEpiReportCodeBook = class(TEpiReportBase)
  private
    FFileName: string;
    FPath: string;
    FRecursive: boolean;
    FDocument: TEpiDocument;
    procedure DoReport(Const Document: TEpiDocument);
  public
    constructor Create(Const Document: TEpiDocument); virtual; overload;
    constructor Create(Const AFileName: string); virtual; overload;
    constructor Create(Const APath: string; Const Recursive: boolean); virtual; overload;
    procedure RunReport; override;
  end;

  { TEpiReportCodeBookHtml }

  TEpiReportCodeBookHtml = class(TEpiReportCodeBook)
  private
    FReportText: TStringList;

    // Misc
    procedure InternalInit;
    procedure AddLine(Const Txt: string);

    // Lines
    procedure Section(Sender: TEpiReportBase; Const Text: string);
    procedure Heading(Sender: TEpiReportBase; Const Text: string);
    procedure Line(Sender: TEpiReportBase; Const Text: string);

    // Table
    procedure TableHeader(Sender: TEpiReportBase; Const Text: string);
    procedure TableFooter(Sender: TEpiReportBase; Const Text: string);
    procedure TableCell(Sender: TEpiReportBase;
      Const Col, Row: Integer; Const Text: string);
  protected
    function GetReportText: string; override;
  public
    procedure RunReport; override;
    constructor Create(const Document: TEpiDocument); override; overload;
    constructor Create(const AFileName: string); override; overload;
    constructor Create(const APath: string; const Recursive: boolean);
       override; overload;
    destructor Destroy; override;
  end;

implementation

uses
  FileUtil, epimiscutils;

{ TEpiReportCodeBook }

procedure TEpiReportCodeBook.DoReport(const Document: TEpiDocument);
var
  i: Integer;
  Cnt: Integer;
  MVCnt: Integer;
  j: Integer;
begin
{  KODEBOG

Rapport genereret         23. May 2011 10:06

Datafil:                  H:\undervisning\datadokumentation og stata intro\EVALUERINGSSKEMA DATADOK.REC
Fil label:                evaluering af datadokumentation og Stata Intro
Fil dato:                 8. Feb 2010 11:38
Checks tilføjet:          Ja (Sidste ændring 8. Feb 2010 11:37)

Antal felter:             37

Obs. totalt:              0
Slettede obs.:            0
Anvendt i kodebogen:      0 observationer

navn ------------------------------------------------------------ Afleveret af :
                   type:  Tekst

                uoplyst:  0/0
         unikke værdier:  0

                  tabel:  Frekv.    Pct.  Værdi   Label

}

  DoSection ('CodeBook');
  DoLineText('');
  DoLineText('Generated: ' + DateTimeToStr(Now));
  DoLineText('');
//  DoLineText('FileName: ' + Document.ProjectSettings.)
  DoLineText('Title: ' + Document.Study.Title.Text);
  DoLineText('Create: ' + FormatDateTime('c', Document.Study.Created));
  DoLineText('Modifed: ' + FormatDateTime('c', Document.Study.ModifiedDate));
  DoLineText('');
  DoLineText('Fields: ' + IntToStr(Document.DataFiles[0].Fields.Count));
  DoLineText('Records: ' + IntToStr(Document.DataFiles[0].Size));
  DoLineText('Deleted: ' + IntToStr(Document.DataFiles[0].DeletedCount));
  DoLineText('');

  for i := 0 to Document.DataFiles[0].Fields.Count - 1 do
  with Document.DataFiles[0].Field[i] do
  begin
    DoHeading('------------------------------------------------------------');
    DoLineText(Name + ': ' + Question.Text);
    DoLineText('Type: ' + EpiTypeNames[FieldType]);
    DoLineText('');

    Cnt := 0;
    MVCnt := 0;
    for j := 0 to Size - 1 do
    begin
      if IsMissing[j] or IsMissingValue[j] then Inc(Cnt);
      if IsMissingValue[j] then inc(MVCnt);
    end;
    DoLineText(Format('Missing: %d (%d) / %d', [Cnt, MVCnt, Size]));
  end;
end;

constructor TEpiReportCodeBook.Create(const Document: TEpiDocument);
begin
  FDocument := Document;
  FFileName := '';
  FPath := '';
end;

constructor TEpiReportCodeBook.Create(const AFileName: string);
begin
  FDocument := nil;
  FFileName := AFileName;
  FPath := '';
end;

constructor TEpiReportCodeBook.Create(const APath: string;
  const Recursive: boolean);
begin
  FDocument := nil;
  FPath := APath;
  FRecursive := Recursive;
  FFileName := '';
end;

procedure TEpiReportCodeBook.RunReport;
var
  FileList: TStringList;
  i: Integer;
begin
  if Assigned(FDocument) then
  begin
    DoReport(FDocument);
    Exit;
  end;

  if FFileName <> '' then
  begin
    FDocument := TEpiDocument.Create('en');
    FDocument.LoadFromFile(FFileName);
    DoReport(FDocument);
    FreeAndNil(FDocument);
  end;

  if FPath <> '' then
  begin
    if DirectoryExistsUTF8(FPath) then
    begin
      FileList := FindAllFiles(FPath, '*.epx;*.epz', FRecursive);

      for i := 0 to FileList.Count - 1 do
      begin
        FDocument := TEpiDocument.Create('en');
        FDocument.LoadFromFile(FileList[i]);
        DoReport(FDocument);
        FreeAndNil(FDocument);

        //
        DoLineText('');
      end;
    end;
  end;
end;

{ TEpiReportCodeBookHtml }

procedure TEpiReportCodeBookHtml.InternalInit;
begin
  FReportText := TStringList.Create;
  OnHeading := @Heading;
  OnLineText := @Line;
  OnSection := @Section;
  OnTableCell := @TableCell;
  OnTableFooter := @TableFooter;
  OnTableHeader := @TableHeader;
end;

procedure TEpiReportCodeBookHtml.AddLine(const Txt: string);
begin
  FReportText.Add(Txt);
end;

procedure TEpiReportCodeBookHtml.Section(Sender: TEpiReportBase;
  const Text: string);
begin
  AddLine('<h1>' + Text + '</h1>');
end;

procedure TEpiReportCodeBookHtml.Heading(Sender: TEpiReportBase;
  const Text: string);
begin
  AddLine('<h2>' + Text + '</h2>');
end;

procedure TEpiReportCodeBookHtml.Line(Sender: TEpiReportBase; const Text: string
  );
begin
//  if Text = '' then
//    AddLine('<br>')
//  else
    AddLine(Text + '<br>');
end;

procedure TEpiReportCodeBookHtml.TableHeader(Sender: TEpiReportBase;
  const Text: string);
begin

end;

procedure TEpiReportCodeBookHtml.TableFooter(Sender: TEpiReportBase;
  const Text: string);
begin

end;

procedure TEpiReportCodeBookHtml.TableCell(Sender: TEpiReportBase; const Col,
  Row: Integer; const Text: string);
begin

end;

function TEpiReportCodeBookHtml.GetReportText: string;
begin
  Result := FReportText.Text;
end;

procedure TEpiReportCodeBookHtml.RunReport;
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
  AddLine('<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">');
  AddLine('');

  // STYLESHEET
  AddLine('<STYLE>');
  AddLine('<!--');
  AddLine('  .body {color: black; background-color: white;  font-size: 1.0em; font-weight: normal}');
  AddLine('');
  AddLine('   p  {color: black   ;font-size: 1.0em ; font-family: proportional,monospace;font-weight: normal; margin: 0em }');
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
end;

constructor TEpiReportCodeBookHtml.Create(const Document: TEpiDocument);
begin
  inherited Create(Document);
  InternalInit;
end;

constructor TEpiReportCodeBookHtml.Create(const AFileName: string);
begin
  inherited Create(AFileName);
  InternalInit;
end;

constructor TEpiReportCodeBookHtml.Create(const APath: string;
  const Recursive: boolean);
begin
  inherited Create(APath, Recursive);
  InternalInit;
end;

destructor TEpiReportCodeBookHtml.Destroy;
begin
  FReportText.Free;
  inherited Destroy;
end;

end.

