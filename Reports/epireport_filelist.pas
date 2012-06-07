unit epireport_filelist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_htmlgenerator;

{
type

  { TEpiReportFileList }

  TEpiReportFileList = class(TEpiReportBase)
  private
    FFileList: TStringList;
  public
    constructor Create(FileList: TStringList);
    procedure   RunReport; override;
  end;

  { TEpiReportFileListHtml }

  TEpiReportFileListHtml = class(TEpiReportFileList)
  private
    FHtmlGenerator: TEpiReportHTMLGenerator;
    FCompleteHtml: Boolean;
  protected
    function GetReportText: string; override;
  public
    constructor Create(FileList: TStringList;
      Const CompleteHtml: boolean = false);
    destructor Destroy; override;
    procedure RunReport; override;
    property HtmlGenerator: TEpiReportHTMLGenerator read FHtmlGenerator;
  end;            }


implementation
{
uses
  epidocument,
  FileUtil;

{ TEpiReportFileList }

constructor TEpiReportFileList.Create(FileList: TStringList);
var
  Doc: TEpiDocument;
  i: Integer;
begin
  if not Assigned(FileList) then
    Raise Exception.Create('TEpiReportFileList: Filelist not assigned!');
  FFileList := FileList;

  if (FFileList.Count > 0) and (not Assigned(FFileList.Objects[0])) then
  for i := 0 to FFileList.Count -1 do
  begin
    Doc := TEpiDocument.Create('');
    Doc.LoadFromFile(FFileList[i]);
    FFileList.Objects[i] := Doc;
  end;
end;

procedure TEpiReportFileList.RunReport;
var
  i: Integer;
begin
  DoTableHeader('', 2, FFileList.Count + 1);
  DoTableCell(0, 0, 'Filename');
  DoTableCell(1, 0, 'Project Title');

  for i := 0 to FFileList.Count - 1 do
  begin
    DoTableCell(0, i + 1, ExtractFileName(FFileList[i]));
    DoTableCell(1, i + 1, TEpiDocument(FFileList.Objects[i]).Study.Title.Text);
  end;
  DoTableFooter('');
end;

{ TEpiReportFileListHtml }

function TEpiReportFileListHtml.GetReportText: string;
begin
  Result := FHtmlGenerator.GetReportText;
end;

constructor TEpiReportFileListHtml.Create(FileList: TStringList;
  const CompleteHtml: boolean);
begin
  inherited Create(FileList);
  FCompleteHtml := CompleteHtml;
  FHtmlGenerator := TEpiReportHTMLGenerator.Create(Self);
end;

destructor TEpiReportFileListHtml.Destroy;
begin
  FHtmlGenerator.Free;
  inherited Destroy;
end;

procedure TEpiReportFileListHtml.RunReport;
begin
  if FCompleteHtml then
    FHtmlGenerator.InitHtml('List of files');

  inherited RunReport;

  if FCompleteHtml then
    FHtmlGenerator.CloseHtml;
end;
}
end.

