unit epireport_filelist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_base, epireport_generator_base;

const
  SEpiReportFileListNoFileList = 'No FileList Assigned';

type
  EEpiReportFileListException = class(Exception);

  { TEpiReportFileList }

  TEpiReportFileList = class(TEpiReportBase)
  private
    FFileList: TStringList;
  protected
    procedure DoSanityCheck; override;
  public
    procedure   RunReport; override;
    property    FileList: TStringList read FFileList write FFileList;
  end;


implementation

uses
  epidocument,
  FileUtil;

{ TEpiReportFileList }

procedure TEpiReportFileList.DoSanityCheck;
begin
  inherited DoSanityCheck;

  if not Assigned(FFileList) then
    DoError(EEpiReportFileListException, SEpiReportFileListNoFileList);
end;

procedure TEpiReportFileList.RunReport;
var
  i: Integer;
  Doc: TEpiDocument;
begin
  if (FFileList.Count > 0) and (not Assigned(FFileList.Objects[0])) then
  for i := 0 to FFileList.Count -1 do
  begin
    Doc := TEpiDocument.Create('');
    Doc.LoadFromFile(FFileList[i]);
    FFileList.Objects[i] := Doc;
  end;

  DoTableHeader('', 5, FFileList.Count + 1);
  DoTableCell(0, 0, 'File No');
  DoTableCell(1, 0, 'Filename');
  DoTableCell(2, 0, 'Project Title');
  DoTableCell(3, 0, 'Created');
  DoTableCell(4, 0, 'Last Edited');

  for i := 0 to FFileList.Count - 1 do
  begin
    Doc := TEpiDocument(FFileList.Objects[i]);
    DoTableCell(0, i + 1, IntToStr(i + 1));
    DoTableCell(1, i + 1, Doc.Study.Title.Text);
    DoTableCell(2, i + 1, Doc.Study.Title.Text);
    DoTableCell(3, i + 1, DateToStr(Doc.Study.Created));
    DoTableCell(4, i + 1, DateToStr(Doc.Study.ModifiedDate));
  end;
  DoTableFooter('');
end;

end.

