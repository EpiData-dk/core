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
    constructor Create(Const Document: TEpiDocument);
    constructor Create(Const AFileName: string); overload;
    constructor Create(Const APath: string; Const Recursive: boolean); overload;
    procedure RunReport; override;
  end;

implementation

uses
  FileUtil;

{ TEpiReportCodeBook }

procedure TEpiReportCodeBook.DoReport(const Document: TEpiDocument);
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
  DoLineText('Generated: ' + FormatDateTime('c', Now));
  DoLineText('');
//  DoLineText('FileName: ' + Document.ProjectSettings.)
  DoLineText('Title: ' + Document.Study.Title.Text);
  DoLineText('Create: ' + FormatDateTime('c', Document.Study.Created));
  DoLineText('Modifed: ' + FormatDateTime('c', Document.Study.ModifiedDate));
  DoLineText('');
  DoLineText('Fields: ' + IntToStr(Document.DataFiles[0].Fields.Count));
  DoLineText('Records: ' + IntToStr(Document.DataFiles[0].Size));
  DoLineText('Deleted: ' + IntToStr(Document.DataFiles[0].DeletedCount));
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

end.

