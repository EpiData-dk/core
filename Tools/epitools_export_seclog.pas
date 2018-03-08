unit epitools_export_seclog;

{$codepage utf8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles, epivaluelabels, epiopenfile;

type

  { TEpiTool_ExportSecurityLog }

  TEpiTool_ExportSecurityLog = class
  private
    ExportID: integer;
    FDocumentFileClass: TEpiDocumentFileClass;
    FErrorMessage: UTF8String;
    function ExportedDatafilePackFunction(Sender: TEpiDataFile; Index: Integer): boolean;
    function OriginalDatafilePackFunction(Sender: TEpiDataFile; Index: Integer
      ): boolean;
    function ReportError(Const Msg: UTF8String): boolean;
  public
    constructor Create; virtual;
    function ExportSecLog(Document: TEpiDocument; ExportAfterNoDays: Integer; DeleteLog: boolean; ExportFilename: UTF8String): boolean; virtual;
    property DocumentFileClass: TEpiDocumentFileClass read FDocumentFileClass write FDocumentFileClass;
    property ErrorMessage: UTF8String read FErrorMessage;
  end;

implementation

uses
  LazFileUtils, epiglobals, episecuritylog, epidatafilestypes;

{ TEpiTool_ExportSecurityLog }

function TEpiTool_ExportSecurityLog.ReportError(const Msg: UTF8String): boolean;
begin
  FErrorMessage := Msg;
  result := false;
end;

function TEpiTool_ExportSecurityLog.ExportedDatafilePackFunction(Sender: TEpiDataFile;
  Index: Integer): boolean;
var
  ID: EpiInteger;
begin
  ID := Sender.Fields.FieldByName['id'].AsInteger[Index];
  // In the export datasets, drop all obs after the ExportID AND drop ID = 0 (since it should only exist in the original dataset)
  result := (ID > ExportID) or (ID = 0);
end;

function TEpiTool_ExportSecurityLog.OriginalDatafilePackFunction(
  Sender: TEpiDataFile; Index: Integer): boolean;
var
  ID: Int64;
begin
  ID := Sender.Fields.FieldByName['id'].AsInteger[Index];
  // In the origianl datasets, drop all obs before the ExportID, but keep the first log entry ID = 0
  result := (ID <= ExportID) and (ID <> 0);
end;

constructor TEpiTool_ExportSecurityLog.Create;
begin
  FDocumentFileClass := nil;
end;

function TEpiTool_ExportSecurityLog.ExportSecLog(Document: TEpiDocument;
  ExportAfterNoDays: Integer; DeleteLog: boolean; ExportFilename: UTF8String
  ): boolean;
var
  NewDoc: TEpiDocument;
  i: Integer;
  DF: TEpiDataFile;
  VLS: TEpiValueLabelSet;
  DFsl: TEpiSecurityDatafile;
  IDvar, Datevar: TEpiField;
  DocFile: TEpiDocumentFile;
begin
  result := false;
  FErrorMessage := '';

  if (not Assigned(Document)) then exit(ReportError('Document Not Assigned'));
  if (not Document.Admin.Initialized) then exit(ReportError('Extended Access not initialized'));

  if (FileExistsUTF8(ExportFilename)) and
     (FileIsReadOnlyUTF8(ExportFilename))
  then
    Exit(ReportError('Export file is not writeable'));

  if Assigned(FDocumentFileClass) then
    DocFile := FDocumentFileClass.Create
  else
    DocFile := TEpiDocumentFile.Create;

  NewDoc := DocFile.CreateClonedDocument(Document);

  // Delete all datafiles that is NOT part of logging.
  for i := NewDoc.DataFiles.Count - 1 downto 0 do
    begin
      DF := NewDoc.DataFiles[i];
      if (not DF.ProtectedItem) then
        DF.Free;
    end;

  // Do the same with valuelabel sets
  for i := NewDoc.ValueLabelSets.Count - 1 downto 0 do
    begin
      VLS := NewDoc.ValueLabelSets[i];
      if (not VLS.ProtectedItem) then
        VLS.Free;
    end;

  // Because the related datafiles of security log, does NOT contain
  // and date/time information, collect the ID's that shold be exported instead.
  DFsl := NewDoc.Logger.SecurityLog;
  IDvar := DFsl.ID;
  Datevar := DFsl.Date;

  ExportID := -1;
  i := DFsl.Size - 1;
  // i >= 1 here, because we NEVER export ID = 0 (design choice)
  while (i >= 1) do
    begin
      if ((Now - ExportAfterNoDays) > Datevar.AsDate[i]) then
        begin
          ExportID := IDvar.AsInteger[i];
          break;
        end;
      Dec(i);
    end;

  if (ExportID < 0) then
    Exit(ReportError('Nothing to export!'));

  for DF in NewDoc.DataFiles do
    DF.Pack(@ExportedDatafilePackFunction);

  result := DocFile.SaveFile(ExportFilename);
  Docfile.Free;

  Document.Logger.LogExportSecurityLog(ExportFilename, Document.Logger.SecurityLog.ID.AsInteger[1], ExportID);
  if (not DeleteLog) then
    Exit(true);

  // Drop existing data
  Document.Logger.KeyLog.Pack(@OriginalDatafilePackFunction);
  Document.Logger.DataLog.Pack(@OriginalDatafilePackFunction);
  Document.Logger.SecurityLog.Pack(@OriginalDatafilePackFunction);
end;

end.

