unit validationunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, UEpiDataFile;

type

  TReportType = (rtNote, rtWarning, rtError, rtFatal);

  TExitResult = (erOk, erAbortField, erAbortFile, erAbortProgram);

  { TReporter }

  TReporter = class
    FLines: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    function ReportEvent(EventType: TReportType; Msg: String): TExitResult; overload;
    function ReportEvent(EventType: TReportType; Msg: String; Args: array of const): TExitResult; overload;
  end;

  { TDatafileValidator }

  TDatafileValidator = class
  private
    function  LoadDf(Fn: String; var Df: TEpiDataFile): boolean;
    function ValidateOriginalDatafile(Df: TEpiDataFile): TExitResult;
    function ValidateExportedDatafile(OrigDf, ExportDf: TEpiDataFile): TExitResult;
  public
    constructor Create;
    destructor Destroy; override;
    Procedure DirectoryHandler(FileIterator: TFileIterator);
    Procedure FileHandler(FileIterator: TFileIterator);
  end;

  // XML comparing.
  procedure XmlVsRec(OrgField, NewField: TEpiField);
  procedure XmlVsDta(OrgField, NewField: TEpiField);
  procedure XmlVsOds(OrgField, NewField: TEpiField);
  procedure XmlVsXls(OrgField, NewField: TEpiField);
  procedure XmlVsTxt(OrgField, NewField: TEpiField);
  procedure XmlVsDbf(OrgField, NewField: TEpiField);

  // Rec comparing.
  procedure RecVsDta(OrgField, NewField: TEpiField);
  procedure RecVsOds(OrgField, NewField: TEpiField);
  procedure RecVsXls(OrgField, NewField: TEpiField);
  procedure RecVsTxt(OrgField, NewField: TEpiField);
  procedure RecVsDbf(OrgField, NewField: TEpiField);

  // Stata comparing.
  procedure DtaVsOds(OrgField, NewField: TEpiField);
  procedure DtaVsXls(OrgField, NewField: TEpiField);
  procedure DtaVsTxt(OrgField, NewField: TEpiField);
  procedure DtaVsDbf(OrgField, NewField: TEpiField);

  // OpenOffice comparing.
  procedure OdsVsXls(OrgField, NewField: TEpiField);
  procedure OdsVsTxt(OrgField, NewField: TEpiField);
  procedure OdsVsDbf(OrgField, NewField: TEpiField);

  // Excel comparing.
  procedure XlsVsTxt(OrgField, NewField: TEpiField);
  procedure XlsVsDbf(OrgField, NewField: TEpiField);

  // Dbase
  procedure TxtVsDbf(OrgField, NewField: TEpiField);

var
  Reporter: TReporter;

implementation

uses
  UImportExport, UStringUtils, UDataFileTypes;

type
  TCompareFields = procedure(FieldA, FieldB: TEpiField);

const
  //  Xml, Rec,       Stata,     Ods,       Xls,       Text,      DBase
  CompareFieldsArray: array[1..6] of array[1..7] of TCompareFields =
{xml}    ((nil, @XmlVsRec, @XmlVsDta, @XmlVsOds, @XmlVsXls, @XmlVsTxt, @XmlVsDbf),
{rec}     (nil, nil,       @RecVsDta, @RecVsOds, @RecVsXls, @RecVsTxt, @RecVsDbf),
{dta}     (nil, nil,       nil,       @DtaVsOds, @DtaVsXls, @DtaVsTxt, @DtaVsDbf),
{ods}     (nil, nil,       nil,       nil,       @OdsVsXls, @OdsVsTxt, @OdsVsDbf),
{xls}     (nil, nil,       nil,       nil,       nil,       @XlsVsTxt, @XlsVsDbf),
{txt}     (nil, nil,       nil,       nil,       nil,       nil,       @TxtVsDbf));

procedure XmlVsRec(OrgField, NewField: TEpiField);
begin

end;

procedure XmlVsDta(OrgField, NewField: TEpiField);
begin

end;

procedure XmlVsOds(OrgField, NewField: TEpiField);
begin

end;

procedure XmlVsXls(OrgField, NewField: TEpiField);
begin

end;

procedure XmlVsTxt(OrgField, NewField: TEpiField);
begin

end;

procedure XmlVsDbf(OrgField, NewField: TEpiField);
begin

end;

procedure RecVsDta(OrgField, NewField: TEpiField);
begin

end;

procedure RecVsOds(OrgField, NewField: TEpiField);
begin

end;

procedure RecVsXls(OrgField, NewField: TEpiField);
begin

end;

procedure RecVsTxt(OrgField, NewField: TEpiField);
begin

end;

procedure RecVsDbf(OrgField, NewField: TEpiField);
begin

end;

procedure DtaVsOds(OrgField, NewField: TEpiField);
begin

end;

procedure DtaVsXls(OrgField, NewField: TEpiField);
begin

end;

procedure DtaVsTxt(OrgField, NewField: TEpiField);
begin

end;

procedure DtaVsDbf(OrgField, NewField: TEpiField);
begin

end;

procedure OdsVsXls(OrgField, NewField: TEpiField);
begin

end;

procedure OdsVsTxt(OrgField, NewField: TEpiField);
begin

end;

procedure OdsVsDbf(OrgField, NewField: TEpiField);
begin

end;

procedure XlsVsTxt(OrgField, NewField: TEpiField);
begin

end;

procedure XlsVsDbf(OrgField, NewField: TEpiField);
begin

end;

procedure TxtVsDbf(OrgField, NewField: TEpiField);
begin

end;


{ TDatafileValidator }

function TDatafileValidator.LoadDf(Fn: String; var Df: TEpiDataFile): boolean;
var
  ImpExp: TEpiImportExport;
  Ext: String;
begin
  ImpExp := nil;

  try
    ImpExp := TEpiImportExport.Create;
    Ext := ExtractFileExt(Fn);

    result := true;
    Case Ext of
      '.recxml', '.rec':
        begin
          if not Assigned(Df) then
            Df := TEpiDataFile.Create;
          DF.Open(Fn);
        end;
      '.dta':
        ImpExp.ImportStata(Fn, Df);
      '.ods':
        ImpExp.ImportSpreadSheet(Fn, Df);
      '.csv', '.txt':
        ImpExp.ImportTXT(Fn, Df, nil);
      '.dbf':
        ImpExp.ImportDBase(Fn, Df);
    else
      Result := false;
    end;
  finally
    if Assigned(ImpExp) then FreeAndNil(ImpExp);
  end;
end;

function TDatafileValidator.ValidateOriginalDatafile(Df: TEpiDataFile): TExitResult;
var
  Fn: String;
  CtrlLines: TStringList;
  i: Integer;
  FieldLines: TStrings;
  FieldCount: LongInt;
  RecCount: LongInt;
begin
  // Validating the original file is done using a <df-filename>.import file, organised in this manner:

  {
   Line          1: Number [n] of fields (includes question lines in EpiData files).
   Line          2: Number [r] of records (includes records marked for deletion).
   Line      3 -> : n lines containing in comma seperated format:
                   a: Field type (as a number according to TFieldType;
                   b: Field length
                   c: Field decimals
                   d: Does this field have a value label (0 = no, 1 = yes)
   Line n+3 -> EOF: any number of lines in comma seperated format:
                   a: Field no. (index into the list above - 0 indexed)
                   b: Record no. (index into the number of records - 1 indexed)
                   c: Data that must be present in this field. For strings encapsulate the data in quotation marks.
  }
  Fn := Df.FileName + ExtensionSeparator + 'import';
  if not FileExistsUTF8(Fn) then
  begin
    // TODO : Report error.
  end;

  CtrlLines := TStringList.Create;
  CtrlLines.LoadFromFile(Fn);

  FieldCount := StrToInt(CtrlLines[0]);
  RecCount   := StrToInt(CtrlLines[1]);

  if Df.NumFields <> FieldCount then
    Reporter.ReportEvent(rtError, 'Original File Check: fieldcount = %d, expected = %d', [Df.NumFields, FieldCount]);

  if df.Size <> RecCount then
    Reporter.ReportEvent(rtError, 'Original File Check: recordcount = %d, expected = %d', [df.Size, RecCount]);

  for i := 0 to FieldCount - 1 do
  with df[i] do
  begin
    SplitString(CtrlLines[2 + i], FieldLines, [',']);

    ApplicationName;
    if FieldType <> TFieldType(StrToInt(FieldLines[0])) then
      ; // TODO : Report errors.

    if FieldLength <> StrToInt(FieldLines[1]) then
      ; // TODO : Report errors.

    if FieldDecimals <> StrToInt(FieldLines[2]) then
      ; // TODO : Report errors.

    if (Assigned(ValueLabelSet) xor (FieldLines[3] = '1')) then
      ; // TODO : Report errors.
  end;
end;

function TDatafileValidator.ValidateExportedDatafile(OrigDf,
  ExportDf: TEpiDataFile): TExitResult;
var
  i: Integer;
  CmpFunc: TCompareFields;
begin

  // Initial tests:
  // 1: Number of datafields must always be the same.
  if OrigDf.NumDataFields <> ExportDf.NumDataFields then
    ; // TODO : Report errors.


  // 2: Number of obs. must be the same.
  if OrigDf.Size <> ExportDf.Size then
    ; // TODO : Report errors.

  if OrigDf.DatafileType < ExportDf.DatafileType then
    CmpFunc := CompareFieldsArray[Ord(OrigDf.DatafileType)][Ord(ExportDf.DatafileType)]
  else
    CmpFunc := CompareFieldsArray[Ord(ExportDf.DatafileType)][Ord(OrigDf.DatafileType)];

  for i := 0 to OrigDf.NumDataFields -1 do
    CmpFunc(OrigDf[i], ExportDf[i]);

end;

constructor TDatafileValidator.Create;
begin
  //
end;

destructor TDatafileValidator.Destroy;
begin
  inherited Destroy;
end;

procedure TDatafileValidator.DirectoryHandler(FileIterator: TFileIterator);
begin
  // So far do nothing (it recurses automatically into the directory).
end;

procedure TDatafileValidator.FileHandler(FileIterator: TFileIterator);
var
  Ext: String;
  ImpExp: TEpiImportExport;
  Fn: String;
  Df: TEpiDataFile;
  i: Integer;
  NewDf: TEpiDataFile;
const
  FormatEndings: array[1..8] of string =
    ('.recxml', '.rec', '.dta', '.ods', '.xls', '.csv', '.txt', '.dbf');
begin
  Ext := ExtractFileExt(Utf8ToAnsi(FileIterator.FileName));
  Fn := FileIterator.FileName;
  Df := nil;

  try
    LoadDf(Fn, Df);

    ValidateOriginalDatafile(Df);

    for i := Low(FormatEndings) to High(FormatEndings) do
    begin
      if Ext = FormatEndings[i] then continue;

      Fn := ChangeFileExt(Fn, FormatEndings[i]);

      Case FormatEndings[i] of
        '.recxml', '.rec':
          begin
            NewDf := Df.Clone();
            NewDf.Save(Fn);
            FreeAndNil(NewDf);
          end;
        '.dta':
          ImpExp.ExportStata(Fn, Df, @ExportStata10);
        '.ods':
          ImpExp.ExportSpreadSheet(Fn, Df, @ExportOpenDocument);
        '.xls':
          ImpExp.ExportSpreadSheet(Fn, Df, @ExportExcel8);
        '.csv':
          ImpExp.ExportTXT(Fn, Df, nil);
        '.dbf':
          ImpExp.ExportDBase(Fn, Df);
      end;

      LoadDf(Fn, NewDf);

      ValidateExportedDatafile(Df, NewDf)

    end;

  finally
    if Assigned(ImpExp) then FreeAndNil(ImpExp);
  end;
end;

{ TReporter }

constructor TReporter.Create;
begin
  FLines := TStringList.Create;
end;

destructor TReporter.Destroy;
begin
  inherited Destroy;
end;

function TReporter.ReportEvent(EventType: TReportType; Msg: String): TExitResult;
begin
  ReportEvent(EventType, Msg, []);
end;

function TReporter.ReportEvent(EventType: TReportType; Msg: String;
  Args: array of const): TExitResult;

var
  TmpStr: String;
begin
  case EventType of
    rtNote:    TmpStr := 'NOTE:' + #9;
    rtWarning: TmpStr := 'WARNING:' + #9;
    rtError:   TmpStr := 'ERROR:' + #9;
    rtFatal:   TmpStr := 'FATAL:' + #9;
  end;
  TmpStr := TmpStr + Format(Msg, Args);

  if EventType = rtFatal then
  begin
    abort;

  end;
  FLines.Add(TmpStr);
end;


finalization
begin
  Reporter.FLines.SaveToFile('');
end;

end.

