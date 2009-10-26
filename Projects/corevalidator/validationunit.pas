unit validationunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, UEpiDataFile;

type

  TReportType = (rtEmpty, rtNote, rtWarning, rtError, rtFatal);

  TExitResult = (erOk, erAbortRecord, erAbortField, erAbortFile, erAbortProgram);

  { TReporter }

  TReporter = class
    FLines: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReportEvent(EventType: TReportType; Msg: String); overload;
    procedure ReportEvent(EventType: TReportType; Msg: String; Args: array of const); overload;
    procedure SaveToFile(Const aFileName: string);
  end;

  { TDatafileValidator }

  TDatafileValidator = class
  private
    function LoadDf(Fn: String; var Df: TEpiDataFile): boolean;
    function HandleExit(CurrentExit: TExitResult): TExitResult;
    function ValidateOriginalDatafile(Df: TEpiDataFile): TExitResult;
    function ValidateExportedDatafile(OrigDf, ExportDf: TEpiDataFile): TExitResult;
  public
    constructor Create;
    destructor Destroy; override;
    Procedure DirectoryHandler(FileIterator: TFileIterator);
    Procedure FileHandler(FileIterator: TFileIterator);
  end;

  // XML comparing.
  procedure XmlToRec(OrgField, NewField: TEpiField);
  procedure XmlToDta(OrgField, NewField: TEpiField);
  procedure XmlToOds(OrgField, NewField: TEpiField);
  procedure XmlToXls(OrgField, NewField: TEpiField);
  procedure XmlToTxt(OrgField, NewField: TEpiField);
  procedure XmlToDbf(OrgField, NewField: TEpiField);

  // Rec comparing.
  procedure RecToDta(OrgField, NewField: TEpiField);
  procedure RecToOds(OrgField, NewField: TEpiField);
  procedure RecToXls(OrgField, NewField: TEpiField);
  procedure RecToTxt(OrgField, NewField: TEpiField);
  procedure RecToDbf(OrgField, NewField: TEpiField);

  // Stata comparing.
  procedure DtaToOds(OrgField, NewField: TEpiField);
  procedure DtaToXls(OrgField, NewField: TEpiField);
  procedure DtaToTxt(OrgField, NewField: TEpiField);
  procedure DtaToDbf(OrgField, NewField: TEpiField);

  // OpenOffice comparing.
  procedure OdsToXls(OrgField, NewField: TEpiField);
  procedure OdsToTxt(OrgField, NewField: TEpiField);
  procedure OdsToDbf(OrgField, NewField: TEpiField);

  // Excel comparing.
  procedure XlsToTxt(OrgField, NewField: TEpiField);
  procedure XlsToDbf(OrgField, NewField: TEpiField);

  // Dbase
  procedure TxtToDbf(OrgField, NewField: TEpiField);

var
  Reporter: TReporter;

implementation

uses
  UImportExport, UStringUtils, UDataFileTypes, settings,
  UEpiDataGlobals;

type
  TCompareFields = procedure(FieldA, FieldB: TEpiField);

const
  //  Xml, Rec,       Stata,     Ods,       Xls,       Text,      DBase
  CompareFieldsArray: array[1..6] of array[1..7] of TCompareFields =
{xml}    ((nil, @XmlToRec, @XmlToDta, @XmlToOds, @XmlToXls, @XmlToTxt, @XmlToDbf),
{rec}     (nil, nil,       @RecToDta, @RecToOds, @RecToXls, @RecToTxt, @RecToDbf),
{dta}     (nil, nil,       nil,       @DtaToOds, @DtaToXls, @DtaToTxt, @DtaToDbf),
{ods}     (nil, nil,       nil,       nil,       @OdsToXls, @OdsToTxt, @OdsToDbf),
{xls}     (nil, nil,       nil,       nil,       nil,       @XlsToTxt, @XlsToDbf),
{txt}     (nil, nil,       nil,       nil,       nil,       nil,       @TxtToDbf));

procedure XmlToRec(OrgField, NewField: TEpiField);
begin

end;

procedure XmlToDta(OrgField, NewField: TEpiField);
begin

end;

procedure XmlToOds(OrgField, NewField: TEpiField);
begin

end;

procedure XmlToXls(OrgField, NewField: TEpiField);
begin

end;

procedure XmlToTxt(OrgField, NewField: TEpiField);
begin

end;

procedure XmlToDbf(OrgField, NewField: TEpiField);
begin

end;

procedure RecToDta(OrgField, NewField: TEpiField);
begin

end;

procedure RecToOds(OrgField, NewField: TEpiField);
begin

end;

procedure RecToXls(OrgField, NewField: TEpiField);
begin

end;

procedure RecToTxt(OrgField, NewField: TEpiField);
begin

end;

procedure RecToDbf(OrgField, NewField: TEpiField);
begin

end;

procedure DtaToOds(OrgField, NewField: TEpiField);
begin

end;

procedure DtaToXls(OrgField, NewField: TEpiField);
begin

end;

procedure DtaToTxt(OrgField, NewField: TEpiField);
begin

end;

procedure DtaToDbf(OrgField, NewField: TEpiField);
begin

end;

procedure OdsToXls(OrgField, NewField: TEpiField);
begin

end;

procedure OdsToTxt(OrgField, NewField: TEpiField);
begin

end;

procedure OdsToDbf(OrgField, NewField: TEpiField);
begin

end;

procedure XlsToTxt(OrgField, NewField: TEpiField);
begin

end;

procedure XlsToDbf(OrgField, NewField: TEpiField);
begin

end;

procedure TxtToDbf(OrgField, NewField: TEpiField);
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

function TDatafileValidator.HandleExit(CurrentExit: TExitResult): TExitResult;
begin
  Result := CurrentExit;
  if VSettings.ErrorsAreFatal then
  begin
    Result := erAbortProgram;
    Exit;
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
  FieldNo: LongInt;
  RecNo: LongInt;
  CmpTxt: string;
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
                   a: Field no. (index into the list above - 1 indexed)
                   b: Record no. (index into the number of records - 1 indexed)
                   c: Data that must be present in this field. For strings: encapsulate the data in quotation marks.
  }

  Fn := Df.FileName + ExtensionSeparator + 'val';
  if not FileExistsUTF8(Fn) then
  begin
    Reporter.ReportEvent(rtFatal, 'Validation file does not exist: %s', [Fn]);
    Result := HandleExit(erAbortFile);
    Exit;
  end;

  CtrlLines := TStringList.Create;
  CtrlLines.LoadFromFile(Fn);

  FieldCount := StrToInt(CtrlLines[0]);
  RecCount   := StrToInt(CtrlLines[1]);

  if Df.NumFields <> FieldCount then
  begin
    Reporter.ReportEvent(rtFatal, 'Original File Check: fieldcount = %d, expected = %d', [Df.NumFields, FieldCount]);
    Result := HandleExit(erAbortFile);
    Exit;
  end;

  if df.Size <> RecCount then
  begin
    Reporter.ReportEvent(rtError, 'Original File Check: recordcount = %d, expected = %d', [df.Size, RecCount]);
    Result := HandleExit(erAbortFile);
    Exit;
  end;

  FieldLines := nil;

  for i := 0 to FieldCount - 1 do
  with df[i] do
  begin
    SplitString(CtrlLines[2 + i], FieldLines, [',']);

    if FieldType <> TFieldType(StrToInt(FieldLines[0])) then
    begin
      Reporter.ReportEvent(rtError, 'Field "%s" has incorrect type. Was: %s, expected: %s',
        [FieldName, FieldTypeNames[Ord(FieldType)], FieldTypeNames[StrToInt(FieldLines[0])]]);
      Result := HandleExit(erAbortField);
      if Result >= erAbortFile then exit;
    end;

    if FieldLength <> StrToInt(FieldLines[1]) then
    begin
      Reporter.ReportEvent(rtError, 'Field "%s" has incorrect length. Was: %d, expected: %d',
        [FieldName, FieldLength, StrToInt(FieldLines[1])]);
      Result := HandleExit(erAbortField);
      if Result >= erAbortFile then exit;
    end;

    if FieldDecimals <> StrToInt(FieldLines[2]) then
    begin
      Reporter.ReportEvent(rtError, 'Field "%s" has incorrect decimals. Was: %d, expected: %d',
        [FieldName, FieldDecimals, StrToInt(FieldLines[2])]);
      Result := HandleExit(erAbortField);
      if Result >= erAbortFile then exit;
    end;

    if (Assigned(ValueLabelSet) xor (FieldLines[3] = '1')) then
    begin
      Reporter.ReportEvent(rtError, 'Field "%s" valuelabels are incorrect. Was: %p, expected: %p',
        [FieldName, ValueLabelSet, StrToInt(FieldLines[3])]);
      Result := HandleExit(erAbortField);
      if Result >= erAbortFile then exit;
    end;
  end;

  for i := FieldCount + 2 to CtrlLines.Count - 1 do
  begin
    SplitString(CtrlLines[i], FieldLines, [',']);
    FieldNo := StrToInt(FieldLines[0]) - 1;
    RecNo   := StrToInt(FieldLines[1]);
    CmpTxt  := FieldLines[2];

    if Df[FieldNo].AsString[RecNo] <> CmpTxt then
    begin
      Reporter.ReportEvent(rtError, 'Field "%s"(%d)[%d] does not contain correct data. Was: %s, expected: %s',
        [Df[FieldNo].FieldName, FieldNo, RecNo, Df[FieldNo].AsString[RecNo], CmpTxt]);
      Result := HandleExit(erAbortRecord);
      if Result >= erAbortFile then exit;
    end;
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
  begin
    Reporter.ReportEvent(rtFatal, 'Exported File Check - Field Count: Original = %d, Exported = %d', [OrigDf.NumFields, ExportDf.NumFields]);
    Result := HandleExit(erAbortFile);
    Exit;
  end;

  // 2: Number of obs. must be the same.
  if OrigDf.Size <> ExportDf.Size then
  begin
    Reporter.ReportEvent(rtFatal, 'Exported File Check - Record Count: Original = %d, Exported = %d', [OrigDf.Size, ExportDf.Size]);
    Result := HandleExit(erAbortFile);
    Exit;
  end;

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
  res: TExitResult;
const
  FormatEndings: array[1..8] of string =
    ('.recxml', '.rec', '.dta', '.ods', '.xls', '.csv', '.txt', '.dbf');
begin
  Ext := ExtractFileExt(Utf8ToAnsi(FileIterator.FileName));
  Fn := FileIterator.FileName;
  Df := nil;
  ImpExp := nil;

  Reporter.ReportEvent(rtEmpty, '');
  Reporter.ReportEvent(rtEmpty, '==================================================');
  Reporter.ReportEvent(rtEmpty, '= Working with file: %s', [Fn]);
  Reporter.ReportEvent(rtEmpty, '==================================================');

  try
    LoadDf(Fn, Df);

    res := ValidateOriginalDatafile(Df);

    if res = erAbortFile then
      Exit;
    if Res = erAbortProgram then
      Abort;

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

      Reporter.ReportEvent(rtNote, '-> Exporting to:  %s', [Fn]);
      ValidateExportedDatafile(Df, NewDf)
    end;

  finally
    if Assigned(ImpExp) then FreeAndNil(ImpExp);
    if Assigned(Df) then FreeAndNil(Df);
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

procedure TReporter.ReportEvent(EventType: TReportType; Msg: String);
begin
  ReportEvent(EventType, Msg, []);
end;

procedure TReporter.ReportEvent(EventType: TReportType; Msg: String;
  Args: array of const);

var
  TmpStr: String;
begin
  case EventType of
    rtNote:    TmpStr := 'NOTE:' + #9;
    rtWarning: TmpStr := 'WARNING:' + #9;
    rtError:   TmpStr := 'ERROR:' + #9;
    rtFatal:   TmpStr := 'FATAL:' + #9;
  else
    TmpStr := #9;
  end;
  TmpStr := TmpStr + Format(Msg, Args);

  if not VSettings.NoOutput then
    WriteLn(TmpStr);

  FLines.Add(TmpStr);
end;

procedure TReporter.SaveToFile(const aFileName: string);
begin
  if aFileName = '' then exit;
  FLines.SaveToFile(aFileName);
end;

end.

