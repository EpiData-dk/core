unit validationunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, UEpiDataFile;

type

  { TDatafileValidator }

  TDatafileValidator = class
  private
    function  LoadDf(Fn: String; var Df: TEpiDataFile): boolean;
    procedure ValidateOriginalDatafile(Df: TEpiDataFile);
    procedure ValidateExportedDatafile(OrigDf, ExportDf: TEpiDataFile);
  public
    constructor Create;
    destructor Destroy; override;
    Procedure DirectoryHandler(FileIterator: TFileIterator);
    Procedure FileHandler(FileIterator: TFileIterator);
  end;

  // XML comparing.
  procedure XmlVsRec(A, B: TEpiField);
  procedure XmlVsDta(A, B: TEpiField);
  procedure XmlVsOds(A, B: TEpiField);
  procedure XmlVsXls(A, B: TEpiField);
  procedure XmlVsTxt(A, B: TEpiField);
  procedure XmlVsDbf(A, B: TEpiField);

  // Rec comparing.
  procedure RecVsDta(A, B: TEpiField);
  procedure RecVsOds(A, B: TEpiField);
  procedure RecVsXls(A, B: TEpiField);
  procedure RecVsTxt(A, B: TEpiField);
  procedure RecVsDbf(A, B: TEpiField);

  // Stata comparing.
  procedure DtaVsOds(A, B: TEpiField);
  procedure DtaVsXls(A, B: TEpiField);
  procedure DtaVsTxt(A, B: TEpiField);
  procedure DtaVsDbf(A, B: TEpiField);

  // OpenOffice comparing.
  procedure OdsVsXls(A, B: TEpiField);
  procedure OdsVsTxt(A, B: TEpiField);
  procedure OdsVsDbf(A, B: TEpiField);

  // Excel comparing.
  procedure XlsVsTxt(A, B: TEpiField);
  procedure XlsVsDbf(A, B: TEpiField);

  // Dbase
  procedure TxtVsDbf(A, B: TEpiField);

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

procedure XmlVsRec(A, B: TEpiField);
begin

end;

procedure XmlVsDta(A, B: TEpiField);
begin

end;

procedure XmlVsOds(A, B: TEpiField);
begin

end;

procedure XmlVsXls(A, B: TEpiField);
begin

end;

procedure XmlVsTxt(A, B: TEpiField);
begin

end;

procedure XmlVsDbf(A, B: TEpiField);
begin

end;

procedure RecVsDta(A, B: TEpiField);
begin

end;

procedure RecVsOds(A, B: TEpiField);
begin

end;

procedure RecVsXls(A, B: TEpiField);
begin

end;

procedure RecVsTxt(A, B: TEpiField);
begin

end;

procedure RecVsDbf(A, B: TEpiField);
begin

end;

procedure DtaVsOds(A, B: TEpiField);
begin

end;

procedure DtaVsXls(A, B: TEpiField);
begin

end;

procedure DtaVsTxt(A, B: TEpiField);
begin

end;

procedure DtaVsDbf(A, B: TEpiField);
begin

end;

procedure OdsVsXls(A, B: TEpiField);
begin

end;

procedure OdsVsTxt(A, B: TEpiField);
begin

end;

procedure OdsVsDbf(A, B: TEpiField);
begin

end;

procedure XlsVsTxt(A, B: TEpiField);
begin

end;

procedure XlsVsDbf(A, B: TEpiField);
begin

end;

procedure TxtVsDbf(A, B: TEpiField);
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
      'recxml', 'rec':
        begin
          if not Assigned(Df) then
            Df := TEpiDataFile.Create;
          DF.Open(Fn);
        end;
      'dta':
        ImpExp.ImportStata(Fn, Df);
      'ods':
        ImpExp.ImportSpreadSheet(Fn, Df);
      'csv', 'txt':
        ImpExp.ImportTXT(Fn, Df, nil);
      'dbf':
        ImpExp.ImportDBase(Fn, Df);
    else
      Result := false;
    end;
  finally
    if Assigned(ImpExp) then FreeAndNil(ImpExp);
  end;
end;

procedure TDatafileValidator.ValidateOriginalDatafile(Df: TEpiDataFile);
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

  CtrlLines := TStringList.Create;
  CtrlLines.LoadFromFile(Fn);

  FieldCount := StrToInt(CtrlLines[0]);
  RecCount   := StrToInt(CtrlLines[1]);

  if Df.NumFields <> FieldCount then
    ;   // TODO : Report errors.

  if df.Size <> RecCount then
    ;   // TODO : Report errors.

  for i := 0 to FieldCount - 1 do
  with df[i] do
  begin
    SplitString(CtrlLines[2 + i], FieldLines, [',']);

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

procedure TDatafileValidator.ValidateExportedDatafile(OrigDf,
  ExportDf: TEpiDataFile);
var
  i: Integer;
begin

  // Initial tests:
  // 1: Number of datafields must always be the same.
  if OrigDf.NumDataFields <> ExportDf.NumDataFields then
    ; // TODO : Report errors.


  // 2: Number of obs. must be the same.
  if OrigDf.Size <> ExportDf.Size then
    ; // TODO : Report errors.


  for i := 0 to OrigDf.NumDataFields -1 do
  begin
//    CompareFieldTypes(OrigDf[i], ExportDf[i])

  end;
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
    ('recxml', 'rec', 'dta', 'ods', 'xls', 'csv', 'txt', 'dbf');
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
        'recxml', 'rec':
          begin
            NewDf := Df.Clone();
            NewDf.Save(Fn);
            FreeAndNil(NewDf);
          end;
        'dta':
          ImpExp.ExportStata(Fn, Df, @ExportStata10);
        'ods':
          ImpExp.ExportSpreadSheet(Fn, Df, @ExportOpenDocument);
        'xls':
          ImpExp.ExportSpreadSheet(Fn, Df, @ExportExcel8);
        'csv':
          ImpExp.ExportTXT(Fn, Df, nil);
        'dbf':
          ImpExp.ExportDBase(Fn, Df);
      end;

      LoadDf(Fn, NewDf);

      ValidateExportedDatafile(Df, NewDf)

    end;

  finally
    if Assigned(ImpExp) then FreeAndNil(ImpExp);
  end;
end;

end.

