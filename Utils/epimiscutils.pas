unit epimiscutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes;

const
  EpiTypeCastArray: array[TEpiFieldType, TEpiFieldType] of boolean =
      // Cast To Types
//                ftBoolean, ftInteger, ftAutoInc, ftFloat, ftDMYDate, ftMDYDate, ftYMDDate, ftDMYToday, ftMDYToday, ftYMDToday, ftTime, ftTimeNow, ftString, ftUpperString
{C} {ftBoolean} ((true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{a} {ftInteger}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{s} {ftAutoInc}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{t} {ftFloat}    (true,      true,      false,     true,    false,     false,     false,     false,      false,      false,      true,   false,     true,     true),
    {ftDMYDate}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{F} {ftMDYDate}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{r} {ftYMDDate}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{o} {ftDMYToday} (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
{m} {ftMDYToday} (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
    {ftYMDToday} (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,     true),
    {ftTime}     (true,      true,      false,     true,    false,     false,     false,     false,      false,      false,      true,   false,     true,     true),
    {ftTimeNow}  (true,      true,      false,     true,    false,     false,     false,     false,      false,      false,      true,   false,     true,     true),
    {ftString}   (true,      false,     false,     false,   false,     false,     false,     false,      false,      false,      false,  false,     true,     true),
  {ftUpperString}(true,      false,     false,     false,   false,     false,     false,     false,      false,      false,      false,  false,     true,     true));


  const EpiTypeNames: array[TEpiFieldType] of string =
    ('Boolean',
     'Integer', 'Auto Increment', 'Float',
     'Date (DMY)', 'Date (MDY)', 'Date (YMD)',
     'Date (DMY - Auto)', 'Date (MDY - Auto)', 'Date (YMD - Auto)',
     'Time', 'Time (Auto)',
     'String', 'Uppercase'
    );

  const EpiTypeNamesShort: array[TEpiFieldType] of string =
    ('B',
     'I', 'Ia', 'F',
     'DMY', 'MDY', 'YMD',
     'DMYa', 'MDYa', 'YMDa',
     'T', 'Ta',
     'S', 'U'
    );

type
  TEpiDialogFilter = (dfEPX, dfEPZ, dfREC, dfText, dfODS, dfXLS, dfDTA, dfDBF, dfSPSS, dfSAS, dfDDI, dfCollection, dfAll);
  TEpiDialogFilters = set of TEpiDialogFilter;

const
  dfImport = [dfEPX, dfEPZ, dfREC, dfDTA];
  dfExport = [dfDTA];


  // File dialog filter functions.
  function GetEpiDialogFilter(DialogFilters: TEpiDialogFilters): string;
  function GetEpiDialogFilterExt(DialogFilters: TEpiDialogFilters): string;

  procedure StreamToZipFile(Const St: TStream; Const ZipFileName: string);
  procedure ZipFileToStream(St: TStream;   Const ZipFileName: string);

  // Encryption Utils
  function StrToSHA1Base64(const S: string): string;

  // Returns true if Name is a field name defined in EpiGlobals.pas
  function IsReservedEpiFieldName(Const Name: string): boolean;


implementation

uses
  zipper, FileUtil, DCPsha1, DCPbase64, epiglobals;

type
  TEpiDialogFilterPair = record
    FilterName: string;
    FilterExt:  string;
  end;
  PEpiDialogFilterPair = ^TEpiDialogFilterPair;

const
  EpiDialogFilterCollection: TEpiDialogFilterPair = (
    FilterName: 'Supported files';
    FilterExt:  '';
  );

  EpiDialogFilterEPX: TEpiDialogFilterPair = (
    FilterName: 'EpiData XML Data file (*.epx)';
    FilterExt:  '*.epx';
  );

  EpiDialogFilterEPZ: TEpiDialogFilterPair = (
    FilterName: 'EpiData XML Zipped Data file (*.epz)';
    FilterExt:  '*.epz';
  );

  EpiDialogFilterREC: TEpiDialogFilterPair = (
    FilterName: 'EpiData data file (*.rec)';
    FilterExt:  '*.rec';
  );

  EpiDialogFilterText: TEpiDialogFilterPair = (
    FilterName: 'Text file (*.txt,*.csv)';
    FilterExt:  '*.csv;*.txt';
  );

  EpiDialogFilterODS: TEpiDialogFilterPair = (
    FilterName: 'Open Document Spreadsheet (*.ods)';
    FilterExt:  '*.ods';
  );

  EpiDialogFilterXLS: TEpiDialogFilterPair = (
    FilterName: 'Excel Spreadsheet (*.xls)';
    FilterExt:  '*.xls';
  );

  EpiDialogFilterDTA: TEpiDialogFilterPair = (
    FilterName: 'Stata file (*.dta)';
    FilterExt:  '*.dta';
  );

  EpiDialogFilterDBF: TEpiDialogFilterPair = (
    FilterName: 'dBase file (*.dbf)';
    FilterExt:  '*.dbf';
  );

  EpiDialogFilterSPSS: TEpiDialogFilterPair = (
    FilterName: 'SPSS Command File (*.sps)';
    FilterExt:  '*.sps';
  );

  EpiDialogFilterSAS: TEpiDialogFilterPair = (
    FilterName: 'SAS Command File (*.sas)';
    FilterExt:  '*.sas';
  );

  EpiDialogFilterDDI: TEpiDialogFilterPair = (
    FilterName: 'DDI XML File (*.xml)';
    FilterExt:  '*.xml';
  );

  EpiDialogFilterAll: TEpiDialogFilterPair = (
    FilterName: 'Show All (*.*)';
    FilterExt:  '*.*';
  );

  EpiDialogFilters: array[TEpiDialogFilter] of PEpiDialogFilterPair =
  ( @EpiDialogFilterEPX,
    @EpiDialogFilterEPZ,
    @EpiDialogFilterREC,
    @EpiDialogFilterText,
    @EpiDialogFilterODS,
    @EpiDialogFilterXLS,
    @EpiDialogFilterDTA,
    @EpiDialogFilterDBF,
    @EpiDialogFilterSPSS,
    @EpiDialogFilterSAS,
    @EpiDialogFilterDDI,
    @EpiDialogFilterCollection,
    @EpiDialogFilterAll
    );

function GetEpiDialogFilter(DialogFilters: TEpiDialogFilters): string;
var
  CollectedExt: string;
  Filter: TEpiDialogFilter;

  function AddFilter(Filter: PEpiDialogFilterPair): string;
  begin
    result := Filter^.FilterName + '|' + Filter^.FilterExt + '|';
    CollectedExt += Filter^.FilterExt + ';';
  end;

begin
  Result := '';
  CollectedExt := '';

  for Filter in DialogFilters do
  begin
    if Filter in [dfCollection] then continue;
    result += AddFilter(EpiDialogFilters[Filter]);
  end;

  if dfCollection in DialogFilters then
    Result := EpiDialogFilterCollection.FilterName + '|' +
              CollectedExt + '|' + Result;
end;

function GetEpiDialogFilterExt(DialogFilters: TEpiDialogFilters): string;
var
  Filter: TEpiDialogFilter;
begin
  for Filter in DialogFilters do
  begin
    if Filter in [dfCollection, dfAll] then continue;
    result += EpiDialogFilters[Filter]^.FilterExt;
  end;
end;

procedure StreamToZipFile(const St: TStream; const ZipFileName: string);
var
  TheZipper: TZipper;
begin
  TheZipper           := TZipper.Create;
  TheZipper.FileName  := ZipFileName;
  TheZipper.InMemSize := St.Size;  // No disk usage, we assume files are "rather" small ~50-100 Mb.
  TheZipper.Entries.AddFileEntry(St, SysToUTF8(ExtractFileName(ChangeFileExt(UTF8ToSys(ZipFileName), '.epx'))));
  TheZipper.ZipAllFiles;
  TheZipper.Free;
end;

type
  TTmpStreamHandler = class
  private
    TSt: TStream;
  public
    constructor create(St: TStream);
    Procedure CreateStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
  end;

  constructor TTmpStreamHandler.create(St: TStream);
  begin
    TSt := St;
  end;

  procedure TTmpStreamHandler.CreateStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
  begin
    if Assigned(TSt) then
      AStream := TSt
    else
      AStream := TMemoryStream.Create;
  end;

procedure ZipFileToStream(St: TStream; const ZipFileName: string);
var
  TheUnZipper: TUnZipper;
  StHandler: TTmpStreamHandler;
begin
  StHandler := TTmpStreamHandler.create(St);
  TheUnZipper           := TUnZipper.Create;
  TheUnZipper.FileName  := ZipFileName;
  TheUnZipper.OnCreateStream := @StHandler.CreateStream;
  TheUnZipper.OnDoneStream   := @StHandler.CreateStream; // This is only needed to prevent TUnZipper from FreeAndNil the stream.
  TheUnZipper.UnZipAllFiles;
  TheUnZipper.Free;
end;

function StrToSHA1Base64(const S: string): string;
var
  Sha1: TDCP_sha1;
  Digest: string;
begin
  SetLength(Digest, 20);
  Sha1 := TDCP_sha1.Create(nil);
  Sha1.Init;
  Sha1.UpdateStr(S);
  Sha1.Final(Digest[1]);
  result := Base64EncodeStr(Digest);
  Sha1.Free;
end;

function IsReservedEpiFieldName(const Name: string): boolean;
begin
  result :=
    (Name = EpiIndexIntegrityFieldName) or
    (Name = EpiDoubleEntryFieldName);
end;

end.

