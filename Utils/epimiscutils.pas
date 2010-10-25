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

  // File dialog filter functions.
  function GetEpiDialogFilter(ShowEPX, ShowEPZ, ShowREC, ShowText, ShowODS, ShowXLS,
    ShowDTA, ShowDBF, ShowQES, ShowCollection, ShowAll: boolean): string;

  procedure StreamToZipFile(Const St: TStream; Const ZipFileName: string);
  procedure ZipFileToStream(Var St: TStream;   Const ZipFileName: string);



implementation

uses
  zipper, FileUtil, LCLProc;

type
  TEpiDialogFilterPair = record
    FilterName: string;
    FilterExt:  string;
  end;

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
    FilterExt:  '*.txt;*.csv';
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

  EpiDialogFilterQES: TEpiDialogFilterPair = (
    FilterName: 'QES file (*.qes)';
    FilterExt:  '*.qes';
  );

  EpiDialogFilterAll: TEpiDialogFilterPair = (
    FilterName: 'Show All (*.*)';
    FilterExt:  '*.*';
  );


function GetEpiDialogFilter(ShowEPX, ShowEPZ, ShowREC, ShowText, ShowODS,
  ShowXLS, ShowDTA, ShowDBF, ShowQES, ShowCollection, ShowAll: boolean): string;
var
  CollectedExt: string;

  function AddFilter(Filter: TEpiDialogFilterPair): string;
  begin
    result := Filter.FilterName + '|' + Filter.FilterExt + '|';
    CollectedExt += Filter.FilterExt + ';';
  end;

begin
  Result := '';
  CollectedExt := '';
  if ShowEPX then
    Result += AddFilter(EpiDialogFilterEPX);
  if ShowEPZ then
    Result += AddFilter(EpiDialogFilterEPZ);
  if ShowREC then
    Result += AddFilter(EpiDialogFilterREC);
  if ShowText then
    Result += AddFilter(EpiDialogFilterText);
  if ShowODS then
    Result += AddFilter(EpiDialogFilterODS);
  if ShowXLS then
    Result += AddFilter(EpiDialogFilterXLS);
  if ShowDTA then
    Result += AddFilter(EpiDialogFilterDTA);
  if ShowDBF then
    Result += AddFilter(EpiDialogFilterDBF);
  if ShowQES then
    Result += AddFilter(EpiDialogFilterQES);

  if ShowCollection then
    Result := EpiDialogFilterCollection.FilterName + '|' +
              CollectedExt + '|' + Result;

  if ShowAll then
    Result += AddFilter(EpiDialogFilterAll);
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

procedure ZipFileToStream(var St: TStream; const ZipFileName: string);
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

end.

