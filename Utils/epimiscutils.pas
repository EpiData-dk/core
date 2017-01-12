unit epimiscutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes;

const
  EpiTypeCastArray: array[TEpiFieldType, TEpiFieldType] of boolean =
      // Cast To Types
//                ftBoolean, ftInteger, ftAutoInc, ftFloat, ftDMYDate, ftMDYDate, ftYMDDate, ftDMYToday, ftMDYToday, ftYMDToday, ftTime, ftTimeNow, ftUpperString ftString, ftMemo
{C} {ftBoolean} ((true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,         true,     true),
{a} {ftInteger}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,         true,     true),
{s} {ftAutoInc}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,         true,     true),
{t} {ftFloat}    (true,      true,      false,     true,    false,     false,     false,     false,      false,      false,      true,   false,     true,         true,     true),
    {ftDMYDate}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,         true,     true),
{F} {ftMDYDate}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,         true,     true),
{r} {ftYMDDate}  (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,         true,     true),
{o} {ftDMYToday} (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,         true,     true),
{m} {ftMDYToday} (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,         true,     true),
    {ftYMDToday} (true,      true,      false,     true,    true,      true,      true,      false,      false,      false,      true,   false,     true,         true,     true),
    {ftTime}     (true,      true,      false,     true,    false,     false,     false,     false,      false,      false,      true,   false,     true,         true,     true),
    {ftTimeNow}  (true,      true,      false,     true,    false,     false,     false,     false,      false,      false,      true,   false,     true,         true,     true),
  {ftUpperString}(true,      false,     false,     false,   false,     false,     false,     false,      false,      false,      false,  false,     true,         true,     true),
    {ftString}   (true,      false,     false,     false,   false,     false,     false,     false,      false,      false,      false,  false,     true,         true,     true),
    {ftMemo}     (true,      false,     false,     false,   false,     false,     false,     false,      false,      false,      false,  false,     true,         true,     true));


  const EpiTypeNames: array[TEpiFieldType] of string =
    ('Boolean',
     'Integer', 'Auto Increment', 'Float',
     'Date (DMY)', 'Date (MDY)', 'Date (YMD)',
     'Date (DMY - Auto)', 'Date (MDY - Auto)', 'Date (YMD - Auto)',
     'Time', 'Time (Auto)',
//     'String', 'Uppercase', 'Memo'
     'Uppercase', 'String', 'Memo'
    );

  const EpiTypeNamesShort: array[TEpiFieldType] of string =
    ('B',
     'I', 'Ia', 'F',
     'DMY', 'MDY', 'YMD',
     'DMYa', 'MDYa', 'YMDa',
     'T', 'Ta',
//     'S', 'U', 'M'
     'U', 'S', 'M'
    );

type
  TEpiDialogFilter = (dfEPX, dfEPZ, dfREC, dfText, dfODS, dfXLS, dfDTA, dfDBF, dfSPSS, dfSAS, dfDDI, dfPGM, dfCollection, dfAll);
  TEpiDialogFilters = set of TEpiDialogFilter;

const
  dfEpiData: TEpiDialogFilters = [dfEPX, dfEPZ, dfCollection];
  dfImport: TEpiDialogFilters  = [dfEPX, dfEPZ, dfREC, dfDTA, dfText, dfCollection];
  dfExport: TEpiDialogFilters  = [dfDTA];


  // File dialog filter functions.
  function GetEpiDialogFilter(DialogFilters: TEpiDialogFilters): string;
  function GetEpiDialogFilterExt(DialogFilters: TEpiDialogFilters): string;
  function GetEpiDialogFilterName(DialogFilters: TEpiDialogFilters): string;

  procedure StreamToZipFile(Const St: TStream; Const ZipFileName: string);
  procedure ZipFileToStream(St: TStream;   Const ZipFileName: string);

  // Encryption Utils
  function StrToSHA1Base64(const S: string): string;

  // Returns true if Name is a field name defined in EpiGlobals.pas
  function IsReservedEpiFieldName(Const Name: string): boolean;

  function CanonicalizeFileName(Const Filename: UTF8String): UTF8String;

  function PostInc(var Value: Integer; Const N: Integer = 1): Integer;
  function PreInc(var Value: Integer; Const N: Integer = 1): Integer;

  function GetHostNameWrapper: UTF8String;
  function GetUserNameWrapper: UTF8String;

  {$IFDEF MSWINDOWS}
  function AssociateFiles(Const ApplicationName, ApplicationDescription,
    ExePath: String): Boolean;
  function UnAssociateFiles(Const ApplicationName, ApplicationDescription,
    ExePath: String): Boolean;
  {$ENDIF}


implementation

uses
  {$IFDEF Windows}
  windows,
  {$ENDIF}
  {$IFDEF unix}
  Unix,
  {$ENDIF}
  zipper, FileUtil, DCPsha1, DCPbase64, epiglobals, ufileassociation, LazUTF8;

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

  EpiDialogFilterPGM: TEpiDialogFilterPair = (
    FilterName: 'EpiData Analysis Progran File (*.pgm)';
    FilterExt:  '*.pgm';
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
    @EpiDialogFilterPGM,
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
  Result := '';
  for Filter in DialogFilters do
  begin
    if Filter in [dfCollection, dfAll] then continue;
    result += EpiDialogFilters[Filter]^.FilterExt;
  end;
end;

function GetEpiDialogFilterName(DialogFilters: TEpiDialogFilters): string;
var
  Filter: TEpiDialogFilter;
begin
  Result := '';
  for Filter in DialogFilters do
  begin
    if Filter in [dfCollection, dfAll] then continue;
    result += EpiDialogFilters[Filter]^.FilterName;
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

function CanonicalizeFileName(const Filename: UTF8String): UTF8String;
var
  C: Char;
const
  {$IFDEF MSWINDOWS}
  ErrorChars = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];
  {$ENDIF}
  {$IFDEF LINUX}
  ErrorChars = ['/'];
  {$ENDIF}
  {$IFDEF DARWIN}
  ErrorChars = ['/', ':'];
  {$ENDIF}
begin
  Result := Filename;

  for C in ErrorChars do
    Result := UTF8StringReplace(Result, C, '_', [rfReplaceAll]);
end;

function PostInc(var Value: Integer; const N: Integer): Integer;
begin
  result := Value;
  Inc(Value, N);
end;

function PreInc(var Value: Integer; const N: Integer): Integer;
begin
  Inc(Value, N);
  result := Value;
end;

function GetHostNameWrapper: UTF8String;
{$IFDEF WINDOWS}
var
  Buffer: Array[0..127] of WideChar;
  Sz: DWORD;
{$ENDIF}
begin
  Result := '';

  {$IFDEF Windows}
  Sz := SizeOf(Buffer);
  GetComputerNameW(Buffer, Sz);
  Result := WideCharToString(Buffer);
  {$ENDIF}
  {$IFDEF unix}
  Result := GetHostName;
  {$ENDIF}
end;

function GetUserNameWrapper: UTF8String;
{$IFDEF WINDOWS}
var
  Buffer: Array[0..127] of WideChar;
  Sz: DWORD;
{$ENDIF}
begin
  Result := '';

  {$IFDEF MSWINDOWS}
  Sz := SizeOf(Buffer);
  GetUserNameW(Buffer, Sz);
  Result := WideCharToString(Buffer);
  {$ENDIF}
  {$IFDEF UNIX}
  Result := GetEnvironmentVariableUTF8('USER');
  {$ENDIF}
  if Result = '' then
    Result := 'Unknown';
end;

{$IFDEF MSWINDOWS}
function DoAssociation(const ApplicationName, ApplicationDescription,
  ExePath: String; Unregister: Boolean): Boolean;
var
  Assoc: TFileAssociation;
  Filter: TEpiDialogFilter;
  S: String;
begin
  Assoc := TFileAssociation.Create(nil);
  Assoc.UnReg := Unregister;
  Assoc.RegisterFileAssociation := true;
  Assoc.RegisterForAllUsers := False;

  Assoc.ApplicationName := ApplicationName;
  Assoc.ApplicationDescription := ApplicationDescription;

  Assoc.Action := '"' + ExePath + '" "%1"';
  Assoc.ActionName := 'Open';

  Result := true;
  for Filter in dfEpiData do
  begin
    if (Filter in [dfAll, dfCollection]) then continue;

    S := GetEpiDialogFilterExt([Filter]);
    Delete(S, 1, 1);
    Assoc.Extension     := S;

    S := GetEpiDialogFilterName([Filter]);
    Delete(S, Length(S) - 7, 8);
    Assoc.ExtensionName := S;

    Result := Assoc.Execute and Result;
  end;
  Assoc.ClearIconCache; //<<-- rebuild icons
  Assoc.Free;
end;

function AssociateFiles(const ApplicationName, ApplicationDescription,
  ExePath: String): Boolean;
begin
  Result := DoAssociation(ApplicationName, ApplicationDescription, ExePath,false);
end;

function UnAssociateFiles(const ApplicationName, ApplicationDescription,
  ExePath: String): Boolean;
begin
  Result := DoAssociation(ApplicationName, ApplicationDescription, ExePath, true);
end;
{$ENDIF}

end.

