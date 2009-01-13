unit UImport;

interface

uses
  UEpiDataFile;

type

  TEpiImport = class(TObject)
  private
    function LoadRec(const FileName: string;
      aOptions: TEpiDataFileOptions=[]): TEpiDataFile;
  protected

  public
    function Load(const FileName: string; aOptions: TEpiDataFileOptions=[]): TEpiDataFile;
    function LoadStata(const FileName: string; aOptions: TEpiDataFileOptions=[]): TEpiDataFile;
    function LoadTXT(const FileName: string; aOptions: TEpiDataFileOptions=[]): TEpiDataFile;
    function LoadSAS(const FileName: string; aOptions: TEpiDataFileOptions=[]): TEpiDataFile;
    function LoadSPSS(const FileName: string; aOptions: TEpiDataFileOptions=[]): TEpiDataFile;
    function LoadXLS(const FileName: string; aOptions: TEpiDataFileOptions=[]): TEpiDataFile;
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils, UStataIO;
{ TEpiImport }

constructor TEpiImport.Create;
begin

end;

destructor TEpiImport.Destroy;
begin

  inherited;
end;

function TEpiImport.Load(const FileName: string; aOptions: TEpiDataFileOptions=[]): TEpiDataFile;
var
  ext: string;
begin
  result := nil;
  if FileName<>'' then
    ext:=AnsiLowerCase(ExtractFileExt(FileName));
  if (ext='.rec') or (ext='') then result := LoadRec(FileName, aOptions)
  else if ext='dta' then result := LoadStata(FileName, aOptions)
  else if ext='txt' then result := LoadTXT(FileName, aOptions)
  else if ext='sas' then result := LoadSAS(FileName, aOptions)
  else if ext='sps' then result := LoadSPSS(FileName, aOptions)
  else if ext='xls' then result := LoadXLS(FileName, aOptions)
  else begin
      //error('Filetype `'+ext+'` not supported');
    exit;
  end;
end;

function TEpiImport.LoadRec(const FileName: string; aOptions: TEpiDataFileOptions=[]): TEpiDataFile;
begin
  result := nil;
  try
    result := TEpiDataFile.Create();
    //result.Options := aOptions;
  finally
    //
  end;
end;

function TEpiImport.LoadSAS(const FileName: string; aOptions: TEpiDataFileOptions=[]): TEpiDataFile;
begin
  result := nil;
//  Error('Import from SAS not implemented');
end;

function TEpiImport.LoadSPSS(const FileName: string; aOptions: TEpiDataFileOptions=[]): TEpiDataFile;
begin
  result := nil;
//  Error('Import from SPSS not implemented');
end;

function TEpiImport.LoadStata(const FileName: string; aOptions: TEpiDataFileOptions=[]): TEpiDataFile;
var
  StataIO: TStataIO;
begin
  result := nil;
  try
    result := TEpiDataFile.Create();
    //result.Options := aOptions;
  finally
    //
  end;
end;

function TEpiImport.LoadTXT(const FileName: string; aOptions: TEpiDataFileOptions=[]): TEpiDataFile;
begin
  result := nil;
  try
    result := TEpiDataFile.Create();
    //result.Options := aOptions;
  finally
    //
  end;
end;

function TEpiImport.LoadXLS(const FileName: string; aOptions: TEpiDataFileOptions=[]): TEpiDataFile;
begin
  result := nil;
//  Error('Import from XLS not implemented');
end;

end.
