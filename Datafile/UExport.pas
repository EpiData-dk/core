unit UExport;

interface

uses
  UEpiDataFile;

type

  TEpiExport = class(TObject)
  private
    procedure SaveRec(const FileName: string;
      const DataFile: TEpiDataFile);
  protected

  public
    procedure Save(const FileName: string; const DataFile: TEpiDataFile);
    procedure SaveStata(const FileName: string; const DataFile: TEpiDataFile);
    procedure SaveTXT(const FileName: string; const DataFile: TEpiDataFile);
    procedure SaveSAS(const FileName: string; const DataFile: TEpiDataFile);
    procedure SaveSPSS(const FileName: string; const DataFile: TEpiDataFile);
    procedure SaveXLS(const FileName: string; const DataFile: TEpiDataFile);
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils;
{ TEpiExport }

constructor TEpiExport.Create;
begin

end;

destructor TEpiExport.Destroy;
begin

  inherited;
end;

procedure TEpiExport.Save(const FileName: string;
  const DataFile: TEpiDataFile);
var
  Ext: string;
begin
  if FileName<>'' then
    ext:=AnsiLowerCase(ExtractFileExt(FileName));
  if (ext='.rec') or (ext='') then SaveRec(FileName, DataFile)
  else if ext='dta' then SaveStata(FileName, DataFile)
  else if ext='txt' then SaveTXT(FileName, DataFile)
  else if ext='sas' then SaveSAS(FileName, DataFile)
  else if ext='sps' then SaveSPSS(FileName, DataFile)
  else if ext='xls' then SaveXLS(FileName, DataFile)
  else begin
      //error('Filetype `'+ext+'` not supported');
    exit;
  end;
end;


procedure TEpiExport.SaveRec(const FileName: string;
  const DataFile: TEpiDataFile);
begin

end;

procedure TEpiExport.SaveSAS(const FileName: string;
  const DataFile: TEpiDataFile);
begin

end;

procedure TEpiExport.SaveSPSS(const FileName: string;
  const DataFile: TEpiDataFile);
begin

end;

procedure TEpiExport.SaveStata(const FileName: string;
  const DataFile: TEpiDataFile);
begin

end;

procedure TEpiExport.SaveTXT(const FileName: string;
  const DataFile: TEpiDataFile);
begin

end;

procedure TEpiExport.SaveXLS(const FileName: string;
  const DataFile: TEpiDataFile);
begin

end;

end.
