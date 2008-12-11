unit USASFileHandler;

interface

uses
  UCustomFileHandler;

type
  TSASFileHandler = class(TCustomFileHandler)
  private

  protected

    function GetFileHandlerType(): TFileHandlerType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Load(filename:string=''; aOptions:TEpiDataFileOptions=[]):boolean; override;
    function Read(RecordNum: integer): boolean; override;
    function Write(RecordNum: integer): boolean; override;
    function Commit: boolean; override;
  end;

implementation

uses
  SysUtils;
  
{ TStataFileHandler }

function TSASFileHandler.Commit: boolean;
begin
  result := false;
end;

constructor TSASFileHandler.Create;
begin
  inherited;

end;

destructor TSASFileHandler.Destroy;
begin

  inherited;
end;

function TSASFileHandler.GetFileHandlerType: TFileHandlerType;
begin
  result := fhSAS
end;

function TSASFileHandler.Load(filename: string;
  aOptions: TEpiDataFileOptions): boolean;
begin
  raise Exception.Create('Import for Stata files not implemented');
end;

function TSASFileHandler.Read(RecordNum: integer): boolean;
begin
  raise Exception.Create('Import for Stata files not implemented');
end;

function TSASFileHandler.Write(RecordNum: integer): boolean;
begin
  result := false;
end;

end.
