unit UXLSFileHandler;

interface

uses
  UCustomFileHandler;

type
  TXLSFileHandler = class(TCustomFileHandler)
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

{ TXLSFileHandler }

function TXLSFileHandler.Commit: boolean;
begin
  result := false;
end;

constructor TXLSFileHandler.Create;
begin
  inherited;

end;

destructor TXLSFileHandler.Destroy;
begin

  inherited;
end;

function TXLSFileHandler.GetFileHandlerType: TFileHandlerType;
begin
  result := fhXLS;
end;

function TXLSFileHandler.Load(filename: string;
  aOptions: TEpiDataFileOptions): boolean;
begin
  raise Exception.Create('Import for SPSS files not implemented');
end;

function TXLSFileHandler.Read(RecordNum: integer): boolean;
begin
  raise Exception.Create('Import for SPSS files not implemented');
end;

function TXLSFileHandler.Write(RecordNum: integer): boolean;
begin
  result := false;
end;

end.
