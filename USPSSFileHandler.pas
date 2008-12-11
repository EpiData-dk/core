unit USPSSFileHandler;

interface

uses
  UCustomFileHandler;

type
  TSPSSFileHandler = class(TCustomFileHandler)
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

{ TSPSSFileHandler }

function TSPSSFileHandler.Commit: boolean;
begin
  result := false;
end;

constructor TSPSSFileHandler.Create;
begin
  inherited;

end;

destructor TSPSSFileHandler.Destroy;
begin

  inherited;
end;

function TSPSSFileHandler.GetFileHandlerType: TFileHandlerType;
begin
  result := fhSPSS;
end;

function TSPSSFileHandler.Load(filename: string;
  aOptions: TEpiDataFileOptions): boolean;
begin
  raise Exception.Create('Import for SPSS files not implemented');
end;

function TSPSSFileHandler.Read(RecordNum: integer): boolean;
begin
  raise Exception.Create('Import for SPSS files not implemented');
end;

function TSPSSFileHandler.Write(RecordNum: integer): boolean;
begin
  result := false;
end;

end.
