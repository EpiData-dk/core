unit UTxtFileHandler;

interface

uses
  UCustomFileHandler;

type
  TTxtFileHandler = class(TCustomFileHandler)
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

{ TTxtFileHandler }

function TTxtFileHandler.Commit: boolean;
begin
  result := false;
end;

constructor TTxtFileHandler.Create;
begin
  inherited;

end;

destructor TTxtFileHandler.Destroy;
begin

  inherited;
end;

function TTxtFileHandler.GetFileHandlerType: TFileHandlerType;
begin
  result := fhTXT;
end;

function TTxtFileHandler.Load(filename: string;
  aOptions: TEpiDataFileOptions): boolean;
begin
  result := false;
end;

function TTxtFileHandler.Read(RecordNum: integer): boolean;
begin
  result := false;
end;

function TTxtFileHandler.Write(RecordNum: integer): boolean;
begin
  result := false;
end;

end.
