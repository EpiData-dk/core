unit UStataFileHandler;

interface

uses
  UCustomFileHandler;

type
  TStataFileHandler = class(TCustomFileHandler)
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



{ TStataFileHandler }

function TStataFileHandler.Commit: boolean;
begin
  result := false;
end;

constructor TStataFileHandler.Create;
begin
  inherited;

end;

destructor TStataFileHandler.Destroy;
begin

  inherited;
end;

function TStataFileHandler.GetFileHandlerType: TFileHandlerType;
begin
  result := fhSTATA;
end;

function TStataFileHandler.Load(filename: string;
  aOptions: TEpiDataFileOptions): boolean;
begin
  result := false;
end;

function TStataFileHandler.Read(RecordNum: integer): boolean;
begin
  result := false;
end;

function TStataFileHandler.Write(RecordNum: integer): boolean;
begin
  result := false;
end;

end.
