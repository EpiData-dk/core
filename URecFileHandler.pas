unit URecFileHandler;

interface

uses
  UCustomFileHandler;


type
  TRecFileHandler = class(TCustomFileHandler)
  private

  protected
    function GetFileHandlerType(): TFileHandlerType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function   load(filename:string=''; aOptions:TEpiDataFileOptions=[]):boolean; override;
    function   Read(RecordNum:integer):boolean; override;
    function   Write(RecordNum:Integer):boolean; override;
    function   Commit:boolean; override;
  end;

implementation

{*************** TRecFileHandler ***********************}

constructor TRecFileHandler.Create();
begin
  inherited Create();
end;

destructor TRecFileHandler.Destroy();
begin
  inherited Destroy();
end;

function TRecFileHandler.load(filename:string=''; aOptions:TEpiDataFileOptions=[]):boolean;
begin
  result := false;
end;

function TRecFileHandler.Read(RecordNum:integer):boolean;
begin
  result := false;
end;

function TRecFileHandler.Write(RecordNum:Integer):boolean;
begin
  result := false;
end;

function TRecFileHandler.Commit:boolean; 
begin
  result := false;
end;
                  
function TRecFileHandler.GetFileHandlerType: TFileHandlerType;
begin
  result := fhREC;
end;

end.
