unit URecFileHandler;

interface

uses
  UCustomFileHandler;


type
  TRecFileHandler = class(TCustomFileHandler)
  private

  protected

  public
    constructor Create; override;
    destructor Destroy; override;
    function   load(filename:string=''; aOptions:TEpiDataFileOptions=[]):boolean;  override; //Loads file in internal structure
  published

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

function TRecFileHandler.load(filename:string=''; aOptions:TEpiDataFileOptions=[]):boolean;  //Loads file in internal structure
begin
end;
end.
