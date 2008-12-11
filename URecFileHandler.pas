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

end.
 