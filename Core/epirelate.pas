unit epirelate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TEpiRelates }

  TEpiRelates = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TEpiRelates }

constructor TEpiRelates.Create;
begin

end;

destructor TEpiRelates.Destroy;
begin
  inherited Destroy;
end;

end.

