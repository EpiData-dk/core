unit epiadmin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TEpiAdmin }

  TEpiAdmin = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TEpiAdmin }

constructor TEpiAdmin.Create;
begin

end;

destructor TEpiAdmin.Destroy;
begin
  inherited Destroy;
end;

end.

