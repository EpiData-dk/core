unit epistudy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TEpiStudy }

  TEpiStudy = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TEpiStudy }

constructor TEpiStudy.Create;
begin

end;

destructor TEpiStudy.Destroy;
begin
  inherited Destroy;
end;

end.

