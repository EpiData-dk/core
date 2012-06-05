unit test1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, customtest, epidocument;


type

  { TTest1 }

  TTest1 = class(TCustomTest)
  private
    FDoc: TEpiDocument;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Run: boolean; override;
  end;

implementation

{ TTest1 }

constructor TTest1.Create;
begin
  inherited Create;
end;

destructor TTest1.Destroy;
begin
  inherited Destroy;
end;

function TTest1.Run: boolean;
begin
  Result := true;
  try
    FDoc := TEpiDocument.Create('en');
  except
    result := false;
  end;
  FDoc.Free;
end;

initialization

begin
  RegisterTest(TTest1);
end;

end.

