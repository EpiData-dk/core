unit customtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  { TCustomTest }

  TCustomTest = class
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   Initialize; virtual;
    function    Run: boolean; virtual; abstract;
  end;
  TCustomTestClass = class of TCustomTest;

 var
   Tests: TFPList = nil;

procedure RegisterTest(ATest: TCustomTestClass);

implementation

procedure RegisterTest(ATest: TCustomTestClass);
begin
  if not Assigned(Tests) then
    Tests := TFPList.Create;
  Tests.Add(Pointer(ATest));
end;

{ TCustomTest }

constructor TCustomTest.Create;
begin
  inherited;
end;

destructor TCustomTest.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomTest.Initialize;
begin
  // Do nothing.
end;

end.

