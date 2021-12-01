unit ut_epicustombase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TestFramework, epicustombase;

type

  { TTestEpiCustomBase }

  TTestEpiCustomBase = class(TTestCase)
  protected
    procedure SetUpOnce; override;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TearDownOnce; override;
  published
    procedure CreateCustomBase;
  end;

implementation

{ TTestEpiCustomBase }

procedure TTestEpiCustomBase.SetUpOnce;
begin
  inherited SetUpOnce;
end;

procedure TTestEpiCustomBase.SetUp;
begin
  inherited SetUp;
end;

procedure TTestEpiCustomBase.TearDown;
begin
  inherited TearDown;
end;

procedure TTestEpiCustomBase.TearDownOnce;
begin
  inherited TearDownOnce;
end;

procedure TTestEpiCustomBase.CreateCustomBase;
begin

end;

initialization
  RegisterTest(TTestEpiCustomBase.Suite);

end.

