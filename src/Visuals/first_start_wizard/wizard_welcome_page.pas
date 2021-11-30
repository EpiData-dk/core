unit wizard_welcome_page;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls;

type

  { TWizardWelcomeFrame }

  TWizardWelcomeFrame = class(TFrame)
    Memo1: TMemo;
  private

  public
  published
    procedure PageLoad();
  end;

implementation

{$R *.lfm}

{ TWizardWelcomeFrame }

procedure TWizardWelcomeFrame.PageLoad();
var
  i: Integer;
begin
  for i := 0 to Memo1.Lines.Count - 1 do
    Memo1.Lines[i] := StringReplace(Memo1.Lines[i], '$PROGRAM$', Application.Title, [rfReplaceAll]);
end;

end.

