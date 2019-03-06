unit wizard_summary_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls;

type

  { TWizardSummaryFrame }

  TWizardSummaryFrame = class(TFrame)
    Memo1: TMemo;
  private

  public

  published
    procedure PageLoad();

  end;

implementation

{$R *.lfm}

{ TWizardSummaryFrame }

procedure TWizardSummaryFrame.PageLoad();
var
  i: Integer;
begin
  for i := 0 to Memo1.Lines.Count - 1 do
    Memo1.Lines[i] := StringReplace(Memo1.Lines[i], '$PROGRAM$', Application.Title, [rfReplaceAll]);
end;

end.

