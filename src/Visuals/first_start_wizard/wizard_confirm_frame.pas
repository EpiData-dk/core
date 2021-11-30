unit wizard_confirm_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, fpjson;

type

  { TWizardConfirmActionsFrame }

  TWizardConfirmActionsFrame = class(TFrame)
    Memo: TMemo;
  private
    FData: TJSONData;
  public

  published
    property Data: TJSONData write FData;
    procedure PageShow();
  end;

implementation

{$R *.lfm}

{ TWizardConfirmActionsFrame }

procedure TWizardConfirmActionsFrame.PageShow();
begin
  with Memo do
    begin
      Clear;
      Append('Please confirm the following actions:');
      Append('* Create the configuration file: ' + FData.FindPath('ConfigFile').AsString);
      Append('* Create the folder: ' + FData.FindPath('DataDir').AsString);
      Append('* Copy examples into: ' + FData.FindPath('DataDir').AsString + 'examples' + DirectorySeparator);
      Append('');
      Append('Once you click NEXT the copying will begin!');
    end;
end;

end.

