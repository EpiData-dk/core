unit Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    cancelBut: TButton;
    applyBut: TButton;
    okBut: TButton;
    ComboBox1: TComboBox;
    decCombo: TComboBox;
    dateCombo: TComboBox;
    decEdit: TEdit;
    dateEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure decComboChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SettingsForm: TSettingsForm;

implementation

{ TSettingsForm }

procedure TSettingsForm.decComboChange(Sender: TObject);
var
  edit: TEdit;
begin
  with (Sender as TComboBox) do
  begin
    edit := decEdit;
    if Sender = dateCombo then
      edit := dateEdit;

    edit.Visible := (Items[ItemIndex] = 'Other');
  end;
end;

initialization
  {$I settings.lrs}

end.

