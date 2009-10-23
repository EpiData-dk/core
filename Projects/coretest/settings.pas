unit Settings;

{$codepage UTF8}
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
    dateFmtCombo: TComboBox;
    decCombo: TComboBox;
    dateCombo: TComboBox;
    decEdit: TEdit;
    dateEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure applyButClick(Sender: TObject);
    procedure dateFmtComboChange(Sender: TObject);
    procedure decComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure okButClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SettingsForm: TSettingsForm;

implementation

uses
  UEpiDataGlobals, UEpiUtils;

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
  applyBut.Enabled := true;
end;

procedure TSettingsForm.applyButClick(Sender: TObject);
begin
  EpiExternalFormatSettings.DecimalSeparator :=
    BoolToStr(decCombo.ItemIndex = decCombo.Items.Count - 1 ,
      decEdit.Text, decCombo.Items[decCombo.ItemIndex])[1];
  EpiExternalFormatSettings.DateSeparator :=
    BoolToStr(dateCombo.ItemIndex = dateCombo.Items.Count - 1 ,
      dateEdit.Text, dateCombo.Items[dateCombo.ItemIndex])[1];
  applyBut.Enabled := false;
end;

procedure TSettingsForm.dateFmtComboChange(Sender: TObject);
begin
  applyBut.Enabled := true;
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
var
  i: LongInt;
begin
  i := decCombo.Items.IndexOf(EpiExternalFormatSettings.DecimalSeparator);
  if i = -1 then
  begin
    decCombo.ItemIndex := decCombo.Items.Count - 1;
    decComboChange(decCombo);
    decEdit.Text := EpiExternalFormatSettings.DecimalSeparator;
  end else
    decCombo.ItemIndex := i;

  i := dateCombo.Items.IndexOf(EpiExternalFormatSettings.DateSeparator);
  if i = -1 then
  begin
    dateCombo.ItemIndex := dateCombo.Items.Count - 1;
    decComboChange(dateCombo);
    dateEdit.Text := EpiExternalFormatSettings.DateSeparator;
  end else
    dateCombo.ItemIndex := i;

  applyBut.Enabled := false;
end;

procedure TSettingsForm.okButClick(Sender: TObject);
begin
  applyButClick(self);
end;

initialization
  {$I settings.lrs}

end.

