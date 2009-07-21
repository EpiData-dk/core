unit uimportform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, UImportExport;

type

  { TImportForm }

  TImportForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    delimiterCombo: TComboBox;
    delimiterEdit: TEdit;
    GroupBox1: TGroupBox;
    delimiterBox: TGroupBox;
    guessRadio: TRadioButton;
    fixedRadio: TRadioButton;
    delimiterRadio: TRadioButton;
    Label1: TLabel;
    qesEdit: TLabeledEdit;
    OpenDialog1: TOpenDialog;
    openBtn: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure delimiterComboChange(Sender: TObject);
    procedure guessRadioChange(Sender: TObject);
    procedure openBtnClick(Sender: TObject);
  private
    { private declarations }
    FImportSetting:  PEpiTxtImportSettings;
  public
    { public declarations }
    property ImportSetting: PEpiTxtImportSettings read FImportSetting;
  end; 

var
  ImportForm: TImportForm;

implementation

{ TImportForm }

procedure TImportForm.delimiterComboChange(Sender: TObject);
begin
  delimiterEdit.Visible :=
    delimiterCombo.ItemIndex = (delimiterCombo.Items.Count - 1);
end;

procedure TImportForm.Button1Click(Sender: TObject);
begin
  if guessRadio.Checked then
    FImportSetting := @ImportTxtGuess
  else begin
    FImportSetting := new(PEpiTxtImportSettings);
    FImportSetting^.QESFileName := qesEdit.Text;
    FImportSetting^.FieldSeparator := BoolToStr(Trim(delimiterEdit.Text) <> '', delimiterEdit.Text, delimiterCombo.Items[delimiterCombo.ItemIndex])[1];
    FImportSetting^.QuoteChar := '"';
    FImportSetting^.UseQESFile := true;
    FImportSetting^.FixedFormat := false;
    FImportSetting^.ImportSettings := new(PEpiImportSettings);
    FImportSetting^.ImportSettings^.EndRecord := -1;
    FImportSetting^.ImportSettings^.StartRecord := 1;
  end;
end;

procedure TImportForm.guessRadioChange(Sender: TObject);
begin
  qesEdit.Visible := not (Sender = guessRadio);
  openBtn.Visible := qesEdit.Visible;
  delimiterBox.Visible := (sender = delimiterRadio);
end;

procedure TImportForm.openBtnClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    qesEdit.Text := OpenDialog1.FileName;
end;

initialization
  {$I uimportform.lrs}

end.


