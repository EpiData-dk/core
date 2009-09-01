unit UMain;

{$mode objfpc}{$H+}

interface

uses
  LResources,
  SysUtils, Variants, Classes, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, UEpiDataFile, Grids,
  UDataFileTypes, Graphics, Menus, ActnList;

type

  { TMainForm }

  TMainForm = class(TForm)
    acQuit: TAction;
    acSettings: TAction;
    ActionList1: TActionList;
    clipBrdChkBox: TCheckBox;
    expclipBrdChkBox: TCheckBox;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    SettingsMenu: TMenuItem;
    QuitMenu: TMenuItem;
    EditMenu: TMenuItem;
    Panel1: TPanel;
    edInputFile: TLabeledEdit;
    SpeedButton1: TSpeedButton;
    readBtn: TButton;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    excelCombo: TComboBox;
    TabSheet1: TTabSheet;
    Memo1: TMemo;
    TabSheet2: TTabSheet;
    Memo2: TMemo;
    TabSheet3: TTabSheet;
    sg: TStringGrid;
    pgBar: TProgressBar;
    CheckBox1: TCheckBox;
    Panel2: TPanel;
    Button2: TButton;
    Button3: TButton;
    TabSheet4: TTabSheet;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    orgChkMemo: TMemo;
    parsedChkMemo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    pgLabel: TLabel;
    SpeedButton2: TSpeedButton;
    edOutputFile: TLabeledEdit;
    saveBtn: TButton;
    CheckBox2: TCheckBox;
    SaveDialog1: TSaveDialog;
    stataCombo: TComboBox;
    closeBtn: TButton;
    TabSheet5: TTabSheet;
    Memo3: TMemo;
    Label4: TLabel;
    cbDebug: TComboBox;
    Panel7: TPanel;
    Button6: TButton;
    Button7: TButton;
    filetypeCombo: TComboBox;
    procedure acQuitExecute(Sender: TObject);
    procedure acSettingsExecute(Sender: TObject);
    procedure clipBrdChkBoxChange(Sender: TObject);
    procedure expclipBrdChkBoxChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure readBtnClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure saveBtnClick(Sender: TObject);
    procedure closeBtnClick(Sender: TObject);
    procedure cbDebugChange(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure filetypeComboChange(Sender: TObject);
  private
    { Private declarations }
    procedure LoadData(ShowAsLabels: boolean);
    function  ShowProgress(Sender: TObject; Percent: Cardinal; Msg: string): TProgressResult;
    procedure GetPassword(Sender: TObject; ReqT: TRequestPasswordType; var password: string);
    procedure ShowDebug(Sender: TObject; Msg: string);
    procedure SetButtons(DatafileIsOpen: Boolean);
    function  UpdateExtension(Index: integer): string;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  Math, UPWform, UEpiUtils, UCheckFileIO,
  UEpiDataGlobals, UImportExport, ucommon, UEpiLog,
  Settings;

var
  Df: TEpiDataFile = nil;

const
  FrmCaption = 'EpiData Software Core Test Project';

procedure TMainForm.readBtnClick(Sender: TObject);
var
  Stream: TStream;
  ChkIO:  TCheckFileIO;
  Lst: TStrings;
  TmpStr: String;
begin
  if (not clipBrdChkBox.Checked) and (Trim(edInputFile.Text) = '') then exit;

  try
    Sg.ColCount := 2;
    Sg.RowCount := 2;
    Memo1.Lines.Clear;
    Memo2.Lines.Clear;
    orgChkMemo.Clear;
    parsedChkMemo.Clear;

    pgLabel.Caption := '';
    ChkIO := nil;
    Stream := nil;
    Lst := nil;

    TmpStr := BoolToStr(clipBrdChkBox.Checked, '', edInputFile.Text);
    LoadDataFile(Df, TmpStr, not CheckBox1.Checked, @ShowProgress, @GetPassword);

    if not Assigned(Df) then
    begin
      EpiLogger.Add('No Datafile Defined, this is probably because importform was canceled.', 1);
      Memo1.Lines.Clear;
      Memo1.Lines.Add('No Datafile Defined.');
      Exit
    end;

    Lst := TinyDocumentation(Df);
    Memo1.Lines.AddStrings(Lst);
    FreeAndNil(Lst);
    Lst := DocumentDataFile(Df);
    Memo2.Lines.AddStrings(Lst);
    FreeAndNil(Lst);

    if FileExists(ChangeFileExt(Df.FileName, '.chk')) then
      orgChkMemo.Lines.LoadFromFile(ChangeFileExt(Df.FileName, '.chk'));

    Stream := TMemoryStream.Create();
    ChkIO := TCheckFileIO.Create();
    ChkIO.WriteCheckToStream(Stream, Df);
    parsedChkMemo.Lines.LoadFromStream(Stream);

    LoadData(false);

    SetButtons(True);
  finally
    if Assigned(ChkIO) then FreeAndNil(ChkIO);
    if Assigned(Stream) then FreeAndNil(Stream);
  end;
end;

procedure TMainForm.clipBrdChkBoxChange(Sender: TObject);
begin
  edInputFile.Enabled := not clipBrdChkBox.Checked;
  SpeedButton1.Enabled := edInputFile.Enabled;
end;

procedure TMainForm.acQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.acSettingsExecute(Sender: TObject);
begin
  if Assigned(SettingsForm) then FreeAndNil(SettingsForm);
  SettingsForm := TSettingsForm.Create(self);
  SettingsForm.ShowModal;
end;

procedure TMainForm.expclipBrdChkBoxChange(Sender: TObject);
begin
  edOutputFile.Enabled := not expclipBrdChkBox.Checked;
  SpeedButton2.Enabled := edOutputFile.Enabled;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(Df) then FreeAndNil(Df);
  TEpiLog.DestroyInstance;
  CloseAction := caFree;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  SetFilter(OpenDialog1);
  if OpenDialog1.Execute then
   edInputFile.Text := OpenDialog1.FileName;
end;

procedure TMainForm.LoadData(ShowAsLabels: boolean);
var
  row, col, i: integer;
  numrecs: integer;
begin
  if (not assigned(Df)) then exit;

  sg.ColCount := Df.NumDataFields + 1;
  sg.RowCount := Max(Df.NumRecords + 1, 1);

  if sg.ColCount > 1 then
    sg.FixedCols := 1;
  if sg.RowCount > 1 then
    sg.FixedRows := 1;

  //Make col-headings
  i := 1;
  for col:=0 to df.NumFields-1 do
    if df[col].Fieldtype <> ftQuestion then
      sg.Cells[PostInc(i), 0] := df[col].FieldName;

  numrecs := df.NumRecords;
  for row := 1 to numrecs do
  begin
    df.Read(row);
    ShowProgress(nil, Floor((row/numrecs)*100), 'Reading records');
    sg.Cells[0, row] := inttostr(row);
    i := 1;
    for col := 0 to df.NumFields - 1 do
    begin
      if df[col].Fieldtype <> ftQuestion then
      begin
        if ShowAsLabels then
          sg.Cells[PostInc(i), row] := df[col].AsValue
        else
          sg.Cells[PostInc(i), row] := df[col].AsFmtData;
      end;
    end;
  end;
end;

function TMainForm.ShowProgress(Sender: TObject; Percent: Cardinal; Msg: string): TProgressResult;
begin
  if (not pgBar.Visible) then pgBar.Visible := true;
  pgBar.Position := Percent;
  result := prNormal;

  // Refreshing the whole form in Lazarus causes a significant slowdown.
  // In D7 refreshing the Progressbar cause graphical fuck-ups in the progressbar.
  if Msg <> pgLabel.Caption then
  begin
    pgLabel.Caption := Msg;
  //  pgLabel.Refresh;
  end;
//  pgBar.Refresh;
  Application.ProcessMessages;
end;

procedure TMainForm.GetPassword(Sender: TObject; ReqT: TRequestPasswordType; var password: string);
var
  FormPW: TFormPW;
begin
  FormPW := TformPW.Create(self);
  try
    FormPW.Filename := df.FileName;
    if ReqT = rpOpen then
      FormPW.DoublePW := false
    else
      FormPW.DoublePW := true;
    if FormPW.ShowModal = mrOK then
      password := FormPW.editPW1.Text;
  finally
    if Assigned(FormPW) then FreeAndNil(FormPW);
  end;
end;

procedure TMainForm.ShowDebug(Sender: TObject; Msg: string);
begin
  Memo3.Lines.Add(Msg);
end;

procedure TMainForm.SetButtons(DatafileIsOpen: Boolean);
begin
  closeBtn.Enabled := DatafileIsOpen;
  saveBtn.Enabled := DatafileIsOpen;
  SpeedButton2.Enabled := DatafileIsOpen;
  expclipBrdChkBox.Enabled := DatafileIsOpen;
  filetypeCombo.Enabled := DatafileIsOpen;


  Caption := FrmCaption;
  if DatafileIsOpen then
    Caption := Caption + ': ' + Df.FileName;
end;

function TMainForm.UpdateExtension(Index: integer): string;
begin
  case Index of
     0: Result := '.rec';
     1: Result := '.dta';
     2: Result := '.dbf';
     3: Result := '.csv';
     4: Result := '.xls';
     5: Result := '.ods';
  else
    Result := '.rec';
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  EpiLogger.DebugLevel := cbDebug.ItemIndex;
  EpiLogger.OnDebugEvent := @ShowDebug;
  EpiLogger.Reset;
  closeBtnClick(nil);

  cbDebug.Items.Clear;
  cbDebug.Items.Add('0 - None');
  cbDebug.Items.Add('1 - Errors/Warnings/System Info');
  cbDebug.Items.Add('2 - Normal');
  cbDebug.Items.Add('3 - Verbose');
  cbDebug.Items.Add('4 - Very Verbose');

  // add instructions
  Memo3.Lines.Add('Purpose of this application: Test core module for reading and saving data');
  Memo3.Lines.Add('Do NOT use this as a conversion tool - except for test situations');
  Memo3.Lines.Add('');
  Memo3.Lines.Add('Report any problems and discuss on the EpiData list');
  Memo3.Lines.Add('Use: Find files with the dialogs and click read - save or close');
  Memo3.Lines.Add('The log contains calls to the core module depending on log level specified');
  Memo3.Lines.Add('============================================================================');
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  LoadData(False);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  LoadData(True);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  pgBar.Width := Panel1.Width - (pgBar.Left * 2);
end;

procedure TMainForm.SpeedButton2Click(Sender: TObject);
var
   Ext: String;
begin
  if SaveDialog1.Execute then
    begin
    edOutputFile.Text := SaveDialog1.FileName;
    Ext := UpdateExtension(FileTypeCombo.ItemIndex);
    edOutputFile.Text := ChangeFileExt(edOutputFile.Text, Ext);
    end;
end;

procedure TMainForm.saveBtnClick(Sender: TObject);
var
  PExpSettings: Pointer;
  Ext: string;
begin
  if (not expclipBrdChkBox.Checked) and (Trim(edOutputFile.Text) = '') then exit;

  if (not expclipBrdChkBox.Checked) then
    Ext := UpdateExtension(FileTypeCombo.ItemIndex)
  else
    Ext := '';

  Ext := ChangeFileExt(BoolToStr(expclipBrdChkBox.Checked, '', edOutputFile.Text), Ext);
  EpiLogger.Add(('Saving file to: ' + BoolToStr(expclipBrdChkBox.Checked, 'Clipboard',Ext)), 1);
  SaveDialog1.FileName := ext;
  {$IFNDEF EPI_DEBUG}
  if fileexists(ext) then
  begin
    EpiLogger.Add('Cannot overwrite existing file: ' + ext, 2);
    Exit;
  end;
  {$ENDIF EPI_DEBUG}

  if stataCombo.Visible then
  begin
    Case stataCombo.ItemIndex of
      0: PExpSettings := @ExportStata4_5;
      1: PExpSettings := @ExportStata6;
      2: PExpSettings := @ExportStata7;
      3: PExpSettings := @ExportStata8_9;
    else
      PExpSettings := @ExportStata10;
    end;
  end;

  if excelCombo.Visible then
  begin
    if excelCombo.ItemIndex = 0 then
      PExpSettings := @ExportExcel5
    else
      PExpSettings := @ExportExcel8;
  end;

  if filetypeCombo.ItemIndex = 5 then
    PExpSettings := @ExportOpenDocument;

{  if filetypeCombo.ItemIndex < 4 then
  begin           }
    if not SaveDataFile(Df, Ext, not CheckBox2.Checked, @ShowProgress, @GetPassword, PExpSettings) then
    begin
      EpiLogger.Add('Error occured during save. Save datafile is not consistent.', 1);
      EpiLogger.Add('Please see "File Info:" tab-page for details.', 1);
      PageControl1.ActivePage := TabSheet5;
      Memo1.Lines.Clear;
      Memo1.Lines.Add(Df.ErrorText);
    end;
{  end else
    EpiLogger.Add('File type not yet implemented', 2);    }
end;

procedure TMainForm.closeBtnClick(Sender: TObject);
begin
  if Assigned(Df) then FreeAndNil(Df);
  SetButtons(False);
  Memo1.Clear;
  Memo2.Clear;
  orgChkMemo.Clear;
  parsedChkMemo.Clear;
  Sg.FixedCols := 0;
  Sg.ColCount  := 1;
  Sg.FixedRows := 0;
  Sg.RowCount  := 1;
  PageControl1.ActivePage := TabSheet5;  // stay on logfile to see what happened
end;

procedure TMainForm.cbDebugChange(Sender: TObject);
begin
  EpiLogger.DebugLevel := cbDebug.ItemIndex;
  EpiLogger.Add(Format('Debug level: %d',[cbDebug.ItemIndex]), 1);
end;

procedure TMainForm.Button6Click(Sender: TObject);
begin
  Memo3.Clear;
  EpiLogger.Reset;
end;

procedure TMainForm.Button7Click(Sender: TObject);
begin
  SaveDialog1.Filter := '';

  if SaveDialog1.Execute then
    EpiLogger.SaveToFile(SaveDialog1.FileName);
end;

procedure TMainForm.filetypeComboChange(Sender: TObject);
var ext : string;
begin
  stataCombo.Hide;
  excelCombo.Hide;
  CheckBox2.Checked := false;
  case TComboBox(Sender).ItemIndex of
    0: CheckBox2.Checked := true;
    1: StataCombo.Show;
    4: excelCombo.Show;
  end;
  Ext := UpdateExtension(TComboBox(Sender).ItemIndex);
  EpiLogger.Add('Filetype chosen: ' + ext, 2);
  edOutputFile.Text := ChangeFileExt(edOutputFile.Text, Ext);
end;

initialization
  {$i umain.lrs}

end.
