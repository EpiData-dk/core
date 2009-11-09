unit UPWform;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  LResources, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type

  { TformPW }

  TformPW = class(TForm)
    Label1: TLabel;
    labelRepeatPW: TLabel;
    editPW1: TEdit;
    editPW2: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    labelFilename: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FDoublePW:boolean;
    procedure SetDoublePW(value:boolean);
    procedure SetFilename(value:string);
  public
    { Public declarations }
    property DoublePW:boolean read FDoublePW write SetDoublePW;
    property Filename:string write SetFilename;
  end;

implementation

procedure TformPW.FormCreate(Sender: TObject);
begin
  labelFilename.Caption:='';
  SetDoublePW(false);
end;

procedure TformPW.btnOKClick(Sender: TObject);
begin

end;

procedure TformPW.SetDoublePW(value:boolean);
begin
  FDoublePW:=value;
  labelRepeatPW.Visible:=FDoublePW;
  editPW2.Visible:=FDoublePW;
end;

procedure TformPW.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=true;
  if ModalResult=mrCancel then exit;
  if FDoublePW then
    begin
      if length(editPW1.Text)<6 then
        begin
          MessageDlg('Password must be 6 characters or longer',mtError,[mbOK],0);
          CanClose:=false;
          exit;
        end;
      if editPW1.text<>editPW2.text then
        begin
          MessageDlg('The password do not match',mtError,[mbOK],0);
          CanClose:=false;
          exit;
        end;
    end;
end;

procedure TformPW.SetFilename(value:string);
begin
  labelFilename.caption:=value;
end;

initialization
  {$i upwform.lrs}

end.