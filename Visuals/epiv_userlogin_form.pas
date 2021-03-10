unit epiv_userlogin_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type

  { TUserLoginForm }

  TUserLoginForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    LoginEdit: TEdit;
    PasswordEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure LoginEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

uses
  LCLType;

{ TUserLoginForm }

procedure TUserLoginForm.LoginEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then
    PasswordEdit.SetFocus;
end;

end.

