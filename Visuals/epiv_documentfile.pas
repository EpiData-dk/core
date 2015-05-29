unit epiv_documentfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiopenfile, epidocument, epiadmin;

type

  { TDocumentFile }

  TDocumentFile = class(TEpiDocumentFile)
  private
    function DoPassWord(Sender: TObject;
      RequestType: TEpiRequestPasswordType;
      RequestNo: Integer;
      var Login: string;
      var Password: string): TEpiRequestPasswordResponse;
    function DoWarning(WarningType: TOpenEpiWarningType; const Msg: string
      ): TOpenEpiWarningResult;
    procedure DoError(const Msg: string);
  protected
    constructor Create; override;
  end;


implementation

uses
  Dialogs, Controls, Forms, StdCtrls, epiv_userlogin_form;

{ TDocumentFile }

constructor TDocumentFile.Create;
begin
  inherited Create;

  OnWarning := @DoWarning;
  OnPassword := @DoPassword;
  OnError := @DoError;
end;

function TDocumentFile.DoWarning(WarningType: TOpenEpiWarningType;
  const Msg: string): TOpenEpiWarningResult;
var
  Buttons: TMsgDlgButtons;
  DefaultBtn: TMsgDlgBtn;
begin
  case WarningType of
    wtSysReadOnly,
    wtLockFile,
    wtDatePatternNoAlt,
    wtTimeBackup2nd:
      begin
        Buttons := mbYesNo;
        DefaultBtn := mbNo;
      end;
    wtDatePattern,
    wtTimeBackup:
      begin
        Buttons := mbYesNoCancel;
        DefaultBtn := mbCancel;
      end;
  end;

  case MessageDlg('Warning:', Msg, mtWarning, Buttons, 0, DefaultBtn) of
    mrYes: Result    := wrYes;
    mrNo:  Result    := wrNo;
    mrCancel: Result := wrCancel;
  else
    Result := wrCancel;
  end;
end;

procedure TDocumentFile.DoError(const Msg: string);
begin
  ShowMessage(Msg);
end;

function TDocumentFile.DoPassWord(Sender: TObject;
  RequestType: TEpiRequestPasswordType; RequestNo: Integer; var Login: string;
  var Password: string): TEpiRequestPasswordResponse;
var
  F: TUserLoginForm;
  ModalRes: Integer;
begin
  if (RequestNo < 3) then
    Result := rprAskOnFail
  else
    Result := rprStopOnFail;


  case RequestType of
    erpSinglePassword:
      Password :=
        PasswordBox('Project Password',
                    'File: ' + FileName + LineEnding +
                    LineEnding +
                    'Project data is password protected.' + LineEnding +
                    'Please enter password:');
    erpUserLogin:
      begin
        F := TUserLoginForm.Create(nil);
        F.Caption := 'Password required for: ' + ExtractFileName(FileName);

        // Forces a close
        if F.ShowModal = mrCancel then
          Result := rprCanceled;

        Login := F.LoginEdit.Text;
        Password := F.PasswordEdit.Text;

        F.Free;
      end;

    erpNewPassword:
      begin
        Password :=
          PasswordBox('Change Password',
                      'Your password has expired!' + LineEnding +
                      'Please enter a new password:');
      end;
  end;

end;

end.

