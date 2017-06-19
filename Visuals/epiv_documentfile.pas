unit epiv_documentfile;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiopenfile, epidocument, epiadmin;

type

  { TDocumentFile }

  TDocumentFile = class(TEpiDocumentFile)
  private
    function DoWarning(WarningType: TOpenEpiWarningType; const Msg: string
      ): TOpenEpiWarningResult;
    procedure DoError(const Msg: string);
  protected
    constructor Create; override;
    function DoPassWord(Sender: TObject;
      RequestType: TEpiRequestPasswordType;
      RequestNo: Integer;
      var Login: UTF8String;
      var Password: UTF8String): TEpiRequestPasswordResponse; virtual;
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
    wtLockFileMissing,
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
  RequestType: TEpiRequestPasswordType; RequestNo: Integer;
  var Login: UTF8String; var Password: UTF8String): TEpiRequestPasswordResponse;
var
  F: TUserLoginForm;
  APassword: String;
begin
  if (RequestNo < 3) then
    Result := rprAskOnFail
  else
    Result := rprStopOnFail;

  Password := '';
  APassword := '';

  case RequestType of
    erpSinglePassword:
      begin
        if (not InputQuery(
                 'Project Password',
                 'File: ' + FileName + LineEnding +
                        LineEnding +
                        'Project data is password protected.' + LineEnding +
                        'Please enter password:',
                 True, APassword)
           )
        then
          Result := rprCanceled
        else
          Password := APassword;
      end;

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

