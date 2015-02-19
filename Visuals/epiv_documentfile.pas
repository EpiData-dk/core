unit epiv_documentfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, epiopenfile, epidocument, epiadmin;

type

  { TDocumentFile }

  TDocumentFile = class(TEpiDocumentFile)
  private
    procedure DoPassWord(Sender: TObject;
      RequestType: TEpiRequestPasswordType;
      var Login: string;
      var Password: string);
    function DoWarning(WarningType: TOpenEpiWarningType; const Msg: string
      ): TOpenEpiWarningResult;
    procedure DoError(const Msg: string);
  protected
    constructor Create; override;
  end;


implementation

uses
  Dialogs, Controls;

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

procedure TDocumentFile.DoPassWord(Sender: TObject;
  RequestType: TEpiRequestPasswordType; var Login: string; var Password: string
  );
begin
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
        Login := 'torsten';
        Password := 'ostemad';
      end;
  end;
end;

end.

