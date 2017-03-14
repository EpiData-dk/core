unit epiv_statusbar_item_progressbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiv_custom_statusbar, epicustombase, ComCtrls, epidocument;

type

  { TEpiVStatusBarItem_ProgressBar }

  TEpiVStatusBarItem_ProgressBar = class(TEpiVCustomStatusBarItem)
  private
    ProgressUpdate: Integer;
    LastUpdate: Integer;
    FProgressbar: TProgressBar;
    procedure InternalProgress(const Sender: TEpiCustomBase;
      ProgressType: TEpiProgressType; CurrentPos, MaxPos: Cardinal;
      var Canceled: Boolean);
  protected
    procedure Update(Condition: TEpiVCustomStatusbarUpdateCondition); override;
  public
    class function Caption: string; override;
    class function Name: string; override;
  public
    constructor Create(AStatusBar: TEpiVCustomStatusBar); override;
    destructor Destroy; override;
    function GetPreferedWidth: Integer; override;
  end;

implementation

uses
  Controls, Forms;

{ TEpiVStatusBarItem_ProgressBar }

procedure TEpiVStatusBarItem_ProgressBar.InternalProgress(
  const Sender: TEpiCustomBase; ProgressType: TEpiProgressType; CurrentPos,
  MaxPos: Cardinal; var Canceled: Boolean);
begin
  case ProgressType of
    eptInit:
      begin
        if (MaxPos > 500) then
          ProgressUpdate := MaxPos div 50
        else
          ProgressUpdate := 1;
        FProgressbar.Position := CurrentPos;
        FProgressbar.Max := MaxPos;
        Visible := true;
        if not (csDestroying in Panel.ComponentState) then
          Application.ProcessMessages;
      end;
    eptFinish:
      begin
//        FProgressbar.Visible := false;
        if not (csDestroying in Panel.ComponentState) then
          Application.ProcessMessages;
        LastUpdate := 0;
        FProgressbar.Position := FProgressbar.Max;
        Visible := false;
      end;
    eptRecords:
      begin
        if CurrentPos > (LastUpdate + ProgressUpdate) then
        begin
          FProgressbar.Position := CurrentPos;
          {$IFNDEF MSWINDOWS}
          Application.ProcessMessages;
          {$ENDIF}
          LastUpdate := CurrentPos;
        end;
      end;
  end;
end;

procedure TEpiVStatusBarItem_ProgressBar.Update(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  inherited Update(Condition);

  case Condition of
    sucDefault: ;
    sucDocFile:
      begin
        if Assigned(Statusbar.DocFile) then
          Statusbar.DocFile.OnProgress := @InternalProgress;

        LastUpdate := 0;
        ProgressUpdate := 0;
        Visible := false;
      end;
    sucDataFile: ;
    sucSelection: ;
    sucSave: ;
    sucExample: ;
  end;
end;

class function TEpiVStatusBarItem_ProgressBar.Caption: string;
begin
  Result := 'Progress Bar';
end;

class function TEpiVStatusBarItem_ProgressBar.Name: string;
begin
  result := 'ProgressBar';
end;

constructor TEpiVStatusBarItem_ProgressBar.Create(
  AStatusBar: TEpiVCustomStatusBar);
begin
  inherited Create(AStatusBar);

  Resizable := true;

  FProgressbar := TProgressBar.Create(Panel);
  FProgressbar.Align := alClient;
  FProgressbar.BorderSpacing.Left := 2;
  FProgressbar.BorderSpacing.Right := 2;
  FProgressbar.Smooth := true;
  FProgressbar.BarShowText := true;
  FProgressbar.Parent := Panel;
end;

destructor TEpiVStatusBarItem_ProgressBar.Destroy;
begin
  inherited Destroy;
end;

function TEpiVStatusBarItem_ProgressBar.GetPreferedWidth: Integer;
begin
  if not Panel.HandleAllocated then
    begin
      Result := inherited GetPreferedWidth;
      Exit;
    end;

  Result := FProgressbar.Left + FProgressbar.Width + 2;
end;

initialization
  EpiV_RegisterCustomStatusBarItem(TEpiVStatusBarItem_ProgressBar);

end.

