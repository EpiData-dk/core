unit epiv_statusbar_item_savetime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiv_custom_statusbar, StdCtrls, fpTimer;

type

  { TEpiVStatusBarItem_SaveTimer }

  TEpiVStatusBarItem_SaveTimer = class(TEpiVCustomStatusBarItem)
    procedure TimerUpdate(Sender: TObject);
  private
    FLastSave: TDateTime;
    FTimer: TFPTimer;
    FLabel: TLabel;
    FTimerLabel: TLabel;
    procedure DoUpdate;
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
  Controls;

{ TEpiVStatusBarItem_SaveTimer }

procedure TEpiVStatusBarItem_SaveTimer.TimerUpdate(Sender: TObject);
begin
  DoUpdate;
  FTimerLabel.Invalidate;
end;

procedure TEpiVStatusBarItem_SaveTimer.DoUpdate;
var
  DiffTime: TDateTime;
begin
  DiffTime := (Now - FLastSave);
  FTimerLabel.Caption := FormatDateTime('HH:MM:SS', DiffTime);
end;

procedure TEpiVStatusBarItem_SaveTimer.Update(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  inherited Update(Condition);

  case Condition of
    sucDefault: ;
    sucDocFile:
      begin
        FTimer.StartTimer;
        FLastSave := Now;
      end;
    sucDataFile: ;
    sucSelection: ;
    sucSave: FLastSave := Now;
  end;
end;

class function TEpiVStatusBarItem_SaveTimer.Caption: string;
begin
  Result := 'Time since last save';
end;

class function TEpiVStatusBarItem_SaveTimer.Name: string;
begin
  result := 'LastSaved';
end;

constructor TEpiVStatusBarItem_SaveTimer.Create(AStatusBar: TEpiVCustomStatusBar
  );
begin
  inherited Create(AStatusBar);
  FTimer := TFPTimer.Create(Panel);
  FTimer.Interval := 500;
  FTimer.OnTimer := @TimerUpdate;

  FLabel := TLabel.Create(Panel);
  FLabel.AnchorParallel(akLeft, 2, Panel);
  FLabel.AnchorVerticalCenterTo(Panel);
  FLabel.Caption := 'Last Saved: ';
  FLabel.Parent := Panel;

  FTimerLabel := TLabel.Create(Panel);
  FTimerLabel.AnchorToNeighbour(akLeft, 2, FLabel);
  FTimerLabel.AnchorVerticalCenterTo(Panel);
  FTimerLabel.Caption := '';
  FTimerLabel.Parent := Panel;
end;

destructor TEpiVStatusBarItem_SaveTimer.Destroy;
begin
  FTimer.StopTimer;
  inherited Destroy;
end;

function TEpiVStatusBarItem_SaveTimer.GetPreferedWidth: Integer;
begin
  if not Panel.HandleAllocated then
    begin
      Result := inherited GetPreferedWidth;
      Exit;
    end;

  Result := FTimerLabel.Left + FTimerLabel.Width + 2;
end;

initialization
  EpiV_RegisterCustomStatusBarItem(TEpiVStatusBarItem_SaveTimer);


end.

