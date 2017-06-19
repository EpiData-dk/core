unit epiv_statusbar_item_progressbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiv_custom_statusbar, epicustombase, ComCtrls, epidocument;

type

  { TEpiVStatusBarItem_ProgressBar }

  TEpiVStatusBarItem_ProgressBar = class(TEpiVCustomStatusBarItem)
  private
    // Progressbare housekeeping
    FMaxPosition: Integer;
    FProgressIncrement: Integer;
    FLastUpdate: Integer;
  private
    FProgressbar: TProgressBar;
    procedure InternalProgress(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
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
  Controls, Forms, episervice_asynchandler;

{ TEpiVStatusBarItem_ProgressBar }

procedure TEpiVStatusBarItem_ProgressBar.InternalProgress(
  const Sender: TEpiCustomBase; const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  CurrentPos: Integer;
begin
  case TEpiXMLProgressEvent(EventType) of
    expeInit:
      begin
        Application.ProcessMessages;

        FMaxPosition := Integer(Data);

        if (FMaxPosition > 500) then
          FProgressIncrement := FMaxPosition div 50
        else
          FProgressIncrement := 1;

        FLastUpdate := 0;
        FProgressbar.Position := 0;
        FProgressbar.Max := FMaxPosition;

        Visible := true;

        if not (csDestroying in Panel.ComponentState) then
          Application.ProcessMessages;
      end;

    expeProgressStep:
      begin
        CurrentPos := Integer(Data);
        if CurrentPos > (FLastUpdate + FProgressIncrement) then
        begin
          FProgressbar.Position := CurrentPos;
          {$IFNDEF MSWINDOWS}
          Application.ProcessMessages;
          {$ENDIF}
          FLastUpdate := CurrentPos;
        end;
      end;

    expeDone:
      begin
        if not (csDestroying in Panel.ComponentState) then
          Application.ProcessMessages;

        FProgressbar.Position := FProgressbar.Max;
        Visible := false;
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
      Visible := false;
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

  EpiAsyncHandlerGlobal.RegisterAsyncHandler(@InternalProgress, eegXMLProgress, Word(expeInit), [esatMain]);
  EpiAsyncHandlerGlobal.RegisterAsyncHandler(@InternalProgress, eegXMLProgress, Word(expeProgressStep), [esatMain]);
  EpiAsyncHandlerGlobal.RegisterAsyncHandler(@InternalProgress, eegXMLProgress, Word(expeDone), [esatMain]);
end;

destructor TEpiVStatusBarItem_ProgressBar.Destroy;
begin
  EpiAsyncHandlerGlobal.UnRegisterAsyncHandler(@InternalProgress, eegXMLProgress, Word(expeInit));
  EpiAsyncHandlerGlobal.UnRegisterAsyncHandler(@InternalProgress, eegXMLProgress, Word(expeProgressStep));
  EpiAsyncHandlerGlobal.UnRegisterAsyncHandler(@InternalProgress, eegXMLProgress, Word(expeDone));

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

