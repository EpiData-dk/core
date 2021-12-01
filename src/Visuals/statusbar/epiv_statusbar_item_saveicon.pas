unit epiv_statusbar_item_saveicon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiv_custom_statusbar, epicustombase, epidocument,
  ExtCtrls;

type

  { TEpiVStatusBarItem_SavingIcon }

  TEpiVStatusBarItem_SavingIcon = class(TEpiVCustomStatusBarItem)
  private
    FSavingIcon: TShape;
    procedure ProgressHook(const Sender: TEpiCustomBase;
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
  Forms, Controls, Graphics, episervice_asynchandler, Dialogs, dateutils;

{ TEpiVStatusBarItem_SavingIcon }

procedure TEpiVStatusBarItem_SavingIcon.ProgressHook(
  const Sender: TEpiCustomBase; const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (not Assigned(Statusbar.DocFile)) then exit;

  case TEpiXMLProgressEvent(EventType) of
    expeInit:
      begin
        FSavingIcon.Brush.Color := clYellow;
        Application.ProcessMessages;
      end;

    expeDone:
      FSavingIcon.Brush.Color := clGreen;

    expeError:
      begin
        FSavingIcon.Brush.Color := clRed;
        Application.ProcessMessages;
      end;
  end;
end;

procedure TEpiVStatusBarItem_SavingIcon.Update(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  inherited Update(Condition);

  case Condition of
    sucDefault: ;
    sucDocFile:
      FSavingIcon.Brush.Color := clGreen;
    sucDataFile: ;
    sucSelection: ;
    sucSave: ;
    sucExample: ;
  end;
end;

class function TEpiVStatusBarItem_SavingIcon.Caption: string;
begin
  Result := 'Saving Indicator';
end;

class function TEpiVStatusBarItem_SavingIcon.Name: string;
begin
  result := 'SaveIcon';
end;

constructor TEpiVStatusBarItem_SavingIcon.Create(
  AStatusBar: TEpiVCustomStatusBar);
begin
  inherited Create(AStatusBar);

  FSavingIcon := TShape.Create(Panel);
  FSavingIcon.Shape := stCircle;
  FSavingIcon.Align := alClient;
  FSavingIcon.BorderSpacing.Around := 2;
  FSavingIcon.Parent := Panel;

  EpiAsyncHandlerGlobal.RegisterAsyncHandler(@ProgressHook, eegXMLProgress, Word(expeInit));
  EpiAsyncHandlerGlobal.RegisterAsyncHandler(@ProgressHook, eegXMLProgress, Word(expeDone));
  EpiAsyncHandlerGlobal.RegisterAsyncHandler(@ProgressHook, eegXMLProgress, Word(expeError));
end;

destructor TEpiVStatusBarItem_SavingIcon.Destroy;
begin
  EpiAsyncHandlerGlobal.UnRegisterAsyncHandler(@ProgressHook, eegXMLProgress, Word(expeInit));
  EpiAsyncHandlerGlobal.UnRegisterAsyncHandler(@ProgressHook, eegXMLProgress, Word(expeDone));
  EpiAsyncHandlerGlobal.UnRegisterAsyncHandler(@ProgressHook, eegXMLProgress, Word(expeError));

  inherited Destroy;
end;

function TEpiVStatusBarItem_SavingIcon.GetPreferedWidth: Integer;
begin
  if not Panel.HandleAllocated then
    begin
      Result := inherited GetPreferedWidth;
      Exit;
    end;

  Result := 24
end;

initialization
  EpiV_RegisterCustomStatusBarItem(TEpiVStatusBarItem_SavingIcon);

end.

