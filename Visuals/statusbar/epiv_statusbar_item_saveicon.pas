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
    FTimeStamp: TDateTime;
    FSavingIcon: TShape;
    FOnOldProgress: TEpiProgressEvent;
    procedure InternalProgress(const Sender: TEpiCustomBase;
      ProgressType: TEpiProgressType; CurrentPos, MaxPos: Cardinal;
      var Canceled: Boolean);
    procedure UpdateDocFile;
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

procedure TEpiVStatusBarItem_SavingIcon.InternalProgress(
  const Sender: TEpiCustomBase; ProgressType: TEpiProgressType; CurrentPos,
  MaxPos: Cardinal; var Canceled: Boolean);
begin
{//  if Assigned(FOnOldProgress) then
//    FOnOldProgress(Sender, ProgressType, CurrentPos, MaxPos, Canceled);

  case ProgressType of
    eptInit:
      if (Sender = Statusbar.DocFile.Document) then
        begin
          FSavingIcon.Brush.Color := clRed;
          Application.ProcessMessages;
        end
      else
        FSavingIcon.Brush.Color := clYellow;

    eptFinish:
      FSavingIcon.Brush.Color := clGreen;

    eptRecords: ;
  end;     }
end;

procedure TEpiVStatusBarItem_SavingIcon.UpdateDocFile;
begin
  if Assigned(Statusbar.DocFile) then
    begin
//      FOnOldProgress := Statusbar.DocFile.OnProgress;
//      Statusbar.DocFile.Document.RegisterOnChangeHook(@ProgressHook);
//      Statusbar.DocFile.OnProgress := @InternalProgress;
      EpiAsyncHandlerGlobal.RegisterAsyncHandler(@ProgressHook, eegXMLProgress, Word(expeInit));
      EpiAsyncHandlerGlobal.RegisterAsyncHandler(@ProgressHook, eegXMLProgress, Word(expeDone));
    end;
end;

procedure TEpiVStatusBarItem_SavingIcon.ProgressHook(
  const Sender: TEpiCustomBase; const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup <> eegXMLProgress) then exit;

  if (not Assigned(FSavingIcon)) then exit;

  case TEpiXMLProgressEvent(EventType) of
    expeInit:
      begin
        if (Sender = Statusbar.DocFile.Document) then
          begin
            FSavingIcon.Brush.Color := clRed;
            Application.ProcessMessages;
          end
        else
          FSavingIcon.Brush.Color := clYellow;
        FTimeStamp := Now;
      end;

    expeDone:
      begin
        FSavingIcon.Brush.Color := clGreen;
//        if SecondOf(FTimeStamp - Now) > 0 then
//          ShowMessage('Save Time: ' + FormatDateTime('SS:ZZZZ', FTimeStamp - Now));
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
      UpdateDocFile;

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
end;

destructor TEpiVStatusBarItem_SavingIcon.Destroy;
begin
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

