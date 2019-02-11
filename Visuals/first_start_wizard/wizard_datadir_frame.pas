unit wizard_datadir_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, EditBtn, StdCtrls, WizardControls, fpjson;

type

  { TWizardSelectDataDirFrame }

  TWizardSelectDataDirFrame = class(TFrame, IWizardPage)
    DirectoryEdit1: TDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure DirectoryEdit1Change(Sender: TObject);
    procedure DirectoryEdit1EditingDone(Sender: TObject);
  private
    FData: TJSONData;
    FWizardManager: IWizardManager;
    function DirectoryIsValid(): boolean;
  public
    procedure GetPageInfo(var PageInfo: TWizardPageInfo);
  published
    property WizardManager: IWizardManager read FWizardManager write FWizardManager;
    property Data: TJSONData write FData;
    procedure PageLoad();
    procedure PageHide();
  end;

implementation

uses
  LazFileUtils;
{$R *.lfm}

{ TWizardSelectDataDirFrame }

procedure TWizardSelectDataDirFrame.DirectoryEdit1EditingDone(Sender: TObject);
begin
  WizardManager.PageStateChanged;
end;

procedure TWizardSelectDataDirFrame.DirectoryEdit1Change(Sender: TObject);
begin
  WizardManager.PageStateChanged;
end;

function TWizardSelectDataDirFrame.DirectoryIsValid(): boolean;
begin
  result := true;
end;

procedure TWizardSelectDataDirFrame.GetPageInfo(var PageInfo: TWizardPageInfo);
begin
  if DirectoryIsValid() then
    Include(PageInfo.EnabledButtons, wbNext)
  else
    Exclude(PageInfo.EnabledButtons, wbNext);
end;

procedure TWizardSelectDataDirFrame.PageLoad();
begin
  DirectoryEdit1.Directory := FData.FindPath('DataDir').AsString;
end;

procedure TWizardSelectDataDirFrame.PageHide();
begin
  if DirectoryIsValid() then
    FData.FindPath('DataDir').AsString := DirectoryEdit1.Directory;
end;

end.

