unit wizard_progress_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, WizardControls, fpjson,
  FileUtil;

type

  { TWizardProgressFrame }

  TWizardProgressFrame = class(TFrame, IWizardPage)
    Label1: TLabel;
    ProgressBar1: TProgressBar;
  private
    FFileCount: Integer;
    FCompleted: boolean;
    FData: TJSONObject;
    FWizardManager: IWizardManager;
    procedure FileCopy(FileIterator: TFileIterator);
    procedure FileCountInc(FileIterator: TFileIterator);
  public
    procedure GetPageInfo(var PageInfo: TWizardPageInfo);
  published
    property WizardManager: IWizardManager read FWizardManager write FWizardManager;
    property Data: TJSONObject read FData write FData;
    procedure PageLoad();
    procedure PageShow();
  end;

implementation

uses
  IniFiles, Dialogs, Math, LazFileUtils;

{$R *.lfm}

{ TWizardProgressFrame }

procedure TWizardProgressFrame.FileCountInc(FileIterator: TFileIterator);
begin
  Inc(FFileCount);
end;

procedure TWizardProgressFrame.FileCopy(FileIterator: TFileIterator);
begin
  CopyFile(FileIterator.FileName, Data.Find('DataDir').AsString + 'examples' + DirectorySeparator + FileIterator.FileInfo.Name, [cffCreateDestDirectory, cffPreserveTime]);
  ProgressBar1.StepIt;
  Application.ProcessMessages;
end;

procedure TWizardProgressFrame.GetPageInfo(var PageInfo: TWizardPageInfo);
begin
  if FCompleted then
    Include(PageInfo.EnabledButtons, wbNext)
  else
    Exclude(PageInfo.EnabledButtons, wbNext);
end;

procedure TWizardProgressFrame.PageLoad();
begin
  FFileCount := 0;
  FCompleted := false;
  WizardManager.PageStateChanged;
end;

procedure TWizardProgressFrame.PageShow();
var
  ConfigFile: UTF8String;
  IniFile: TIniFile;
  Lines: TStringList;
  FS: TFileSearcher;
  S: TJSONStringType;
begin
  // Create the configuration file:
  Label1.Caption := 'Creating configuration file:';
  Application.ProcessMessages;
  ConfigFile := Data.Find('ConfigFile').AsString;

  if (Data.Find('IsAnalysis').AsBoolean) then
    begin
      Lines := TStringList.Create;
      Lines.Append('cd "' + Data.Find('DataDir').AsString + '";');

      S := 'set "TUTORIAL FOLDER" := "' + Data.Find('DocsDir').AsString + '";';
      if (not DirectoryExistsUTF8(Data.Find('DocsDir').AsString)) then
        begin
          Lines.Append('// Remove the "//" on the next line when the path has been corrected');
          S := '// ' + S;
        end;
      Lines.Append(S);

      Lines.Append('cls;');
      Lines.SaveToFile(ConfigFile);
      Lines.Free;
    end
  else
    begin
      IniFile := TIniFile.Create(ConfigFile);
      IniFile.WriteString('advanced', 'WorkingDirectory', Data.Find('DataDir').AsString);
      IniFile.WriteString('advanced', 'TutorialDirectory', Data.Find('DocsDir').AsString);
      IniFile.UpdateFile;
      IniFile.Free;
    end;

  try
    Label1.Caption := 'Locating files to copy:';
    ProgressBar1.Style := pbstMarquee;
    Application.ProcessMessages;

    FS := TFileSearcher.Create;
    FS.OnFileFound := @FileCountInc;
    FS.Search(Data.Find('ExamplesDir').AsString, '*.*');
    FS.Free;

    Label1.Caption := 'Copying files:';
    ProgressBar1.Style := pbstNormal;
    ProgressBar1.Min := 0;
    ProgressBar1.Max := Math.Max(1, FFileCount - 1);
    ProgressBar1.Step := 1;
    Application.ProcessMessages;

    FS := TFileSearcher.Create;
    FS.OnFileFound := @FileCopy;
    FS.Search(Data.Find('ExamplesDir').AsString, '*.*');
    FS.Free;

    Label1.Caption := 'Completed successfully!';
    ProgressBar1.Position := ProgressBar1.Max;
    FCompleted := true;
  except
    Label1.Caption := 'Copying failed!'
  end;

  Application.ProcessMessages;
  WizardManager.PageStateChanged;
end;

end.

