unit wizard_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  {$IFDEF DARWIN}MacOSAll,{$ENDIF}
  WizardControls, fpjson;


type
  {$IFDEF DARWIN}
  TBundleURLFunction = function(bundle: CFBundleRef): CFURLRef mwpascal;
  {$ENDIF}

  { TInitializationWizard }

  TInitializationWizard = class(TForm)
    DescriptionLabel: TLabel;
    TitleLabel: TLabel;
    TopPanel: TPanel;
    WizardButtonPanel1: TWizardButtonPanel;
    WizardManager1: TWizardManager;
    procedure WizardManager1PageLoad(Sender: TObject; Page: TWizardPage);
    procedure WizardManager1PageShow(Sender: TObject; Page: TWizardPage);
    procedure WizardManager1PageStateChange(Sender: TObject; Page: TWizardPage);
  private
    FConfigFile: UTF8String;
    FData: TJSONObject;
    {$IFDEF DARWIN}
    function GetBundlePath(URLFunction: TBundleURLFunction): string;
    {$ENDIF}
  public
    constructor Create(TheOwner: TComponent; ConfigFile: UTF8String);
    destructor Destroy; override;
  end;

function CheckAndStartWizard(SettingFileName: UTF8String): boolean;

implementation

uses
  {$IFDEF WINDOWS}windirs,{$ENDIF}
  wizard_datadir_frame, wizard_welcome_page, wizard_summary_frame, LazFileUtils,
  LuiRTTIUtils, wizard_progress_frame, wizard_confirm_frame;

function CheckAndStartWizard(SettingFileName: UTF8String): boolean;
var
  wiz: TInitializationWizard;
begin
  if FileExistsUTF8(SettingFileName) then
    begin
      Result := true;
      Exit;
    end;

  wiz := TInitializationWizard.Create(nil, SettingFileName);
  result := (wiz.ShowModal = mrOK);
  wiz.Free;
end;

{$R *.lfm}

{ TInitializationWizard }

procedure TInitializationWizard.WizardManager1PageLoad(Sender: TObject;
  Page: TWizardPage);
begin
  SetObjectProperties(Page.Control, ['Data', FData]);
end;

procedure TInitializationWizard.WizardManager1PageShow(Sender: TObject;
  Page: TWizardPage);
begin
  TitleLabel.Caption := Page.Caption;
  DescriptionLabel.Caption := Page.Description;
end;

procedure TInitializationWizard.WizardManager1PageStateChange(Sender: TObject;
  Page: TWizardPage);
begin
  //
end;

{$IFDEF DARWIN}
function TInitializationWizard.GetBundlePath(URLFunction: TBundleURLFunction
  ): string;
var
  MainBundle: CFBundleRef;
  URLFunctionRef: CFURLRef;
  CFPath: CFStringRef;
  utf16len: CFIndex;
begin
  MainBundle := CFBundleGetMainBundle;

  if Assigned(MainBundle) then
    begin
      { get the URL pointing to the SharedSupport of the bundle }
      URLFunctionRef := URLFunction(MainBundle);

      if Assigned(URLFunctionRef) then
        begin
          { convert the url to a POSIX path }
          CFPath := CFURLCopyFileSystemPath(URLFunctionRef, kCFURLPOSIXPathStyle);

          { convert to UTF-8 -- this is not really clean since in theory the
            ansi-encoding could be different, but
              a) all file i/o routines on Darwin expect utf-8-encoded strings
              b) there is no easy way to convert the Unix LANG encoding
                 setting to an equivalent CoreFoundation encoding
          }
          utf16len := CFStringGetLength(CFPath);

          // +1 for extra terminating #0 in the worst case, so the pos below
          // will always find the #0
          setlength(result, utf16len * 3 + 1);

          if CFStringGetCString(CFPath, @result[1], length(result), kCFStringEncodingUTF8) then
            { truncate to actual length, #0 cannot appear in a file path }
            setlength(result, pos(#0, result) - 1)
          else
            result := '';
          CFRelease(CFPath);
        end
      else
       result := '';
    end
  else
    result := '';

  WriteLn('GetBundlePath: ' + Result);
end;
{$ENDIF}

constructor TInitializationWizard.Create(TheOwner: TComponent;
  ConfigFile: UTF8String);
var
  DataDir, DocsDir, Examplesdir, Path: String;
begin
  inherited Create(TheOwner);

  {$IF     defined(LINUX)}
  DataDir      := GetUserDir + 'EpiData/';
  DocsDir      := '/usr/share/doc/' + OnGetApplicationName() + DirectorySeparator;
  ExamplesDir  := '/usr/share/' + OnGetApplicationName() + DirectorySeparator;
  {$ELSEIF defined(DARWIN)}
  DataDir      := GetUserDir + 'EpiData' + DirectorySeparator;
  Path := GetBundlePath(@CFBundleCopyBundleURL) + DirectorySeparator + GetBundlePath(@CFBundleCopySharedSupportURL);
  DocsDir      := Path + DirectorySeparator + 'docs' + DirectorySeparator;
  ExamplesDir  := Path + DirectorySeparator + 'examples' + DirectorySeparator;
  {$ELSE   define(MSWINDOWS)}
  DataDir      := GetWindowsSpecialDir(CSIDL_PERSONAL) + 'EpiData' + DirectorySeparator;
  DocsDir      := Application.Location + DirectorySeparator + 'docs' + DirectorySeparator;
  ExamplesDir  := Application.Location + DirectorySeparator + 'examples' + DirectorySeparator;
  {$ENDIF}

  FData := TJSONObject.Create(
             ['DataDir', DataDir,
              'DocsDir', DocsDir,
              'ExamplesDir', ExamplesDir,
              'ConfigFile', ConfigFile,
              'IsAnalysis', OnGetApplicationName() = 'epidataanalysis'
             ]
           );

  WizardManager1.PageByName('WelcomePage').ControlClass    := TWizardWelcomeFrame;
  WizardManager1.PageByName('DataDirPage').ControlClass    := TWizardSelectDataDirFrame;
  WizardManager1.PageByName('BeforeCopyPage').ControlClass := TWizardConfirmActionsFrame;
  WizardManager1.PageByName('ProgressPage').ControlClass   := TWizardProgressFrame;
  WizardManager1.PageByName('SummaryPage').ControlClass    := TWizardSummaryFrame;
  WizardManager1.PageIndex := 0;
end;

destructor TInitializationWizard.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

end.

