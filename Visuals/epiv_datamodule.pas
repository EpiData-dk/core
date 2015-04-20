unit epiv_datamodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, Dialogs,
  epimiscutils,
  epicustombase,
  epidatafiles,
  epidatafilestypes;

const
  // ImageIndixes:
  epiv_iiPointer               = 0;
  epiv_iiBoolean               = 1;
  epiv_iiInteger               = 2;
  epiv_iiFloat                 = 3;
  epiv_iiDate                  = 4;
  epiv_iiString                = 5;
  epiv_iiTime                  = 6;
  epiv_iiAutoInc               = 7;
  epiv_iiAutoDate              = 8;
  epiv_iiAutoTime              = 9;
  epiv_iiSection               = 10;
  epiv_iiHeading               = 11;
  epiv_iiOther                 = 12;
  epiv_iiEdit                  = 13;
  epiv_iiExtender              = 14;
  epiv_iiDelete                = 15;
  epiv_iiErase                 = 16;
  epiv_iiExport                = 17;
  epiv_iiImport                = 18;
  epiv_iiOpenFile              = 19;
  epiv_iiSaveFile              = 20;
  epiv_iiSaveFileAs            = 21;
  epiv_iiPrint                 = 22;
  epiv_iiAlignCenterHorz       = 23;
  epiv_iiAlignCenterVert       = 24;
  epiv_iiAlignLeft             = 25;
  epiv_iiAlignRight            = 26;
  epiv_iiAlignEvenDistVert     = 27;
  epiv_iiAlignEvenDistHorz     = 28;
  epiv_iiAlignTop              = 29;
  epiv_iiAlignBottom           = 30;
  epiv_iiAddElem               = 31;
  epiv_iiRemoveElem            = 32;
  epiv_iiMoveElemRight         = 33;
  epiv_iiMoveElemLeft          = 34;
  epiv_iiMoveElemUp            = 35;
  epiv_iiMoveElemDown          = 36;
  epiv_iiEpiDataIcon           = 37;
  epiv_iiAnalysisIcon          = 38;
  epiv_iiEntryClientIcon       = 39;
  epiv_iiManagerIcon           = 40;
  epiv_iiNewProject            = 41;
  epiv_iiClose                 = 42;
  epiv_iiDocument              = 43;
  epiv_iiDataform              = 44;
  epiv_iiProjectDetails        = 45;
  epiv_iiGarbageBin            = 46;

type

  { TDM }

  TDM = class(TDataModule)
    Icons16: TImageList;
    MainOpenDialog: TOpenDialog;
  private
    { private declarations }
  public
    { Images }
    function GetImageIndex(Const Ft: TEpiFieldType): Integer;
    function GetImageIndex(Const ControlItem: TEpiCustomControlItem): Integer;
  public
    { Open/Import files }
    function OpenDlgFile(Filters: TEpiDialogFilters; Out FileName: string): boolean;
    function OpenDlgFiles(Filters: TEpiDialogFilters; var FileNames: TStrings): boolean;
    function OpenDlgEpiFile(Out FileName: string): boolean;
    function OpenDlgEpiFiles(var FileNames: TStrings): boolean;
    function OpenDlgImportFile(Out FileName: string): boolean;
    function OpenDlgImportFiles(var FileNames: TStrings): boolean;
  end;

function DM: TDM;

implementation

var
  aDM: TDM;

function DM: TDM;
begin
  Result := aDM;
end;

{$R *.lfm}

{ TDM }

function TDM.GetImageIndex(const Ft: TEpiFieldType): Integer;
begin
  Result := -1;

  case Ft of
    ftBoolean:
      Result := epiv_iiBoolean;
    ftInteger:
      Result := epiv_iiInteger;
    ftFloat:
      Result := epiv_iiFloat;
    ftDMYDate,
    ftMDYDate,
    ftYMDDate:
      Result := epiv_iiDate;
    ftTime:
      Result := epiv_iiTime;
    ftString,
    ftUpperString:
      Result := epiv_iiString;
    ftAutoInc:
      Result := epiv_iiAutoInc;
    ftDMYAuto,
    ftMDYAuto,
    ftYMDAuto:
      Result := epiv_iiAutoDate;
    ftTimeAuto:
      Result := epiv_iiAutoTime;
  end;
end;

function TDM.GetImageIndex(const ControlItem: TEpiCustomControlItem): Integer;
begin
  if ControlItem.InheritsFrom(TEpiSection) then
    Result := epiv_iiSection;

  if ControlItem.InheritsFrom(TEpiHeading) then
    Result := epiv_iiHeading;

  if ControlItem.InheritsFrom(TEpiField) then
    Result := GetImageIndex(TEpiField(ControlItem).FieldType);
end;

function TDM.OpenDlgFile(Filters: TEpiDialogFilters; out FileName: string
  ): boolean;
begin
  MainOpenDialog.Options := MainOpenDialog.Options - [ofAllowMultiSelect];
  MainOpenDialog.Filter := GetEpiDialogFilter(Filters);
  Result := MainOpenDialog.Execute;
  FileName := MainOpenDialog.FileName;
end;

function TDM.OpenDlgFiles(Filters: TEpiDialogFilters; var FileNames: TStrings
  ): boolean;
begin
  if not Assigned(FileNames) then
    Filenames := TStringList.Create;

  MainOpenDialog.Options := MainOpenDialog.Options + [ofAllowMultiSelect];
  MainOpenDialog.Filter := GetEpiDialogFilter(Filters);
  Result := MainOpenDialog.Execute;

  FileNames.Assign(MainOpenDialog.Files);
end;

function TDM.OpenDlgEpiFile(out FileName: string): boolean;
begin
  result := OpenDlgFile(dfEpiData, FileName);
end;

function TDM.OpenDlgEpiFiles(var FileNames: TStrings): boolean;
begin
  result := OpenDlgFiles(dfEpiData, FileNames);
end;

function TDM.OpenDlgImportFile(out FileName: string): boolean;
begin
  result := OpenDlgFile(dfImport, FileName);
end;

function TDM.OpenDlgImportFiles(var FileNames: TStrings): boolean;
begin
  result := OpenDlgFiles(dfImport, FileNames);
end;

procedure InitDM;
begin
  if not Assigned(aDM) then
    aDM := TDM.Create(nil);
end;

procedure FinalizeDM;
begin
  aDM.Free;
end;

initialization
  InitDM;

finalization
  FinalizeDM;

end.

