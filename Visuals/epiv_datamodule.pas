unit epiv_datamodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, Dialogs,
  epimiscutils,
  epicustombase,
  epidatafiles,
  epidatafilestypes;

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
      Result := 1;
    ftInteger:
      Result := 2;
    ftFloat:
      Result := 3;
    ftDMYDate,
    ftMDYDate,
    ftYMDDate:
      Result := 4;
    ftTime:
      Result := 5;
    ftString,
    ftUpperString:
      Result := 6;
    ftAutoInc:
      Result := 7;
    ftDMYAuto,
    ftMDYAuto,
    ftYMDAuto:
      Result := 8;
    ftTimeAuto:
      Result := 9;
  end;
end;

function TDM.GetImageIndex(const ControlItem: TEpiCustomControlItem): Integer;
begin
  if ControlItem.InheritsFrom(TEpiSection) then
    Result := 10;

  if ControlItem.InheritsFrom(TEpiHeading) then
    Result := 11;

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

