unit epitools_append;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  epidocument,
  epidatafiles;

type

  TEpiToolAppendErrorEvent = function(): boolean of object;

  { TEpiToolAppend }

  TEpiToolAppend = class
  private
    FDataFileNames: TStrings;
    FFieldNames: TStrings;
    FOnError: TNotifyEvent;
    procedure SetOnError(AValue: TNotifyEvent);
    procedure DoError();
  protected
    function    CompatabilityCheck(MainField, AppendField: TEpiField): boolean; virtual;
    function    KeyFieldCheck(MainDataFile, AppendDataFile: TEpiDataFile): boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Append(MainDocument, AppendDocument: TEpiDocument): boolean; overload;
    function    Append(MainDataFile, AppendDataFile: TEpiDataFile): boolean; overload;
  public
    // Events
    property    OnError: TNotifyEvent read FOnError write SetOnError;
    property    DataFileNames: TStrings read FDataFileNames;
    property    FieldNames: TStrings read FFieldNames;
  end;

implementation

uses
  epidatafileutils, epidatafilestypes, math;

{ TEpiToolAppend }

procedure TEpiToolAppend.SetOnError(AValue: TNotifyEvent);
begin
  if FOnError = AValue then Exit;
  FOnError := AValue;
end;

procedure TEpiToolAppend.DoError;
begin

end;

function TEpiToolAppend.CompatabilityCheck(MainField, AppendField: TEpiField
  ): boolean;
begin
  result := (MainField.FieldType = AppendField.FieldType);
end;

function TEpiToolAppend.KeyFieldCheck(MainDataFile, AppendDataFile: TEpiDataFile
  ): boolean;
var
  MDF: TEpiDataFile;
  ADF: TEpiDataFile;
  MainRunner: Integer;
  AppendRunner: Integer;
  i: Integer;
  MKF: TEpiField;
  AKF: TEpiField;
  MainSortField: TEpiField;
  AppendSortField: TEpiField;


  function CompareKeyFields(Const Idx1, Idx2: Integer): TValueSign;
  var
    i: Integer;
    MKF: TEpiField;
    AKF: TEpiField;
  begin
    for i := 0 to MDF.KeyFields.Count -1 do
    begin
      MKF := MDF.KeyFields[i];
      AKF := ADF.KeyFields.FieldByName[MKF.Name];

      CompareFieldRecords(Result, MKF, AKF, Idx1, Idx2,
        MKF.FieldType = ftString);

      if Result <> 0 then exit;
    end;
  end;

begin
  if (MainDataFile.KeyFields.Count <> AppendDataFile.KeyFields.Count)
  then
    DoError();


  for i := 0 to MainDataFile.KeyFields.Count - 1 do
  begin
    MKF := MainDataFile.KeyFields[i];
    AKF := AppendDataFile.KeyFields.FieldByName[MKF.Name];

    if not CompatabilityCheck(MKF, AKF) then
      DoError();
  end;

  MDF := TEpiDataFile(MainDataFile.Clone(nil));
  ADF := TEpiDataFile(AppendDataFile.Clone(nil));

  MainSortField := TEpiField.CreateField(nil, ftInteger);
  AppendSortField := TEpiField.CreateField(nil, ftInteger);

  for i := 0 to MainDataFile.Size - 1 do
    MainSortField.AsInteger[i] := i;

  for i := 0 to AppendDataFile.Size - 1 do
    AppendSortField.AsInteger[i] := i;


  MDF.SortRecords(MDF.KeyFields);
  ADF.SortRecords(ADF.KeyFields);

  MainRunner := 0;
  AppendRunner := 0;

  while (MainRunner < MDF.Size) and (AppendRunner < ADF.Size) do
  begin
    case CompareKeyFields(MainRunner, AppendRunner) of
      -1: Inc(MainRunner);
      0:  DoError();
      1:  Inc(AppendRunner);
    end;
  end;
end;

constructor TEpiToolAppend.Create;
begin
  FDataFileNames := TStringList.Create;
  FFieldNames    := TStringList.Create;
end;

destructor TEpiToolAppend.Destroy;
begin
  FFieldNames.Free;
  FDataFileNames.Free;
  inherited Destroy;
end;

function TEpiToolAppend.Append(MainDocument, AppendDocument: TEpiDocument
  ): boolean;
var
  DF: TEpiDataFile;
  i: Integer;
begin
  if (MainDocument = AppendDocument) then
    DoError();

  if (not Assigned(MainDocument)) then
    DoError();

  if (not Assigned(AppendDocument)) then
    DoError();

  for i := 0 to MainDocument.DataFiles.Count - 1 do
  begin
    DF := MainDocument.DataFiles[i];

    if (DataFileNames.Count > 0) and
       (DataFileNames.IndexOf(DF.Name) < 0)
    then
      Continue;

    Append(DF, AppendDocument.DataFiles[i]);
  end;
end;

function TEpiToolAppend.Append(MainDataFile, AppendDataFile: TEpiDataFile
  ): boolean;
var
  MainField: TEpiField;
  AppendField: TEpiField;
  StartPos: Integer;
  i: Integer;
  j: Integer;
begin
  StartPos := MainDataFile.Size;
  MainDataFile.Size := MainDataFile.Size + AppendDataFile.Size;

  for i := 0 to MainDataFile.Fields.Count -1  do
  begin
    MainField := MainDataFile.Field[i];

    if (FieldNames.Count > 0) and
       (FieldNames.IndexOf(MainField.Name) < 0)
    then
      Continue;

    AppendField := AppendDataFile.Fields.FieldByName[MainField.Name];

    // Compatability Check!
    if not CompatabilityCheck(MainField, AppendField) then
      DoError();

    // Seperate ftFloat, because converting to variant causes trouble.
    if MainField.FieldType = ftFloat then
      for j := 0 to AppendDataFile.Size - 1 do
        MainField.AsFloat[j + StartPos] := AppendField.AsFloat[j]
    else
      for j := 0 to AppendDataFile.Size - 1 do
        MainField.AsValue[j + StartPos] := AppendField.AsValue[j];
  end;
end;

end.

