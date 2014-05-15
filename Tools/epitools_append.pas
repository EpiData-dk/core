unit epitools_append;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  epidocument,
  epidatafiles;

type

  TEpiToolWarningResult = (wrStop, wrContinue);
  TEpiToolAppendResult = (eapFailed, eapPartialSuccess, eapSuccess);

  TEpiToolAppendErrorEvent = procedure(Sender: TObject; Const Msg: string) of object;
  TEpiToolAppendWarningEvent = procedure(Sender: TObject; Const Msg: string;
    out Result: TEpiToolWarningResult) of object;

  { TEpiToolAppend }

  TEpiToolAppend = class
  private
    FDataFileNames: TStrings;
    FFieldNames: TStrings;
    FOnError: TEpiToolAppendErrorEvent;
    FOnWarning: TEpiToolAppendWarningEvent;
    procedure   DoError(Const Msg: String);
    procedure   DoWarning(Const Msg: String; Out WarningResult: TEpiToolWarningResult);
    procedure   SetOnError(AValue: TEpiToolAppendErrorEvent);
    procedure   SetOnWarning(AValue: TEpiToolAppendWarningEvent);
    function    Min(Const Res1, Res2: TEpiToolAppendResult): TEpiToolAppendResult;
  protected
    function    CompatabilityCheck(MainField, AppendField: TEpiField;
      out Msg: string): boolean; virtual;
    function    KeyFieldCheck(MainDataFile, AppendDataFile: TEpiDataFile;
      out Msg: string): boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Append(MainDocument, AppendDocument: TEpiDocument): TEpiToolAppendResult; overload;
    function    Append(MainDataFile, AppendDataFile: TEpiDataFile): TEpiToolAppendResult; overload;
  public
    // Events
    property    OnError: TEpiToolAppendErrorEvent read FOnError write SetOnError;
    property    OnWarning: TEpiToolAppendWarningEvent read FOnWarning write SetOnWarning;
    property    DataFileNames: TStrings read FDataFileNames;
    property    FieldNames: TStrings read FFieldNames;
  end;

implementation

uses
  epidatafileutils, epidatafilestypes, math;

{ TEpiToolAppend }

procedure TEpiToolAppend.SetOnError(AValue: TEpiToolAppendErrorEvent);
begin
  if FOnError = AValue then Exit;
  FOnError := AValue;
end;

procedure TEpiToolAppend.DoError(const Msg: String);
begin
  if Assigned(OnError) then
    OnError(Self, Msg);
end;

procedure TEpiToolAppend.DoWarning(const Msg: String; out
  WarningResult: TEpiToolWarningResult);
begin
  WarningResult := wrStop;

  if Assigned(OnWarning) then
    OnWarning(Self, Msg, WarningResult);
end;

procedure TEpiToolAppend.SetOnWarning(AValue: TEpiToolAppendWarningEvent);
begin
  if FOnWarning = AValue then Exit;
  FOnWarning := AValue;
end;

function TEpiToolAppend.Min(const Res1, Res2: TEpiToolAppendResult
  ): TEpiToolAppendResult;
var
  R1: Integer;
  R2: Integer;
begin
  R1 := Integer(Res1);
  R2 := Integer(Res2);

  result := TEpiToolAppendResult(Math.Min(R1, R2));;
end;

function TEpiToolAppend.CompatabilityCheck(MainField, AppendField: TEpiField;
  out Msg: string): boolean;
begin
  result := (MainField.FieldType = AppendField.FieldType);

  if (not Result) then
    Msg := Format(
      'Field %s (%s) and %s (%s) does not have the same type!',
      [MainField.Name, MainField.DataFile.Caption.Text,
       AppendField.Name, AppendField.DataFile.Caption.Text]);
end;

function TEpiToolAppend.KeyFieldCheck(MainDataFile,
  AppendDataFile: TEpiDataFile; out Msg: string): boolean;
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
  Result := false;

  if (MainDataFile.KeyFields.Count <> AppendDataFile.KeyFields.Count)
  then
    begin
      Msg :=
       'DataForm ' + MainDataFile.Caption.Text + 'and DataForm ' + AppendDataFile.Caption.Text +
         'does not have the same number of fields in Key!' + LineEnding +
       'Continuing will append remaining dataforms.' + LineEnding +
       LineEnding +
       'Continue?';

      Exit;
    end;

  if (MainDataFile.KeyFields.Count = 0) then
    Exit(true);

  for i := 0 to MainDataFile.KeyFields.Count - 1 do
  begin
    MKF := MainDataFile.KeyFields[i];
    AKF := AppendDataFile.KeyFields.FieldByName[MKF.Name];

    if not CompatabilityCheck(MKF, AKF, Msg) then
      Exit;
  end;

  try

    // TODO : Rewrite to NOT clone into owner, when merged with Related Datasets!
    MDF := TEpiDataFile(MainDataFile.Clone(MainDataFile.Owner));
    ADF := TEpiDataFile(AppendDataFile.Clone(AppendDataFile.Owner));

    MainSortField := MDF.NewField(ftInteger);
    AppendSortField := ADF.NewField(ftInteger);

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
        0:  begin
              Msg := 'Identical keys found!: ' + LineEnding +
                     'Main record no: ' + MainSortField.AsString[MainRunner] + LineEnding +
                     'Append record no: ' + AppendSortField.AsString[AppendRunner];
              Exit;
            end;
        1:  Inc(AppendRunner);
      end;
    end;
  finally
    MDF.Free;

    ADF.Free;
  end;

  Result := true;
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
  ): TEpiToolAppendResult;
var
  MDF: TEpiDataFile;
  ADF: TEpiDataFile;
  i: Integer;
  WRes: TEpiToolWarningResult;
  Msg: string;
begin
  Result := eapFailed;

  if (not Assigned(MainDocument)) then
  begin
    DoError('Main project not assigned!');
    Exit;
  end;

  if (not Assigned(AppendDocument)) then
  begin
    DoError('Append project not assigned!');
    Exit;
  end;

  if (MainDocument = AppendDocument) then
  begin
    DoError('Cannot append project to itself!');
    Exit;
  end;

  result := eapSuccess;
  for i := 0 to MainDocument.DataFiles.Count - 1 do
  begin
    MDF := MainDocument.DataFiles[i];
    ADF := AppendDocument.DataFiles[i];

    if (DataFileNames.Count > 0) and
       (DataFileNames.IndexOf(MDF.Name) < 0)
    then
      Continue;

    Result := Min(Result, Append(MDF, ADF));

    if (Result = eapFailed)
    then
      Exit;
  end;
end;

function TEpiToolAppend.Append(MainDataFile, AppendDataFile: TEpiDataFile
  ): TEpiToolAppendResult;
var
  MainField: TEpiField;
  AppendField: TEpiField;
  StartPos: Integer;
  i: Integer;
  j: Integer;
  Msg: string;
  WRes: TEpiToolWarningResult;
begin
  if not KeyFieldCheck(MainDataFile, AppendDataFile, Msg) then
  begin
    Msg := Msg + LineEnding +
           'Continuing will append remaining dataforms.' +
           LineEnding +
           'Continue?';
    DoWarning(Msg, WRes);
    case WRes of
      wrStop:
        Exit(eapFailed);
      wrContinue:
        Exit(eapPartialSuccess);
    end;
  end;

  Result := eapSuccess;

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
    if not Assigned(AppendField) then
      Continue;

    // Compatability Check!
    if not CompatabilityCheck(MainField, AppendField, Msg) then
    begin
      DoWarning(Msg, WRes);
      case WRes of
        wrStop:
          Exit(eapFailed);
        wrContinue:
          begin
            result := eapPartialSuccess;
            Continue;
          end;
      end;
    end;

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

