unit epireport_report_countbyid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, epireport_base,
  epidatafiles, epireport_generator_base, epiopenfile;

type

  { TEpiReportCountById }

  TEpiReportCBIDSumStatCallback = procedure(UniqueObs, CombinedObs: Integer;
    MissingObs: TBoundArray; ResultDF: TEpiDataFile;
    CountFields: TEpiFields) of object;

  TEpiReportCBIDOption = (
    ercoShowSumstats,
    ercoShowDetailList
  );
  TEpiReportCBIDOptions = set of TEpiReportCBIDOption;

  TEpiReportCountById = class(TEpiReportBase)
  private
    ResultDF: TEpiDataFile;
    ValueFieldList: TEpiFields;
    CountsFieldList: TEpiFields;
    procedure DoCounts(Const DF: TEpiDataFile; Const DFIndex: integer);
    procedure DoSumStats;
    procedure DoReport;
    function GetDataFileIndexInFileList(Const DataFile: TEpiDataFile): PtrInt;
  public
    constructor Create(ReportGenerator: TEpiReportGeneratorBase);
       override;
    destructor  Destroy; override;
    procedure   DoSanityCheck; override;
    procedure   RunReport; override;
  private
    FDataFiles: TEpiDataFiles;
    FFieldNames: TStrings;
    FDocumentFiles: TEpiDocumentFileList;
    FOnSumStatsComplete: TEpiReportCBIDSumStatCallback;
    FOptions: TEpiReportCBIDOptions;
  public
    property    DataFiles: TEpiDataFiles read FDataFiles write FDataFiles;
    property    FieldNames: TStrings read FFieldNames write FFieldNames;
    // If Assigned, the report will use the files attached to include a line in the report stating the file no. a given DataFile was in.
    property    DocumentFiles: TEpiDocumentFileList read FDocumentFiles write FDocumentFiles;
    property    Options: TEpiReportCBIDOptions read FOptions write FOptions;
    property    OnSumStatsComplete: TEpiReportCBIDSumStatCallback read FOnSumStatsComplete write FOnSumStatsComplete;
  end;


implementation

uses
  epimiscutils, epidatafilestypes, epidocument, epidatafileutils,
  lazutf8sysutils, epireport_types;

resourcestring
  rsDatafilesNotAssigned = 'DataFiles not assigned to report';
  rsFieldlistNotAssigned = 'Field names not assigned to report';

const
  COUNT_BY_ID_DOCUMENTFILE_KEY = 'COUNT_BY_ID_DOCUMENTFILE_KEY';

{ TEpiReportCountById }

procedure TEpiReportCountById.DoCounts(const DF: TEpiDataFile;
  const DFIndex: integer);
var
  OrgSortingField: TEpiField;
  LocalValueFields: TEpiFields;
  F: TEpiField;
  i: Integer;
  Index: integer;
  j: Integer;

  function FindIndexInResultDF(Const DFIndex: integer;
    out Index: integer): boolean;
  var
    i, j: integer;
    CmpResult: TValueSign;
    F: TEpiField;
  begin
    for i := 0 to ResultDF.Size - 1 do
    begin
      Index := i;

      for j := 0 to LocalValueFields.Count - 1 do
      begin
        F := ValueFieldList.FieldByName[LocalValueFields[j].Name];
        CompareFieldRecords(CmpResult, F, LocalValueFields[j], i, DFIndex);
        if CmpResult <> 0 then break;
      end;

      if CmpResult = ZeroValue then Exit(true);
    end;
    Index := -1;
    Result := false;
  end;

begin
  // Create a field for restoring old sorting.
  OrgSortingField := DF.NewField(ftInteger);
  for i := 0 to OrgSortingField.Size - 1 do
    OrgSortingField.AsInteger[i] := i;

  // Fetch list of field in current DF with same names as in user specified
  // field list.
  LocalValueFields := TEpiFields.Create(nil);
  for i := 0 to FieldNames.Count - 1 do
  begin
    F := DF.Fields.FieldByName[FieldNames[i]];
    if Assigned(F) then
      LocalValueFields.AddItem(F);
  end;

  DF.SortRecords(LocalValueFields);

  for i := 0 to DF.Size - 1 do
  begin
    if (not FindIndexInResultDF(i, Index)) then
    begin
      ResultDF.NewRecords();
      Index := ResultDF.Size - 1;
      for j := 0 to LocalValueFields.Count - 1 do
      begin
        F := ValueFieldList.FieldByName[LocalValueFields[j].Name];
        if Assigned(F) then
          F.AsString[ResultDf.Size - 1] := LocalValueFields[j].AsString[i]
      end;
    end;

    if CountsFieldList[DFIndex].IsMissing[Index] then
      CountsFieldList[DFIndex].AsInteger[Index] := 1
    else
      CountsFieldList[DFIndex].AsInteger[Index] := CountsFieldList[DFIndex].AsInteger[Index] + 1;
  end;
  DF.SortRecords(OrgSortingField);
  FreeAndNil(OrgSortingField);
end;

procedure TEpiReportCountById.DoSumStats;
var
  UniqueObs: Integer;
  F: TEpiField;
  HasAll: Boolean;
  CombinedObs: Integer;
  j: Integer;
  MissingObs: TBoundArray;
  i: Integer;

begin
  UniqueObs := ResultDF.Size;
  CombinedObs := 0;
  SetLength(MissingObs, CountsFieldList.Count);

  for i := 0 to ResultDF.Size  - 1 do
  begin
    HasAll := true;
    for j := 0 to CountsFieldList.Count - 1 do
    begin
      F := CountsFieldList[j];

      if F.IsMissing[i] then
      begin
        Inc(MissingObs[j]);
        HasAll := false;
        Break;
      end;
    end;

    if HasAll then
      Inc(CombinedObs);
  end;

  if (ercoShowSumstats in FOptions) then
    begin
      DoTableHeader('', 2, 3 + CountsFieldList.Count);

      I := 0;
      DoTableCell(0, I, 'Test');                              DoTableCell(1, PostInc(I), 'Result');
      DoTableCell(0, I, 'Total Combinations');                DoTableCell(1, PostInc(I), IntToStr(UniqueObs));
      DoTableCell(0, I, 'Present in all files');              DoTableCell(1, PostInc(I), IntToStr(CombinedObs));

      for J := 0 to CountsFieldList.Count - 1 do
      begin
        F := CountsFieldList[J];
        DoTableCell(0, I, 'File ' + IntToStr(PtrInt(F.FindCustomData(COUNT_BY_ID_DOCUMENTFILE_KEY))) + ': ' +
                          'Not found in ' + F.Question.Text + LineEnding +
                          '  Percentage contained:');
        DoTableCell(1, PostInc(I),
          IntToStr(MissingObs[J]) + LineEnding +
          FloatToStrF(100 * (UniqueObs - MissingObs[j]) / UniqueObs, ffFixed, 2, 1)
        );
      end;

      DoTableFooter('');
    end;

  if (Assigned(OnSumStatsComplete)) then
    OnSumStatsComplete(UniqueObs, CombinedObs, MissingObs, ResultDF, CountsFieldList);
end;

procedure TEpiReportCountById.DoReport;
var
  S: String;
  i: Integer;
  j: Integer;
  F: TEpiField;
begin
  ResultDF.SortRecords(ValueFieldList);

//  S := FieldList[0].Name;
//  for i := 1 to FieldList.Count - 1 do
//    S += ', ' + FieldList[i].Name;

  S := FieldNames[0];
  for i := 1 to FieldNames.Count - 1 do
    S += ', ' + FieldNames[i];

//  DoLineText('');
//  DoLineText(IntToStr(ResultDF.Size) + ' different values found.');
//  DoLineText('');


  { Do a summerized table first }
  DoSumStats;

  if (ercoShowDetailList in FOptions) then
    begin
      DoTableHeader('Field(s): ' + S, 1 + CountsFieldList.Count, 1 + ResultDF.Size);

      DoTableCell(0, 0, 'Value(s)', tcaLeftAdjust, [tcoRightBorder]);

      for i := 0 to CountsFieldList.Count - 1 do
      begin
        F := CountsFieldList[i];
        DoTableCell(
           1 + i, 0,
          'File '  + IntToStr(PtrInt(F.FindCustomData(COUNT_BY_ID_DOCUMENTFILE_KEY))) + ': ' + LineEnding +
            F.Question.Text,
          tcaLeftAdjust, [tcoRightBorder]);
      end;

      for i := 0 to ResultDF.Size - 1 do
      begin
        S := ValueFieldList[0].AsString[i];
        for j := 1 to ValueFieldList.Count - 1 do
          S += ', ' + ValueFieldList[j].AsString[i];

        DoTableCell(0, 1 + i, S);

        for j := 0 to CountsFieldList.Count - 1 do
          DoTableCell(1 + j, 1 + i, CountsFieldList[j].AsString[i]);
      end;

      DoTableFooter('');
    end;
end;

function TEpiReportCountById.GetDataFileIndexInFileList(
  const DataFile: TEpiDataFile): PtrInt;
begin
  Result := 1;

  while Result < FDocumentFiles.Count do
  begin
    if FDocumentFiles[Result-1].Document.DataFiles.IndexOf(DataFile) >= 0 then
      Break;

    Inc(Result);
  end;
end;

constructor TEpiReportCountById.Create(ReportGenerator: TEpiReportGeneratorBase);
begin
  inherited Create(ReportGenerator);

  ResultDF := TEpiDataFile.Create(nil);
  ValueFieldList := TEpiFields.Create(nil);
  CountsFieldList := TEpiFields.Create(nil);

  FOptions := [ercoShowDetailList, ercoShowSumstats];
end;

destructor TEpiReportCountById.Destroy;
begin
  FreeAndNil(CountsFieldList);
  FreeAndNil(ValueFieldList);
  FreeAndNil(ResultDF);

  inherited Destroy;
end;

procedure TEpiReportCountById.DoSanityCheck;
begin
  inherited DoSanityCheck;

{  if not Assigned(FDocuments) then
    DoError(EEpiReportBaseException, rsDocumentsNotAssigned);

  if not Assigned(FFieldList) then
    DoError(EEpiReportBaseException, rsFieldlistNotAssigned);  }
end;

procedure TEpiReportCountById.RunReport;
var
  i: Integer;
  F: TEpiField;
  Df: TEpiDataFile;
  Fn: TEpiField;
  Ft: TEpiFieldType;
  j: Integer;
  InitFt: Boolean;
  S: String;
  W: QWord;
begin
  inherited RunReport;

  // TC: (2014-12-09)
  // After discussion with JL the possiblity to compare field from different
  // DF's with different types have been removed. Therefor the code below,
  // finding the best possible field type, has been commented out.
  // OLD COMMENT:
  // Since we allow fields with various type to be compared,
  // loop through all DF's for a given fieldname and find the
  // best common field type.
  // We do this to avoid cases were sorting using string fields
  // unnessesary -> the sorting of numbers as strings are a mess!

  {
  for i := 0 to FieldNames.Count - 1 do
  begin
    InitFt := false;
    S := FieldNames[i];

//    for j := 0 to Documents.Count - 1 do
    for DF in DataFiles do
    begin
      Fn := Df.Fields.FieldByName[S];

      if not InitFt then
        Ft := FN.FieldType
      else
        Ft := Max(Ft, Fn.FieldType);

      InitFt := true;
    end;

    F := ResultDF.NewField(Ft);
    F.Name := S;
    ValueFieldList.AddItem(F);
  end;     }

  for S in FieldNames do
  begin
    F := ResultDF.NewField(DataFiles[0].Fields.FieldByName[S].FieldType);
    F.Name := S;
    ValueFieldList.AddItem(F);
  end;

  i := 0;
  for DF in DataFiles do
  begin
    F := ResultDF.NewField(ftInteger);
    F.Name := 'Field' + IntToStr(i + 1);

    if Assigned(FDocumentFiles) then
      F.AddCustomData(COUNT_BY_ID_DOCUMENTFILE_KEY, TObject(GetDataFileIndexInFileList(DF)));

    F.Question.Text := DF.Caption.Text;
    F.Length := 4;
    F.ResetData;
    CountsFieldList.AddItem(F);
    inc(i);
  end;

  i := 0;
  W := GetTickCount64;
  for DF in DataFiles do
  begin
    DoCounts(DF, i);
    Inc(i);
  end;
  W:= GetTickCount64 - W;

  DoReport;
end;

end.

