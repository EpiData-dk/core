unit epiexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles, epidatafilestypes, epivaluelabels,
  epieximtypes, epiexportsettings;

type

  { TEpiExport }

  TEpiExport = class(TObject)
  private
    FExportEncoding: TEpiEncoding;
    FExportLines: TStrings;
    function    PrepareExportDocument(Settings: TEpiExportSetting): TEpiDocument;
    function    EncodeString(Const Str: string; Encoding: TEpiEncoding): string;
    procedure   RaiseError(Const Msg: string);
    procedure   WriteByte(St: TStream; Val: ShortInt);
    procedure   WriteWord(St: TStream; Val: SmallInt);
    procedure   WriteInt(St: TStream; Val: LongInt);
    procedure   WriteSingle(St: TStream; Val: Single);
    procedure   WriteDouble(St: TStream; Val: Double);
    procedure   WriteEncString(St: TStream; Const Str: string; Const Count: Integer; Terminate: Boolean = True);
    procedure   WriteString(St: TStream; Const Str: string; Const Count: Integer; Terminate: Boolean = True);
  protected
    function    ExportStata(Const ExportSettings: TEpiStataExportSetting): Boolean;
    function    ExportCSV(Const Settings: TEpiCSVExportSetting): boolean;
    function    ExportSPSS(Const Settings: TEpiSPSSExportSetting): boolean;
    function    ExportSAS(Const Settings: TEpiSASExportSetting): boolean;
    function    ExportDDI(Const Settings: TEpiDDIExportSetting): boolean;
    function    ExportEPX(Const Settings: TEpiEPXExportSetting): boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Export(Const Settings: TEpiExportSetting): boolean;
    property    ExportEncoding: TEpiEncoding read FExportEncoding write FExportEncoding default eeUTF8;
  end;

implementation

uses
  FileUtil, epistringutils, math, LConvEncoding, dateutils, LazUTF8,
  epiexport_ddi, strutils, epicustombase, epidatafileutils;


{ TEpiExport }

function TEpiExport.PrepareExportDocument(Settings: TEpiExportSetting
  ): TEpiDocument;
var
  Doc: TEpiDocument;
  NewDoc: TEpiDocument;
  NewDF: TEpiDataFile;
  CIList: TEpiCustomControlItemList;
  NewCIList: TEpiCustomControlItemList;
  i: Integer;
  RecordCoundField: TEpiField;
begin
  Doc := Settings.Doc;
  Result := TEpiDocument(Settings.Doc.Clone);
  NewDF  := Result.DataFiles[Settings.DataFileIndex];

{  // Valuelabels export:
  if not Settings.ExportValueLabels then
    Result.ValueLabelSets.Clear;         }

  // Structure export:
  CIList := Doc.DataFiles[Settings.DataFileIndex].ControlItems;
  NewCIList := NewDf.ControlItems;
  for i := 0 to CIList.Count - 1 do
    if (Settings.Fields.IndexOf(CIList[i]) < 0) then
      NewCIList.GetItemByName(CIList[i].Name).Free;

  // Selected records:
  // Negative record cound => Structure only.
  if (Settings.ToRecord - Settings.FromRecord) < 0 then
  begin
    NewDF.Size := 0;
  end else begin
    RecordCoundField := TEpiField.CreateField(nil, ftInteger);
    RecordCoundField.Size := (Settings.ToRecord - Settings.FromRecord) + 1;
    for i := Settings.FromRecord to Settings.ToRecord do
    begin
      if NewDF.Deleted[i] then
        RecordCoundField.AsInteger[i - Settings.FromRecord] := 1
      else
        RecordCoundField.AsInteger[i - Settings.FromRecord] := 0;
      NewDF.Deleted[i] := false
    end;

    for i := 0 to Settings.FromRecord - 1 do
      NewDF.Deleted[i] := true;
    for i := Settings.ToRecord + 1 to NewDF.Size - 1 do
      NewDF.Deleted[i] := true;

    NewDF.Pack;

    for i := 0 to RecordCoundField.Size - 1 do
      if RecordCoundField.AsInteger[i] = 1 then
        NewDF.Deleted[i] := true;
  end;

  // Export deleted:
  if not Settings.ExportDeleted then
    NewDF.Pack;
end;

function TEpiExport.EncodeString(const Str: string; Encoding: TEpiEncoding
  ): string;
begin
  result := ConvertEncoding(Str, 'utf8', EpiEncodingToString[Encoding]);
end;

procedure TEpiExport.RaiseError(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure TEpiExport.WriteByte(St: TStream; Val: ShortInt);
begin
  St.Write(Val, 1);
end;

procedure TEpiExport.WriteWord(St: TStream; Val: SmallInt);
begin
  Val := NtoLE(Val);
  St.Write(Val, 2);
end;

procedure TEpiExport.WriteInt(St: TStream; Val: LongInt);
begin
  Val := NtoLE(Val);
  St.Write(Val, 4);
end;

procedure TEpiExport.WriteSingle(St: TStream; Val: Single);
begin
  St.Write(Val, 4);
end;

procedure TEpiExport.WriteDouble(St: TStream; Val: Double);
begin
  St.Write(Val, 8);
end;

procedure TEpiExport.WriteEncString(St: TStream; const Str: string;
  const Count: Integer; Terminate: Boolean);
begin
  WriteString(St, EncodeString(Str, ExportEncoding), Count, Terminate);
end;

procedure TEpiExport.WriteString(St: TStream; const Str: string;
  const Count: Integer; Terminate: Boolean);
var
  StrBuf: PChar;
  z: integer;
begin
  if Terminate then z := 0 else z := 1;
  StrBuf := StrAlloc(Count + z);
  StrPLCopy(PChar(@StrBuf[0]), Str, Count - 1 + z);
  St.Write(StrBuf[0], Count);
  StrDispose(StrBuf);
end;

constructor TEpiExport.Create;
begin
  FExportEncoding := eeUTF8;
  FExportLines := TStringList.Create;
end;

destructor TEpiExport.Destroy;
begin
  FExportLines.Free;
  inherited Destroy;
end;

function TEpiExport.Export(const Settings: TEpiExportSetting): boolean;
var
  OldDoc: TEpiDocument;
begin
  // Pre-process the current document.
  Result := Settings.SanetyCheck;
  if not Result then exit;

  OldDoc := Settings.Doc;
  Settings.Doc := PrepareExportDocument(Settings);
  if (Settings is TEpiCustomValueLabelExportSetting) and
     (not TEpiCustomValueLabelExportSetting(Settings).ExportValueLabels)
  then
    Settings.Doc.ValueLabelSets.Clear;

  // CSV
  if Settings is TEpiCSVExportSetting then
    Result := (ExportCSV(TEpiCSVExportSetting(Settings)));

  // Stata
  if Settings is TEpiStataExportSetting then
    Result := (ExportStata(TEpiStataExportSetting(Settings)));

  // SPSS
  if Settings is TEpiSPSSExportSetting then
    Result := (ExportSPSS(TEpiSPSSExportSetting(Settings)));

  // SAS
  if Settings is TEpiSASExportSetting then
    Result := (ExportSAS(TEpiSASExportSetting(Settings)));

  // DDI
  IF Settings is TEpiDDIExportSetting then
    Result := (ExportDDI(TEpiDDIExportSetting(Settings)));

  if Settings is TEpiEPXExportSetting then
    Result := (ExportEPX(TEpiEPXExportSetting(Settings)));

  Settings.Doc.Free;
  Settings.Doc := OldDoc;
end;

function TEpiExport.ExportStata(const ExportSettings: TEpiStataExportSetting
  ): Boolean;
var
  ValBuf,
  ByteBuf: Array of Byte;
  TypeList,
  CharBuf: Array of Char;
  NVar, NObs,
  I, J, TmpInt: Integer;
  TmpStr: string;
  TmpChar: Char;
  CurRec: Integer;
  CurField: Integer;
  DataFile: TEpiDataFile;
  FieldNames: TStrings;
  DataStream: TFileStream;
  VLblSet: TEpiValueLabelSet;
  TimeFields: TStringList;
  Fn: String;
  Df: TEpiDataFile;
  NObsPos: Int64;
  Flds: TEpiFields;
  RecCount: Integer;
  WrittenValueLabelSets: TEpiValueLabelSets;

  // Version specific variables.
  FieldNameLength, StrBaseNum, FileLabelLength,
  FmtLength: Integer;
  ByteChar, IntChar, LongChar,
  FloatChar, DoubleChar: Char;
  MissingBaseNum: Cardinal;

  procedure WriteMissingFloat(Const MisVal: Word);
  var
    Val: Double;
    FltByte: Array[0..7] of Byte absolute Val;
  begin
    FltByte[0] := 0;
    FltByte[1] := 0;
    FltByte[2] := 0;
    FltByte[3] := 0;
    FltByte[4] := hi(MisVal);
    FltByte[5] := lo(MisVal);
    FltByte[6] := $e0;
    FltByte[7] := $7f;
    WriteDouble(DataStream, Val);
  end;


  function UniqueValueLabelName(Const Str: string; Const Count: Integer): string;
  var
    i: integer;
  begin
    result := copy(StringReplace(Str, ' ', '_', [rfReplaceAll]), 1, Count-1);
    i := 1;
    if result = '' then result := 'ValueLabel';
    while WrittenValueLabelSets.ItemExistsByName(result) do
    begin
      result := Copy(result, 1, Count - Length(IntToStr(i - 1))) + IntToStr(i);
      Inc(i);
    end;
  end;

begin
  Result := false;

  Fn := ExportSettings.ExportFileName;
  Df := ExportSettings.Doc.DataFiles[ExportSettings.DataFileIndex];
  Flds := Df.Fields;


  with Df do
  try
    DataStream := TFileStream.Create(UTF8ToSys(Fn), fmCreate);

    // Version specific settings:
    // - "original" setting from Ver. 4
    FieldNameLength := 9;
    FileLabelLength := 32;
    StrBaseNum      := $7F;
    FmtLength       := 12;
    MissingBaseNum  := 1;
    ByteChar        := 'b';
    IntChar         := 'i';
    LongChar        := 'l';
    FloatChar       := 'f';
    DoubleChar      := 'd';
    // - changed in Ver. 6
    if ExportSettings.Version >= dta6 THEN
      FileLabelLength := 81;
    // - change in Ver. 7
    IF ExportSettings.Version >= dta7 THEN
      FieldNameLength := 33;
    // - changed in Ver. 8
    IF ExportSettings.Version >= dta8 THEN
    BEGIN
      StrBaseNum := 0;
      MissingBaseNum := 27;
      ByteChar   := StataByteConst;
      IntChar    := StataIntConst;
      LongChar   := StataLongConst;
      FloatChar  := StataFloatConst;
      DoubleChar := StataDoubleConst;
    END;
    // - changed in Ver. 10
    if ExportSettings.Version >= dta10 THEN
      FmtLength := 49;

    NVar := Flds.Count;
    NObs := Df.Size;// (ExportSettings.ToRecord - ExportSettings.FromRecord) + 1;

    // ********************************
    //           STATA HEADER
    // ********************************
    SetLength(ByteBuf, 4);
    ByteBuf[0] := Byte(ExportSettings.Version);
    ByteBuf[1] := 2;                                          // Use LOHI order of data
    ByteBuf[2] := 1;                                          // Filetype - only 1 is legal value
    ByteBuf[3] := 0;                                          // Unused
    DataStream.Write(ByteBuf[0], 4);
    WriteWord(DataStream, NVar);                                       // Number of Variables

    // Since we at this stage do NOT know about delete records,
    // store the position and revert to write the correct number.
    NObsPos := DataStream.Position;
    WriteInt(DataStream,  NObs);                                       // Number of records


    IF trim(Caption.Text) <> '' THEN
      TmpStr := Trim(Caption.Text)
    ELSE
      TmpStr := Format('Datafile created by EpiData based on %s', [SysToUTF8(ExtractFilename(UTF8ToSys(Fn)))]);

    // data_label \0 terminated.
    WriteEncString(DataStream, TmpStr, FileLabelLength);

    // time_stamp \0 terminated (not used in epidata)
    TmpStr := FormatDateTime('dd mmm yyyy hh":"nn', Now);
    WriteString(DataStream, TmpStr, 18);

    // ********************************
    //         STATA DESCRIBTORS
    // ********************************
    // - typlist: the variable's types
    SetLength(TypeList, NVar);
    SetLength(ByteBuf, NVar);
    FOR i := 0 to NVar - 1 DO
    BEGIN
      WITH Flds[i] DO
      BEGIN
        CASE FieldType OF
          ftInteger, ftAutoInc:
            BEGIN
              IF Length <= 2 THEN
                TmpChar := ByteChar
              ELSE IF Length <= 5 THEN
                TmpChar := IntChar
              ELSE IF Length <= 9 THEN
                TmpChar := LongChar
              ELSE IF Length >= 10 THEN
                TmpChar := DoubleChar;  //&&
            END;
          ftString, ftUpperString:
            if ExportEncoding = eeUTF8 then
              TmpChar := Chr(207)      // Since UTF-8 is multibyte there is no definit length to the #bytes used in a string.
            else
              TmpChar := Chr(Length);  // Userchosen ansi encoding, here 1 char = 1 byte! Always.
          ftBoolean:
            TmpChar := ByteChar;
          ftDMYDate, ftMDYDate, ftYMDDate,
          ftDMYAuto, ftMDYAuto, ftYMDAuto:
             TmpChar := LongChar;
          ftTime, ftTimeAuto,
          ftFloat:
             TmpChar := DoubleChar;
        END;  //Case
        ByteBuf[i] := Ord(TmpChar);
        TypeList[i] := TmpChar;
        // update typelist to consts. - bytebuf[] is written,
        // typelist is used later on for Case Typelist[i] of...
        if TypeList[i] = ByteChar then   TypeList[i] := StataByteConst;
        if TypeList[i] = IntChar then    TypeList[i] := StataIntConst;
        if TypeList[i] = LongChar then   TypeList[i] := StataLongConst;
        if TypeList[i] = FloatChar then  TypeList[i] := StataFloatConst;
        if TypeList[i] = DoubleChar then TypeList[i] := StataDoubleConst;
      END;  //with
    END;  //for

    DataStream.Write(ByteBuf[0], NVar);

    // - varlist: variable names
    FieldNames := TStringList.Create;
    FOR i :=0 TO NVar - 1 DO
    BEGIN
      WITH Flds[i] DO
      BEGIN
        case ExportSettings.FieldNameCase of
          fncUpper: TmpStr := UTF8UpperCase(Trim(Name));
          fncLower: TmpStr := UTF8LowerCase(Trim(Name));
          fncAsIs:  TmpStr := Trim(Name);
        end;
        TmpStr := CreateUniqueAnsiVariableName(TmpStr, FieldNameLength - 1, FieldNames);
        WriteString(DataStream, TmpStr, FieldNameLength);
      END;   //with
    END;  //for eN

    // - srtlist: sortorder of fields}
    //   No sortorder is written, only zeros to indicated end of list}
    SetLength(ByteBuf, 2 * (NVar + 1));
    FillChar(ByteBuf[0], 2 * (NVar + 1), 0);
    DataStream.Write(ByteBuf[0], 2 * (NVar + 1));

    // - Fmtlist: list of formats of the variables
    TimeFields := TStringList.Create;
    FOR i := 0 TO NVar - 1 DO
    WITH Flds[i] DO
    BEGIN
      CASE FieldType OF
        ftInteger, ftAutoInc:
          TmpStr := '%' + IntToStr(Length) + '.0f';
        ftFloat:
          TmpStr := '%' + IntToStr(Length) + '.' + IntToStr(Decimals) + 'f';
        ftBoolean:
          TmpStr := '%1.0f';
        ftString, ftUpperString:
          TmpStr := '%' + IntToStr(Ord(TypeList[i])) + 's';
        ftDMYDate, ftMDYDate, ftYMDDate,
        ftDMYAuto, ftMDYAuto, ftYMDAuto:
          TmpStr := '%d';
        ftTime, ftTimeAuto:
          if ExportSettings.Version >= dta10 then
          begin
            // Stata 10 supports a new time format!
            TmpStr := '%tcHH:MM:SS';
            TimeFields.AddObject(Name, Field[i]);
          end
          else
            TmpStr := '%6.5f';
      END;   //case FeltType
      WriteString(DataStream, TmpStr, FmtLength);
    END;  //for - with

    // RE-format valuelabel sets.
    WrittenValueLabelSets := TEpiValueLabelSets.Create(nil);
    for i := 0 to ValueLabels.Count - 1 do
    with ValueLabelSet[i] do
    begin
      if LabelType <> ftInteger then continue;

      VLblSet := TEpiValueLabelSet(Clone(WrittenValueLabelSets));
      VLblSet.Name := UniqueValueLabelName(EncodeString(VLblSet.Name, ExportEncoding), FieldNameLength);
      AddCustomData('StataValueLabelsKey', VLblSet);
      WrittenValueLabelSets.AddItem(VLblSet);

      TmpInt := $7fffffe6;  // Stata missingvalue ".a"
      for j := 0 to VLblSet.Count - 1 do
        if VLblSet[j].IsMissingValue then
        begin
          TEpiIntValueLabel(VLblSet[j]).Value := TmpInt;
          Inc(TmpInt);
        end;
    end;

    // - lbllist: names af value label
    for i := 0 to NVar - 1 do
    with Flds[i] do
    begin
      TmpStr := '';
      if Assigned(ValueLabelSet) and
         (ValueLabelSet.LabelType = ftInteger)
      then
        TmpStr := TEpiValueLabelSet(ValueLabelSet.FindCustomData('StataValueLabelsKey')).Name;

      WriteString(DataStream, TmpStr, FieldNameLength);
    end;

    // ********************************
    //      STATA VARIABLE LABELS
    // ********************************
    FOR i := 0 TO NVar - 1 DO
    WITH Flds[i] DO
      WriteEncString(DataStream, Trim(Question.Text), FileLabelLength);

    // ********************************
    //      STATA EXPANSION FIELDS
    // ********************************
    if ExportSettings.Version < dta7 then
    begin
      // Expansion field is 3 bytes before version 7
      // Skip expansion field - since we don't know the format (documents no
      // longer exists on how they are formatted).
      WriteByte(DataStream, 0); // 3 bytes....
      WriteWord(DataStream, 0);
    end;
    if ExportSettings.Version >= dta7 then
    begin
      // Expansion field is 5 bytes from version 7
      if ExportSettings.ExportLines.Count > 0 then
      begin
        // We start out by writing the length of the notes in a "special" characteristic called 'note0'
        WriteByte(DataStream, 1);
        TmpStr := IntToStr(ExportSettings.ExportLines.Count);
        // TmpInt = len  (sum of 2 * 33 + length(TmpStr)
        TmpInt := 2*33 + Length(TmpStr) + 1;
        WriteInt(DataStream, TmpInt);
        WriteString(DataStream, '_dta', 33);
        WriteString(DataStream, 'note0', 33);
        WriteString(DataStream, TmpStr, Length(TmpStr) + 1);
      end;
      for i := 0 to ExportSettings.ExportLines.Count - 1 do
      begin
        TmpStr := ExportSettings.ExportLines[i];
        WriteByte(DataStream, 1);
        // TmpInt = len  (sum of 2 * 33 + length(TmpStr)
        TmpInt := 33 +                 // First variable name or _dta for notes regarding the dataset.
                  33 +                 // Character name, in our case 'noteX'
                  Length(TmpStr) + 1;
        WriteInt(DataStream, TmpInt);
        WriteString(DataStream, '_dta', 33);
        WriteString(DataStream, 'note' + IntToStr(i+1), 33);
        WriteString(DataStream, TmpStr, Length(TmpStr) + 1);
      end;

      for i := 0 to TimeFields.Count -1 do
      with TimeFields do
      begin
        WriteByte(DataStream, 1);
        TmpInt := 2*33 + 2;  // 2 = 1 char for "1" and 1 char for #0;
        WriteInt(DataStream, TmpInt);
        WriteString(DataStream, Strings[i], 33);
        WriteString(DataStream, 'note0', 33);
        WriteString(DataStream, '1', 2);

        TmpStr := 'Time variable: Formatted with %tcHH:MM:SS. See "help dates_and_times, marker(formatting)"Ha for details. Date coded as Jan. 1st 1960.';
        WriteByte(DataStream, 1);
        TmpInt := 2*33 + Length(TmpStr) + 1;
        WriteInt(DataStream, TmpInt);
        WriteString(DataStream, Strings[i], 33);
        WriteString(DataStream, 'note1', 33);
        WriteString(DataStream, TmpStr, Length(TmpStr) + 1);
      end;
      TimeFields.Free;

      // End expansion field
      WriteByte(DataStream, 0);
      WriteInt(DataStream, 0); // 5 bytes in total...
    end;

    // ********************************
    //          STATA DATA
    // ********************************
    TRY
      RecCount := 0;

//      FOR CurRec := ExportSettings.FromRecord TO ExportSettings.ToRecord DO
      FOR CurRec := 0 TO Df.Size - 1 DO
      BEGIN
//        if Deleted[CurRec] then continue;
        Inc(RecCount);

        FOR CurField := 0 TO NVar - 1 DO
        With Flds[CurField] do
        Case TypeList[CurField] of
          StataByteConst:
            begin
              // Specific missing values
              // TODO : MissingValues (STATA)
              if IsMissing[CurRec] then
                WriteByte(DataStream, $65)
              else if IsMissingValue[CurRec] then
              begin
                VLblSet := TEpiValueLabelSet(ValueLabelSet.FindCustomData('StataValueLabelsKey'));
                TmpInt := ValueLabelSet.IndexOf(ValueLabelSet.ValueLabel[AsValue[CurRec]]);
                WriteByte(DataStream, (TEpiIntValueLabel(VLblSet[TmpInt]).Value - $7fffffe5 + $65));
              end else
                WriteByte(DataStream, AsInteger[CurRec]);
            end;
          StataIntConst:
            begin
              if IsMissing[CurRec] then
                WriteWord(DataStream, $7fe5)
              else if IsMissingValue[CurRec] then
              begin
                VLblSet := TEpiValueLabelSet(ValueLabelSet.FindCustomData('StataValueLabelsKey'));
                TmpInt := ValueLabelSet.IndexOf(ValueLabelSet.ValueLabel[AsValue[CurRec]]);
                WriteWord(DataStream, (TEpiIntValueLabel(VLblSet[TmpInt]).Value - $7fffffe5 + $7fe5));
              end else
                WriteWord(DataStream, AsInteger[CurRec]);
            end;
          StataLongConst:
            begin
              if IsMissing[CurRec] then
                WriteInt(DataStream, $7FFFFFFF)
              else if IsMissingValue[CurRec] then
              begin
                VLblSet := TEpiValueLabelSet(ValueLabelSet.FindCustomData('StataValueLabelsKey'));
                TmpInt := ValueLabelSet.IndexOf(ValueLabelSet.ValueLabel[AsValue[CurRec]]);
                WriteInt(DataStream, TEpiIntValueLabel(VLblSet[TmpInt]).Value);
              end else
                {Date is converted from Delphi's/Lazarus 30/12-1899 base
                 to Stata's 1/1-1960 base by substracting 21916 days}
                If (FieldType in DateFieldTypes) then
                  WriteInt(DataStream, AsDate[CurRec] - StataBaseDate)
                else
                  WriteInt(DataStream, AsInteger[CurRec]);
            end;
          StataDoubleConst:
            begin
              if IsMissing[CurRec] then
                WriteMissingFloat(0)
              else if (IsMissingValue[CurRec]) and
                      (FieldType = ftInteger)
              then
              begin
                VLblSet := TEpiValueLabelSet(ValueLabelSet.FindCustomData('StataValueLabelsKey'));
                TmpInt := ValueLabelSet.IndexOf(ValueLabelSet.ValueLabel[AsValue[CurRec]]);
                TmpInt := TEpiIntValueLabel(VLblSet[TmpInt]).Value - $7fffffe5;
                WriteMissingFloat(TmpInt);
              end else
                if (FieldType in TimeFieldTypes) and
                   (ExportSettings.Version >= dta10) then
                  WriteDouble(DataStream, round(MilliSecondSpan(StataBaseDate + AsDateTime[CurRec], StataBaseDateTime)))
                else
                  WriteDouble(DataStream, AsFloat[CurRec]);
            end
        else
          WriteString(DataStream, AsString[CurRec], 207);
        end;
      END;  //for CurObs
    EXCEPT

      RaiseError('Error in exporting to Stata');
      Exit;
    END;  //try..Except

    // In case some deleted records were not exported.
    if RecCount <> NObs then
    begin
      DataStream.Seek(NObsPos, soBeginning);
      WriteInt(DataStream, RecCount);
      DataStream.Position := DataStream.Size;
    end;

    {Write VALUE-LABELS}
    if ExportSettings.ExportValueLabels then
    begin
      IF ExportSettings.Version = dta4 THEN
      BEGIN
  {      //write value labels in Stata ver. 4/5 format
        FOR I := 0 TO WritenValueLabels.Count - 1 DO
        BEGIN
          {Fill out value label header}
          VLblSet := ValueLabels.ValueLabelSetByName(WritenValueLabels[i]);
          WriteInts(VLblSet.Count, 2);
          WriteString(UniqueValueLabels[i], FieldNameLength);

          {Fill out entries}
          FOR j := 0 TO VLblSet.Count - 1 DO
          BEGIN
            // TODO : ValueLabels in STATA
            WriteInts(0 {StrToInt(VLblSet.Values[j])}, 2);
            WriteString(''{VLblSet.Labels[j]}, 8);
          END;  //for j
        END;       }
      END ELSE BEGIN
        //write value labels in Stata ver. 6+ format
        SetLength(CharBuf, 32000);      // Write Txt[] - max posible length is 32000 chars.
        FOR I := 0 TO WrittenValueLabelSets.Count - 1 DO
        BEGIN
          VLblSet := WrittenValueLabelSets[i];
          NObs := VLblSet.Count;
          SetLength(ByteBuf, 4 * NObs);   // Write Off[]
          SetLength(ValBuf,  4 * NObs);   // Write Val[]
          FillChar(CharBuf[0], 32000, 0); // reset Txt[]

          CurRec := 0;                    // Holds Off[i] index into Txt[]
          for J := 0 to NObs - 1 do
          begin
            Move(CurRec, ByteBuf[J * 4], 4);
            TmpInt := TEpiIntValueLabel(VLblSet.ValueLabels[J]).Value;
            Move(TmpInt, ValBuf[J * 4], 4);
            TmpStr := EncodeString(VLblSet.ValueLabels[J].TheLabel.Text, ExportEncoding);
            Move(TmpStr[1], CharBuf[CurRec], Length(TmpStr));
            Inc(CurRec, Length(TmpStr) + 1);
          end;
          {Fill out value label header}
          TmpInt := 4 +                   // n
                    4 +                   // txtlen
                    (4 * NObs) +          // off[]
                    (4 * NObs) +          // val[]
                    CurRec;               // txt[]

          WriteInt(DataStream, TmpInt);                                     // len
          WriteString(DataStream, VLblSet.Name, FieldNameLength);           // labname
          DataStream.Write(#0#0#0, 3);                                      // padding...

          {Fill out value_label_table}
          WriteInt(DataStream, NObs);                                       // n
          WriteInt(DataStream, CurRec);                                     // txtlen
          DataStream.Write(ByteBuf[0], 4 * NObs);                           // off[]
          DataStream.Write(ValBuf[0], 4 * NObs);                            // val[]
          DataStream.Write(CharBuf[0], CurRec);                             // txt[]
        END;  //write value labels in stata 6 version
      END;  //for i
    end;

    Result := true;
  finally
    if Assigned(DataStream) then FreeAndNil(DataStream);
    //if Assigned(WritenValueLabels) then FreeAndNil(WritenValueLabels);
//    if Assigned(UniqueValueLabels) then FreeAndNil(UniqueValueLabels);
  end;
end;

function TEpiExport.ExportCSV(const Settings: TEpiCSVExportSetting): boolean;
var
  DataStream: TFileStream;
  FieldSep: String;
  QuoteCh: String;
  i: Integer;
  NewLine: String;
  TmpStr, S: String;
  FieldCount: Integer;
  BackUpSettings: TFormatSettings;
  CurRec: Integer;
  L: Cardinal;
  Df: TEpiDataFile;
  Fixed: Boolean;
  Fields: TEpiFields;
begin
  Result := false;

  Df := Settings.Doc.DataFiles[Settings.DataFileIndex];
  Fields := Df.Fields;

  try
    DataStream := TFileStream.Create(UTF8ToSys(Settings.ExportFileName), fmCreate);

    FieldSep := Settings.FieldSeparator;
    QuoteCh  := Settings.QuoteChar;
    NewLine  := Settings.NewLine;
//    FieldCount := Settings.Fields.Count;
    Fixed      := Settings.FixedFormat;

    {Write Field Names}
    if Settings.ExportFieldNames then
    begin
      TmpStr := '';
      for i := 0 to Fields.Count - 1 do
      begin
        S := EncodeString(Fields[i].Name, Settings.Encoding);
        L := Fields[i].Length;
        if (Settings.Encoding = eeUTF8) and Fixed then
          TmpStr += UTF8Copy(S, 1, L) + DupeString(' ', L - UTF8Length(S))
        else if Fixed then
          TmpStr += Format('%-' + IntToStr(L) + '.' + IntToStr(L) + 's', [S])
        else
          TmpStr += S + FieldSep;
      end;
      Delete(TmpStr, Length(TmpStr), 1);
      TmpStr += NewLine;
      TmpStr := EncodeString(TmpStr, Settings.Encoding);
      DataStream.Write(TmpStr[1], Length(TmpStr));
    end;

    BackUpSettings := FormatSettings;
    FormatSettings.DateSeparator := Settings.DateSeparator[1];
    FormatSettings.TimeSeparator := Settings.TimeSeparator[1];
    FormatSettings.DecimalSeparator := Settings.DecimalSeparator[1];

    { Write Data }
//    for CurRec := Settings.FromRecord to Settings.ToRecord do
    for CurRec := 0 to Df.Size - 1 do
    begin
      TmpStr := '';

      // TODO : Condition checking!
      if Df.Deleted[CurRec] then continue;

      // Using AsString should take care of formatting since it uses FormatSettings.
      for i := 0 to Fields.Count - 1 do
      with Fields[i] do
      begin
        if IsMissing[CurRec] then
          S := ''
        else
          S := AsString[CurRec];

        L := Length;
        if (FieldType in StringFieldTypes) then
        begin
          S := EncodeString(S, Settings.Encoding);
          if (QuoteCh <> '') and (not Fixed) then
          begin
            S := AnsiQuotedStr(S, QuoteCh[1]);
            Inc(L, 2);
          end;
        end;

        if (Settings.Encoding = eeUTF8) and Fixed then
          S := S + DupeString(' ', L - UTF8Length(S))
        else if Fixed then
          S := Format('%-' + IntToStr(L) + '.' + IntToStr(L) + 's', [S])
        else
          S += FieldSep;

        TmpStr += S;
      end;

      Delete(TmpStr, Length(TmpStr), 1);
      TmpStr += NewLine;
      DataStream.Write(TmpStr[1], Length(TmpStr));
    end;
    result := true;
  finally
    DataStream.Free;
    FormatSettings := BackUpSettings;
  end;
end;

function TEpiExport.ExportSPSS(const Settings: TEpiSPSSExportSetting): boolean;
var
  CSVSetting: TEpiCSVExportSetting;
  ExpLines: TStringList;
  S: String;
  Col: Integer;
  i: Integer;
  Df: TEpiDataFile;
  TmpLines: TStringList;
  j: Integer;
  CurrentDecimalSeparator: Char;
  Fields: TEpiFields;
begin
  Result := false;

  // First export the data:
  CSVSetting := TEpiCSVExportSetting.Create;
  CSVSetting.Assign(Settings);
  with CSVSetting do begin
    ExportFileName   := ChangeFileExt(Settings.ExportFileName, '.txt');

    // CSV Settings
    Encoding         := eeUTF8;
    QuoteChar        := '';
    FixedFormat      := false;
    ExportFieldNames := false;
    DateSeparator    := '/';
    TimeSeparator    := ':';
    DecimalSeparator := '.';
    FieldSeparator   := Settings.Delimiter;
    NewLine          := LineEnding;
  end;

  if not ExportCSV(CSVSetting) then exit;
  Settings.AdditionalExportSettings := CSVSetting;
  Df := Settings.Doc.DataFiles[Settings.DataFileIndex];
  Fields := Df.Fields;

  // HEADER INFORMATION:
  TmpLines := TStringList.Create;
  ExpLines := TStringList.Create;
  ExpLines.Append('* EpiData created two files during export');
  ExpLines.Append('* .');
  ExpLines.Append('* 1. ' + Settings.ExportFileName + ' .');
  ExpLines.append('*    is this SPSS command file');
  ExpLines.Append('* 2. ' + CSVSetting.ExportFileName + ' .');
  ExpLines.Append('*    is an ASCII text file with the raw data');
  ExpLines.Append('*');
  ExpLines.Append('* You may modify the commands before running it');
  ExpLines.Append('* Uncomment (remove the *) the last command (SAVE) if the');
  ExpLines.Append('* command file should save the data as a SPSS datafile');
  ExpLines.Append('');
  // Always export using "." as decimal separator. SPSS only supports "." and ","
  // so we go for safe option and make exporting to CSV easier.
  ExpLines.Append('SET DECIMAL=dot.');
  // Define the entire dataset.
  ExpLines.Append('DATA LIST');
  // The CSV file.
  ExpLines.append('  FILE = "' + CSVSetting.ExportFileName + '"');
  ExpLines.append('  ENCODING="UTF8"');
  ExpLines.Append('  FREE ("' + CSVSetting.FieldSeparator  + '")');

  // RECORDS is basically the number of lines used to define a single record (in epidata).
  // SPSS does not seem to have a max character count on the length of lines in the CSV file
  // so we export: 1 .epx record -> 1 .csv line.
  ExpLines.Append('  RECORDS = 1');

  // Field name and position information!
  S := '  / ';
  for i := 0 to Fields.Count - 1 do
  with Fields[i] do
  begin
    // The SPSS command file should not be all too difficult to read.. ;)
    // - hence we break somewhere after 80 characters.
    if System.Length(S) > 80 then
    begin
      ExpLines.Append(S);
      S := '    '
    end;

    {
      Fieldname formatting in SPSS looks like this:
        / varname {[(format)]} [varname...]
    }
    // varname
    S += Copy(Name, 1, 64);

    // [(format)]
    case FieldType of
      ftBoolean,
      ftInteger,
      ftAutoInc:
        S += '(F' + IntToStr(Length) + ')';
      ftFloat:
        S += '(F' + IntToStr(Length) + '.' + IntToStr(Decimals) + ')';
      ftDMYDate,
      ftDMYAuto:
        S += '(EDATE10)';
      ftMDYDate,
      ftMDYAuto:
        S += '(ADATE10)';
      ftYMDDate,
      ftYMDAuto:
        S += '(SDATE10)';
      ftTime,
      ftTimeAuto:
        S += '(TIME8)';
      ftString,
      ftUpperString:
        S += '(A' + IntToStr(Length) + ')';
    end;
    S += ' ';
  end;
  // Write the last line along with the trailing "."
  ExpLines.Append(S + '.');
  ExpLines.Strings[ExpLines.Count - 1];

  // Dataset Label
  //  FILE LABEL <text>
  if Df.Caption.Text <> '' then
  begin
    ExpLines.Append('FILE LABEL ' + AnsiQuotedStr(Df.Caption.Text, '"'));
    ExpLines.Append('');
  end;

  // Variable Labels
  TmpLines.Clear;
  for i := 0 to Fields.Count - 1 do
  with Fields[i] do
    if Question.Text <> '' then
      TmpLines.Append('  ' + Name + ' ' + AnsiQuotedStr(Question.Text, '"'));

  if TmpLines.Count > 0 then
  begin
    TmpLines.Add('.');
    ExpLines.Append('VARIABLE LABELS');
    ExpLines.AddStrings(TmpLines);
  end;


  // Value Labels:
  //
  // VALUE LABELS
  //    <varname(s)>
  //       <value>  <"label">
  //       <value>  <"label">..
  //  / <varname(s)> ...
  TmpLines.Clear;
  S := '  ';
  CurrentDecimalSeparator := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := '.';
  for i := 0 to Fields.Count - 1 do
  with Fields[i] do
  begin
    if not (Assigned(ValueLabelSet)) then continue;

    // [/] <varname>
    TmpLines.Add(' ' + S + Name);

    // <value> <"label">
    for j := 0 to ValueLabelSet.Count - 1 do
      TmpLines.Add('    "' + ValueLabelSet[j].ValueAsString + '" "' + ValueLabelSet[j].TheLabel.Text + '"');

    S := '/ ';
  end;
  DefaultFormatSettings.DecimalSeparator := CurrentDecimalSeparator;

  if TmpLines.Count > 0 then
  begin
    TmpLines.Add('.');
    ExpLines.Append('VALUE LABELS');
    ExpLines.AddStrings(TmpLines);
  end;

  ExpLines.Append('execute.');
  ExpLines.Append('*********** Uncomment next line to save file ******************.');
  ExpLines.Append('* SAVE OUTFILE="' + ChangeFileExt(Settings.ExportFileName,'.sav') + '".');
  ExpLines.Append('***************************************************************.');
  ExpLines.Append('*.');

  ExpLines.SaveToFile(UTF8ToSys(Settings.ExportFileName));
  result := true;
end;

function TEpiExport.ExportSAS(const Settings: TEpiSASExportSetting): boolean;
var
  CSVSetting: TEpiCSVExportSetting;
  Df: TEpiDataFile;
  TmpLines: TStringList;
  ExpLines: TStringList;
  VLList: TStringList;
  i: Integer;
  Idx: Integer;
  j: Integer;
  S: String;
  Col: Integer;
  Flds: TEpiFields;
begin
  Result := false;

  // First export the data:
  CSVSetting := TEpiCSVExportSetting.Create;
  CSVSetting.Assign(Settings);
  with CSVSetting do begin
    ExportFileName   := ChangeFileExt(Settings.ExportFileName, '.txt');

    // CSV Settings
    QuoteChar        := '';
    FixedFormat      := true;
    ExportFieldNames := false;
    DateSeparator    := '/';
    TimeSeparator    := ':';
    DecimalSeparator := '.';
  end;

  if not ExportCSV(CSVSetting) then exit;
  Settings.AdditionalExportSettings := CSVSetting;

  Df := Settings.Doc.DataFiles[Settings.DataFileIndex];
  Flds := Df.Fields;

  // HEADER INFORMATION:
  TmpLines := TStringList.Create;
  ExpLines := TStringList.Create;
  ExpLines.Append('* EpiData created two files during export');
  ExpLines.Append('* .');
  ExpLines.Append('* 1. ' + Settings.ExportFileName + ' .');
  ExpLines.append('*    is this SAS command file');
  ExpLines.Append('* 2. ' + CSVSetting.ExportFileName + ' .');
  ExpLines.Append('*    is an ASCII text file with the raw data');
  ExpLines.Append('*');
  ExpLines.Append('* You may modify the commands before running it;');
  ExpLines.Append('');


  // Preliminary ValueLabel Sets are printed.
  if Settings.ExportValueLabels then
  begin
    VLList := TStringList.Create;

    // first build list of used VLSets.
    for i := 0 to Flds.Count - 1 do
    with Flds[i] do
    begin
      if Assigned(ValueLabelSet) and
         not (VLList.Find(ValueLabelSet.Name, Idx))
      then
        VLList.AddObject(ValueLabelSet.Name, ValueLabelSet);
    end;

    // The output content
    TmpLines.Clear;
    for i := 0 to VLList.Count - 1 do
    with TEpiValueLabelSet(VLLIst.Objects[i]) do
    begin
      // Name the Valuelabel set.
      TmpLines.Add('  VALUE ' + Name);

      // Print the labels.
      for j := 0 to Count - 1 do
      with ValueLabels[j] do
        TmpLines.Add('   ' + ValueAsString + ' = "' + TheLabel.Text + '"' + BoolToStr(j = (Count-1), ';', ''));
    end;

    if TmpLines.Count > 0 then
    begin
      ExpLines.Add('PROC FORMAT;');
      ExpLines.AddStrings(TmpLines);
      ExpLines.Add('run;');
      ExpLines.Add('');
    end;
  end;

  ExpLines.Add('DATA ' + Df.Name + '(LABEL="' + Df.Caption.Text + '");');
  ExpLines.Add('  INFILE "' + CSVSetting.ExportFileName + '";');
  ExpLines.Add('  INPUT');

  S := '   ';
  Col := 1;
  for i := 0 to Flds.Count - 1 do
  with Flds[i] do
  begin
    // The SAS command file should not all too difficult to read.. ;)
    // - hence we break somewhere after 80 characters.
    if System.Length(S) > 80 then
    begin
      ExpLines.Append(S);
      S := '   '
    end;

    {
      Fieldname formatting in SAS looks like this:
        varname {[$] col location  [Informat]} [varname...]
    }
    // varname
    S += Name;

    // {[$]
    if FieldType in StringFieldTypes then
      S += ' $';

    // col "start"
    S += ' ' + IntToStr(Col);

    //  col "end"
    if (FieldType in StringFieldTypes) then
      S += '-' + IntToStr(Col + (Length * 3) - 1)   // To cover up for UTF-8 lengths.
    else if Length > 1 then
      S += '-' + IntToStr(Col + Length - 1);

    case FieldType of
      ftFloat:
        S += ' .' + IntToStr(Decimals);
      ftDMYDate,
      ftDMYAuto:
        S += ' ddmmyy10.';
      ftMDYDate,
      ftMDYAuto:
        S += ' mmddyy10.';
      ftYMDDate,
      ftYMDAuto:
        S += ' yymmdd10.';
      ftTime: ;
      ftTimeAuto: ;
    end;
    S += ' ';
    if (FieldType in StringFieldTypes) then
      Inc(Col, Length * 3)
    else
      Inc(Col, Length);
  end;
  // Write the last line along with the trailing ";"
  ExpLines.Append(S + ';');
  ExpLines.Add('');

  // Variable Labels
  TmpLines.Clear;
  for i := 0 to Flds.Count - 1 do
  with TEpiField(Flds[i]) do
    if Question.Text <> '' then
      TmpLines.Append('  ' + Name + ' = ' + AnsiQuotedStr(Question.Text, '"'));

  if TmpLines.Count > 0 then
  begin
    TmpLines.Add(';');
    ExpLines.Append('LABEL');
    ExpLines.AddStrings(TmpLines);
    ExpLines.Add('');
  end;


  // Fields <-> ValueLabels association
  TmpLines.Clear;
  for i := 0 to Flds.Count - 1 do
  with Flds[i] do
  begin
    if Assigned(ValueLabelSet) then
      TmpLines.Add('  ' + Name + ' ' + ValueLabelSet.Name + '.');
  end;

  if TmpLines.Count > 0 then
  begin
    TmpLines.Add(';');
    ExpLines.Add('FORMAT');
    ExpLines.AddStrings(TmpLines);
  end;

  ExpLines.SaveToFile(UTF8ToSys(Settings.ExportFileName));
  Result := true;
end;

function TEpiExport.ExportDDI(const Settings: TEpiDDIExportSetting): boolean;
var
  DDIExporter: TEpiDDIExport;
begin
  DDIExporter := TEpiDDIExport.Create;
  DDIExporter.ExportDDI(Settings);
  DDIExporter.Free;
end;

function TEpiExport.ExportEPX(const Settings: TEpiEPXExportSetting): boolean;
begin
  Result := false;
  try
    Settings.Doc.SaveToFile(Settings.ExportFileName);
    Result := true;
  finally

  end;
end;

end.

