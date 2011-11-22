unit epiexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles, epidatafilestypes, epivaluelabels,
  epieximtypes;

type

  { TEpiExport }

  TEpiExport = class(TObject)
  private
    FExportEncoding: TEpiEncoding;
    FExportLines: TStrings;
    function    EncodeString(Const Str: string): string;
    procedure   RaiseError(Const Msg: string);
    procedure   WriteByte(St: TStream; Val: ShortInt);
    procedure   WriteWord(St: TStream; Val: SmallInt);
    procedure   WriteInt(St: TStream; Val: LongInt);
    procedure   WriteSingle(St: TStream; Val: Single);
    procedure   WriteDouble(St: TStream; Val: Double);
    procedure   WriteEncString(St: TStream; Const Str: string; Const Count: Integer; Terminate: Boolean = True);
    procedure   WriteString(St: TStream; Const Str: string; Const Count: Integer; Terminate: Boolean = True);
  public
    constructor Create;
    destructor  Destroy; override;
    function    ExportStata(Const aFilename: string; Const Doc: TEpiDocument;
      Const DatafileIndex: integer = 0;
      Const StataVersion: TEpiStataVersion = dta8): Boolean;
    property    ExportEncoding: TEpiEncoding read FExportEncoding write FExportEncoding default eeUTF8;
    property    ExportLines: TStrings read FExportLines;
  end;

implementation

uses
  FileUtil, epistringutils, math, LConvEncoding, dateutils;


{ TEpiExport }

function TEpiExport.EncodeString(const Str: string): string;
begin
  result := ConvertEncoding(Str, 'utf8', EpiEncodingToString[ExportEncoding]);
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
  WriteString(St, EncodeString(Str), Count, Terminate);
end;

procedure TEpiExport.WriteString(St: TStream; Const Str: string; Const Count: Integer; Terminate: Boolean = True);
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

function TEpiExport.ExportStata(const aFilename: string;
  const Doc: TEpiDocument; const DatafileIndex: integer;
  const StataVersion: TEpiStataVersion): Boolean;
var
  ValBuf,
  ByteBuf: Array of Byte;
  TypeList,
  CharBuf: Array of Char;
  NVar, NObs,
  I, J, TmpInt: Integer;
  TmpStr: string;
  TmpChar: Char;
  WritenValueLabels: TStringList;
  UniqueValueLabels: TStringList;
  CurRec: Integer;
  CurField: Integer;
  DataFile: TEpiDataFile;

  // Version specific variables.
  FieldNameLength, StrBaseNum, FileLabelLength,
  FmtLength: Integer;
  ByteChar, IntChar, LongChar,
  FloatChar, DoubleChar: Char;
  MissingBaseNum: Cardinal;
  FieldNames: TStrings;
  DataStream: TFileStream;
  VLblSet: TEpiValueLabelSet;
  TimeFields: TStringList;

  procedure WriteFloat(Val: Double; Const MisVal: string);
  var
    FltByte: Array[0..7] of Byte absolute Val;
  begin
    if MisVal[1] = '.' then
      case MisVal[2] of
        'a': FltByte[5] := 1;
        'b': FltByte[5] := 2;
        'c': FltByte[5] := 3;
      else
        FltByte[5] := 0;
      end;
    WriteDouble(DataStream, Val);
  end;


  function UniqueValueLabelName(Const Str: string; Const Count: Integer): string;
  var
    i, j: integer;
  begin
    result := copy(StringReplace(EpiUtf8ToAnsi(Str), ' ', '_', [rfReplaceAll]), 1, Count-1);
    i := 1;
    while UniqueValueLabels.Find(result, j) do
    begin
      result := Copy(result, 1, Count - Length(IntToStr(i - 1))) + IntToStr(i);
      Inc(i);
    end;
  end;

begin
  Result := false;
  // Sanity checks:
  if Trim(aFilename) = '' then Exit;

  if not Assigned(Doc) then Exit;

  with Doc.DataFiles[DatafileIndex] do
  try
    DataStream := TFileStream.Create(aFileName, fmCreate);

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
    if StataVersion >= dta6 THEN
      FileLabelLength := 81;
    // - change in Ver. 7
    IF StataVersion >= dta7 THEN
      FieldNameLength := 33;
    // - changed in Ver. 8
    IF StataVersion >= dta8 THEN
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
    if StataVersion >= dta10 THEN
      FmtLength := 49;

    NVar := Fields.Count;
    NObs := Size;

    // ********************************
    //           STATA HEADER
    // ********************************
    SetLength(ByteBuf, 4);
    ByteBuf[0] := Byte(StataVersion);
    ByteBuf[1] := 2;                                          // Use LOHI order of data
    ByteBuf[2] := 1;                                          // Filetype - only 1 is legal value
    ByteBuf[3] := 0;                                          // Unused
    DataStream.Write(ByteBuf[0], 4);
    WriteWord(DataStream, NVar);                                       // Number of Variables
    WriteInt(DataStream,  NObs);                                       // Number of records


    IF trim(Caption.Text) <> '' THEN
      TmpStr := Trim(Caption.Text)
    ELSE
      TmpStr := Format('Datafile created by EpiData based on %s', [SysToUTF8(ExtractFilename(UTF8ToSys(aFilename)))]);

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
      WITH Field[i] DO
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
          ftDMYToday, ftMDYToday, ftYMDToday:
             TmpChar := LongChar;
          ftTime, ftTimeNow,
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
      WITH Field[i] DO
      BEGIN
        TmpStr := Trim(Id);
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
    WITH Field[i] DO
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
        ftDMYToday, ftMDYToday, ftYMDToday:
          TmpStr := '%d';
        ftTime, ftTimeNow:
          if StataVersion >= dta10 then
          begin
            // Stata 10 supports a new time format!
            TmpStr := '%tcHH:MM:SS';
            TimeFields.AddObject(Id, Field[i]);
          end
          else
            TmpStr := '%6.5f';
      END;   //case FeltType
      WriteString(DataStream, TmpStr, FmtLength);
    END;  //for - with

    // - lbllist: names af value label
    WritenValueLabels := TStringList.Create();
    WritenValueLabels.Sorted := true;
    UniqueValueLabels := TStringList.Create();
    UniqueValueLabels.Sorted := true;
    for i := 0 to NVar - 1 do
    with Field[i] do
    begin
      TmpStr := '';
      if Assigned(ValueLabelSet) and (FieldType = ftInteger) then
      begin
        TmpStr := ValueLabelSet.Id;
        if WritenValueLabels.Find(TmpStr, j) then
          // ValuelabelSet already made unique and prepared for finale write.
          TmpStr := UniqueValueLabels[j]
        else begin
          // ValuelabelSet not seen before...
          WritenValueLabels.Add(TmpStr);
          TmpStr := UniqueValueLabelName(TmpStr, FieldNameLength);
          if TmpStr <> '' then
            UniqueValueLabels.Add(TmpStr);
        end;
      end;
      WriteString(DataStream, TmpStr, FieldNameLength);
    end;

    // ********************************
    //      STATA VARIABLE LABELS
    // ********************************
    FOR i := 0 TO NVar - 1 DO
    WITH Field[i] DO
      WriteEncString(DataStream, Trim(Question.Text), FileLabelLength);

    // ********************************
    //      STATA EXPANSION FIELDS
    // ********************************
    if StataVersion < dta7 then
    begin
      // Expansion field is 3 bytes before version 7
      // Skip expansion field - since we don't know the format (documents no
      // longer exists on how they are formatted).
      WriteByte(DataStream, 0); // 3 bytes....
      WriteWord(DataStream, 0);
    end;
    if StataVersion >= dta7 then
    begin
      // Expansion field is 5 bytes from version 7
      if ExportLines.Count > 0 then
      begin
        // We start out by writing the length of the notes in a "special" characteristic called 'note0'
        WriteByte(DataStream, 1);
        TmpStr := IntToStr(ExportLines.Count);
        // TmpInt = len  (sum of 2 * 33 + length(TmpStr)
        TmpInt := 2*33 + Length(TmpStr) + 1;
        WriteInt(DataStream, TmpInt);
        WriteString(DataStream, '_dta', 33);
        WriteString(DataStream, 'note0', 33);
        WriteString(DataStream, TmpStr, Length(TmpStr) + 1);
      end;
      for i := 0 to ExportLines.Count - 1 do
      begin
        TmpStr := ExportLines[i];
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

        TmpStr := 'Time variable: Formatted with %tcHH:MM:SS. See "help dates_and_times, marker(formatting)" for details. Date coded as Jan. 1st 1960.';
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
      FOR CurRec := 0 TO NObs-1 DO
      BEGIN
        FOR CurField := 0 TO NVar - 1 DO
        With Field[CurField] do
        Case TypeList[CurField] of
          StataByteConst:
            begin
              // Specific missing values
              // TODO : MissingValues (STATA)
              if IsMissing[CurRec] then
                WriteByte(DataStream, $7F)
              else
                WriteByte(DataStream, AsInteger[CurRec]);
            end;
          StataIntConst:
            begin
              if IsMissing[CurRec] then
                WriteWord(DataStream, $7FFF)
              else
                WriteWord(DataStream, AsInteger[CurRec]);
            end;
          StataLongConst:
            begin
              if IsMissing[CurRec] then
                WriteInt(DataStream, $7FFFFFFF)
              else
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
                WriteDouble(DataStream, Power(2, 1023))
              else
                if (FieldType in TimeFieldTypes) and
                   (StataVersion >= dta10) then
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

    {Write VALUE-LABELS}
    IF StataVersion = dta4 THEN
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
      FOR I := 0 TO WritenValueLabels.Count - 1 DO
      BEGIN
        VLblSet := ValueLabels.GetValueLabelSetById(WritenValueLabels[i]);
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
          TmpStr := EncodeString(VLblSet.ValueLabels[J].TheLabel.Text);
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
        WriteString(DataStream, UniqueValueLabels[I], FieldNameLength);   // labname
        DataStream.Write(#0#0#0, 3);                                      // padding...

        {Fill out value_label_table}
        WriteInt(DataStream, NObs);                                       // n
        WriteInt(DataStream, CurRec);                                     // txtlen
        DataStream.Write(ByteBuf[0], 4 * NObs);                           // off[]
        DataStream.Write(ValBuf[0], 4 * NObs);                            // val[]
        DataStream.Write(CharBuf[0], CurRec);                             // txt[]
      END;  //write value labels in stata 6 version
    END;  //for i

    Result := true;
  finally
    if Assigned(DataStream) then FreeAndNil(DataStream);
    if Assigned(WritenValueLabels) then FreeAndNil(WritenValueLabels);
    if Assigned(UniqueValueLabels) then FreeAndNil(UniqueValueLabels);
  end;
end;

end.

