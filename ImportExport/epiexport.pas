unit epiexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, epidatafilestypes, epivaluelabels,
  epieximtypes;

type

  { TEpiExport }

  TEpiExport = class(TObject)
  private
    FExportEncoding: TEpiEncoding;
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
    function    ExportStata(Const aFilename: string; Const DataFile: TEpiDataFile): Boolean;
    property    ExportEncoding: TEpiEncoding read FExportEncoding write FExportEncoding;
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
end;

destructor TEpiExport.Destroy;
begin
  inherited Destroy;
end;

function TEpiExport.ExportStata(const aFilename: string;
  const DataFile: TEpiDataFile): Boolean;
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
//  VLblSet: TValueLabelSet;

  // Version specific variables.
  FieldNameLength, StrBaseNum, FileLabelLength,
  FmtLength: Integer;
  ByteChar, IntChar, LongChar,
  FloatChar, DoubleChar: Char;
  MissingBaseNum: Cardinal;
  FieldNames: TStrings;
  DataStream: TFileStream;
  FileVersion: TEpiStataVersion;
  VLblSet: TEpiValueLabelSet;

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

  if not Assigned(DataFile) then Exit;

  with DataFile do
  try
    DataStream := TFileStream.Create(aFileName, fmCreate);
    FileVersion := dta8;                    // Default to Version 8

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
    if FileVersion >= dta6 THEN
      FileLabelLength := 81;
    // - change in Ver. 7
    IF FileVersion >= dta7 THEN
      FieldNameLength := 33;
    // - changed in Ver. 8
    IF FileVersion >= dta8 THEN
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
    if FileVersion >= dta10 THEN
      FmtLength := 49;

    NVar := Fields.Count;
    NObs := Size;

    // ********************************
    //           STATA HEADER
    // ********************************
    SetLength(ByteBuf, 4);
    ByteBuf[0] := Byte(FileVersion);
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
            // Using stringlength of 207 is maximun covered by Stata file format (at least) since version #4
            // We do this to maximize the use of UTF8 charaters. Users can then manually adjust dataset in
            // Stata to get a smaller file size.
            TmpChar := Chr(207);
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
        // update typelist to consts.
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
        TmpStr := Trim(Name);
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
          TmpStr := '%' + IntToStr(207) + 's';
        ftDMYDate, ftMDYDate, ftYMDDate,
        ftDMYToday, ftMDYToday, ftYMDToday:
          TmpStr := '%d';
        ftTime, ftTimeNow:
          if FileVersion >= dta10 then
            // Stata 10 supports a new time format!
            TmpStr := '%tcHH:NN:SS'
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
        TmpStr := ValueLabelSet.Name;
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
    // - skip expansion fields
    WriteWord(DataStream, 0);
    WriteByte(DataStream, 0); // 3 bytes....
    if FileVersion >= dta7 then  // Expansion field is 5 bytes from version 7
      WriteWord(DataStream, 0); // 5 bytes in total...

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
                   (FileVersion >= dta10) then
                  WriteDouble(DataStream, MilliSecondsBetween(StataBaseDate + AsDate[CurRec], StataBaseDateTime))
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
    IF FileVersion = dta4 THEN
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
        VLblSet := ValueLabels.GetValueLabelSetByName(WritenValueLabels[i]);
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

