unit epiexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, epidatafilestypes;

type

  { TEpiExport }

  TEpiExport = class(TObject)
  private
    procedure   RaiseError(Const Msg: string);
    procedure   WriteByte(St: TStream; Val: ShortInt);
    procedure   WriteWord(St: TStream; Val: SmallInt);
    procedure   WriteInt(St: TStream; Val: LongInt);
    procedure   WriteSingle(St: TStream; Val: Single);
    procedure   WriteDouble(St: TStream; Val: Double);
    procedure   WriteString(St: TStream; Const Str: string; Const Count: Integer; Terminate: Boolean = True);
  public
    constructor Create;
    destructor  Destroy; override;
    function    ExportStata(Const aFilename: string; Const DataFile: TEpiDataFile): Boolean;
  end;

implementation

uses
  FileUtil, epistringutils, math;

{ TEpiExport }

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

procedure TEpiExport.WriteString(St: TStream; Const Str: string; Const Count: Integer; Terminate: Boolean = True);
var
  StrBuf: PChar;
  z: integer;
begin
  if Terminate then z := 0 else z := 1;
  StrBuf := StrAlloc(Count + z);
  StrPLCopy(PChar(@StrBuf[0]), EpiUtf8ToAnsi(Str), Count - 1 + z);
  St.Write(StrBuf[0], Count);
  StrDispose(StrBuf);
end;

constructor TEpiExport.Create;
begin

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
  FileVersion: Integer;

Const
  ByteConst   = #251;
  IntConst    = #252;
  LongConst   = #253;
  FloatConst  = #254;
  DoubleConst = #255;

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


  function UniqueValueLabelName(Str: string; Const Count: Integer): string;
  var
    i, j: integer;
  begin
    result := copy(Str, 1, Count-1);
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
    FileVersion := 0; //ExpSetting^.FileVersion;

    if not (FileVersion in [$69, $6C, $6E, $71, $72]) then
      FileVersion := $72;                    // Default to Version 10 (Latest of May 2009)

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
    if FileVersion >= $6C THEN
      FileLabelLength := 81;
    // - change in Ver. 7
    IF FileVersion >= $6E THEN
      FieldNameLength := 33;
    // - changed in Ver. 8
    IF FileVersion >= $71 THEN
    BEGIN
      StrBaseNum := 0;
      MissingBaseNum := 27;
      ByteChar   := ByteConst;
      IntChar    := IntConst;
      LongChar   := LongConst;
      FloatChar  := FloatConst;
      DoubleChar := DoubleConst;
    END;
    // - changed in Ver. 10
    if FileVersion >= $72 THEN
      FmtLength := 49;

    NVar := Fields.Count;
    NObs := Size;

    // ********************************
    //           STATA HEADER
    // ********************************
    SetLength(ByteBuf, 4);
    ByteBuf[0] := FileVersion;
    ByteBuf[1] := 2;                                          // Use LOHI order of data
    ByteBuf[2] := 1;                                          // Filetype - only 1 is legal value
    ByteBuf[3] := 0;                                          // Unused
    DataStream.Write(ByteBuf[0], 4);
    WriteWord(DataStream, NVar);                                       // Number of Variables
    WriteInt(DataStream,  NObs);                                       // Number of records


    IF trim(Name.Text) <> '' THEN
      TmpStr := Trim(UTF8ToSys(Name.Text))
    ELSE
      TmpStr := Format('Datafile created by EpiData based on %s', [ExtractFilename(UTF8ToSys(aFilename))]);

    IF Length(TmpStr) > (FileLabelLength - 1) THEN
      TmpStr := Copy(TmpStr, 1, FileLabelLength - 1);

    // data_label \0 terminated.
    WriteString(DataStream, TmpStr, FileLabelLength);

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
 {            IF Length > 80 THEN
              TmpChar := Chr(207)
            ELSE
              TmpChar := Chr(StrBaseNum + FieldLength);}
          ftBoolean:
            TmpChar := ByteChar;
          ftDMYDate, ftMDYDate, ftYMDDate,
          ftDMYToday, ftMDYToday, ftYMDToday:
             TmpChar := LongChar;
          ftFloat:
             TmpChar := DoubleChar;
        END;  //Case
        ByteBuf[i] := Ord(TmpChar);
        TypeList[i] := TmpChar;
        // update typelist to consts.
        if TypeList[i] = ByteChar then   TypeList[i] := ByteConst;
        if TypeList[i] = IntChar then    TypeList[i] := IntConst;
        if TypeList[i] = LongChar then   TypeList[i] := LongConst;
        if TypeList[i] = FloatChar then  TypeList[i] := FloatConst;
        if TypeList[i] = DoubleChar then TypeList[i] := DoubleConst;
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
      END;   //case FeltType
      WriteString(DataStream, TmpStr, FmtLength);
    END;  //for - with

    // - lbllist: names af value label
    SetLength(ByteBuf, 33*NVar);
    FillByte(ByteBuf[0], 33*NVar, 0);
    DataStream.Write(ByteBuf[0], 33*NVar);
    // TODO : VALUELABELS
{    WritenValueLabels := TStringList.Create();
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
          TmpStr := ''
        else
          WritenValueLabels.Add(TmpStr);

        // Only for interger fields.
        if not (FieldType = ftInteger) then
          TmpStr := '';

        TmpStr := UniqueValueLabelName(TmpStr, FieldNameLength);
        if TmpStr <> '' then
          UniqueValueLabels.Add(TmpStr);
      end;
      WriteString(TmpStr, FieldNameLength);
    end;}

    // ********************************
    //      STATA VARIABLE LABELS
    // ********************************
    FOR i := 0 TO NVar - 1 DO
    BEGIN
      WITH Field[i] DO
      BEGIN
        TmpStr := Trim(EpiUtf8ToAnsi(Question.Caption.Text));
        WriteString(DataStream, TmpStr, FileLabelLength);
      END;  //with
    END;  //for eN

    // ********************************
    //      STATA EXPANSION FIELDS
    // ********************************
    // - skip expansion fields
    WriteWord(DataStream, 0);
    WriteByte(DataStream, 0); // 3 bytes....
    if FileVersion >= $6E then
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
          ByteConst:   begin
                          // Specific missing values
                          // TODO : MissingValues (STATA)
                          {
                          IF (FileVersion >= $71) AND (FieldType in [ftInteger, ftIDNUM, ftFloat]) AND
                             (FieldDecimals = 0) AND (FieldLength < 10) THEN
                          BEGIN
                            if Assigned(CheckField) then
                            begin
                              IF TmpStr = CheckField.MissingValues[0] THEN TmpStr := '.a';
                              IF TmpStr = CheckField.MissingValues[1] THEN TmpStr := '.b';
                              IF TmpStr = CheckField.MissingValues[2] THEN TmpStr := '.c';
                            end;
                            IF TmpStr = FileProperties.GlobalMissingVal[0] THEN TmpStr := '.a';
                            IF TmpStr = FileProperties.GlobalMissingVal[1] THEN TmpStr := '.b';
                            IF TmpStr = FileProperties.GlobalMissingVal[2] THEN TmpStr := '.c';
                          END;     }
                         if IsMissing[CurRec] then
                           WriteByte(DataStream, $7F)
                         else
                           WriteByte(DataStream, AsInteger[CurRec]);
                       end;
          IntConst:    begin
                         if IsMissing[CurRec] then
                           WriteWord(DataStream, $7FFF)
                         else
                           WriteWord(DataStream, AsInteger[CurRec]);
                       end;
          LongConst:   begin
                         if IsMissing[CurRec] then
                           WriteInt(DataStream, $7FFFFFFF)
                         else
                           {Date is converted from Delphi's/Lazarus 30/12-1899 base
                            to Stata's 1/1-1960 base by substracting 21916 days}
                           If (FieldType in DateFieldTypes) then
                             WriteInt(DataStream, AsDate[CurRec] - 21916)
                           else
                             WriteInt(DataStream, AsInteger[CurRec]);
                       end;
          DoubleConst: begin
                         if IsMissing[CurRec] then
                           WriteDouble(DataStream, Power(2, 1023))
                         else
                           WriteDouble(DataStream, AsFloat[CurRec]);
                       end
        else
          WriteString(DataStream, AsString[CurRec], 207);
        end;



(*


          TmpStr := AsString[CurRec-1];

          IF trim(TmpStr)='' THEN
            TmpStr := '..';


          Case TypeList[CurField] of
            ByteConst,
            IntConst,
            LongConst:
              begin
                Case TypeList[CurField] of
                  ByteConst:
                    begin
                      I := $7F;
                      J := 1;
                    end;
                  IntConst:
                    Begin
                      I := $7FFF;
                      J := 2;
                    end;
                  LongConst:
                    Begin
                      I := $7FFFFFFF;
                      J := 4;
                    end;
                end;
                if TmpStr[1] = TEpiStringField.DefaultMissing[1] then
                begin
                  TmpInt := I - MissingBaseNum + 1;
                  case TmpStr[2] of
                    'a': Inc(TmpInt);
                    'b': Inc(TmpInt, 2);
                    'c': Inc(TmpInt, 3);
                  end;
                end else begin
                  // Dates can be encoded as LongInts.
                  If (FieldType in DateFieldTypes) then
                    {Date is converted from Delphi's/Lazarus 30/12-1899 base
                     to Stata's 1/1-1960 base by substracting 21916 days}
                    TmpInt := AsDate[CurRec-1] - 21916
                  else if FieldType = ftBoolean then
                    TmpInt := Integer(StrToBool(TmpStr))
                  else
                    TmpInt := StrToInt(TmpStr);
                end;
                WriteInts(TmpInt, J);
              end; // Byte, Int and Long.
//          FloatConst: ( We never export to Float type.)
            DoubleConst:
              Begin
                if TmpStr = TEpiStringField.DefaultMissing then
                  WriteFloat(Power(2, 1023), TmpStr)
                else
                  WriteFloat(StrToFloat(TmpStr), '-');
              End;
          else
{           if (FieldType in DateFieldTypes) and not
               EpiIsDate(TmpStr, FieldType) then
            begin
              ErrorCode := EPI_EXPORT_FAILED;
              ErrorText := Format(Lang(22306, 'Illegal date found in record # %d, field %s~Export terminates.'), [CurRec, FieldName]);
              EpiLogger.AddError(Classname, 'ExportStata', ErrorText, 22306);
              Exit;
            end;}
            if TmpStr = '..' then TmpStr := '';
            WriteString(TmpStr, Length, False);
          end;
        END;  //for CurField      *)
      END;  //for CurObs
    EXCEPT

      RaiseError('Error in exporting to Stata');
      Exit;
    END;  //try..Except

    {Write VALUE-LABELS}
    // TODO : VALUELABELS
(*    IF FileVersion = $69 THEN
    BEGIN
      //write value labels in Stata ver. 4/5 format
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
      END;
    END ELSE BEGIN
      //write value labels in Stata ver. 6+ format
      SetLength(CharBuf, 32000);      // Write Txt[] - max posible length is 32000 chars.
      FOR I := 0 TO WritenValueLabels.Count - 1 DO
      BEGIN
        VLblSet := ValueLabels.ValueLabelSetByName(WritenValueLabels[i]);
        NObs := VLblSet.Count;
        SetLength(ByteBuf, 4 * NObs);   // Write Off[]
        SetLength(ValBuf,  4 * NObs);   // Write Val[]
        FillChar(CharBuf[0], 32000, 0); // reset Txt[]

        CurRec := 0;                    // Holds Off[i] index into Txt[]
        for J := 0 to NObs - 1 do
        begin
          Move(CurRec, ByteBuf[J * 4], 4);
          // TODO : ValueLabels in STATA
          TmpInt := 0; //StrToInt(VLblSet.Values[J]);
          Move(TmpInt, ValBuf[J * 4], 4);
          // TODO : ValueLabels in STATA
          TmpStr := ' '; //EpiUtf8ToAnsi(VLblSet.Labels[J]);
          Move(TmpStr[1], CharBuf[CurRec], Length(TmpStr));
          Inc(CurRec, Length(TmpStr) + 1);
        end;
        {Fill out value label header}
        TmpInt := 4 +                   // n
                  4 +                   // txtlen
                  (4 * NObs) +          // off[]
                  (4 * NObs) +          // val[]
                  CurRec;               // txt[]

        WriteInts(TmpInt, 4);                                   // len
        WriteString(UniqueValueLabels[I], FieldNameLength);     // labname
        WriteInts(0, 3);                                        // padding...

        {Fill out value_label_table}
        WriteInts(NObs, 4);                                     // n
        WriteInts(CurRec, 4);                                   // txtlen
        DataStream.Write(ByteBuf[0], 4 * NObs);                  // off[]
        DataStream.Write(ValBuf[0], 4 * NObs);                   // val[]
        DataStream.Write(CharBuf[0], CurRec);                    // txt[]
      END;  //write value labels in stata 6 version
    END;  //for i*)

    Result := true;
  finally
    if Assigned(DataStream) then FreeAndNil(DataStream);
//    if Assigned(WritenValueLabels) then FreeAndNil(WritenValueLabels);
//    if Assigned(UniqueValueLabels) then FreeAndNil(UniqueValueLabels);
  end;
end;

end.

