unit epiexport_stata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles, epivaluelabels,
  epiexportsettings, epieximtypes, epidatafilestypes, epicustombase, fgl;

type

  { TEpiStataExport }

  TEpiStataExport = class
  private
    const
      STATA_CONTENT_KEY = 'STATA_CONTENT_KEY';
    type
      TIntValueLabelMap = specialize TFPGMap<EpiInteger, Integer>;
      TFloatValueLabelMap = specialize TFPGMap<EpiFloat, Double>;

      TStataContent = record
        Typ: Integer;
        Name: string;
        Format: string;
        IntValueLabelMap: TIntValueLabelMap;
        FloatValueLabelMap: TFloatValueLabelMap;
      end;
      PStataContent = ^TStataContent;
  private
    FDataFile: TEpiDataFile;
    FDataFileSetting: TEpiExportDatafileSettings;
    FStataSettings: TEpiStataExportSetting;
    FStataVersion: TEpiStataVersion;
    FStream: TStream;
  private
    function EncodeString(Const S: String): string;

  private
    procedure WriteStartTag(Const TagName: string; MapIndex: Integer = -1);
    procedure WriteEndTag(Const TagName: string);

    // Write text content in tag
    procedure  WriteAsString(Const S: string); overload;
    procedure  WriteAsString(Const S: string; Const Len: Integer); overload;
    procedure  WriteByte(Value: Byte);
    procedure  WriteWord(Value:  Word);
    procedure  WriteDWord(Value: DWord);
    procedure  Write6Word(Value: QWord); // The very-very unusual 6-byte integer, used by the StrLs in Stata 14
    procedure  WriteQWord(Value: QWord);

{    // Write signed data
    procedure  WriteSbyte(Value: ShortInt);
    procedure  WriteInt(Value: SmallInt);
    procedure  WriteLong(Value: LongInt);    }
    procedure  WriteDouble(Value: Double);

  private
    { CustomData Helpers }
    FMapOffsets: Array[0..13] of QWord;
    function MissingDouble(Const MisVal: Word): Double;
    procedure SetupFields;
    procedure SetupValueLabels;
    function StataContent(Const Field: TEpiField): PStataContent;

  private
    { .dta section loaders }
    procedure WriteHeader;
    procedure WriteMap;
    procedure WriteVariableTypes;
    procedure WriteVariableNames;
    procedure WriteSortList;
    procedure WriteFormat;
    procedure WriteValueLabelNames;
    procedure WriteVariabelLabels;
    procedure WriteCharacteristics;
    procedure WriteData;
    procedure WriteStrls;
    procedure WriteValueLabels;
    procedure WriteValueLabel(ValueLabelSet: TEpiValueLabelSet);
  public
    function  ExportStata(StataSettings: TEpiStataExportSetting): boolean;
  end;

implementation

uses
  epistringutils, LConvEncoding, LazUTF8, epifields_helper,
  math, epimiscutils;

{ TEpiStataExport }

function TEpiStataExport.EncodeString(const S: String): string;
begin
  result := ConvertEncoding(
              S,
              'utf8',
              EpiEncodingToString[FStataSettings.Encoding]
            );
end;

procedure TEpiStataExport.WriteStartTag(const TagName: string; MapIndex: Integer
  );
begin
  if (MapIndex >= 0) then
    FMapOffsets[MapIndex] := FStream.Position;
  WriteAsString('<' + TagName + '>');
end;

procedure TEpiStataExport.WriteEndTag(const TagName: string);
begin
  WriteAsString('</' + TagName + '>');
end;

procedure TEpiStataExport.WriteAsString(const S: string);
begin
  WriteAsString(S, Length(S));
end;

procedure TEpiStataExport.WriteAsString(const S: string; const Len: Integer);
var
  S1: String;
  L: Integer;
begin
  S1 := S;

  L := Length(S1);
  if (L < Len) then
  begin
    SetLength(S1, Len);
    FillChar(S1[L + 1], Len - L, #0);
  end;

  FStream.Write(S1[1], Len);
end;

procedure TEpiStataExport.WriteByte(Value: Byte);
begin
  FStream.WriteByte(Value);
end;

procedure TEpiStataExport.WriteWord(Value: Word);
begin
  FStream.WriteWord(Value);
end;

procedure TEpiStataExport.WriteDWord(Value: DWord);
begin
  FStream.WriteDWord(Value);
end;

procedure TEpiStataExport.Write6Word(Value: QWord);
var
  Buffer: array[0..7] of byte absolute Value;
begin
  FStream.WriteBuffer(Buffer[0], 6);
//  FStream.WriteQWord(Value);
end;

procedure TEpiStataExport.WriteQWord(Value: QWord);
begin
  FStream.WriteQWord(Value);
end;

procedure TEpiStataExport.WriteDouble(Value: Double);
begin
  FStream.Write(Value, 8);
end;

function TEpiStataExport.MissingDouble(const MisVal: Word): Double;
var
  FltByte: Array[0..7] of Byte absolute Result;
begin
  FltByte[0] := 0;
  FltByte[1] := 0;
  FltByte[2] := 0;
  FltByte[3] := 0;
  FltByte[4] := hi(MisVal);
  FltByte[5] := lo(MisVal);
  FltByte[6] := $e0;
  FltByte[7] := $7f;
end;

function IntCompareValueLabelKeys(const Key1, Key2: EpiInteger): Integer;
begin
  result := CompareValue(Key2, Key1);
end;

function FloatCompareValueLabelKeys(const Key1, Key2: EpiFloat): Integer;
begin
  result := CompareValue(Key2, Key1);
end;

procedure TEpiStataExport.SetupFields;
var
  F: TEpiField;
  Content: PStataContent;
  StataType: Integer;
  I64: EpiInteger;
  IntValueLabelMap: TIntValueLabelMap;
  IMissingVal: Integer;
  FloatValueLabelMap: TFloatValueLabelMap;
  i: Integer;

begin
  for F in FDataFile.Fields do
  begin
    Content := New(PStataContent);


    // Find appropriate Stata type
    case F.FieldType of
      ftBoolean:
        StataType := StataByteConstXML;

      ftInteger,
      ftAutoInc:
        begin
          I64 := TEpiIntField(F).MaxValue;
          case I64 of
            // Stata byte
            0..StataMaxByte:
              StataType := StataByteConstXML;

            (StataMaxByte+1)..StataMaxInt:
              StataType := StataIntConstXML;

            (StataMaxInt+1)..StataMaxLong:
              StataType := StataLongConstXML;
          else
            StataType := StataDoubleConstXML;
          end;

          I64 := TEpiIntField(F).MinValue;
          case I64 of
            // Stata byte
            StataMinByte..0:
              StataType := Min(StataByteConstXML, StataType);

            StataMinInt..(StataMinByte-1):
              StataType := Min(StataIntConstXML, StataType);

            StataMinLong..(StataMinInt-1):
              StataType := Min(StataLongConstXML, StataType);
          end;
        end;

      ftString,
      ftUpperString:
        begin
          if (F.MaxByteLength > 2045) then
            StataType := StataStrLsConstXML
          else
            StataType := F.Length;
        end;

      ftMemo:
        begin
          I := F.MaxByteLength;
          if (I > 2045) then
            StataType := StataStrLsConstXML
          else
            StataType := I;
        end;

      ftDMYDate, ftMDYDate, ftYMDDate,
      ftDMYAuto, ftMDYAuto, ftYMDAuto:
        StataType := StataLongConstXML;

      ftTime, ftTimeAuto,
      ftFloat:
        StataType := StataDoubleConstXML;
    end;
    Content^.Typ := StataType;


    // Convert EpiData missing values to Stata missing values.
    if Assigned(F.ValueLabelSet) and
       (F.ValueLabelSet.MissingCount > 0)
    then
      case F.FieldType of
        ftInteger:
          begin
            IntValueLabelMap := TIntValueLabelMap.Create;
            IntValueLabelMap.OnKeyCompare := @IntCompareValueLabelKeys;

            for i := 0 to F.ValueLabelSet.Count - 1 do
              if F.ValueLabelSet[i].IsMissingValue then
                IntValueLabelMap.Add(TEpiIntValueLabel(F.ValueLabelSet[i]).Value);

            case Content^.Typ of
              StataLongConstXML   : IMissingVal := StataMaxLong + 2;
              StataIntConstXML    : IMissingVal := StataMaxInt  + 2;
              StataByteConstXML   : IMissingVal := StataMaxByte + 2;
            end;

            for i := 0 to IntValueLabelMap.Count - 1 do
              IntValueLabelMap.Data[i] := PostInc(IMissingVal);

            Content^.IntValueLabelMap := IntValueLabelMap;
          end;

        ftFloat:
          begin
            FloatValueLabelMap := TFloatValueLabelMap.Create;
            FloatValueLabelMap.OnKeyCompare := @FloatCompareValueLabelKeys;

            for i := 0 to F.ValueLabelSet.Count - 1 do
              if F.ValueLabelSet[i].IsMissingValue then
                FloatValueLabelMap.Add(TEpiFloatValueLabel(F.ValueLabelSet[i]).Value);

            IMissingVal := 1;

            for i := 0 to FloatValueLabelMap.Count - 1 do
              FloatValueLabelMap.Data[i] := MissingDouble(PostInc(IMissingVal));

            Content^.FloatValueLabelMap := FloatValueLabelMap;
          end;
      end;

    F.AddCustomData(STATA_CONTENT_KEY, TObject(Content));
  end;
end;

procedure TEpiStataExport.SetupValueLabels;
var
  Len: Integer;
  ValueLabelNames: TStringList;
  VLset: TEpiCustomItem;

  function UniqueValueLabelName(Const Str: string; Const Count: Integer): string;
  var
    i: integer;
  begin
    case FStataVersion of
      dta13: Result := EncodeString(Str);
      dta14: Result := Str;
    end;

    Result := UTF8Copy(StringReplace(Result, ' ', '_', [rfReplaceAll]), 1, Count-1);

    i := 1;
    if result = '' then result := '_ValueLabel';
    while ValueLabelNames.IndexOf(result) >= 0 do
    begin
      result := Copy(result, 1, Count - Length(IntToStr(i - 1))) + IntToStr(i);
      Inc(i);
    end;
  end;

begin
  ValueLabelNames := TStringList.Create;

  for VLset in FDataFile.ValueLabels do
  // ValueLabels in Stata 13+ can have up to 32-characters (UTF-8 or Not)
     VLSet.Name := UniqueValueLabelName(VLSet.Name, 32);

  ValueLabelNames.Free;
end;

function TEpiStataExport.StataContent(const Field: TEpiField): PStataContent;
begin
  Result := PStataContent(Field.FindCustomData(STATA_CONTENT_KEY));
end;

procedure TEpiStataExport.WriteHeader;
var
  S: String;
begin
  // HEADER
  WriteStartTag('header', 0);

  // <release>
  WriteStartTag('release');
  WriteAsString(IntToStr(Integer(FStataSettings.Version)));
  WriteEndTag('release');

  // <byteorder>
  WriteStartTag('byteorder');
  {$IFDEF ENDIAN_LITTLE}
  WriteAsString('LSF');
  {$ELSE}
  WriteAsString('MSF');
  {$ENDIF}
  WriteEndTag('byteorder');

  // <K>  = Number of fields
  WriteStartTag('K');
  WriteWord(FDataFile.Fields.Count);
  WriteEndTag('K');

  // <N>  = Number of observartions/records
  WriteStartTag('N');
  case FStataVersion of
    dta13: WriteDWord(FDataFile.Size);
    dta14: WriteQWord(FDataFile.Size);
  end;
  WriteEndTag('N');

  // <label>  = EpiData DataFile Caption
  WriteStartTag('label');
  case FStataVersion of
    dta13:
      begin
        S := EpiCutString(EncodeString(FDataFile.Caption.Text), 80);
        WriteByte(Length(S));
      end;
    dta14:
      begin
        S := EpiCutString(FDataFile.Caption.Text, 80);
        WriteWord(Length(S));
      end;
  end;
  if Length(S) > 0 then
    WriteAsString(S);
  WriteEndTag('label');

  // <timestamp> - we ignore the timestamp.
  WriteStartTag('timestamp');
  WriteByte(17);
  WriteAsString(FormatDateTime('dd mmm yyyy hh":"nn', Now));
  WriteEndTag('timestamp');

  WriteEndTag('header');
end;

procedure TEpiStataExport.WriteMap;
begin
  WriteStartTag('map', 1);

  //1.       <stata_data>, definitionally 0
  WriteQWord(FMapOffsets[0]);
  //2.       <map>
  WriteQWord(FMapOffsets[1]);
  //3.       <variable_types>
  WriteQWord(FMapOffsets[2]);
  // 4.       <varnames>
  WriteQWord(FMapOffsets[3]);
  // 5.       <sortlist>
  WriteQWord(FMapOffsets[4]);
  // 6.       <formats>
  WriteQWord(FMapOffsets[5]);
  // 7.       <value_label_names>
  WriteQWord(FMapOffsets[6]);
  // 8.       <variable_labels>
  WriteQWord(FMapOffsets[7]);
  // 9.       <characteristics>
  WriteQWord(FMapOffsets[8]);
  // 10.       <data>
  WriteQWord(FMapOffsets[9]);
  // 11.       <strls>
  WriteQWord(FMapOffsets[10]);
  // 12.       <value_labels>
  WriteQWord(FMapOffsets[11]);
  // 13.       </stata_data>
  WriteQWord(FMapOffsets[12]);
  // 14.       end-of-file
  WriteQWord(FMapOffsets[13]);

  WriteEndTag('map');
end;

procedure TEpiStataExport.WriteVariableTypes;
var
  F: TEpiField;
begin
  WriteStartTag('variable_types', 2);

  for F in FDataFile.Fields do
    WriteWord(StataContent(F)^.Typ);

  WriteEndTag('variable_types');
end;

procedure TEpiStataExport.WriteVariableNames;
var
  Len: Integer;
  F: TEpiField;
  S: String;
  FieldNames: TStrings;
begin
  WriteStartTag('varnames', 3);

  case FStataVersion of
    //  32 Bytes (* 4 for UTF-8) and a terminal #0
    dta13: Len := (32 * 1) + 1;
    dta14: Len := (32 * 4) + 1;
  end;

  FieldNames := TStringList.Create;
  for F in FDataFile.Fields do
    begin
      case FStataSettings.FieldNameCase of
        fncUpper: S := UTF8UpperCase(F.Name);
        fncLower: S := UTF8LowerCase(F.Name);
        fncAsIs:  S := F.Name;
      end;

      case FStataSettings.Version of
        dta13: S := CreateUniqueAnsiVariableName(S, 32, FieldNames, true);
        dta14: S := CreateUniqueAnsiVariableName(S, 32, FieldNames, false);
      end;

      WriteAsString(S, Len);
      StataContent(F)^.Name := S;
    end;
  FieldNames.Free;

  WriteEndTag('varnames');
end;

procedure TEpiStataExport.WriteSortList;
var
  i: Integer;
begin
  WriteStartTag('sortlist', 4);

  for i := 1 to FDataFile.Fields.Count + 1 do
    WriteWord(0);

  WriteEndTag('sortlist');
end;

procedure TEpiStataExport.WriteFormat;
var
  S: String;
  Len: Integer;
  F: TEpiField;
begin
  // <format>
  WriteStartTag('formats', 5);

  case FStataVersion of
    dta13: Len := 49;
    dta14: Len := 57;
  end;

  for F in FDataFile.Fields do
    begin
      case F.FieldType of
        ftInteger, ftAutoInc:
          S := '%' + IntToStr(F.Length) + '.0f';
        ftFloat:
          S := '%' + IntToStr(F.Length) + '.' + IntToStr(F.Decimals) + 'f';
        ftBoolean:
          S := '%1.0f';
        ftString, ftUpperString, ftMemo:
          S := '%' + IntToStr(F.Length) + 's';
        ftDMYDate, ftMDYDate, ftYMDDate,
        ftDMYAuto, ftMDYAuto, ftYMDAuto:
          S := '%d';
        ftTime, ftTimeAuto:
          if FStataSettings.Version >= dta10 then
            begin
              // Stata 10 supports a new time format!
              S := '%tcHH:MM:SS';
            end
          else
            S := '%6.5f';
      end;

      WriteAsString(S, Len);
      StataContent(F)^.Format := S;
    end;

  WriteEndTag('formats');
end;

procedure TEpiStataExport.WriteValueLabelNames;
var
  Len: Integer;
  F: TEpiField;
  S: String;
begin
  WriteStartTag('value_label_names', 6);

  case FStataVersion of
    //  32 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := (32 * 1) + 1; // 33
    dta14: Len := (32 * 4) + 1; // 129
  end;

  for F in FDataFile.Fields do
    begin
      S := '';
      if Assigned(F.ValueLabelSet) and
         (F.FieldType = ftInteger)
      then
        S := F.ValueLabelSet.Name;

      WriteAsString(S, Len);
    end;

  WriteEndTag('value_label_names');
end;

procedure TEpiStataExport.WriteVariabelLabels;
var
  Len: Integer;
  F: TEpiField;
  S: String;
begin
  WriteStartTag('variable_labels', 7);

  case FStataVersion of
    //  80 Bytes (* 4 for UTF-8) and a terminal #0
    dta13: Len := (80 * 1) + 1; // 81
    dta14: Len := (80 * 4) + 1; // 321
  end;

  for F in FDataFile.Fields do
    begin
      S := F.Question.Text;

      case FStataVersion of
        dta13: S := EpiCutString(EpiUtf8ToAnsi(S), 80);
        dta14: S := EpiCutString(S, 80);
      end;

      WriteAsString(S, Len);
    end;

  WriteEndTag('variable_labels');
end;

procedure TEpiStataExport.WriteCharacteristics;
var
  Len: Integer;
  S: String;
  F: TEpiField;
  I: Integer;
  j: Integer;
begin
  WriteStartTag('characteristics', 8);

  case FStataVersion of
    //  32 bytes (* 4 for UTF-8) and a terminal #0
    dta13: Len := (32 * 1) + 1; // 81
    dta14: Len := (32 * 4) + 1; // 321
  end;

  // Write export lines to datafile notes first.
  if FStataSettings.ExportLines.Count > 0 then
  begin
    WriteStartTag('ch');

    // We start out by writing the length of the notes in a "special" characteristic called 'note0'
    S := IntToStr(FStataSettings.ExportLines.Count);

    // I = len  (sum of 2 * 33 + length(TmpStr)
    I := 2 * Len + Length(S) + 1;
    WriteDWord(I);
    WriteAsString('_dta', Len);
    WriteAsString('note0', Len);
    WriteAsString(S, Length(S) + 1);
    WriteEndTag('ch');
  end;

  for j := 0 to FStataSettings.ExportLines.Count - 1 do
  begin
    WriteStartTag('ch');

    S := FStataSettings.ExportLines[j];
    if FStataVersion = dta13 then
      S := EpiUtf8ToAnsi(S);

    I := Len +                 // First variable name or _dta for notes regarding the dataset.
         Len +                 // Character name, in our case 'noteX'
         Length(S) + 1;
    WriteDWord(I);
    WriteAsString('_dta', Len);
    WriteAsString('note' + IntToStr(j+1), Len);
    WriteAsString(S, Length(S) + 1);

    WriteEndTag('ch');
  end;

  // Then write notes for individual fields.
  for F in FDataFile.Fields do
  begin
    if not (F.FieldType in TimeFieldTypes) then
      Continue;

    WriteStartTag('ch');

    I := 2 * Len + 2;  // 2 = 1 char for "1" and 1 char for #0;
    WriteDWord(I);
    WriteAsString(F.Name, Len);
    WriteAsString('note0', Len);
    WriteAsString('1', 2);

    WriteEndTag('ch');
    WriteStartTag('ch');

    S := 'Time variable: Formatted with %tcHH:MM:SS. See "help dates_and_times, marker(formatting)" for details. Date coded as Jan. 1st 1960.';
    I := 2 * Len + Length(S) + 1;
    WriteDWord(I);
    WriteAsString(F.Name, Len);
    WriteAsString('note1', Len);
    WriteAsString(S, Length(S) + 1);

    WriteEndTag('ch');
  end;

  WriteEndTag('characteristics');
end;

procedure TEpiStataExport.WriteData;
var
  CurRec: Integer;
  F: TEpiField;
  Val: EpiInteger;
  FVal: Double;
begin
  WriteStartTag('data', 9);

  for CurRec := 0 to FDataFile.Size - 1 do
  begin
    for F in FDataFile.Fields do
    begin
      Case StataContent(F)^.Typ of
        StataStrLsConstXML:
          begin
            if F.IsMissing[CurRec] then
              WriteQWord(0)  // GSO = (0,0)
            else
              case FStataVersion of
                dta13:
                  begin
                    WriteDWord(FDataFile.Fields.IndexOf(F) +1);
                    WriteDWord(CurRec + 1);
                  end;
                dta14:
                  begin
                    WriteWord(FDataFile.Fields.IndexOf(F) + 1);
                    Write6Word(CurRec + 1);
                  end;
              end;
          end;

        StataDoubleConstXML:
          begin
            if F.IsMissing[CurRec] then
              FVal := MissingDouble(0)
            else
              FVal := F.AsFloat[CurRec];

            if (F.IsMissingValue[CurRec]) and
               (F.FieldType <> ftInteger)
            then
              FVal := StataContent(F)^.FloatValueLabelMap.KeyData[F.AsFloat[CurRec]];

            WriteDouble(FVal);
          end;

        StataLongConstXML:
          begin
            if F.IsMissing[CurRec] then
              Val := StataMaxLong + 1
            else
              Val := F.AsInteger[CurRec];

            if (F.FieldType in DateFieldTypes) then
              Dec(Val, StataBaseDate);

            if F.IsMissingValue[CurRec] then
              Val := StataContent(F)^.IntValueLabelMap.KeyData[F.AsInteger[CurRec]];

            WriteDWord(Val);
          end;

        StataIntConstXML:
          begin
            Val := F.AsInteger[CurRec];

            if F.IsMissing[CurRec] then
              Val := StataMaxInt + 1;

            if F.IsMissingValue[CurRec] then
              Val := StataContent(F)^.IntValueLabelMap.KeyData[Val];

            WriteWord(Val);
          end;

        StataByteConstXML:
          begin
            Val := F.AsInteger[CurRec];

            if F.IsMissing[CurRec] then
              Val := StataMaxByte + 1;

            if F.IsMissingValue[CurRec] then
              Val := StataContent(F)^.IntValueLabelMap.KeyData[Val];

            WriteByte(Val);
          end;
      else
        WriteAsString(F.AsString[CurRec], StataContent(F)^.Typ);
      end;
    end;
  end;

  WriteEndTag('data');
end;

procedure TEpiStataExport.WriteStrls;
var
  RecNo: Integer;
  F: TEpiField;
  S: String;
  L: Integer;
begin
  WriteStartTag('strls', 10);

  for RecNo := 0 to FDataFile.Size -1 do
  begin
    for F in FDataFile.Fields do
    begin
      if (StataContent(F)^.Typ <> StataStrLsConstXML) then Continue;
      if F.IsMissing[RecNo] then continue;

      // GSO:
      WriteAsString('GSO');

      // v:
      WriteDWord(FDataFile.Fields.IndexOf(F) + 1);

      // o:
      case FStataVersion of
        dta13:
          WriteDWord(RecNo + 1);
        dta14:
          WriteQWord(RecNo + 1);
      end;

      // t:  (Always as ASCII)
      WriteByte(130);

      S := F.AsString[RecNo];
      L := Length(S) + 1;  // +1 because the string MUST end in \0

      // len:
      WriteDWord(L);

      // content:
      WriteAsString(S, L);
    end;
  end;

  WriteEndTag('strls');
end;

procedure TEpiStataExport.WriteValueLabels;
var
  VLSet: TEpiValueLabelSet;
begin
  WriteStartTag('value_labels', 11);

  for VLSet in FDataFile.ValueLabels do
    if VLSet.LabelType = ftInteger then
      WriteValueLabel(VLSet);

  WriteEndTag('value_labels');
end;

procedure TEpiStataExport.WriteValueLabel(ValueLabelSet: TEpiValueLabelSet);
var
  Len: Integer;
  VL:     TEpiIntValueLabel;
  i: Integer;

  // Stata ValueLabelTable
{       value_label_table      len   format     comment
  ----------------------------------------------------------
  n                        4   int        number of entries
  txtlen                   4   int        length of txt[]
  off[]                  4*n   int array  txt[] offset table
  val[]                  4*n   int array  sorted value table
  txt[]               txtlen   char       text table
  ---------------------------------------------------------- }

  N:      DWord;
  TxtLen: DWord;
  Off:    Array of DWord;
  Val:    Array of LongInt;
  Txt:    Array of String;

  // For keeping track of the offset index:
  OffIdx: DWord;
begin
  WriteStartTag('lbl');

  N := ValueLabelSet.Count;
  SetLength(Off, N);
  SetLength(Val, N);
  SetLength(Txt, N);
  OffIdx := 0;

  for i := 0 to ValueLabelSet.Count - 1 do
  begin
    VL := TEpiIntValueLabel(ValueLabelSet[i]);

    Off[i] := OffIdx;
    Val[i] := VL.Value;
    Txt[i] := VL.TheLabel.Text;
    Inc(OffIdx, Length(Txt[i]) + 1);
  end;

  case FStataVersion of
    //  32 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := (32 * 1) + 1; // 33
    dta14: Len := (32 * 4) + 1; // 129
  end;

  // Length of "value_label_table"
  WriteDWord(4 +            // n
             4 +            // txtlen
             (4 * N) +      // off[]
             (4 * N) +      // val[]
             OffIdx);       // txt[]

  // Labelname and padding...
  WriteAsString(ValueLabelSet.Name, Len + 3);

  // n
  WriteDWord(N);

  // txtlen
  WriteDWord(OffIdx);

  // off[]
  for i := 0 to N - 1 do
    WriteDWord(Off[i]);

  // val[]
  for i := 0 to N - 1 do
    WriteDWord(Val[i]);

  for i := 0 to N - 1 do
    WriteAsString(Txt[i], Length(Txt[i]) + 1);

  WriteEndTag('lbl');
end;

function TEpiStataExport.ExportStata(StataSettings: TEpiStataExportSetting
  ): boolean;
var
  i: Integer;
begin
  FStataSettings := StataSettings;
  FStataVersion  := FStataSettings.Version;

  for i := 0 to StataSettings.DatafileSettings.Count - 1 do
  begin
    FDataFileSetting := TEpiExportDatafileSettings(StataSettings.DatafileSettings[i]);
    FDataFile        := StataSettings.Doc.DataFiles.GetDataFileByName(FDataFileSetting.DatafileName);
    FStream          := FDataFileSetting.ExportStream;

    SetupFields;

    WriteStartTag('stata_dta');

    WriteHeader;
    WriteMap;
    WriteVariableTypes;
    WriteVariableNames;
    WriteSortList;
    WriteFormat;
    WriteValueLabelNames;
    WriteVariabelLabels;
    WriteCharacteristics;
    WriteData;
    WriteStrls;
    WriteValueLabels;

    FMapOffsets[12] := FStream.Position;
    WriteEndTag('stata_dta');
    FMapOffsets[13] := FStream.Position;

    FStream.Position := FMapOffsets[1];
    WriteMap;
  end;
  Result := true;
end;

end.

