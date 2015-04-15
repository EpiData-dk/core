unit epiimport_stata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles, epivaluelabels, epieximtypes,
  epicustombase, epidatafilestypes;

type

  { TEpiStataImport }

  TEpiStataImport = class
  private
    type
      DateType = (tnone, tc, td, tw, tm, tq, th, ty);

  private
    { Internal variables }
    FDocument: TEpiDocument;
    FDataFile: TEpiDataFile;
    FStream:   TStream;
    FStataVersion:    TEpiStataVersion;
    FByteOrder:       TEpiStataEndian;
    FFieldCount:      Word;
    FObsCount:        QWord;
    FRecordDataLength: Integer;
    FDateTypeList:     Array of DateType;

    FVariableTypes:   Array of Word;
    FVariableNames:   TStrings;
    FVariableFormats: TStrings;
    FVariableLabels:  TStrings;

    FValuelabelNames: TStrings;

  private
    { Events }
    procedure DoError(Const Msg: String);
    function  DoProgress(ProgressType: TEpiProgressType;
      Const Current, Max: Cardinal): boolean;
    procedure DoControlItemPosition(Const Item: TEpiCustomControlItem;
      var Top, Left: Integer);
  private
    { Stream Read functions }
    // Reads from the stream a start-/end tag with the given name. If not found reports an error.
    function  ReadStartTag(Const Name: String;
      Const FailOnNotFound: boolean = true): boolean;
    function  ReadEndTag(Const Name: String): boolean;

    // Read text content in tag
    function  ReadAsString(Length: Integer): string;
    function  ReadByte:  Byte;
    function  ReadWord:  Word;
    function  ReadDWord: DWord;
    function  Read6Word: QWord; // The very-very unusual 6-byte integer, used by the StrLs in Stata 14
    function  ReadQWord: QWord;

    // Mostly for data reading (signed types)
    function  ReadSByte: ShortInt;  // Signed Byte
    function  ReadSWord: SmallInt;  // Signed Word
    function  ReadSDWord: LongInt;  // Signed DWord
    function  ReadSQWord: Int64;    // Signed QWord
    function  ReadSingle: Single;   // Signed single precision float
    function  ReadDouble: Double;   // Signed double precision float

  private
    function  IsMissing(Const Val: Integer; Const FieldIndex: DWord; OutVal: EpiInteger): Boolean; overload;
    function  IsMissing(Const Val: Single; out OutVal: EpiFloat): Boolean; overload;
    function  IsMissing(Const Val: Double; out OutVal: EpiFloat): Boolean; overload;
  private
    { EpiData methods }
    procedure CreateFields;
    procedure TrimFields;

  private
    { .dta section loaders }
    procedure ReadHeader;
    procedure ReadMap;
    procedure ReadVariableTypes;
    procedure ReadVariableNames;
    procedure ReadSortList;
    procedure ReadFormat;
    procedure ReadValueLabelNames;
    procedure ReadVariabelLabels;
    procedure ReadCharacteristics;
    procedure ReadCharacteristic;
    procedure ReadData(ImportData: Boolean);
    procedure ReadStrls(ImportData: Boolean);
    procedure ReadValueLabels;
    procedure ReadValueLabel;
  public
    function    ImportStata(
                  Const DataStream: TStream;
                  Const Doc: TEpiDocument;
                  var DataFile: TEpiDataFile;
                  ImportData: boolean = true
                ): Boolean; overload;
  private
    FOnControlItemPosition: TEpiControlItemPosition;
    FOnProgress: TEpiProgressEvent;
  public
    property    OnProgress: TEpiProgressEvent read FOnProgress write FOnProgress;
    property    OnControlItemPosition: TEpiControlItemPosition read FOnControlItemPosition write FOnControlItemPosition;
  end;


implementation

uses
  epistringutils, LazUTF8, dateutils, math;

const
  STATA_STRLS_VVAL = 'STATA_STRLS_VVAL';
  STATA_STRLS_OVAL = 'STATA_STRLS_OVAL';
  STATA_MISSING_FIELD = 'STATA_MISSING_FIELD';

{ TEpiStataImport }

procedure TEpiStataImport.DoError(const Msg: String);
begin
//  Raise Exception.Create(Msg);
end;

function TEpiStataImport.DoProgress(ProgressType: TEpiProgressType;
  const Current, Max: Cardinal): boolean;
begin
  Result := false;

  if Assigned(OnProgress) then
    OnProgress(FDataFile, ProgressType, Current, Max, Result);
end;

procedure TEpiStataImport.DoControlItemPosition(
  const Item: TEpiCustomControlItem; var Top, Left: Integer);
begin
  if Assigned(OnControlItemPosition) then
    OnControlItemPosition(Self, Item, Top, Left);
end;

function TEpiStataImport.ReadStartTag(const Name: String;
  const FailOnNotFound: boolean): boolean;
var
  TagName: String;
  S: String;
  Len: Integer;
  CPos: Int64;
begin
  TagName := '<' + Name + '>';
  Len :=  Length(TagName);

  CPos := FStream.Position;
  S := ReadAsString(Len);

  Result := (S = TagName);
  if (not Result) then
  begin
    if (FailOnNotFound) then
      DoError('Tag not found: ' + TagName)
    else
      FStream.Position := CPos;
  end;
end;

function TEpiStataImport.ReadEndTag(const Name: String): boolean;
var
  TagName: String;
  S: String;
  Len: Integer;
begin
  TagName := '</' + Name + '>';
  Len :=  Length(TagName);
  S := ReadAsString(Len);

  Result := (S = TagName);
  if not Result then
    DoError('Tag not found: ' + TagName);
end;

function TEpiStataImport.ReadAsString(Length: Integer): string;
var
  Buffer: Array of char;
begin
  if (Length = 0) then
    Exit('');

  SetLength(Buffer, Length + 1);
  FStream.Read(Buffer[0], Length);
  Buffer[Length] := #0;

  Result := StrPas(@Buffer[0]);
end;

function TEpiStataImport.ReadByte: Byte;
begin
  result := FStream.ReadByte;
end;

function TEpiStataImport.ReadWord: Word;
begin
  Result := FStream.ReadWord;

  case FByteOrder of
    eseLittleEndian: Result := LEtoN(Result);
    eseBigEndian:    Result := BEtoN(Result);
  end;
end;

function TEpiStataImport.ReadDWord: DWord;
begin
  Result := FStream.ReadDWord;

  case FByteOrder of
    eseLittleEndian: Result := LEtoN(Result);
    eseBigEndian:    Result := BEtoN(Result);
  end;
end;

function TEpiStataImport.Read6Word: QWord;
var
  Buffer: Array[0..7] of byte;
begin
  Buffer[0] := 0;
  Buffer[1] := 0;
  FStream.ReadBuffer(Buffer[2], 6);
  Result := QWord(Buffer);
end;

function TEpiStataImport.ReadQWord: QWord;
begin
  Result := FStream.ReadQWord;

  case FByteOrder of
    eseLittleEndian: Result := LEtoN(Result);
    eseBigEndian:    Result := BEtoN(Result);
  end;
end;

function TEpiStataImport.ReadSByte: ShortInt;
begin
  Result := ReadByte;
end;

function TEpiStataImport.ReadSWord: SmallInt;
begin
  Result := FStream.ReadWord;

  case FByteOrder of
    eseLittleEndian: Result := LEtoN(Result);
    eseBigEndian:    Result := BEtoN(Result);
  end;
end;

function TEpiStataImport.ReadSDWord: LongInt;
begin
  Result := FStream.ReadDWord;

  case FByteOrder of
    eseLittleEndian: Result := LEtoN(Result);
    eseBigEndian:    Result := BEtoN(Result);
  end;
end;

function TEpiStataImport.ReadSQWord: Int64;
begin
  Result := FStream.ReadQWord;

  case FByteOrder of
    eseLittleEndian: Result := LEtoN(Result);
    eseBigEndian:    Result := BEtoN(Result);
  end;
end;

function TEpiStataImport.ReadSingle: Single;
var
  Val: DWord;
  Buf: Array[0..3] of Byte absolute Val;
begin
  Val := ReadDWord;
  Result := Single(Buf);
end;

function TEpiStataImport.ReadDouble: Double;
var
  Val: QWord;
  Buf: Array[0..7] of Byte absolute Val;
begin
  Val := ReadQWord;
  Result := Double(Buf);
end;

function TEpiStataImport.IsMissing(const Val: Integer; const FieldIndex: DWord;
  OutVal: EpiInteger): Boolean;
begin
  Result := false;

  Case FVariableTypes[FieldIndex] of
    StataByteConstXML: OutVal := (Val -       $65);
    StataIntConstXML:  OutVal := (Val -     $7FE5);
    StataLongConstXML: OutVal := (Val - $7FFFFFE5);
  end;

  Result := (OutVal >= 0);

  if not (Result) then
    OutVal := Val;
end;


function TEpiStataImport.IsMissing(const Val: Single; out OutVal: EpiFloat
  ): Boolean;
var
  Buf:    Array[0..3] of Byte absolute Val;
begin
  Result := false;

  // Single missing values look like this (in MSF format)
  // 7F 00 ?? 00
  // Where ?? is a multiple of 8 wrt. actual value.
  //   .a = $08
  //   .b = $10
  //   .c = $18
  //   ....
  //   .z = $d0
  if (Buf[3]=$7F) and ((Buf[0] AND Buf[2]) = 0) then
  begin
    Result := true;
    OutVal := Integer(Buf[1] div $08);
  end else
    OutVal := Val;
end;

function TEpiStataImport.IsMissing(const Val: Double; out OutVal: EpiFloat
  ): Boolean;
var
  Buf: Array[0..7] of Byte absolute Val;
begin
  Result := false;

  // Single missing values look like this (in MSF format)
  // 7F E0 ?? 00 00 00 00 00
  // Where ?? is the actual value.
  //   .a = $01
  //   .b = $02
  //   .c = $03
  //   ....
  //   .z = $1a
  if (Buf[7]=$7F) and (Buf[6]=$E0) and
     ((Buf[0] or Buf[1] or Buf[2] or Buf[3] or Buf[4]) = 0)
  then
  begin
    Result := true;
    OutVal := Integer(Buf[5]);
  end else
    OutVal := Val;
end;

procedure TEpiStataImport.CreateFields;
var
  i: Integer;
  FormatStr: String;
  j: Integer;
  TmpField: TEpiField;
  FieldType: TEpiFieldType;
  StrLsVVals: TEpiField;
  StrLsOVals: TEpiField;
  ATop: Integer;
  ALeft: Integer;
  MissingField: TEpiField;
begin
  SetLength(FDateTypeList, FFieldCount);
  FRecordDataLength := 0;

  FDataFile.Size := FObsCount;

  for i := 0 to FFieldCount - 1 do
  begin
    StrLsVVals := nil;
    StrLsOVals := nil;

    // Find basic type from Stata types
    Case FVariableTypes[i] of
      1..2045:
        begin
          FieldType := ftString;
          Inc(FRecordDataLength, FVariableTypes[i]);
        end;

      StataStrLsConstXML:
        begin
          FieldType := ftString;

          // Create an Int field to hold field indices during data-read.
          StrLsVVals := TEpiField.CreateField(nil, ftInteger);
          StrLsVVals.Size := FObsCount;

          // Create an Int field to hold value indices during data-read.
          StrLsOVals := TEpiField.CreateField(nil, ftInteger);
          StrLsOVals.Size := FObsCount;
          Inc(FRecordDataLength, 8);
        end;

      StataByteConstXML,
      StataIntConstXML,
      StataLongConstXML:
        begin
          FieldType := ftInteger;
          case FVariableTypes[i] of
            StataByteConstXML: Inc(FRecordDataLength, 1);
            StataIntConstXML:  Inc(FRecordDataLength, 2);
            StataLongConstXML: Inc(FRecordDataLength, 4);
          end;

          MissingField := TEpiField.CreateField(nil, ftInteger);
          MissingField.Size := FObsCount;
        end;

      StataFloatConstXML,
      StataDoubleConstXML:
        begin
          FieldType := ftFloat;
          if FVariableTypes[i] = StataFloatConstXML then
            Inc(FRecordDataLength, 4)
          else
            Inc(FRecordDataLength, 8);

          MissingField := TEpiField.CreateField(nil, ftInteger);
          MissingField.Size := FObsCount;
        end;

      else
        begin
          DoError('Unknown variable type found in Stata-file');
        end;
    end;

    FormatStr := UTF8UpperString(FVariableFormats[i]);
    if not (FormatStr[1] = '%') then
    begin
      DoError(Format('Unknown format specified for variable no: %d', [i+1]));
      Exit;
    end;

    // "%-..." formats are the left adjusted versions. Not needed in EpiData, just skip.
    j := 0;
    if FormatStr[2] = '-' then
      Inc(j);

    // Find specific date/time fields based on Formatting options
    FDateTypeList[i] := tnone;
    if FormatStr[Length(FormatStr)] <> 'S' then
    begin
      if FormatStr[2+j] = 'T' then
        Inc(j);

      Case Char(FormatStr[2+j]) of
        // Date (and time formats)
        'C': // Time - count of millisecs since 1/1-1960 00:00:00.000
          begin
            FieldType := ftTime;
            FDateTypeList[i] := tc;
          end;
        'D', // Date - count of days:       since 1/1-1960
        'W', //      - count of weeks
        'M', //      - count of months
        'Q', //      - count of quartes
        'H', //      - count of half years
        'Y': // Year - count of years:      since 0 AD.
          begin
            FieldType := ftDMYDate;
            Case Char(FormatStr[2+j]) of
              'D': FDateTypeList[i] := td;
              'W': FDateTypeList[i] := tw;
              'M': FDateTypeList[i] := tm;
              'Q': FDateTypeList[i] := tq;
              'H': FDateTypeList[i] := th;
              'Y': FDateTypeList[i] := ty;
            end;
          end;
        // Number
        '0'..'9': ;
      else
        DoError(Format('Unknown format specified for variable no: %d', [i+1]));
        Exit;
      end;
    end;

    // Now created fields
    TmpField := FDataFile.NewField(FieldType);
    if Assigned(StrLsVVals) then
      TmpField.AddCustomData(STATA_STRLS_VVAL, StrLsVVals);
    if Assigned(StrLsOVals) then
      TmpField.AddCustomData(STATA_STRLS_OVAL, StrLsOVals);
    if Assigned(MissingField) then
      TmpField.AddCustomData(STATA_MISSING_FIELD, MissingField);

    with TmpField do
    begin
      // Length and Decimal is found post-data read.
      Length   := 0;
      Decimals := 0;

      // Dates:
      if FieldType in (DateFieldTypes + TimeFieldTypes) then
      begin
        if FieldType in TimeFieldTypes then
          Length := 8
        else
          Length := 10;
        Decimals := 0;
      end;

      // Variable names + variable labels
      Name          := FVariableNames[i];
      Question.Text := FVariableLabels[i];

      ATop := 0;
      ALeft := 0;

      DoControlItemPosition(TmpField, ATop, ALeft);
      Top  := ATop;
      Left := ALeft;
    end;
  end;
end;

procedure TEpiStataImport.TrimFields;
var
  F: TEpiField;
  Len: Integer;
  Dec: Integer;
  i: Integer;
  ULen: PtrInt;
  S: String;
  P: SizeInt;
  Fmt: String;
  j: Integer;
  MissingField: TEpiField;
  MaxMissing: Integer;
  VLSet: TEpiValueLabelSet;
  VL: TEpiCustomValueLabel;
  MissingValue: EpiInteger;
begin
  // First find field lengths / Decimals
  for j := 0 to FFieldCount - 1 do
  begin
    F := FDataFile[j];

    Len := 0;
    Dec := 0;

    if F.FieldType in FloatFieldTypes then
    begin
      Fmt := FVariableFormats[j];

      for i := 0 to F.Size - 1 do
      begin
        if F.IsMissing[i] then
          Continue;

        S := Trim(Format(Fmt, [F.AsFloat[i]]));
        ULen := UTF8Length(S);
        P := Pos(DefaultFormatSettings.DecimalSeparator, S);

        if P > 0 then
        begin
          Dec := Max(Dec, ULen - P);
          Len := Max(Len, P - 1);
        end else
          Len := Max(Len, ULen);
      end;

      F.Length   := Len + 1 + Dec;
      F.Decimals := Dec;
    end else begin
      for i := 0 to F.Size - 1 do
        Len := Max(Len, UTF8Length(F.AsString[i]));

      F.Length   := Len;
      F.Decimals := 0;
    end;
  end;


  // Now adapt missingvalues if present.
  for j := 0 to FFieldCount - 1 do
  begin
    F := FDataFile[j];

    if (F.FieldType in StringFieldTypes) then
      Continue;

    MissingField := TEpiField(F.FindCustomData(STATA_MISSING_FIELD));

    VLSet := F.ValueLabelSet;
    if (not Assigned(VLSet)) then
    begin
      VLSet := FDocument.ValueLabelSets.NewValueLabelSet(F.FieldType);
      VLSet.Name := F.Name + '_missingvalues';
      F.ValueLabelSet := VLSet;
    end;

    if (F.FieldType in FloatFieldTypes) then
      Len := F.Length - 1 - F.Decimals
    else
      Len := F.Length;

    for i := 0 to FDataFile.Size - 1 do
    begin
      if MissingField.IsMissing[i] then continue;

      MissingValue := (10 ** Len) - MissingField.AsInteger[i];
      if not VLSet.ValueLabelExists[MissingValue] then
      begin
        VL := VLSet.NewValueLabel;
        TEpiIntValueLabel(VL).Value := MissingValue;
        VL.TheLabel.Text := '.' + Char(MissingValue + 96);
        VL.IsMissingValue := true;
      end;
    end;
  end;
  {
                // This is a missing value type.
                if MisVal >= 0 then
                begin
                  // This corresponds to Stata's ".a", ".b", and ".c"
                  if (MisVal > 0) and
                     (not (TmpField.FieldType in DateFieldTypes+TimeFieldTypes)) then
                  begin
                    VLSet := TmpField.ValueLabelSet;
                    if not Assigned(VLSet) then
                    begin
                      VLSet := ValueLabels.NewValueLabelSet(ftFloat);
                      VLSet.Name := TmpField.Name + '_MissingLabel';
                      TmpField.ValueLabelSet := VLSet;
                    end;

                    // TODO: What should .a -> .z float missing value be?
                    TmpFlt := (10 ** 10) - MisVal;
                    if not VLSet.ValueLabelExists[TmpInt] then
                    begin
                      VL := VLSet.NewValueLabel;
                      TEpiFloatValueLabel(VL).Value := TmpFlt;
                      VL.TheLabel.Text := '.' + Char(MisVal + 96);
                      VL.IsMissingValue := true;
                    end;
                    TmpField.AsFloat[CurRec] := TmpFlt;
                  end else
                    TmpField.IsMissing[CurRec] := true;
                end else begin



                Case FVariableTypes[CurField] of
                  StataByteConstXML:
                    begin
                      I := ReadSByte;
                      J := $65;
                    end;
                  StataIntConstXML:
                    begin
                      I := ReadSWord;
                      J := $7FE5;
                    end;
                  StataLongConstXML:
                    begin
                      I := ReadSDWord;
                      J := $7FFFFFE5;
                    end;
                end;

                // This is a missing value type.
                if (I >= J) then
                begin

                  // This corresponds to Stata's ".a", ".b", and ".c"
                  if ((I - J) > 0) and
                     (not (TmpField.FieldType in (DateFieldTypes + TimeFieldTypes)))  then
                  begin
                    VLSet := TmpField.ValueLabelSet;
                    if not Assigned(VLSet) then
                    begin
                      VLSet := ValueLabels.NewValueLabelSet(ftInteger);
                      VLSet.Name := TmpField.Name + '_MissingLabel';
                      TmpField.ValueLabelSet := VLSet;
                    end;

                    TmpInt := (10 ** TmpField.Length) - MisVal;
                    if not VLSet.ValueLabelExists[TmpInt] then
                    begin
                      VL := TEpiIntValueLabel(VLSet.NewValueLabel);
                      TEpiIntValueLabel(VL).Value := TmpInt;
                      VL.TheLabel.Text := '.' + Char(MisVal + 96);
                      VL.IsMissingValue := true;
                    end;
                    TmpField.AsInteger[CurRec] := TmpInt;
                  end else
                    TmpField.IsMissing[CurRec] := true;

                  // Read next field/value;
                  Continue;
                end;
  }
end;

procedure TEpiStataImport.ReadHeader;
var
  S: String;
  Len: Word;
begin
  // HEADER
  ReadStartTag('header');

  // <release>
  ReadStartTag('release');
  FStataVersion := TEpiStataVersion(StrToInt(ReadAsString(3)));
  if not (FStataVersion in [dta13, dta14]) then
    DoError('Unknown Stata Version');
  ReadEndTag('release');


  // <byteorder>
  ReadStartTag('byteorder');
  S := ReadAsString(3);
  if S = 'MSF' then
    FByteOrder := eseBigEndian
  else if S = 'LSF' then
    FByteOrder := eseLittleEndian
  else
    DoError('Unknown Byteorder');
  ReadEndTag('byteorder');

  // <K>  = Number of fields
  ReadStartTag('K');
  FFieldCount := ReadWord;
  ReadEndTag('K');

  // <N>  = Number of observartions/records
  ReadStartTag('N');
  case FStataVersion of
    dta13: FObsCount := ReadDWord;
    dta14: FObsCount := ReadQWord;
  end;
  ReadEndTag('N');


  // <label>  = EpiData DataFile Caption
  ReadStartTag('label');
  Case FStataVersion of
    dta13: Len := ReadByte;
    dta14: Len := ReadWord;
  end;

  S := ReadAsString(Len);
  if (FStataVersion = dta13) then
    S := EpiUnknownStrToUTF8(S);

  FDataFile.Caption.Text := S;
  ReadEndTag('label');

  // <timestamp> - we ignore the timestamp.
  ReadStartTag('timestamp');
  Len := ReadByte;
  ReadAsString(Len);
  ReadEndTag('timestamp');

  ReadEndTag('header');
end;

procedure TEpiStataImport.ReadMap;
begin
  // <map> - a list of indices referencing individual sections for the stream.
  // Currently we are not using these.

  ReadStartTag('map');

  //1.       <stata_data>, definitionally 0
  ReadQWord;
  //2.       <map>
  ReadQWord;
  //3.       <variable_types>
  ReadQWord;
  // 4.       <varnames>
  ReadQWord;
  // 5.       <sortlist>
  ReadQWord;
  // 6.       <formats>
  ReadQWord;
  // 7.       <value_label_names>
  ReadQWord;
  // 8.       <variable_labels>
  ReadQWord;
  // 9.       <characteristics>
  ReadQWord;
  // 10.       <data>
  ReadQWord;
  // 11.       <strls>
  ReadQWord;
  // 12.       <value_labels>
  ReadQWord;
  // 13.       </stata_data>
  ReadQWord;
  // 14.       end-of-file
  ReadQWord;

  ReadEndTag('map');
end;

procedure TEpiStataImport.ReadVariableTypes;
var
  i: Integer;
begin
  SetLength(FVariableTypes, FFieldCount);

  // <variable_types>
  ReadStartTag('variable_types');

  for i := 0 to FFieldCount - 1 do
    FVariableTypes[i] := ReadWord;

  ReadEndTag('variable_types');
end;

procedure TEpiStataImport.ReadVariableNames;
var
  Len: Integer;
  i: Integer;
begin
  FVariableNames := TStringList.Create;

  // <varnames>
  ReadStartTag('varnames');

  case FStataVersion of
    //  32 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := (32 * 1) + 1;
    dta14: Len := (32 * 4) + 1;
  end;

  for i := 0 to FFieldCount - 1 do
    FVariableNames.Add(ReadAsString(Len));

  if (FStataVersion = dta13) then
    EpiUnknownStringsToUTF8(FVariableNames);

  ReadEndTag('varnames');
end;

procedure TEpiStataImport.ReadSortList;
var
  i: Integer;
begin
  // <sortlist>  - Not used in epidata.
  ReadStartTag('sortlist');
  for i := 1 to FFieldCount + 1 do
    ReadWord;
  ReadEndTag('sortlist');
end;

procedure TEpiStataImport.ReadFormat;
var
  Len: Integer;
  i: Integer;
begin
  FVariableFormats := TStringList.Create;

  // <format>
  ReadStartTag('formats');

  case FStataVersion of
    //  32 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := 49;
    dta14: Len := 57;
  end;

  for i := 0 to FFieldCount - 1 do
    FVariableFormats.Add(ReadAsString(Len));

  if (FStataVersion = dta13) then
    EpiUnknownStringsToUTF8(FVariableFormats);

  ReadEndTag('formats');
end;

procedure TEpiStataImport.ReadValueLabelNames;
var
  Len: Integer;
  i: Integer;
begin
  FValuelabelNames := TStringList.Create;

  // <value_label_names>
  ReadStartTag('value_label_names');

  case FStataVersion of
    //  32 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := (32 * 1) + 1; // 33
    dta14: Len := (32 * 4) + 1; // 129
  end;

  for i := 0 to FFieldCount - 1 do
    FValuelabelNames.Add(ReadAsString(Len));

  if (FStataVersion = dta13) then
    EpiUnknownStringsToUTF8(FValuelabelNames);

  ReadEndTag('value_label_names');
end;

procedure TEpiStataImport.ReadVariabelLabels;
var
  Len: Integer;
  i: Integer;
begin
  FVariableLabels := TStringList.Create;

  // <value_label_names>
  ReadStartTag('variable_labels');

  case FStataVersion of
    //  80 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := (80 * 1) + 1; // 81
    dta14: Len := (80 * 4) + 1; // 321
  end;

  for i := 0 to FFieldCount - 1 do
    FVariableLabels.Add(ReadAsString(Len));

  if (FStataVersion = dta13) then
    EpiUnknownStringsToUTF8(FVariableLabels);

  ReadEndTag('variable_labels');
end;

procedure TEpiStataImport.ReadCharacteristics;
var
  CPos: Int64;
begin
  ReadStartTag('characteristics');

  // Read consecutive <ch> tags...
  while true do
  begin
    if not ReadStartTag('ch', false) then
      Break;

    ReadCharacteristic;
  end;

  ReadEndTag('characteristics');
end;

procedure TEpiStataImport.ReadCharacteristic;
var
  Len: DWord;
begin
  // Read "llll", length of Characteristic.
  Len := ReadDWord;
  // For now - just skip that part.
  // TODO: Read Characteristics
  ReadAsString(Len);

  ReadEndTag('ch');
end;

procedure TEpiStataImport.ReadData(ImportData: Boolean);
var
  CurRec: Integer;
  CurField: Integer;
  I: EpiInteger;
  TmpField: TEpiField;
  S: String;
  O: QWord;
  V: DWord;
  TmpFloat: EpiFloat;
  Mis: Boolean;
begin
  ReadStartTag('data');

  if not ImportData then
  begin
    FStream.Seek(FObsCount * FRecordDataLength, soCurrent);
  end else try
    // ********************************
    //          STATA DATA
    // ********************************
    DoProgress(eptInit, 0, FObsCount);

    FOR CurRec := 0 TO FObsCount -1 DO
    BEGIN
      DoProgress(eptRecords, CurRec, FObsCount);

      FOR CurField := 0 TO FFieldCount - 1 DO
      BEGIN
        TmpField := FDataFile.Field[Curfield];

        Case FVariableTypes[CurField] of
          StataStrLsConstXML:
            begin
              case FStataVersion of
                dta13:
                  begin
                    V := ReadDWord - 1;
                    O := ReadDWord - 1;
                  end;
                dta14:
                  begin
                    V := ReadWord - 1;
                    O := Read6Word - 1;
                  end;
              end;
              TEpiField(TmpField.FindCustomData(STATA_STRLS_VVAL)).AsInteger[CurRec] := V;
              TEpiField(TmpField.FindCustomData(STATA_STRLS_OVAL)).AsInteger[CurRec] := O;
            end;


          StataByteConstXML,
          StataIntConstXML,
          StataLongConstXML:
            begin
              Case FVariableTypes[CurField] of
                StataByteConstXML: I := ReadSByte;
                StataIntConstXML:  I := ReadSWord;
                StataLongConstXML: I := ReadSDWord;
              end;

              if IsMissing(I, CurField, I) then
              begin
                TmpField.IsMissing[CurRec]  := True;
                TEpiField(TmpField.FindCustomData(STATA_MISSING_FIELD)).AsInteger[CurRec] := I;
                Continue;
              end;

              {Date is converted from Stata's 1/1-1960 base to Lazarus's 30/12-1899 base}
              case FDateTypeList[CurField] of
                tnone: TmpField.AsInteger[CurRec]  := I;                                     // Do nothing - conversion is not needed.
                tc:    TmpField.AsDateTime[CurRec] := IncMilliSecond(StataBaseDateTime, I);  // I - measured in ms. since 1960.
                td:    TmpField.AsDateTime[CurRec] := IncDay(StataBaseDateTime,   I);
                tw:    TmpField.AsDateTime[CurRec] := IncWeek(StataBaseDateTime,  I);
                tm:    TmpField.AsDateTime[CurRec] := IncMonth(StataBaseDateTime, I);
                tq:    TmpField.AsDateTime[CurRec] := IncMonth(StataBaseDateTime, I * 3);
                th:    TmpField.AsDateTime[CurRec] := IncMonth(StataBaseDateTime, I * 6);
                ty:    TmpField.AsDateTime[CurRec] := IncYear(StataBaseDateTime,  I);
              end;
            end;

          StataFloatConstXML,
          StataDoubleConstXML:
            Begin
              if FVariableTypes[CurField] = StataFloatConstXML then
                Mis := IsMissing(ReadSingle, TmpFloat)
              else
                Mis := IsMissing(ReadDouble, TmpFloat);

              if Mis then
              begin
                TmpField.IsMissing[CurRec] := True;
                TEpiField(TmpField.FindCustomData(STATA_MISSING_FIELD)).AsInteger[CurRec] := trunc(TmpFloat);
                Continue;
              end;

              {Date is converted from Stata's 1/1-1960 base to Lazarus's 30/12-1899 base}
              case FDateTypeList[CurField] of
                tnone: TmpField.AsFloat[CurRec]    := TmpFloat;                                                 // Do nothing - conversion is not needed.
                tc:    TmpField.AsDateTime[CurRec] := IncMilliSecond(StataBaseDateTime, trunc(TmpFloat));       // I - measured in ms. since 1960.
                td:    TmpField.AsDateTime[CurRec] := IncDay(StataBaseDateTime,   trunc(TmpFloat));
                tw:    TmpField.AsDateTime[CurRec] := IncWeek(StataBaseDateTime,  trunc(TmpFloat));
                tm:    TmpField.AsDateTime[CurRec] := IncMonth(StataBaseDateTime, trunc(TmpFloat));
                tq:    TmpField.AsDateTime[CurRec] := IncMonth(StataBaseDateTime, trunc(TmpFloat) * 3);
                th:    TmpField.AsDateTime[CurRec] := IncMonth(StataBaseDateTime, trunc(TmpFloat) * 6);
                ty:    TmpField.AsDateTime[CurRec] := IncYear(StataBaseDateTime,  trunc(TmpFloat));
              end;
            end;
        else
          // This is a string field.
          S := ReadAsString(FVariableTypes[CurField]);

          if (FStataVersion <= dta13) then
            S := EpiUnknownStrToUTF8(S);

          TmpField.AsString[CurRec] := S;
        end;
      END;  //for CurField
    END;  //for CurRec

    DoProgress(eptFinish, CurRec, FObsCount);
  EXCEPT
    DoError('Error reading data from Stata-file');
    Exit;
  END;  //try..except

  ReadEndTag('data');
end;

procedure TEpiStataImport.ReadStrls(ImportData: Boolean);
var
  Field: TEpiField;
  V: Int64;
  O: Int64;
  j: QWord;
  i: Integer;
  GSO: String;
  gsoV: DWord;
  gsoO: QWord;
  gsoType: Byte;
  gsoLen: DWord;
  S: String;
begin
  ReadStartTag('strls');

  for j := 0 to FObsCount - 1 do
  begin
    for i := 0 to FFieldCount - 1 do
    begin
      Field := FDataFile.Field[i];

      if (not Assigned(Field.FindCustomData(STATA_STRLS_VVAL)))
      then
        Continue;

      V := TEpiField(Field.FindCustomData(STATA_STRLS_VVAL)).AsInteger[j];
      O := TEpiField(Field.FindCustomData(STATA_STRLS_OVAL)).AsInteger[j];

      // Empty StrLs content!
      if (V < 0) and (O < 0) then
        Continue;

      // The StrLs is unique and not seen before! It belongs to this field
      // and at this record no.
      if (V = i) and (O = j) then
      begin
        GSO := ReadAsString(3);
        if (GSO <> 'GSO') then
          DoError('Incorrect GSO header in StrLs!');

        gsoV := ReadDWord - 1;
        case FStataVersion of
          dta13: gsoO := ReadDWord - 1;
          dta14: gsoO := ReadQWord - 1;
        end;

        if (V <> gsoV) or
           (O <> gsoO)
        then
          DoError('Incorrect GSO content in StrLs!');

        gsoType := ReadByte;
        if (not (gsoType in [129, 130]))
        then
          DoError('Incorrect GSO type in StrLs!');

        gsoLen  := ReadDWord;
        S := ReadAsString(gsoLen);

        if (FStataVersion = dta13) then
          S := EpiUnknownStrToUTF8(S);

        if ImportData then
          Field.AsString[j] := S;

        Continue;
      end;

      // This StrLs has been seen before, use the reference field and record no.
      // to get the existing content.
      if (O < j) or ((O = j) and (V < i)) then
      begin
        if ImportData then
          Field.AsString[j] := FDataFile.Field[V].AsString[O];

        Continue;
      end;

      DoError('Incorrect GSO: Forward declaration');
    end;
  end;

  ReadEndTag('strls');
end;

procedure TEpiStataImport.ReadValueLabels;
begin
  ReadStartTag('value_labels');

  while true do
  begin
    if (not ReadStartTag('lbl', false)) then
      break;

    ReadValueLabel;
  end;

  ReadEndTag('value_labels');
end;

procedure TEpiStataImport.ReadValueLabel;
var
  Len: DWord;
  LabelLen: DWord;
  LabelName: String;
  N: DWord;
  TxtLen: DWord;
  Off: Array of DWord;
  Val: Array of DWord;
  Txt: Array of Char;
  VLSet: TEpiValueLabelSet;
  VL: TEpiIntValueLabel;
  i: Integer;
  S: String;
begin
  // Len
  Len := ReadDWord;

  case FStataVersion of
    //  80 Characters (* 4 for UTF-8) and a terminal #0
    dta13: LabelLen := (32 * 1) + 1; // 33
    dta14: LabelLen := (32 * 4) + 1; // 129
  end;

  // Labelname...
  LabelName := ReadAsString(LabelLen);
  if FStataVersion = dta13 then
    LabelName := EpiUnknownStrToUTF8(LabelName);

  // Padding...
  ReadAsString(3);

  // Value Label Table
  N := ReadDWord;
  TxtLen := ReadDWord;

  SetLength(Off, N);
  for i := 0 to N - 1 do
    Off[i] := ReadDWord;

  SetLength(Val, N);
  for i := 0 to N - 1 do
    Val[i] := ReadDWord;

  SetLength(Txt, TxtLen);
  FStream.ReadBuffer(Txt[0], TxtLen);

  VLSet := FDocument.ValueLabelSets.NewValueLabelSet(ftInteger);
  VLSet.Name := LabelName;

  for i := 0 to N -1 do
  begin
    VL := TEpiIntValueLabel(VLset.NewValueLabel);
    VL.Value := Val[i];

    S := StrPas(@Txt[Off[i]]);
    if (FStataVersion = dta13) then
      S := EpiUnknownStrToUTF8(S);

    VL.TheLabel.Text := S;
  end;

  ReadEndTag('lbl');
end;

function TEpiStataImport.ImportStata(const DataStream: TStream;
  const Doc: TEpiDocument; var DataFile: TEpiDataFile; ImportData: boolean
  ): Boolean;
begin
  FStream := DataStream;
  FStream.Position := 0;

  FDocument := Doc;

  if not Assigned(DataFile) then
    DataFile := Doc.DataFiles.NewDataFile;

  FDataFile := DataFile;
  FDataFile.Fields.Sorted := false;

  try
    ReadStartTag('stata_dta');

    ReadHeader;
    ReadMap;
    ReadVariableTypes;
    ReadVariableNames;
    ReadSortList;
    ReadFormat;
    ReadValueLabelNames;
    ReadVariabelLabels;

    // Now all essential details regarding Fields are read, build the EpiData TEpiField(s).
    CreateFields;

    ReadCharacteristics;
    ReadData(ImportData);
    ReadStrls(ImportData);
    ReadValueLabels;

    ReadEndTag('stata_dta');

    // Now find the correct lengts
    TrimFields;

    Result := true;
  finally
  end;
end;

end.

