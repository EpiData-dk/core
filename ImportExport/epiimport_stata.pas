unit epiimport_stata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles, epieximtypes;

type

  { TEpiStataImport }

  TEpiStataImport = class
  private
    type
      DateType = (tnone, tc, td, tw, tm, tq, th, ty);

  private
    { Internal variables }
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
  private
    { Stream Read functions }

    // Reads from the stream a start-/end tag with the given name. If not found reports an error.
    function  ReadStartTag(Const Name: String): boolean;
    function  ReadEndTag(Const Name: String): boolean;

    // Read text content in tag
    function  ReadAsString(Length: Integer): string;
    function  ReadByte:  Byte;
    function  ReadWord:  Word;
    function  ReadDWord: DWord;
    // The very-very unusual 6-byte integer, used by the StrLs in Stata 14
    function  Read6Word: QWord;
    function  ReadQWord: QWord;

    // Mostly for data reading (signed types)
    function  ReadSByte: ShortInt;  // Signed Byte
    function  ReadSWord: SmallInt;  // Signed Word
    function  ReadSDWord: LongInt;  // Signed DWord
    function  ReadSQWord: Int64;    // Signed QWord

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
    procedure ReadStrls;
    procedure ReadValueLabels;
  public
    function    ImportStata(
                  Const DataStream: TStream;
                  Const Doc: TEpiDocument;
                  var DataFile: TEpiDataFile;
                  ImportData: boolean = true
                ): Boolean; overload;
  private
    FOnProgress: TEpiProgressEvent;
  public
    property    OnProgress: TEpiProgressEvent read FOnProgress write FOnProgress;
  end;


implementation

uses
  epistringutils, epidatafilestypes, LazUTF8, dateutils;

const
  STATA_STRLS_FIELD = 'STATA_STRLS_FIELD';

{ TEpiStataImport }

procedure TEpiStataImport.DoError(const Msg: String);
begin
  Raise Exception.Create(Msg);
end;

function TEpiStataImport.DoProgress(ProgressType: TEpiProgressType;
  const Current, Max: Cardinal): boolean;
begin
  Result := false;

  if Assigned(OnProgress) then
    OnProgress(FDataFile, ProgressType, Current, Max, Result);
end;

function TEpiStataImport.ReadStartTag(const Name: String): boolean;
var
  TagName: String;
  S: String;
  Len: Integer;
begin
  TagName := '<' + Name + '>';
  Len :=  Length(TagName);
  S := ReadAsString(Len);

  Result := (S = TagName);
  if not Result then
    DoError('Tag not found: ' + TagName);
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

  Result := String(Buffer);
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
  Buffer: Array[6] of byte;
begin
  FStream.ReadBuffer(Buffer[0], 6);
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

procedure TEpiStataImport.CreateFields;
var
  i: Integer;
  FormatStr: String;
  j: Integer;
  TmpField: TEpiField;
  FieldType: TEpiFieldType;
  StrLsField: TEpiField;
begin
  SetLength(FDateTypeList, FFieldCount);
  FRecordDataLength := 0;

  for i := 0 to FFieldCount - 1 do
  begin
    StrLsField := nil;

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
          // Create an Int field to hold indices during data-read.
          StrLsField := TEpiField.CreateField(nil, ftInteger);
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
        end;

      StataFloatConstXML,
      StataDoubleConstXML:
        begin
          FieldType := ftFloat;
          if FVariableTypes[i] = StataFloatConstXML then
            Inc(FRecordDataLength, 4)
          else
            Inc(FRecordDataLength, 8);
        end;

      else
        begin
          DoError('Unknown variable type found in Stata-file');
        end;
    end;

    FormatStr := UTF8UpperString(FVariableFormats[i]);
    if not (FormatStr[1] = '%') then
    BEGIN
      DoError(Format('Unknown format specified for variable no: %d', [i+1]));
      Exit;
    END;

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
    if Assigned(StrLsField) then
      TmpField.AddCustomData(STATA_STRLS_FIELD, StrLsField);

    TmpField.BeginUpdate;
    with TmpField do
    begin
      Top      := -1;

      // Length and Decimal is found post-data read.
      Length   := 0;
      Decimals := 0;
    end;

    // Dates:
    if TmpField.FieldType in (DateFieldTypes + TimeFieldTypes) then
    begin
      if TmpField.FieldType in TimeFieldTypes then
        TmpField.Length := 8
      else
        TmpField.Length := 10;
      TmpField.Decimals := 0;
    end;

    // Variable names + variable labels
    TmpField.Name          := FVariableNames[i];
    TmpField.Question.Text := FVariableLabels[i];
    TmpField.EndUpdate;
  end;
end;

procedure TEpiStataImport.TrimFields;
begin
  // TODO: Trim fields (set correct length) according to data content
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
    CPos := FStream.Position;

    if not ReadStartTag('ch')
    then
      begin
        FStream.Position := CPos;
        Break;
      end;
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
  ReadAsString(Len);

  ReadEndTag('ch');
end;

procedure TEpiStataImport.ReadData(ImportData: Boolean);
var
  CurRec: Integer;
  CurField: Integer;
  I, J: LongInt;
  TmpField: TEpiField;
  StrLsField: TEpiField;
  StrLsVal: QWord;
begin
  ReadStartTag('data');

  if not ImportData then
  begin
    FStream.Seek(FObsCount * FRecordDataLength, soCurrent);
  end else try
    // ********************************
    //          STATA DATA
    // ********************************
    DoProgress(eptInit, 0, NObs);

    FOR CurRec := 0 TO FObsCount -1 DO
    BEGIN
      DoProgress(eptRecords, CurRec, NObs);

      FOR CurField := 0 TO FFieldCount - 1 DO
      BEGIN
        TmpField := FDataFile.Field[Curfield];

        Case FVariableTypes[CurField] of
          StataStrLsConstXML:
            begin
              StrLsField := TEpiField(TmpField.RemoveCustomData(STATA_STRLS_FIELD));

              // we really only need the O number from the (V, O) tuple.
              case FStataVersion of
                dta13:
                  begin
                    ReadDWord;
                    StrLsField.AsInteger[CurRec] := ReadDWord;
                  end;
                dta14:
                  begin
                    ReadWord;
                    StrLsField.AsInteger[CurRec] := ReadDWord;
                  end;
              end;

            end;


          StataByteConstXML,
          StataIntConstXML,
          StataLongConstXML:
            begin
              Case FVariableTypes[CurField] of
                StataByteConstXML:
                  begin
                    I := ReadSByte;
                    J := $65;
                  end;
                StataIntConst:
                  begin
                    I := ReadSWord;
                    J := $7FE5;
                  end;
                StataLongConst:
                  begin
                    I := ReadSDWord;
                    J := $7FFFFFE5;
                  end;
              end;

              // This is a missing value type.
              if (I >= J) then
              begin

{                // This corresponds to Stata's ".a", ".b", and ".c"
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
                  TmpField.IsMissing[CurRec] := true;}

                // Read next field/value;
                Continue;
              end;


              {Date is converted from Stata's 1/1-1960 base to Lazarus's 30/12-1899 base}
              case DateTypeList[CurField] of
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
(*            Begin
              if FVariableTypes[CurField] = StataFloatConst then
                TmpFlt := ReadSingleMissing(MisVal)
              else
                TmpFlt := ReadDoubleMissing(MisVal);

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
                {Date is converted from Stata's 1/1-1960 base to Lazarus's 30/12-1899 base}
                case DateTypeList[CurField] of
                  tnone: TmpField.AsFloat[CurRec]    := TmpFlt;                                  // Do nothing - conversion is not needed.
                  tc:    TmpField.AsDateTime[CurRec] := IncMilliSecond(StataBaseDateTime, trunc(TmpFlt));       // I - measured in ms. since 1960.
                  td:    TmpField.AsDateTime[CurRec] := IncDay(StataBaseDateTime,   TruncToInt(TmpFlt));
                  tw:    TmpField.AsDateTime[CurRec] := IncWeek(StataBaseDateTime,  TruncToInt(TmpFlt));
                  tm:    TmpField.AsDateTime[CurRec] := IncMonth(StataBaseDateTime, TruncToInt(TmpFlt));
                  tq:    TmpField.AsDateTime[CurRec] := IncMonth(StataBaseDateTime, TruncToInt(TmpFlt) * 3);
                  th:    TmpField.AsDateTime[CurRec] := IncMonth(StataBaseDateTime, TruncToInt(TmpFlt) * 6);
                  ty:    TmpField.AsDateTime[CurRec] := IncYear(StataBaseDateTime,  TruncToInt(TmpFlt));
                end;
              end;
            end;     *)
        else
          // This is a string field.
          S := ReadAsString(FVariableTypes[i]);

          if (FStataVersion <= dta13) then
            S := EpiUnknownStrToUTF8(S);

          TmpField.AsString[CurRec] := S;
        end;
      END;  //for CurField
    END;  //for CurRec

    DoProgress(eptFinish, CurRec, NObs);
  EXCEPT
    RaiseError(Exception, 'Error reading data from Stata-file');
    Exit;
  END;  //try..except

  ReadEndTag('data');
end;

procedure TEpiStataImport.ReadStrls;
begin
  ReadStartTag('strls');
  ReadEndTag('strls');
end;

procedure TEpiStataImport.ReadValueLabels;
begin
  ReadStartTag('valuelabels');
  ReadEndTag('valuelabels');
end;

function TEpiStataImport.ImportStata(const DataStream: TStream;
  const Doc: TEpiDocument; var DataFile: TEpiDataFile; ImportData: boolean
  ): Boolean;
begin
  FStream := DataStream;
  FStream.Position := 0;

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
    ReadStrls;
    ReadValueLabels;

    ReadEndTag('stata_dta');
  finally
  end;
end;

end.

