unit epiimport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles, epidatafilestypes, epiadmin,
  epivaluelabels, epieximtypes, epiimport_stata, epicustombase;

type

  // Import exceptions
  EIncorrectPasswordException = Exception;

  TEpiClipBoardReadHook = procedure (ClipBoardLine: TStrings) of object;


  { TEpiImport }

  TEpiImport = class(TObject)
  private
    FImportEncoding: TEpiEncoding;
    FOnClipBoardRead: TEpiClipBoardReadHook;
    FOnRequestPassword: TRequestPasswordEvent;
  private
    { Stata help functions }
    FStataImport: TEpiStataImport;  // Importer for Stata v 117+  (XML)
    procedure   RaiseError(EClass: ExceptClass; Const Msg: string);
    procedure   ReadBuf(Const St: TStream; var Buf: Array of Byte; Count: Integer);
    function    ReadInts(Const St: TStream; Count: Integer): Integer;
    function    ReadSInts(Const St: TStream; Count: Integer): Integer;
    function    ReadSingle(Const St: TStream): Single;
    function    ReadDouble(Const St: TStream): Double;
    function    ReEncodeString(Const Str: string): string;
    function    StringFromBuffer(AChar: PChar; MaxLength: Integer): string;
  private
    { Rec help functions }
    function    ReadInteger(Const St: TStream; Width: Byte): Integer;
    function    ReadString(Const St: TStream; Width: Byte): String; overload;
    function    ReadString(Const St: TStream): string; overload;
  private
    { Txt help functionc }
    function    GuessTxtFile(DataFile: TEpiDataFile; Lines: TStrings;
      out FieldList: TList;
      out SkipFirstLine: boolean;
      out FieldSeparator: Char): boolean;
    procedure   FindFieldType(var Value: String; var PossibleTypes: TEpiFieldTypes);
    function    IsInteger(Const S: String; out Val: EpiInteger): boolean;
    function    IsFloat(Const S: String;   out Val: EpiFloat): boolean;
    function    IsDate(const S: String; Const Ft: TEpiFieldType; out Val: EpiDate): boolean;
    function    IsTime(Const s: string;    out Val: EpiTime): boolean;
    function    IsBoolean(Const S: string; out Val: EpiBool): boolean;
    function    IsUppercase(Const S: string; out Val: EpiString): boolean;
    function    MinFt(Const Ft1, Ft2: TEpiFieldType): TEpiFieldType;
  private
    FImportCasing: TEpiFieldNamingCase;
    FOnControlItemPosition: TEpiControlItemPosition;
    FOnProgress: TEpiProgressEvent;
    procedure SetOnControlItemPosition(AValue: TEpiControlItemPosition);
    function    TruncToInt(e: Extended): integer;
    function    DoProgress(ProgressType: TEpiProgressType;
      Const Current, Max: Cardinal): boolean;
    procedure   DoControlItemPosition(Const Item: TEpiCustomControlItem;
      var Top, Left: Integer);
  public
    constructor Create;
    destructor  Destroy; override;

    function    ImportRec(const DataStream: TStream; var DataFile: TEpiDataFile; ImportData: boolean): boolean; overload;
    function    ImportRec(Const aFileName: string;   var DataFile: TEpiDataFile; ImportData: boolean = true): boolean; overload;

    function    ImportStata(Const DataStream: TStream; Const Doc: TEpiDocument; var DataFile: TEpiDataFile; ImportData: boolean = true): Boolean; overload;
    function    ImportStata(Const aFilename: string;   Const Doc: TEpiDocument; var DataFile: TEpiDataFile; ImportData: boolean = true): Boolean; overload;

    function    ImportTxt(const DataStream: TStream; var DataFile: TEpiDataFile; ImportData: boolean): boolean; overload;
    function    ImportTxt(Const aFileName: string;   var DataFile: TEpiDataFile; ImportData: boolean = true): boolean; overload;

    property    OnClipBoardRead: TEpiClipBoardReadHook read FOnClipBoardRead write FOnClipBoardRead;

    // The RequestPasswordEvent does in this case not require a login name - since old .REC files do no support logins. It is discarded and not used.
    property    OnRequestPassword: TRequestPasswordEvent read FOnRequestPassword write FOnRequestPassword;
    property    OnProgress: TEpiProgressEvent read FOnProgress write FOnProgress;
    // If the OnControlItemPosition event is set, the importer request for a position of the new ControlItem.
    // The Top, Left parameters will either be
    //   a)  0  = "I do not carry positional information, just give me a position"
    //   b)  1+ = "I subsequent values are equal, position should be the same. Eg. if two consecutive calls contain the same
    //             Top value, then these two Items should both have the same resulting top value"
    property    OnControlItemPosition: TEpiControlItemPosition read FOnControlItemPosition write SetOnControlItemPosition;
    property    ImportEncoding: TEpiEncoding read FImportEncoding write FImportEncoding default eeGuess;
    // Import casing only relevant for .rec files, since they are considere case-incensitive.
    property    ImportCasing: TEpiFieldNamingCase read FImportCasing write FImportCasing;
  end;

implementation

uses
  FileUtil, epistringutils, DCPbase64, DCPrijndael, DCPsha1, math, strutils,
  LazUTF8, dateutils, LConvEncoding, LazUTF8Classes, epimiscutils, epiconvertutils,
  epidatafileutils, LazFileUtils;

var
  BigEndian: boolean = false;

{ TEpiImport }

function TEpiImport.ReadInteger(const St: TStream; Width: Byte): Integer;
begin
  Result := StrToInt(ReadString(St, Width));
end;

function TEpiImport.ReadString(const St: TStream; Width: Byte): String;
var
  Buf: Array of char;
begin
  SetLength(Buf, Width + 1);
  St.Read(Buf[0], Width);
  Buf[Width] := #0;
  Result := trim(String(Buf));
end;

function TEpiImport.ReadString(const St: TStream): string;
var
  C: Char;
begin
  C := #0;
  Result := '';
  repeat
   C := Char(St.ReadByte);
   Result += C;
  until (C = #10) or (C = #13);

  if (C = #13) then
    C := Char(St.ReadByte);

  if C <> #10 then
    // Rewind that one byte, this was a MAC ending...
    St.Seek(-1, soCurrent);
end;

procedure TEpiImport.RaiseError(EClass: ExceptClass; const Msg: string);
begin
  raise EClass.Create(Msg);
end;

procedure TEpiImport.ReadBuf(const St: TStream; var Buf: array of Byte;
  Count: Integer);
var
  i: Integer;
  TmpByte: Byte;
begin
  St.Read(Buf[0], Count);

  if BigEndian then
    for i := 0 to (Count div 2)-1 do
    begin
      TmpByte := Buf[i];
      Buf[i]  := Buf[(Count - 1) - i];
      Buf[(Count - 1) - i] := TmpByte;
    end;
end;

function TEpiImport.ReadInts(const St: TStream; Count: Integer): Integer;
var
  Buf: Array[0..3] of Byte;
begin
  FillChar(Buf, 4, 0);
  ReadBuf(St, Buf, Count);
  Result := Integer(Buf);
end;

function TEpiImport.ReadSInts(const St: TStream; Count: Integer): Integer;
var
  Buf: Array[0..3] of Byte;
  Buf2: Array[0..1] of Word absolute Buf;
begin
  FillChar(Buf, 4, 0);
  ReadBuf(St, Buf, Count);
  case Count of
    1: Result := ShortInt(Buf[0]);
    2: Result := SmallInt(Buf2[0]);
    4: Result := LongInt(Buf);
  end;
end;

function TEpiImport.ReadSingle(const St: TStream): Single;
var
  Buf: Array[0..3] of Byte;
begin
  ReadBuf(St, Buf, 4);
  Result := Single(Buf);
end;

function TEpiImport.ReadDouble(const St: TStream): Double;
var
  Buf: Array[0..7] of Byte;
begin
  ReadBuf(St, Buf, 8);
  Result := Double(Buf);
end;

function TEpiImport.ReEncodeString(const Str: string): string;
begin
  if ImportEncoding = eeGuess then
    Result := EpiUnknownStrToUTF8(Str)
  else
    Result := ConvertEncoding(Str, EpiEncodingToString[ImportEncoding], 'utf8')
end;

function TEpiImport.StringFromBuffer(AChar: PChar; MaxLength: Integer): string;
var
  I: integer;
  Src: PChar;
begin
  result := '';
  I := 0;
  Src := Pointer(AChar);
  while (Src[i] <> #0) and (I < MaxLength) do
  begin
    Result := Result + Src[i];
    Inc(i);
  end;
  Result := ReEncodeString(Result);
end;

function TEpiImport.TruncToInt(e: Extended): integer;
begin
  Result:=integer(Trunc(e));
end;

procedure TEpiImport.SetOnControlItemPosition(AValue: TEpiControlItemPosition);
begin
  if FOnControlItemPosition = AValue then Exit;
  FOnControlItemPosition := AValue;
  FStataImport.OnControlItemPosition := FOnControlItemPosition;
end;

function TEpiImport.DoProgress(ProgressType: TEpiProgressType; const Current,
  Max: Cardinal): boolean;
begin
  result := false;
  if Assigned(OnProgress) then
    OnProgress(nil, ProgressType, Current, Max, Result);
end;

procedure TEpiImport.DoControlItemPosition(const Item: TEpiCustomControlItem;
  var Top, Left: Integer);
begin
  if Assigned(OnControlItemPosition) then
    OnControlItemPosition(Self, Item, Top, Left);
end;

function TEpiImport.GuessTxtFile(DataFile: TEpiDataFile; Lines: TStrings; out
  FieldList: TList; out SkipFirstLine: boolean; out FieldSeparator: Char
  ): boolean;
var
  tabcount, semicoloncount, commacount,
  spacecount: Integer;
  istab, issemi, iscomma, isspace: Boolean;
  i, LineCount, FieldCount, w: Integer;
  TmpStr: string;
  TmpField: TEpiField;
  PossibleTypes: array of TEpiFieldTypes;
  FieldTypeList: array of TEpiFieldType;
  FieldStrings: TStrings;
  j: Integer;
  TmpFT: TEpiFieldType;
  FieldNameInRow1: Boolean;
  ok: Boolean;
  AllTypes: Longint;
  Ft: TEpiFieldType;
  IntVal: EpiInteger;
  BoolVal: EpiBool;
  FloatVal: EpiFloat;
  DateVal: EpiDate;
  ATop: Integer;
  ALeft: Integer;

begin
  result := false;
  SkipFirstLine := false;

  FieldStrings := nil;
  TmpField := nil;
  FieldList := TList.Create;
  try
    tabcount:=0;   semicoloncount:=0; commacount:=0;   spacecount:=0;
    LineCount := Lines.Count;
    w := 0;
    for i := 0 to LineCount - 1 do
    begin
      TmpStr := TrimRight(Lines[i]);
      if Trim(TmpStr) = '' then continue;
      Inc(w);
      inc(tabcount,       StrCountChars(TmpStr, [#9],  '"'));
      inc(semicoloncount, StrCountChars(TmpStr, [';'], '"'));
      inc(commacount,     StrCountChars(TmpStr, [','], '"'));
      inc(spacecount,     StrCountChars(TmpStr, [' '], '"'));
    end;
    tabcount       := tabcount       div w;
    semicoloncount := semicoloncount div w;
    commacount     := commacount     div w;
    spacecount     := spacecount     div w;

    istab:=true;   issemi:=true; iscomma:=true;   isspace:=true;

    { Look for field separator char }
    // 1000 lines should be enougt...
    for i :=0 to Math.Min(1000, LineCount - 1) do
    begin
      TmpStr := TrimRight(Lines[i]);
      if trim(TmpStr)='' then continue;

      if (istab) then
      begin
        w := StrCountChars(TmpStr, [#9], '"');
        if w = 0 then istab := false;
        if w > tabcount then istab := false;
      end;

      if (issemi) then
      begin
        w := StrCountChars(TmpStr, [';'], '"');
        if w = 0 then issemi := false;
        if w > semicoloncount then issemi := false;
      end;

      if (iscomma) then
      begin
        w := StrCountChars(TmpStr, [','], '"');
        if w = 0 then iscomma := false;
        if w > commacount then iscomma := false;
      end;

      if (isspace) then
      begin
        w := StrCountChars(TmpStr, [' '], '"');
        if w = 0 then isspace := false;
        if w > spacecount then isspace := false;
      end;
    end; //for

    { Priority: tab, semicolon, comma, space    }
    if istab        then begin FieldSeparator :=  #9; FieldCount := tabcount + 1;       end
    else if issemi  then begin FieldSeparator := ';'; FieldCount := semicoloncount + 1; end
    else if iscomma then begin FieldSeparator := ','; FieldCount := commacount + 1;     end
    else if isspace then begin FieldSeparator := ' '; FieldCount := spacecount + 1;     end
    else begin
      RaiseError(Exception, 'Illegal format of textfile. Field separator not found.');
      Exit;
    end;

    // Guess field type.
    // Skip first line since it may contain headings/field names.
    SetLength(PossibleTypes, FieldCount);
    // Remove Boolean type in detection, because we really do not want users to use boolean fields - it is bad practice.
    FillDWord(PossibleTypes[0], FieldCount, Longint(AllFieldTypes - AutoFieldTypes - BoolFieldTypes));
    for i := 1 to LineCount - 1 do
    begin
      DoProgress(eptRecords, i, LineCount * 3);
      TmpStr := Lines[i];
      if Trim(TmpStr) = '' then continue;

      SplitString(TmpStr, FieldStrings, [FieldSeparator], ['"']);

      for j := 0 to FieldStrings.Count -1 do
      begin
        TmpStr := FieldStrings[j];

        if TmpStr = TEpiStringField.DefaultMissing then
          TmpStr := '';
        if TmpStr = '' then continue;

        FindFieldType(TmpStr, PossibleTypes[j]);
      end;
    end;

    SetLength(FieldTypeList, FieldCount);
    FillByte(FieldTypeList[0], FieldCount, Byte(ftString));
    for i := 0 to FieldCount - 1 do
    begin
      for Ft in PossibleTypes[i] do
        FieldTypeList[i] := MinFt(Ft, FieldTypeList[i]);
    end;

    // Create Fields.
    for i := 0 to FieldCount - 1 do
    begin
      TmpField := DataFile.NewField(FieldTypeList[i]);
      TmpField.BeginUpdate;

      FieldList.Add(TmpField);

      if FieldTypeList[i] in (IntFieldTypes + FloatFieldTypes + StringFieldTypes)
      then
        with TmpField do
        begin
          Length := 0;
          Decimals := 0;
        end;

      DoControlItemPosition(TmpField, ATop, ALeft);
      TmpField.Top := ATop;
      TmpField.Left := ALeft;
      TmpField.EndUpdate;
    end;

    // Guess field lengths
    // Skip first line since it may contain headings/field names.
    for i := 1 to LineCount - 1 do
    begin
      DoProgress(eptRecords, i + LineCount, LineCount * 3);
      TmpStr := Lines[i];
      if Trim(TmpStr) = '' then continue;

      SplitString(TmpStr, FieldStrings, [FieldSeparator], ['"']);

      for j := 0 to FieldStrings.Count -1 do
      with TEpiField(FieldList[j]) do
      begin
        TmpStr := FieldStrings[j];

        if (TmpStr = '.') or (Trim(TmpStr) = '') then continue;

        if FieldType in IntFieldTypes
        then
          Length := Max(Length, UTF8Length(TmpStr));

        if FieldType in FloatFieldTypes
        then
          begin
            Length := Max(Length, UTF8Length(TmpStr));
            if (StrCountChars(Tmpstr, ['.', ','], '"') > 0) then
              Decimals := UTF8Length(Tmpstr) - Pos(BoolToStr(Pos('.', Tmpstr) > 0, '.', ','), TmpStr);
          end;

        if FieldType in StringFieldTypes
        then
          Length := Max(Length, UTF8Length(TmpStr));
      end;
    end;



    // Guess field names (and variable labels).
    // And correct fieldtypes if FieldLength = 0 (this indicates that fieldtype found
    // - previously did not succeed. Make type = ftAlfa and Length = 1;
    FieldNameInRow1 := false;
    SplitString(Lines[0], FieldStrings, [FieldSeparator], ['"']);

    for i := 0 to FieldStrings.Count - 1 do
    begin
      TmpStr := FieldStrings[i];
      TmpField := TEpiField(FieldList[i]);

      if (TmpField.Length = 0) and (TmpField.FieldType = ftInteger) then
      begin
        // TODO : Fix so that previous field is removed!!!
{        Raise Exception.Create('INCORRECTLY IMPLEMENTED. PLEASE NOTIFY EPIDATA.');
        TmpField := TEpiField.CreateField(ftString);
        TmpField.FieldLength := 1;
        TmpField.FieldName := DataFile.DataFields[i].FieldName;
        TmpField.FieldDecimals := DataFile.DataFields[i].FieldDecimals;  }
      end;

      case TmpField.FieldType of
        ftInteger:
          ok := IsInteger(TmpStr, IntVal);
        ftString, ftUpperString:
          ok := True;
        ftBoolean:
          Ok := IsBoolean(TmpStr, BoolVal);
        ftFloat:
          ok := IsFloat(TmpStr, FloatVal);
        ftDMYDate,
        ftMDYDate,
        ftYMDDate:
          ok := IsDate(TmpStr, TmpField.FieldType, DateVal);
      end;

      if (not ok) then
      begin
        FieldNameInRow1 := true;
        Break;
      end;
    end;

    if FieldNameInRow1 then
    begin
      for i := 0 to FieldStrings.Count - 1 do
      begin
        DataFile.Field[i].Name := AutoFieldName(FieldStrings[i]);
        DataFile.Field[i].Question.Text := FieldStrings[i];
      end;
      SkipFirstLine := true;
    end;
    result := true;
  finally
    if Assigned(FieldStrings) then FreeAndNil(FieldStrings);
  end;
end;

procedure TEpiImport.FindFieldType(var Value: String;
  var PossibleTypes: TEpiFieldTypes);
var
  ft: TEpiFieldType;
  DataVal: EpiDate;
  FloatVal: EpiFloat;
  TimeVal: EpiTime;
  IntVal: EpiInteger;
  BoolVal: EpiBool;
  StringVal: EpiString;
begin
  // Only string is possible, we can do nothing to change it.
  if PossibleTypes = [ftString] then
    Exit;

  // TIME
  if (ftTime in PossibleTypes) and
     (not IsTime(Value, TimeVal))
  then
    Exclude(PossibleTypes, ftTime);

  // DATES
  for Ft in (DateFieldTypes - AutoFieldTypes) do
    if (Ft in PossibleTypes) and
       (not IsDate(Value, Ft, DataVal))
    then
      Exclude(PossibleTypes, Ft);

  // FLOAT
  if (ftFloat in PossibleTypes) and
     (not IsFloat(Value, FloatVal))
  then
    Exclude(PossibleTypes, ftFloat);

  // INTEGER
  if (ftInteger in PossibleTypes) and
     (not IsInteger(Value, IntVal))
  then
    Exclude(PossibleTypes, ftInteger);

  // BOOLEAN
  if (ftBoolean in PossibleTypes) and
     (not IsBoolean(Value, BoolVal))
  then
    Exclude(PossibleTypes, ftBoolean);

  if (ftUpperString in PossibleTypes) and
     (not IsUppercase(Value, StringVal))
  then
    Exclude(PossibleTypes, ftUpperString);

  // No check for ftString... everything can be read as string.
end;

function TEpiImport.IsInteger(const S: String; out Val: EpiInteger): boolean;
begin
  result := TryStrToInt64(S, Val);
  if not Result then
    Val := TEpiIntField.DefaultMissing;
end;

function TEpiImport.IsFloat(const S: String; out Val: EpiFloat): boolean;
var
  Fmt: TFormatSettings;
begin
  Fmt := DefaultFormatSettings;
  Fmt.DecimalSeparator := '.';

  result := TryStrToFloat(S, Val, Fmt);
  if result then exit;

  Fmt.DecimalSeparator := ',';
  result := TryStrToFloat(S, Val, Fmt);

  if not Result then
    Val := TEpiFloatField.DefaultMissing;
end;

function TEpiImport.IsDate(const S: String; const Ft: TEpiFieldType; out
  Val: EpiDate): boolean;
var
  Msg: string;
  Sep: Char;
const
  DateSeparators:  TCharSet = ['-', '/', '\', '.'];
begin
  result := false;
  for Sep in DateSeparators do
    Result := Result or EpiStrToDate(S, Sep, Ft, Val, Msg);

  if not Result then
    Val := TEpiDateField.DefaultMissing;
end;

function TEpiImport.IsTime(const s: string; out Val: EpiTime): boolean;
var
  Msg: string;
  Sep: Char;
const
  TimeSeparators:  TCharSet = ['-', ':', '.'];
begin
  Result := false;
  for Sep in TimeSeparators do
    result := result or EpiStrToTime(S, Sep, Val, Msg);

  if not Result then
    Val := TEpiDateTimeField.DefaultMissing;
end;

function TEpiImport.IsBoolean(const S: string; out Val: EpiBool): boolean;
var
  T: String;
begin
  result := false;

  if Length(S) = 0 then exit;
  T := UTF8LowerCase(S);

  if Length(T) = 1 then
  begin
    result :=
      (S[1] in ['y','1','t']) or
      (S[1] in ['n','0','F']);

    if Result then
      Val := Byte(S[1] in ['y','1','t']);
  end;

  if Length(T) > 1 then
  begin
    result :=
      (S[1] in ['y','t']) or
      (S[1] in ['n','f']);

    if Result then
      Val := Byte(S[1] in ['y','1','t']);
  end;

  if not Result then
    Val := TEpiBoolField.DefaultMissing;
end;

function TEpiImport.IsUppercase(const S: string; out Val: EpiString): boolean;
var
  U: String;
begin
  U := UTF8UpperCase(S);
  result := UTF8CompareStr(U, S) = 0;

  if result then
    Val := S
  else
    Val := TEpiStringField.DefaultMissing;
end;

function TEpiImport.MinFt(const Ft1, Ft2: TEpiFieldType): TEpiFieldType;
var
  CompareSet: TEpiFieldTypes;
  Ft1Set: TEpiFieldTypes;
  Ft2Set: TEpiFieldTypes;
begin
  Result := Ft1;

  // The "order" of field types should be as follows:
  //  1: Bool
  //  2: Int
  //  3: Float
  //  3: Date/Time
  //  4: String
  //
  // This order differes from CompareFieldTypeOrder, bacause
  // when importing data from string file, the text representation
  // order is somewhat different.

  CompareSet := BoolFieldTypes;
  if (Ft2 in BoolFieldTypes) and
     (not (Ft1 in CompareSet))
  then
    Exit(Ft2);

  CompareSet := CompareSet + (IntFieldTypes);
  if (Ft2 in IntFieldTypes) and
     (not (Ft1 in CompareSet))
  then
    Exit(Ft2);

  CompareSet := CompareSet + (FloatFieldTypes);
  if (Ft2 in FloatFieldTypes) and
     (not (Ft1 in CompareSet))
  then
    Exit(Ft2);

  CompareSet := CompareSet + (DateFieldTypes + TimeFieldTypes);
  if (Ft2 in DateFieldTypes + TimeFieldTypes) and
     (not (Ft1 in CompareSet))
  then
    Exit(Ft2);

  CompareSet := CompareSet + StringFieldTypes;
  if (Ft2 in StringFieldTypes) and
     (not (Ft1 in CompareSet))
  then
    Exit(Ft2);
end;

constructor TEpiImport.Create;
begin
  FImportEncoding := eeGuess;
  FStataImport    := TEpiStataImport.Create;
end;

destructor TEpiImport.Destroy;
begin
  FStataImport.Free;
  inherited Destroy;
end;

function TEpiImport.ImportRec(const DataStream: TStream;
  var DataFile: TEpiDataFile; ImportData: boolean): boolean;
var
  TxtFile: TextFile;
  TxtLine: string;
  TempInt: Int64;
  HeaderLineCount: Integer;
  ValCode: Integer;
  ValCodeFloat: Extended;
  TotFieldLength: Integer;
  CurrentLine: Integer;
  TmpFieldChar, Dummy: Char;
  TmpFieldTypeInt,
  TmpFieldColor, TmpQuestX, TmpQuestY, TmpLength,
  TmpFieldX, TmpFieldY, TmpQuestColor: Integer;
  TmpName: string[10];
  TmpLabel, TmpStr: string;
  CurRec: Integer;
  StrBuf: String;
  BufPos: Integer;
  EncData: String;
  Stop: Boolean;
  TmpFieldType: TEpiFieldType;
  FieldIsQuestion: Boolean;
  EHeading: TEpiHeading;
  EField: TEpiField;
  VariableLabel: String;
//  DataStream: TMemoryStream;
  CharBuf: Array of char;
  IsCrypt: TBits;
  i: Integer;
  Decrypter: TDCP_rijndael;
  LocalDateSeparator: Char;
  UTmpStr: String;
  VLName: String;
  NewVLset: Boolean;
  VLSet: TEpiValueLabelSet;
  VL: TEpiCustomValueLabel;
  Lines: TStringList;
  ImportFormatSettings: TFormatSettings;
  ApproxRecCount: Integer;
  C: Char;

const
  // Convert old REC file fieldtype number to new order of fieldtypes.
  FieldTypeConversionTable: array[0..20] of TEpiFieldType = (
  //   0           1            2           3              4
  //   ftInteger,  ftString,    ftDate,     ftUpperAlfa,   ftCheckBox,
       ftInteger,  ftString,    ftMDYDate,  ftUpperString, ftBoolean,

  //   Note on 6:  Since new format support ~18 digits in new integer field type
  //     the old "double real" is converted, as it contains no decimals (by
  //     definition). Floating fields with digits has a FieldTypeNo > 100, and
  //     is handled seperately in the code below.
  //   5           6            7           8              9
  //   ftBoolean,  ftFloat,     ftPhoneNum, ftTime,        ftLocalNum,
       ftBoolean,  ftInteger,   ftString,   ftTime,        ftString,

  //   10          11           12          13             14
  //   ftToday,    ftEuroDate,  ftIDNUM,    ftRes4,        ftRes5,
       ftMDYAuto, ftDMYDate,   ftAutoInc,  ftString,      ftString,

  //   15          16           17          18             19
  //   ftQuestion, ftEuroToday, ftSoundex,  ftCrypt,       ftYMDDate,
       ftString,   ftDMYAuto,  ftString,   ftString,      ftYMDDate,

  //   20
  //   ftYMDAuto);
       ftYMDAuto
  );

  function RequestPassword(Const EncryptedString: string): boolean;
  var
    S, FPassword: string;
    Digest: Pointer;
    Hash: TDCP_sha1;
    InitVector: array[0..15] of byte;
  begin
    result := false;
    if Assigned(FOnRequestPassword) then
      FOnRequestPassword(Self, S, FPassword);
    try
      // The initialization of the IV like this, makes importing encrypted .rec
      // files possible, without using the DCP1COMPAT defines
      // in DCPCrypt package.
      GetMem(Digest,TDCP_sha1.GetHashSize div 8);
      Hash:= TDCP_sha1.Create(nil);
      Hash.Init;
      Hash.UpdateStr(FPassword);
      Hash.Final(Digest^);
      Hash.Free;

      S := Base64DecodeStr(EncryptedString);
      FillChar(InitVector, 16, $FF);
      Decrypter := TDCP_rijndael.Create(nil);
      Decrypter.Init(Digest^, TDCP_sha1.GetHashSize, @InitVector);
      Decrypter.EncryptECB(InitVector, InitVector);
      Decrypter.SetIV(InitVector);
      DeCrypter.DecryptCFB8bit(S[1], S[1], Length(S));
      DeCrypter.Reset;
      Result := (AnsiCompareText(FPassword, S) = 0);
    except
      Abort;
    end;
  end;

  function TextPos(var F: Textfile): Cardinal;
  begin
    with TTextRec(F) do
    begin
      Result := FileSeek(Handle, 0, 1);
      if Mode = FMOutput then
        inc(Result, BufPos)
      else if BufEnd <> 0 then
        Dec(Result, BufEnd-BufPos);
    end;
  end;

begin
  result := false;

  if not Assigned(DataFile) then
    DataFile := TEpiDataFile.Create(nil);

  ImportFormatSettings := DefaultFormatSettings;
  ImportFormatSettings.DecimalSeparator := '.';
  ImportFormatSettings.DateSeparator := '/';

  with DataFile do
  try
    TxtLine := ReadString(DataStream);

    // --- Read "First Line" header ---
    //ReadLn(TxtFile, TxtLine);

    // - Password
    TempInt := Pos('~KQ:', AnsiUpperCase(TxtLine));
    if TempInt > 0 then
      if not RequestPassword(Copy(TxtLine, TempInt + 4, Pos(':KQ~', AnsiUpperCase(TxtLine)) - (TempInt + 4))) then
        RaiseError(EIncorrectPasswordException, 'Incorrect Password');

    // - FileLabel
    StrBuf := '';
    if Pos('FILELABEL: ', AnsiUpperCase(TxtLine)) > 0 then
      StrBuf :=  EpiUnknownStrToUTF8(Copy(TxtLine, Pos('FILELABEL: ', AnsiUpperCase(TxtLine)) + Length('FILELABEL: ') , Length(TxtLine)));
    Caption.Text := Trim(StrBuf);

    // - Header lines:
    TempInt := Pos(' ', TxtLine)-1;
    if TempInt = -1 then TempInt := Length(TxtLine);
    Val(Copy(TxtLine, 1, TempInt), HeaderLineCount, ValCode);
    if ValCode > 0 then
      RaiseError(Exception, 'Incorrect format!');

    // Read field defining header lines.
    TotFieldLength := 0;

    IsCrypt := TBits.Create(HeaderLineCount);
    for CurrentLine := 1 to HeaderLineCount do
    begin
      // Display Character (pos: 1        => width: 1)
      TmpFieldChar := Char(DataStream.ReadByte);

      // FieldName         (pos: 2-11     => width: 10)
      TmpName := ReadString(DataStream, 10);

      // byte pad          (pos: 12       => width: 1) (skipped)
      DataStream.ReadByte;

      // Question Column   (pos: 13-16    => width: 4)
      TmpQuestX := ReadInteger(DataStream, 4);

      // Question Line     (pos: 17-20    => width: 4)
      TmpQuestX := ReadInteger(DataStream, 4);

      // Question Color    (pos: 21-24    => widht: 4) (skipped)
      ReadInteger(DataStream, 4);

      // Field Column      (pos: 25-28    => width: 4)
      TmpFieldX := ReadInteger(DataStream, 4);

      // Field Line        (pos: 29-32    => width: 4)
      TmpFieldY := ReadInteger(DataStream, 4);

      // Field Type        (pos: 33-36    => widht: 4)
      TmpFieldTypeInt := ReadInteger(DataStream, 4);

      // Field Width       (pos: 37-40    => widht: 4)
      TmpLength :=  ReadInteger(DataStream, 4);

      // Field Color       (pos: 41-44    => widht: 4)
      TmpFieldColor :=  ReadInteger(DataStream, 4);

      // byte pad          (pos: 45       => width: 1) (skipped)
      DataStream.ReadByte;

      // Question          (pos: 46+      => width: until end of line)
      TmpLabel := ReadString(DataStream);

{      ReadLn(TxtFile,
             TmpFieldChar, TmpName, TmpQuestX, TmpQuestY,
             TmpQuestColor, TmpFieldX, TmpFieldY, TmpFieldTypeInt, TmpLength,
             TmpFieldColor, dummy, TmpLabel);      }

      // Field types.
      FieldIsQuestion := false;
      if TmpFieldTypeInt >= 100 then
      begin
        // Type > 100 => float field
        TmpFieldType := ftFloat;
        // Old Float field with 0 decimals (Type = 100) are converted to Integers.
        if TmpFieldTypeInt = 100 then
          TmpFieldType := ftInteger;
      end else begin
        // Normal field type recognition.
        TmpFieldType := FieldTypeConversionTable[TmpFieldTypeInt];

        // This is an encrypted field... (new XML does not have single encrypted fields.)
        if (TmpFieldTypeInt = 18) then
          IsCrypt.Bits[Fields.Count] := true;

        // This is not a data field, but a question field.
        if (TmpFieldTypeInt = 15) or (TmpLength = 0) then
          FieldIsQuestion := true;
      end;

      // Unsupported field are automatically converted to string (ftString) fields.
      if (TmpFieldType in DateFieldTypes) and (TmpLength < 10) then
        TmpFieldType := ftString;

      // Trim text information.
      TmpName := Trim(TmpName);
      case ImportCasing of
        fncUpper: TmpName := UTF8UpperCase(TmpName);
        fncLower: TmpName := UTF8LowerCase(TmpName);
        fncAsIs: ;
      end;

      TmpLabel := Trim(TmpLabel);

      if FieldIsQuestion then
      begin
        EHeading := NewHeading;
        EHeading.BeginUpdate;
        with EHeading do
        begin
          Caption.Text := EpiUnknownStrToUTF8(TmpLabel);

          DoControlItemPosition(EHeading, TmpQuestX, TmpQuestY);
          Left         := TmpQuestX;
          Top          := TmpQuestY;
        end;
        EHeading.EndUpdate;
        Continue;
      end;

      EField := NewField(TmpFieldType);
      EField.BeginUpdate;
      with EField do
      begin
        Length         := TmpLength;
        Decimals       := 0;
        if TmpFieldTypeInt >= 100 then
          Decimals := TmpFieldTypeInt - 100;

        with Question do
        begin
          VariableLabel := EpiUnknownStrToUTF8(StringReplace(TmpLabel, '_', '-', [rfReplaceAll]));
          // In old style .REC files, first word in label is the name of the field. Remove it.
          if Pos(TmpName, VariableLabel) > 0 then
            VariableLabel := Trim(Copy(VariableLabel, System.Length(TmpName)+1, System.Length(VariableLabel)));
          Text := VariableLabel;
        end;
        // Ensure valid variable name.
        Name := TmpName;

        DoControlItemPosition(EField, TmpFieldY, TmpFieldX);
        Left           := TmpFieldX;
        Top            := TmpFieldY;

        // Summerize field findings.
        TotFieldLength := TotFieldLength + Length;
      end;  // With EField
      EField.EndUpdate;
    end; // For CurrentLine

    // Position for reading and check for corruptness.
    TotFieldLength := TotFieldLength + (((TotFieldLength - 1) DIV 78) + 1) * 3; { MaxRecLineLength = 78 }
//    TmpLength := TextPos(TxtFile);
//    CloseFile(TxtFile);

    LocalDateSeparator := DateSeparator;
    DefaultFormatSettings.DateSeparator := '/';  // This was standard in old .rec files.

    if ImportData then
    begin
{      DataStream := TMemoryStream.Create;
      DataStream.LoadFromFile(UTF8ToSys(AFilename));
      DataStream.Position := TmpLength; }


      ApproxRecCount := (DataStream.Size - TmpLength) div TotFieldLength;
      DoProgress(eptInit, 0, ApproxRecCount);

      SetLength(CharBuf, TotFieldLength);
      BeginUpdate;
      CurRec := 0;

      while true do
      begin
        DoProgress(eptRecords, CurRec, ApproxRecCount);

        I := DataStream.Read(CharBuf[0], TotFieldLength);
        if (I <> TotFieldLength) then
        begin
          break;
          // TODO : Exit gracefully when reading beyond last record...
          // Could be an imcomplete record.
        end;

        NewRecords();  // Increments by one in size, but expands using "growth factor".
        StrBuf := CharBuf[High(CharBuf) - 2];
        if StrBuf = '?' then
          Deleted[CurRec] := true
        else if StrBuf = '^' then
          Verified[CurRec] := true;

        StrBuf := StringReplace(string(CharBuf), '!'#13#10, '', [rfReplaceAll]);
        BufPos := 1;
        for i := 0 TO Fields.Count - 1 DO
        with Fields[i] do begin
          TmpStr := Trim(Copy(StrBuf, BufPos, Length));

          if TmpStr = '' then
          begin
            IsMissing[CurRec] := true;
            Inc(BufPos, Length);
            continue;
          end;

          if IsCrypt.Bits[i] then
          begin
            EncData := Base64DecodeStr(TmpStr);
            Decrypter.DecryptCFB8bit(EncData[1], EncData[1], System.Length(EncData));
            TmpStr := Trim(EncData);
            Decrypter.Reset;
          end;
          if FieldType = ftFloat then       // "." was Always dec. separator in old .REC files.
            TmpStr := StringReplace(TmpStr, '.', DecimalSeparator, [rfReplaceAll]);
          AsString[CurRec] := EpiUnknownStrToUTF8(TmpStr);
          Inc(BufPos, Length);
        end;
        Inc(CurRec);
      end;
      DoProgress(eptFinish, ApproxRecCount, ApproxRecCount);
      EndUpdate;
    end;

    // A little hack to extract filename and load the .chk file.
    TmpStr := '';
    if DataStream is TFileStreamUTF8 then
      TmpStr := TFileStreamUTF8(DataStream).FileName;

    TmpStr := ChangeFileExt(TmpStr, '.chk');
    if Not FileExistsUTF8(TmpStr) then
      TmpStr := ChangeFileExt(TmpStr, '.CHK');

    if FileExistsUTF8(TmpStr) then
    try
      // Import CHK files.
      Lines := TStringList.Create;
      Lines.LoadFromFile(UTF8ToSys(TmpStr));
      EpiUnknownStringsToUTF8(Lines);

      i := 0;
      while (i < Lines.Count) do
      begin
        TmpStr := Trim(Lines[i]);
        UTmpStr := UTF8UpperCase(TmpStr);

        if Pos('LABELBLOCK', UTmpStr) > 0 then
        begin
          // LABELBLOCK found.
          while true do
          begin
            Inc(i);
            TmpStr := Trim(Lines[i]);
            UTmpStr := UTF8UpperCase(TmpStr);
            if Pos('LABEL', UTmpStr) > 0 then
            begin
              Copy2SpaceDel(TmpStr);
              VLName := Trim(TmpStr);

              NewVLset := true;
              Inc(i);
              TmpStr := Trim(Lines[i]);
              UTmpStr := UTF8UpperCase(TmpStr);
              while not ((Pos('END', UTmpStr) > 0) and (Length(UTmpStr) = 3)) do
              begin // Read individual labels.
                // Values may be incapsulated in "..."
                if TmpStr[1] = '"' then
                begin
                  StrBuf := AnsiDequotedStr(Trim(TmpStr), '"');
                  Delete(TmpStr, 1, Length(StrBuf)+2);
                end else
                  StrBuf := Trim(Copy2SpaceDel(TmpStr));

                if NewVLset then
                begin
                  if TryStrToInt(StrBuf, ValCode) then
                    TmpFieldType := ftInteger
                  else if TryStrToFloat(StrBuf, ValCodeFloat) then
                    TmpFieldType := ftFloat
                  else
                    TmpFieldType := ftString;
                  VLSet := ValueLabels.NewValueLabelSet(TmpFieldType);
                  VLSet.Name := VLName;
                  NewVLset := false;
                end;

                VL := VLSet.NewValueLabel;
                case VLSet.LabelType of
                  ftInteger: TEpiIntValueLabel(VL).Value := StrToInt(Strbuf);
                  ftFloat:   TEpiFloatValueLabel(VL).Value := StrToFloat(StrBuf, ImportFormatSettings);
                  ftString:  TEpiStringValueLabel(VL).Value := StrBuf;
                end;
                VL.TheLabel.Text := AnsiDequotedStr(Trim(TmpStr), '"');

                Inc(i);
                TmpStr := Trim(Lines[i]);
                UTmpStr := UTF8UpperCase(TmpStr);
              end; // while POS
              Continue;
            end; // LABEL
            Break;
          end; // while true
          Inc(i);
        end; {else
        if Fields.FieldExistsByName(TmpStr) then
        begin
          // Field block - try to find simple COMMENT LEGAL USE construct.
        end;}
        Inc(i);
      end;
    finally
      Lines.Free;
    end;
  finally
    DefaultFormatSettings.DateSeparator := LocalDateSeparator;
//    if Assigned(DataStream) then DataStream.Free;
  end;
  result := true;
end;

function TEpiImport.ImportRec(const aFileName: string;
  var DataFile: TEpiDataFile; ImportData: boolean): boolean;
var
  FS: TStream;
begin
  if not FileExistsUTF8(aFilename) then exit(false);

  try
    FS := TFileStreamUTF8.Create(aFilename, fmOpenRead);
    Result := ImportRec(FS, DataFile, ImportData);
  finally
    FS.Free;
  end;
end;


function TEpiImport.ImportStata(const DataStream: TStream;
  const Doc: TEpiDocument; var DataFile: TEpiDataFile; ImportData: boolean
  ): Boolean;
type
  DateType = (tnone, tc, td, tw, tm, tq, th, ty);
var
  ByteBuf, ValBuf: Array of Byte;
  TypeList,
  CharBuf: Array of Char;
  DateTypeList: Array of DateType;
  NVar, NObs, CurRec, CurField,
  Sum, I, J, TmpInt: integer;
  TmpFlt: Double;
  TmpField: TEpiField;
  StrBuf: string;

  // Version specific variables.
  FieldNameLength, StrBaseNum, FileLabelLength,
  FmtLength: Integer;
  ByteChar, IntChar, LongChar,
  FloatChar, DoubleChar: Char;
  MissingBaseNum: Cardinal;
  DecS: Char;
  TmpFieldType: TEpiFieldType;
  FileVersion: TEpiStataVersion;
  VarDataLength: Integer;

  FieldList: TFPList;
  VLSet: TEpiValueLabelSet;
  VL: TEpiCustomValueLabel;
  MisVal: Integer;
  ExpType: Integer;
  VarName: String;
  Character: String;
  DFNotes: TStringList;
  FieldNotes: TStringList;
  txtlen: Integer;
  Off: Array of Integer;
  Val: Array of Integer;
  n: Integer;
  S: String;
  ATop: Integer;
  ALeft: Integer;

  function ReadSingleMissing(var MisVal: Integer): Single;
  var
    Buf: Array[0..3] of Byte absolute Result;
  begin
    MisVal := -1;
    Result := ReadSingle(DataStream);

    if (Buf[3]=$7F) and ((Buf[0] AND Buf[2]) = 0) then
      MisVal := Buf[1] div $08
  end;

  function ReadDoubleMissing(var MisVal: integer): Double;
  var
    Buf: Array[0..7] of Byte absolute Result;
  begin
    MisVal := -1;
    Result := ReadDouble(DataStream);

    if (Buf[6]=$E0) AND (Buf[7]=$7F) and ((Buf[0] AND Buf[1] AND Buf[2] AND Buf[3] AND Buf[4]) = 0) then
      MisVal := Buf[5];
  end;

begin
  result := false;

  // ********************************
  //           STATA HEADER
  // ********************************

  // With Stata 13+ (dta 117+), the format have changed significantly
  // ie. to a XML like structure, hence we must make a test for this
  // first.
  SetLength(CharBuf, 11);
  DataStream.Read(CharBuf[0], 11);
  S := StringFromBuffer(@CharBuf[0], 11);

  // first line in Stata 13+ is <stata_dta>
  if (S = '<stata_dta>') // (CharBuf[0] = '<') and (CharBuf[10] = '>')
  then
    begin
      result := FStataImport.ImportStata(DataStream, Doc, DataFile, ImportData);
      Exit;
    end;

  if not Assigned(DataFile) then
    DataFile := Doc.DataFiles.NewDataFile;

  DataFile.Fields.Sorted := false;

  With DataFile do
  try

    DataStream.Position := 0;

    SetLength(ByteBuf, 4);
    DataStream.Read(ByteBuf[0], 4);
    FileVersion := TEpiStataVersion(ByteBuf[0]);

    if not (FileVersion in [dta4..dta12]) then
    BEGIN
      RaiseError(Exception, 'Unknown Stata Version');
      Exit;
    END;

    // Version specific settings:
    // - "original" setting from Ver. 4
    FieldNameLength := 9;
    FileLabelLength := 32;
    StrBaseNum      := $7F;
    FmtLength       := 12;
    MissingBaseNum  := 0;
    ByteChar        := 'b';
    IntChar         := 'i';
    LongChar        := 'l';
    FloatChar       := 'f';
    DoubleChar      := 'd';
    // - changed in Ver. 6
    if FileVersion >= dta6 THEN     // dta_108
      FileLabelLength := 81;
    // - change in Ver. 7
    IF FileVersion >= dta7 THEN     // dta_110
      FieldNameLength := 33;
    // - changed in Ver. 8
    IF FileVersion >= dta8 THEN     // dta_113
    BEGIN
      StrBaseNum := 0;
      MissingBaseNum := 26;
      ByteChar   := StataByteConst;
      IntChar    := StataIntConst;
      LongChar   := StataLongConst;
      FloatChar  := StataFloatConst;
      DoubleChar := StataDoubleConst;
    END;
    // - changed in Ver. 10
    if FileVersion >= dta10 THEN     // dta_114
      FmtLength := 49;

    // byteorder: NumBuff[1]
    IF not (ByteBuf[1] in [1, 2]) THEN
    BEGIN
      RaiseError(Exception, 'Unknown Stata Byte Order');
      Exit;
    END;
    if ByteBuf[1] = 1 then
      BigEndian := true;

    // filetype: NumBuff[2] (always 1)   -  NumBuff[3] not used.
    IF ByteBuf[2]<>1 THEN
    BEGIN
      RaiseError(Exception, 'Stata Error: NumBuf[2] MUST be "1"');
      Exit;
    END;

    // nvars (number of vars)
    nVar := ReadInts(DataStream, 2);

    // nobs (number of obs)
    NObs := ReadInts(DataStream, 4);
    // Don't mess with the records if only structure is imported.
    if ImportData then
      Size := NObs;

    // data_label \0 terminated.
    SetLength(CharBuf, FileLabelLength);
    DataStream.Read(CharBuf[0], FileLabelLength);
    Caption.Text := StringFromBuffer(PChar(@CharBuf[0]), FileLabelLength);

    // time_stamp \0 terminated (not used in epidata)
    DataStream.Read(CharBuf[0], 18);

    // ********************************
    //         STATA DESCRIBTORS
    // ********************************
    // - typlist: the variable's types
    SetLength(TypeList, NVar);
    SetLength(DateTypeList, NVar);
    DataStream.Read(TypeList[0], nVar);

    // - varlist: variable names
    SetLength(CharBuf, FieldNameLength * NVar);
    DataStream.Read(CharBuf[0], FieldNameLength * NVar);

    // - Skip reading sorting list
    DataStream.Seek(2 * (NVar + 1), soCurrent);

    // - Fmtlist: list of formats of the variables
    //            only relevant for dates.
    SetLength(ByteBuf, FmtLength * NVar);
    DataStream.Read(ByteBuf[0], FmtLength * NVar);

    VarDataLength := 0;
    FieldList := TFPList.Create;
    FieldList.Capacity := NVar;
    FOR i := 0 TO NVar - 1 DO
    BEGIN
      // Update typelist to consts...
      if TypeList[i] = ByteChar then   TypeList[i] := StataByteConst;
      if TypeList[i] = IntChar then    TypeList[i] := StataIntConst;
      if TypeList[i] = LongChar then   TypeList[i] := StataLongConst;
      if TypeList[i] = FloatChar then  TypeList[i] := StataFloatConst;
      if TypeList[i] = DoubleChar then TypeList[i] := StataDoubleConst;

      // As default - expect that FloatConst and DoubleConst are floating points.
      // - detect if this is a time/date format later on.
      IF (TypeList[i] in [StataFloatConst, StataDoubleConst]) THEN
        TmpFieldType := ftFloat
      ELSE IF (TypeList[i] in [StataByteConst, StataIntConst, StataLongConst]) THEN
        TmpFieldType := ftInteger
      ELSE
      BEGIN
        if (Ord(TypeList[i]) - StrBaseNum) < 0 then
        BEGIN
          RaiseError(Exception, 'Unknown variable type found in Stata-file');
          Exit;
        END;
        TmpFieldType := ftString;
      END;

      {Handle formats...}
      StrBuf := Trim(UpperCase(StringFromBuffer(PChar(@ByteBuf[i * FmtLength]), FmtLength)));
      if not (StrBuf[1] = '%') then
      BEGIN
        RaiseError(Exception, Format('Unknown format specified for variable no: %d', [i+1]));
        Exit;
      END;

      j := 0;
      if StrBuf[2] = '-' then
        Inc(j);

      DateTypeList[i] := tnone;
      // Strings have already been defined in <typlist>.
      if StrBuf[Length(StrBuf)] <> 'S' then
      begin
        if StrBuf[2+j] = 'T' then
          Inc(j);

        Case Char(StrBuf[2+j]) of
          // Date (and time formats)
          'C': // Time - count of millisecs since 1/1-1960 00:00:00.000
            begin
              TmpFieldType := ftTime;
              DateTypeList[i] := tc;
            end;
          'D', // Date - count of days:       since 1/1-1960
          'W', //      - count of weeks
          'M', //      - count of months
          'Q', //      - count of quartes
          'H', //      - count of half years
          'Y': // Year - count of years:      since 0 AD.
            begin
              TmpFieldType := ftDMYDate;
              Case Char(StrBuf[2+j]) of
                'D': DateTypeList[i] := td;
                'W': DateTypeList[i] := tw;
                'M': DateTypeList[i] := tm;
                'Q': DateTypeList[i] := tq;
                'H': DateTypeList[i] := th;
                'Y': DateTypeList[i] := ty;
              end;
            end;
          // Number
          '0'..'9': ;
        else
          RaiseError(Exception, Format('Unknown format specified for variable no: %d', [i+1]));
          Exit;
        end;
      end;

      TmpField := NewField(TmpFieldType);
      FieldList.Add(TmpField);
      TmpField.BeginUpdate;
      WITH TmpField DO
      BEGIN
        Top      := -1;
        Length   := 0;
        Decimals := 0;
      END;

      // - typelist
      case TypeList[i] of
        StataByteConst: begin
                     TmpField.Length := 3;
                     Inc(VarDataLength, 1);
                   end;
        StataIntConst:  begin
                     TmpField.Length := 5;
                     Inc(VarDataLength, 2);
                   end;
        StataLongConst: begin
                     TmpField.Length := 10;
                     Inc(VarDataLength, 4);
                   end;
        StataFloatConst,
        StataDoubleConst:
          Begin
            TmpField.Length := 18;
            TmpField.Decimals := 4;
            if TypeList[i] = StataFloatConst then
              Inc(VarDataLength, 4)
            else
              Inc(VarDataLength, 8);
          End;
      else
        TmpField.Length := Ord(TypeList[i]) - StrBaseNum;
        Inc(VarDataLength, TmpField.Length);
      end;

      // Dates:
      if TmpField.FieldType in DateFieldTypes + TimeFieldTypes then
      begin
        if TmpField.FieldType in TimeFieldTypes then
          TmpField.Length := 8
        else
          TmpField.Length := 10;
        TmpField.Decimals := 0;
      end;

      // - varlist
      StrBuf := StringFromBuffer(PChar(@CharBuf[i * FieldNameLength]), FieldNameLength);
      TmpField.Name := Trim(StrBuf);
    END;

    // - lbllist: names af value label
    SetLength(CharBuf, FieldNameLength * NVar);
    DataStream.Read(CharBuf[0], FieldNameLength * NVar);
    FOR i:=0 TO nVar-1 DO
    BEGIN
      TmpField := Fields[i];
      StrBuf := Trim(StringFromBuffer(PChar(@CharBuf[i * FieldNameLength]), FieldNameLength));
      StrBuf := StringReplace(StrBuf, ' ', '_', [rfReplaceAll]);

      IF StrBuf <> '' THEN
      BEGIN
        VLSet := ValueLabels.GetValueLabelSetByName(StrBuf);
        if not Assigned(VLSet) then
        begin
          VLSet := ValueLabels.NewValueLabelSet(ftInteger);
          VLSet.Name := StrBuf
        end;

        TmpField.ValueLabelSet := VLSet;
      END;
    END;

    // ********************************
    //      STATA VARIABLE LABELS aka. EpiData Questions.
    // ********************************
    SetLength(CharBuf, FileLabelLength * NVar);
    DataStream.Read(CharBuf[0], FileLabelLength * NVar);
    FOR i := 0 TO nVar-1 DO
    BEGIN
      TmpField := TEpiField(FieldList[i]);
      StrBuf := Trim(StringFromBuffer(PChar(@CharBuf[i * FileLabelLength]), FileLabelLength));
      StrBuf := StringReplace(StrBuf, '#', ' ', [rfReplaceAll]);
      StrBuf := StringReplace(StrBuf, '>', ' ', [rfReplaceAll]);
      StrBuf := StringReplace(StrBuf, '<', ' ', [rfReplaceAll]);
      StrBuf := StringReplace(StrBuf, '_', ' ', [rfReplaceAll]);
      TmpField.Question.Text := StrBuf;

      // No more field information exists. The update may complete.

      DoControlItemPosition(TmpField, ATop, ALeft);

      TmpField.Top  := ATop;
      TmpField.Left := ALeft;

      TmpField.EndUpdate;
    END;



    // ********************************
    //      STATA EXPANSION FIELDS
    // ********************************
    if FileVersion < dta7 then
    begin
      // - skip expansion fields for Stata < v7 (the format is unknown and without any documentation)
      repeat
        ExpType := ReadInts(DataStream, 1);
        I := ReadInts(DataStream, 2);
        if (ExpType > 0) then
          DataStream.Seek(I, soCurrent);
      until (ExpType = 0);
    end else begin
      DFNotes := TStringList.Create;
      FieldNotes := TStringList.Create;
      FieldNotes.OwnsObjects := true;
      FieldNotes.Sorted := true;

      repeat
        ExpType := ReadInts(DataStream, 1);
        I := ReadInts(DataStream, 4);

        case ExpType of
          0: ; // DO not thing, we break in "until"
          1:   // Stata characteristics... read for notes.
            begin
              SetLength(CharBuf, I);
              DataStream.Read(CharBuf[0], I);

              VarName   := StringFromBuffer(@CharBuf[0], 33);
              Character := StringFromBuffer(@CharBuf[33], 33);
              StrBuf    := StringFromBuffer(@CharBuf[66], I - 66);

              // We do not handle other characteristics about fields/dataset
              // than notes!
              if LeftStr(Character, 4) <> 'note' then continue;
              if (VarName <> '_dta') and (not Fields.ItemExistsByName(VarName)) then continue;  // TODO : Give warning feedback that note exists for unknown field.
              // TmpInt = Note number. Note0 is the actual count of notes, but since there is no order to which the notes are placed in
              // the expansion fields section, we really do not care!
              Delete(Character, 1, 4);
              TmpInt := StrToInt(Character);
              if TmpInt = 0 then continue;

              if VarName = '_dta' then
              begin
                // Note for the dataset
                if TmpInt > DFNotes.Count then for J := (DFNotes.Count + 1) to TmpInt do DFNotes.Add('');
                DFNotes[TmpInt-1] := StrBuf;
              end else begin
                // Note for a field.
                if not FieldNotes.Find(VarName, I) then
                  I := FieldNotes.AddObject(VarName, TStringList.Create);
                with TStringList(FieldNotes.Objects[I]) do
                begin
                  if TmpInt > Count then for J := (Count + 1) to TmpInt do Add('');
                  Strings[TmpInt-1] := StrBuf;
                end;
              end;
            end
        else
          // Skip expansion, this is a non-documented vender expansion.
          DataStream.Seek(I, soCurrent);
        end;
      until ExpType = 0;

      for I := 0 to DFNotes.Count - 1 do
        Notes.Text := Notes.Text + DFNotes[I] + LineEnding;

      for i := 0 to FieldNotes.Count - 1 do
      begin
        TmpField := Fields.FieldByName[FieldNotes[i]];
        for j := 0 to TStringList(FieldNotes.Objects[i]).Count -1 do
          TmpField.Notes.Text := TmpField.Notes.Text + LineEnding + TStringList(FieldNotes.Objects[i]).Strings[J];
      end;
      DFNotes.Free;
      FieldNotes.Clear;
      FieldNotes.Free;
    end;

    I := DataStream.Position;
    if not ImportData then
    begin
      DataStream.Seek(NObs * VarDataLength, soCurrent);
    end else try
      // ********************************
      //          STATA DATA
      // ********************************
      DoProgress(eptInit, 0, NObs);

      FOR CurRec := 0 TO nObs -1 DO
      BEGIN
        DoProgress(eptRecords, CurRec, NObs);

        FOR CurField := 0 TO Fields.Count - 1 DO
        BEGIN
          TmpField := Field[Curfield];
          Case TypeList[CurField] of
            StataByteConst,
            StataIntConst,
            StataLongConst:
              begin
                Case TypeList[CurField] of
                  StataByteConst:
                    begin
                      I := ReadSInts(DataStream, 1);
                      J := $7F;
                    end;
                  StataIntConst:
                    begin
                      I := ReadSInts(DataStream, 2);
                      J := $7FFF;
                    end;
                  StataLongConst:
                    begin
                      I := ReadSInts(DataStream, 4);
                      J := $7FFFFFFF;
                    end;
                end;
                MisVal := (MissingBaseNum - (J - I));

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
                end else begin
                  {Date is converted from Stata's 1/1-1960 base to Lazarus's 30/12-1899 base}
                  case DateTypeList[CurField] of
                    tnone: TmpField.AsInteger[CurRec]  := I;                                  // Do nothing - conversion is not needed.
                    tc:    TmpField.AsDateTime[CurRec] := IncMilliSecond(StataBaseDateTime, I);       // I - measured in ms. since 1960.
                    td:    TmpField.AsDateTime[CurRec] := IncDay(StataBaseDateTime,   I);
                    tw:    TmpField.AsDateTime[CurRec] := IncWeek(StataBaseDateTime,  I);
                    tm:    TmpField.AsDateTime[CurRec] := IncMonth(StataBaseDateTime, I);
                    tq:    TmpField.AsDateTime[CurRec] := IncMonth(StataBaseDateTime, I * 3);
                    th:    TmpField.AsDateTime[CurRec] := IncMonth(StataBaseDateTime, I * 6);
                    ty:    TmpField.AsDateTime[CurRec] := IncYear(StataBaseDateTime,  I);
                  end;
                end;
              end;
            StataFloatConst,
            StataDoubleConst:
              Begin
                if TypeList[CurField] = StataFloatConst then
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
              end;
          else
            // This is a string field.
            // +1 Because we need a termination character in case all bytes in field
            // are used for text.
            SetLength(CharBuf, TmpField.Length + 1);
            FillChar(CharBuf[0], Length(CharBuf), 0);
            DataStream.Read(CharBuf[0], TmpField.Length);
            // Hack - for some reason an empty PChar strings are not correctly assing the empty string.
            // => garbage is be stored in StrBuf!
            if CharBuf[0] = #0 then
              StrBuf := ''
            else
              StrBuf := StringFromBuffer(PChar(@CharBuf[0]), Length(CharBuf));
            TmpField.AsString[CurRec] := StrBuf;
          end;
        END;  //for CurField
      END;  //for CurRec

      DoProgress(eptFinish, CurRec, NObs);
    EXCEPT
      RaiseError(Exception, 'Error reading data from Stata-file');
      Exit;
    END;  //try..except

    IF (ValueLabels.Count > 0) AND (DataStream.Position < DataStream.Size - 4) THEN
    BEGIN
      IF FileVersion = dta4 THEN
      BEGIN
        {Read value labels definitions - if present}
        WHILE DataStream.Position < DataStream.Size - 2 DO
        BEGIN
          J := ReadInts(DataStream, 2);                //get number of entries in label
          SetLength(CharBuf, 10);
          DataStream.Read(CharBuf[0], 10); //Load label definition
          VLSet := ValueLabels.GetValueLabelSetByName(string(CharBuf));
          SetLength(CharBuf, 8);
          FOR i := 0 TO J - 1 DO
          BEGIN
            TmpInt := ReadInts(DataStream, 2);
            DataStream.Read(CharBuf[0], 8);
            if VLSet.ValueLabelExists[TmpInt] then
            BEGIN
              RaiseError(Exception, 'Duplicate value label name found');
              Exit;
            END;
            VL := VLSet.NewValueLabel;
            TEpiIntValueLabel(VL).Value := TmpInt;
            VL.TheLabel.Text := EpiUnknownStrToUTF8(string(CharBuf));
          END;  //for n
        END;  //if not end of DataStream
        //if stataversion 4
      END ELSE BEGIN
        { Value labels for stata version 6+ }
        WHILE DataStream.Position < DataStream.Size - 4 DO
        BEGIN
          DataStream.Seek(4, soCurrent);                                   // Skip: Length of value_label_table (vlt)
          SetLength(CharBuf, FieldNameLength);

          DataStream.Read(CharBuf[0], FieldNameLength);                    // Read label-name
          StrBuf := StringFromBuffer(PChar(@CharBuf[0]), FieldNameLength);
          StrBuf := StringReplace(StrBuf, ' ', '_', [rfReplaceAll]);
          VLSet := ValueLabels.GetValueLabelSetByName(StrBuf);             // Get ValueLabelSet
          if not Assigned(VLSet) then
          begin
            VLSet := ValueLabels.NewValueLabelSet(ftInteger);
            VLset.Name := StrBuf;
          end;

          DataStream.Seek(3, soCurrent);                                   // byte padding


          n := ReadInts(DataStream, 4);                                    // Number of entries in label
          txtlen := ReadInts(DataStream, 4);                               // Length of txt[]

          SetLength(Off, n);
          SetLength(Val, n);
          SetLength(CharBuf, txtlen);

          DataStream.Read(Off[0], 4 * n);                                  // Read Off[]
          DataStream.Read(Val[0], 4 * n);                                  // Read Val[]
          DataStream.Read(CharBuf[0], txtlen);                             // Read Txt[]

          FOR I := 0 TO n - 1 DO
          BEGIN
            if (Val[I] >= $7FFFFFE5) then                                 // ignore valuelabels with
              Continue;                                                   // missing, the ones we wish to handle
                                                                          // are dealt with during data-loading.
            VL := VLSet.NewValueLabel;
            TEpiIntValueLabel(VL).Value := Val[I];
            VL.TheLabel.Text := StringFromBuffer(PChar(@CharBuf[Off[I]]), 32000);
          END;  //for i
        END;  //while
      END;  //if stataversion 6+
    END;
    // successfully loaded the file.

    DataFile.Fields.Sorted := true;
    Result := true;
  finally

  end;
end;

function TEpiImport.ImportStata(const aFilename: string;
  const Doc: TEpiDocument; var DataFile: TEpiDataFile; ImportData: boolean
  ): Boolean;
var
  FS: TStream;
begin
  if not FileExistsUTF8(aFilename) then exit(false);

  try
    FS := TFileStreamUTF8.Create(aFilename, fmOpenRead);
    Result := ImportStata(FS, Doc, DataFile, ImportData);
  finally
    FS.Free;
  end;
end;

function TEpiImport.ImportTxt(const DataStream: TStream;
  var DataFile: TEpiDataFile; ImportData: boolean): boolean;
var
  ImportLines: TStrings;
  FieldLines: TStrings;
  lStart: Integer;
  i: Integer;
  TmpStr: String;
  TmpField: TEpiField;
  Ok: Boolean;
  skipfirstline: boolean;
  FieldSeparator: Char;
  FieldCount: Integer;
  j: Integer;
  FieldList: TList;
  NewDecSep: Char;
  LocalDFSetting: TFormatSettings;
  RecIdx: Integer;
  FloatVal: EpiFloat;
  DateVal: EpiDate;
  TimeVal: EpiTime;
  IntVal: EpiInteger;
  BoolVal: EpiBool;
begin
  result := false;

  if not Assigned(DataFile) then
    DataFile := TEpiDataFile.Create(nil);

  ImportLines := nil;
  FieldLines := nil;
  FieldList := nil;

  // IMPORT.
  With DataFile do
  TRY
    ImportLines := TStringList.Create;

    // Importing from ClipBoard?
    if not Assigned(DataStream) then
    begin
      if Assigned(OnClipBoardRead) then
        OnClipBoardRead(ImportLines);
    end else
      ImportLines.LoadFromStream(DataStream);

    DoProgress(eptInit, 0, ImportLines.Count * 3);

    EpiUnknownStringsToUTF8(ImportLines);

    if ImportLines.Count = 0 then
      RaiseError(Exception, 'ClipBoard or File contains no data.');

    // Guess structure based on content.
    if not GuessTxtFile(DataFile, ImportLines, FieldList, skipfirstline, FieldSeparator) then
      Exit;

    FieldCount := 0;
    skipfirstline := true;

    if not ImportData then
      Exit(true);

    if SkipFirstLine then
      lStart := 1
    else
      lStart := 0;

    FieldLines := TStringList.Create;
    for i := lStart to ImportLines.Count -1 do
    begin
      DoProgress(eptRecords, i + (ImportLines.Count * 2), ImportLines.Count * 3);
      if Trim(ImportLines[i]) = '' then continue;
      SplitString(ImportLines[i], FieldLines, [FieldSeparator], ['"']);

      NewRecords(1);

      if FieldLines.Count > FieldList.Count then
      begin
        RaiseError(Exception,
          Format('Error in line %d. To many delimiters - found %d, should be %d', [i + 1, FieldLines.Count - 1, Fields.Count - 1]));
        Exit;
      end;

      RecIdx := Size - 1;

      for j := 0 to FieldLines.Count -1 do
      begin
        TmpStr   := FieldLines[j];
        TmpField := TEpiField(FieldList[j]);

        // If someone accidentally wrote the display missing char (usually ".") in the
        // file, convert it to '' (empty) and write missing to field.
        if (TmpStr = TEpiStringField.DefaultMissing) then
          TmpStr := '';
        if TmpStr = '' then
        begin
          TmpField.IsMissing[RecIdx] := true;
          Continue;
        end;

        case TmpField.FieldType of
          ftInteger:
            if IsInteger(TmpStr, IntVal) then TmpField.AsInteger[RecIdx] := IntVal;
          ftString, ftUpperString:
            TmpField.AsString[RecIdx] := TmpStr;
          ftBoolean:
            if IsBoolean(TmpStr, BoolVal) then TmpField.AsBoolean[RecIdx] := BoolVal;
          ftFloat:
            if IsFloat(TmpStr, FloatVal) then TmpField.AsFloat[RecIdx] := FloatVal;
          ftDMYDate, ftMDYDate, ftYMDDate:
            if IsDate(TmpStr, TmpField.FieldType, DateVal) then TmpField.AsDate[RecIdx] := DateVal;
          ftTime:
            if IsTime(TmpStr, TimeVal) then TmpField.AsTime[RecIdx] := TimeVal;
        end;

        if TmpField.IsMissing[RecIdx] and (TmpStr <> '') then
        begin
          RaiseError(Exception,
            Format('Error in line %d' + LineEnding +
                   'Data (''%s'') is not compliant with the fieldtype of field %s (%s).',
                   [RecIdx+1, TmpStr, TmpField.Name, EpiTypeNames[TmpField.FieldType]]
            )
          );
          Exit;
        end;

        IF UTF8Length(TmpStr) > TmpField.Length THEN
        BEGIN
          RaiseError(Exception,
            Format('Error in line %d' + LineEnding +
                   'Data (''%s'') too wide to fit in the field %s',
                   [RecIdx+1, TmpStr, TmpField.Name]
            )
          );
          Exit;
        END;
      end;
    end;

  finally
    DoProgress(eptFinish, ImportLines.Count, ImportLines.Count);
    if Assigned(ImportLines) then FreeAndNil(ImportLines);
    if Assigned(FieldLines) then FreeAndNil(FieldLines);
    if Assigned(FieldList) then FieldList.Free;
  end;
end;

function TEpiImport.ImportTxt(const aFileName: string;
  var DataFile: TEpiDataFile; ImportData: boolean): boolean;
var
  FS: TStream;
begin
  if (aFileName <> '') and
     (not FileExistsUTF8(aFilename))
  then
    exit(false);

  try
    FS := nil;

    if aFileName <> '' then
      FS := TFileStreamUTF8.Create(aFilename, fmOpenRead);

    Result := ImportTxt(FS, DataFile, ImportData);
  finally
    FS.Free;
  end;
end;

end.

