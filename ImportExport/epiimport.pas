unit epiimport; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument,epidatafiles, epidatafilestypes, epiadmin;

type

  TEpiClipBoardReadHook = procedure (ClipBoardLine: TStrings) of object;

  { TEpiImport }

  TEpiImport = class(TObject)
  private
    FOnClipBoardRead: TEpiClipBoardReadHook;
    FOnRequestPassword: TRequestPasswordEvent;
    procedure   RaiseError(Const Msg: string);
    procedure   ReadBuf(Const St: TStream; var Buf: Array of Byte; Count: Integer);
    function    ReadInts(Const St: TStream; Count: Integer): Integer;
    function    ReadSingle(Const St: TStream): Single;
    function    ReadDouble(Const St: TStream): Double;
    function    StringFromBuffer(AChar: PChar; MaxLength: Integer): string;
  public
    constructor Create;
    destructor  Destroy; override;
    function    ImportRec(Const aFileName: string; var DataFile: TEpiDataFile;
      ImportData: boolean = true): boolean;
    function    ImportStata(Const aFilename: string; var DataFile: TEpiDataFile;
      ImportData: boolean = true): Boolean;
    function    ImportQES(Const aFilename: string; var DataFile: TEpiDataFile;
      ActiveSection: TEpiSection; FieldPrefix: string): Boolean;
    property    OnClipBoardRead: TEpiClipBoardReadHook read FOnClipBoardRead write FOnClipBoardRead;
    // The RequestPasswordEvent does in this case not require a login name - since old .REC files do no support logins. It is discarded and not used.
    property    OnRequestPassword: TRequestPasswordEvent read FOnRequestPassword write FOnRequestPassword;
  end;

implementation

uses
  FileUtil, epistringutils, DCPbase64, DCPrijndael, DCPsha1, math, strutils,
  epiqeshandler;

var
  BigEndian: boolean = false;

{ TEpiImport }

procedure TEpiImport.RaiseError(const Msg: string);
begin
  raise Exception.Create(Msg);
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
  Result := EpiUnknownStrToUTF8(Result);
end;

constructor TEpiImport.Create;
begin

end;

destructor TEpiImport.Destroy;
begin
  inherited Destroy;
end;

function TEpiImport.ImportRec(const aFileName: string;
  var DataFile: TEpiDataFile; ImportData: boolean): boolean;
var
  TxtFile: TextFile;
  TxtLine: string;
  TempInt: Int64;
  HeaderLineCount: Integer;
  ValCode: Integer;
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
  DataStream: TMemoryStream;
  CharBuf: Array of char;
  IsCrypt: TBits;
  i: Integer;
  Decrypter: TDCP_rijndael;
  LocalDateSeparator: Char;

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
       ftMDYToday, ftDMYDate,   ftAutoInc,  ftString,      ftString,

  //   15          16           17          18             19
  //   ftQuestion, ftEuroToday, ftSoundex,  ftCrypt,       ftYMDDate,
       ftString,   ftDMYToday,  ftString,   ftString,      ftYMDDate,

  //   20
  //   ftYMDToday);
       ftYMDToday
  );


  function RequestPassword(Const EncryptedString: string): boolean;
  var
    S, FPassword: string;
  begin
    result := false;
    if Assigned(FOnRequestPassword) then
      FOnRequestPassword(Self, S, FPassword);
    try
      S := Base64DecodeStr(EncryptedString);
      FPassword := 'fakepass';
      Decrypter := TDCP_rijndael.Create(nil);
      DeCrypter.InitStr(FPassword, TDCP_sha1);
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

  if aFilename = '' then exit;
  if not FileExistsUTF8(aFilename) then
    RaiseError('File does not exists');

  if not Assigned(DataFile) then
    DataFile := TEpiDataFile.Create(nil);

  with DataFile do
  try
    AssignFile(TxtFile, UTF8ToSys(aFilename));
    {$PUSH}
    {$I-}
    System.Reset(TxtFile);
    {$POP}
    if IOResult() > 0 then
      RaiseError(Format('Data file %s could not be opened.',[AFilename]));
    // --- Read "First Line" header ---
    ReadLn(TxtFile, TxtLine);

    // - Password
    TempInt := Pos('~KQ:', AnsiUpperCase(TxtLine));
    if TempInt > 0 then
      if not RequestPassword(Copy(TxtLine, TempInt + 4, Pos(':KQ~', AnsiUpperCase(TxtLine)) - (TempInt + 4))) then
        RaiseError('Incorrect Password');

    // - FileLabel
    StrBuf := '';
    if Pos('FILELABEL: ', AnsiUpperCase(TxtLine)) > 0 then
      StrBuf :=  EpiUnknownStrToUTF8(Copy(TxtLine, Pos('FILELABEL: ', AnsiUpperCase(TxtLine)) + Length('FILELABEL: ') , Length(TxtLine)));
    Name.Text := StrBuf;

    // - Header lines:
    TempInt := Pos(' ', TxtLine)-1;
    if TempInt = -1 then TempInt := Length(TxtLine);
    Val(Copy(TxtLine, 1, TempInt), HeaderLineCount, ValCode);
    if ValCode > 0 then
      RaiseError(Format('Incorrect format of file: %S', [aFilename]));

    // Read field defining header lines.
    TotFieldLength := 0;

    IsCrypt := TBits.Create(HeaderLineCount);
    for CurrentLine := 1 to HeaderLineCount do
    begin
      ReadLn(TxtFile,
             TmpFieldChar, TmpName, TmpQuestX, TmpQuestY,
             TmpQuestColor, TmpFieldX, TmpFieldY, TmpFieldTypeInt, TmpLength,
             TmpFieldColor, dummy, TmpLabel);

      // Field types.
      FieldIsQuestion := false;
      if TmpFieldTypeInt >= 100 then
      begin
        // Type > 100 => float field
        TmpFieldType := ftFloat;
        if TmpFieldTypeInt = 100 then
          TmpFieldType := ftInteger;
      end else begin
        // Normal field type recognition.
        TmpFieldType := FieldTypeConversionTable[TmpFieldTypeInt];

        // This is an encrypted field... (new XML does not have single encrypted fields.)
        if (TmpFieldTypeInt = 18) then
          IsCrypt.Bits[CurrentLine-1] := true;

        // This is not a data field, but a question field.
        if (TmpFieldTypeInt = 15) or (TmpLength = 0) then
          FieldIsQuestion := true;
      end;

      // Unsupported field are automatically converted to string (ftString) fields.
      if (TmpFieldType in DateFieldTypes) and (TmpLength < 10) then
        TmpFieldType := ftString;

      // Trim text information.
      TmpName := Trim(TmpName);
      TmpLabel := Trim(TmpLabel);

      if FieldIsQuestion then
      begin
        EHeading := NewHeading;
        EHeading.BeginUpdate;
        with EHeading do
        begin
          Name.Text    := EpiUnknownStrToUTF8(TmpName);
          Caption.Text := EpiUnknownStrToUTF8(TmpLabel);
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
        Left           := TmpFieldX;
        Top            := TmpFieldY;
        Length         := TmpLength;
        Decimals       := 0;
        if TmpFieldTypeInt >= 100 then
          Decimals := TmpFieldTypeInt - 100;

        with Question do
        begin
          Left        := TmpQuestX;
          Top         := TmpQuestY;

          VariableLabel := EpiUnknownStrToUTF8(StringReplace(TmpLabel, '_', '-', [rfReplaceAll]));
          // In old style .REC files, first word in label is the name of the field. Remove it.
          if Pos(TmpName, VariableLabel) > 0 then
            VariableLabel := Trim(Copy(VariableLabel, System.Length(TmpName)+1, System.Length(VariableLabel)));
          Caption.Text := VariableLabel;
        end;
        // Ensure valid variable name.
        Name := TmpName;

        // Summerize field findings.
        TotFieldLength := TotFieldLength + Length;
      end;  // With EField
      EField.EndUpdate;
    end; // For CurrentLine

    // Position for reading and check for corruptness.
    TotFieldLength := TotFieldLength + (((TotFieldLength - 1) DIV 78) + 1) * 3; { MaxRecLineLength = 78 }
    TmpLength := TextPos(TxtFile);
    CloseFile(TxtFile);
    DataStream := nil;

    LocalDateSeparator := DateSeparator;
    DateSeparator := '/';  // This was standard in old .rec files.

    if ImportData then
    begin
      DataStream := TMemoryStream.Create;
      DataStream.LoadFromFile(UTF8ToSys(AFilename));
      DataStream.Position := TmpLength;

      SetLength(CharBuf, TotFieldLength);
      BeginUpdate;
      CurRec := 0;

      while true do
      begin
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
      EndUpdate;
    end;

    // TODO : Import .CHK files.
  finally
//    CloseFile(TxtFile);
    DateSeparator := LocalDateSeparator;
    if Assigned(DataStream) then DataStream.Free;
  end;
  result := true;
end;

function TEpiImport.ImportStata(const aFilename: string;
  var DataFile: TEpiDataFile; ImportData: boolean): Boolean;
var
  ByteBuf, ValBuf: Array of Byte;
  TypeList,
  CharBuf: Array of Char;
  NVar, NObs, CurRec, CurField,
  Sum, I, J, TmpInt: integer;
  TmpFlt: Double;
  TmpField: TEpiField;
  StrBuf: string;
  WideBuf: WideString;
//  TmpValSet: TValueLabelSet;

  // Version specific variables.
  FieldNameLength, StrBaseNum, FileLabelLength,
  FmtLength: Integer;
  ByteChar, IntChar, LongChar,
  FloatChar, DoubleChar: Char;
  MissingBaseNum: Cardinal;
  DecS: Char;
  TmpFieldType: TEpiFieldType;
  DataStream: TFileStream;
  FileVersion: Byte;
  VarDataLength: Integer;

  FieldList: TFPList;

  function ReadSingleMissing(var MisVal: string): Single;
  var
    Buf: Array[0..3] of Byte absolute Result;
  begin
    MisVal := '';
    Result := ReadSingle(DataStream);

    if (Buf[3]=$7F) then
    begin
      if (Buf[0] AND Buf[2]) = 0 then
      case Buf[1] of
        $08: MisVal := '9';
        $10: MisVal := '8';
        $18: MisVal := '7';
      else
        MisVal := '-';
      end;
    end;
  end;

  function ReadDoubleMissing(var MisVal: string): Double;
  var
    Buf: Array[0..7] of Byte absolute Result;
  begin
    MisVal := '';
    Result := ReadDouble(DataStream);

    if (Buf[6]=$E0) AND (Buf[7]=$7F) then
    begin
      if (Buf[0] AND Buf[1] AND Buf[2] AND Buf[3] AND Buf[4]) = 0 then
      case Buf[5] of
        $01: MisVal := '9';
        $02: MisVal := '8';
        $03: MisVal := '7';
      else
        MisVal := '-';
      end;
    end;
  end;

Const
  ByteConst   = #251;
  IntConst    = #252;
  LongConst   = #253;
  FloatConst  = #254;
  DoubleConst = #255;

begin
  result := false;

  if not Assigned(DataFile) then
    DataFile := TEpiDataFile.Create(nil);

  With DataFile do
  try
    DataStream := TFileStream.Create(aFileName, fmOpenRead);

    // ********************************
    //           STATA HEADER
    // ********************************
    SetLength(ByteBuf, 4);
    DataStream.Read(ByteBuf[0], 4);
    FileVersion := ByteBuf[0];

    // ds_format: NumBuff[0]
    //  Stata Version 4    = $69;
    //  Stata Version 6    = $6C;
    //  Stata Version 7    = $6E;
    //  Stata Version 8+9  = $71;
    //  Stata Version 10   = $72;
    if not (FileVersion in [$69, $6C, $6E, $71, $72]) then
    BEGIN
      RaiseError('Unknown Stata Version');
      Exit;
    END;

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

    // byteorder: NumBuff[1]
    IF not (ByteBuf[1] in [1, 2]) THEN
    BEGIN
      RaiseError('Unknown Stata Byte Order');
      Exit;
    END;
    if ByteBuf[1] = 1 then
      BigEndian := true;

    // filetype: NumBuff[2] (always 1)   -  NumBuff[3] not used.
    IF ByteBuf[2]<>1 THEN
    BEGIN
      RaiseError('Stata Error: NumBuf[2] MUST be "1"');
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
    Name.Text := StringFromBuffer(PChar(@CharBuf[0]), FileLabelLength);

    // time_stamp \0 terminated (not used in epidata)
    DataStream.Read(CharBuf[0], 18);

    // ********************************
    //         STATA DESCRIBTORS
    // ********************************
    // - typlist: the variable's types
    SetLength(TypeList, NVar);
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
      if TypeList[i] = ByteChar then   TypeList[i] := ByteConst;
      if TypeList[i] = IntChar then    TypeList[i] := IntConst;
      if TypeList[i] = LongChar then   TypeList[i] := LongConst;
      if TypeList[i] = FloatChar then  TypeList[i] := FloatConst;
      if TypeList[i] = DoubleChar then TypeList[i] := DoubleConst;


      IF (TypeList[i] in [FloatConst, DoubleConst]) THEN
        TmpFieldType := ftFloat
      ELSE IF (TypeList[i] in [ByteConst, IntConst, LongConst]) THEN
        TmpFieldType := ftInteger
      ELSE
      BEGIN
        if (Ord(TypeList[i]) - StrBaseNum) < 0 then
        BEGIN
          RaiseError('Unknown variable type found in Stata-file');
          Exit;
        END;
        TmpFieldType := ftString;
      END;

      {Handle formats...}
      StrBuf := Trim(AnsiUpperCase(StringFromBuffer(PChar(@ByteBuf[i * FmtLength]), FmtLength)));
      if not (StrBuf[1] = '%') then
      BEGIN
        RaiseError(Format('Unknown format specified for variable no: %d', [i+1]));
        Exit;
      END;

      j := 0;
      if StrBuf[2] = '-' then
        j := 1;

      // Strings have already been defined in <typlist>.
      if StrBuf[Length(StrBuf)] <> 'S' then
      begin
        Case Char(StrBuf[2+j]) of
          // Date (and time formats) (Time not supported by EpiData yet).
          'T',
          'D':
            Begin
              TmpFieldType := ftDMYDate;
              // Detailed format.
              if Length(StrBuf) > (2+j) then
              begin
                if (Pos('D', StrBuf) > Pos('M', StrBuf)) or
                   (Pos('D', StrBuf) > Pos('N', StrBuf)) or
                   (Pos('D', StrBuf) > Pos('L', StrBuf)) then
                  TmpFieldType := ftMDYDate;
              end;
            End;
          // Number
          '0'..'9': ;
        else
          RaiseError(Format('Unknown format specified for variable no: %d', [i+1]));
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
        ByteConst: begin
                     TmpField.Length := 3;
                     Inc(VarDataLength, 1);
                   end;
        IntConst:  begin
                     TmpField.Length := 5;
                     Inc(VarDataLength, 2);
                   end;
        LongConst: begin
                     TmpField.Length := 10;
                     Inc(VarDataLength, 4);
                   end;
        FloatConst,
        DoubleConst:
          Begin
            TmpField.Length := 18;
            TmpField.Decimals := 4;
            if TypeList[i] = FloatConst then
              Inc(VarDataLength, 4)
            else
              Inc(VarDataLength, 8);
          End;
      else
        TmpField.Length := Ord(TypeList[i]) - StrBaseNum;
        Inc(VarDataLength, TmpField.Length);
      end;

      // Dates:
      if TmpField.FieldType in DateFieldTypes then
        TmpField.Length := 10;

      // - varlist
      StrBuf := StringFromBuffer(PChar(@CharBuf[i * FieldNameLength]), FieldNameLength);
      TmpField.Name := Trim(StrBuf);
    END;

    // - lbllist: names af value label
    SetLength(CharBuf, FieldNameLength * NVar);
    DataStream.Read(CharBuf[0], FieldNameLength * NVar);
    // TODO : Stata Value Labels!
{    FOR i:=0 TO nVar-1 DO
    BEGIN
      TmpField := Fields[i];
      StrBuf := Trim(StringFromBuffer(PChar(@CharBuf[i * FieldNameLength]), FieldNameLength));
      IF StrBuf <> '' THEN
      BEGIN
        TmpField.ValueLabelSet := TValueLabelSet.Create(nil, ftInteger);
        TmpField.ValueLabelSet.Name := StrBuf;
        TmpField.ValueLabelSet.LabelScope := vlsInternal;
        ValueLabels.AddValueLabelSet(TmpField.ValueLabelSet);
      END;
    END;  //for i    }

    // ********************************
    //      STATA VARIABLE LABELS
    // ********************************
    SetLength(CharBuf, FileLabelLength * NVar);
    DataStream.Read(CharBuf[0], FileLabelLength * NVar);
    J := 0;
    FOR i := 0 TO nVar-1 DO
    BEGIN
      TmpField := TEpiField(FieldList[i]);
      StrBuf := Trim(StringFromBuffer(PChar(@CharBuf[i * FileLabelLength]), FileLabelLength));
      IF Length(StrBuf)>50 THEN
        StrBuf := Copy(StrBuf, 1, 48) + '..';
      J := Max(J, Length(StrBuf));
      StrBuf := StringReplace(StrBuf, '#', ' ', [rfReplaceAll]);
      StrBuf := StringReplace(StrBuf, '>', ' ', [rfReplaceAll]);
      StrBuf := StringReplace(StrBuf, '<', ' ', [rfReplaceAll]);
      StrBuf := StringReplace(StrBuf, '_', ' ', [rfReplaceAll]);
      TmpField.Question.Caption.Text := StrBuf;

      // No more field information exists. The update may complete.
      TmpField.EndUpdate;
    END;

    // ********************************
    //      STATA EXPANSION FIELDS
    // ********************************
    // - skip expansion fields
    SetLength(ByteBuf, 1);
    REPEAT
      DataStream.Read(ByteBuf[0], 1); //data type code
      IF FileVersion >= $6E THEN
        I := ReadInts(DataStream, 4)
      ELSE
        I := ReadInts(DataStream, 2);
      IF (ByteBuf[0] > 0) OR (I > 0) THEN
        DataStream.Seek(I, soCurrent);
    UNTIL (DataStream.Position >= DataStream.Size-1) OR ((I=0) AND (ByteBuf[0]=0));


    if not ImportData then
    begin
      DataStream.Seek(NVar * VarDataLength, soCurrent);
    end else try
      // ********************************
      //          STATA DATA
      // ********************************
      FOR CurRec := 0 TO nObs -1 DO
      BEGIN
        FOR CurField := 0 TO Fields.Count - 1 DO
        BEGIN
          TmpField := Field[Curfield];
          Case TypeList[CurField] of
            ByteConst,
            IntConst,
            LongConst:
              begin
                Case TypeList[CurField] of
                  ByteConst:
                    begin
                      I := ReadInts(DataStream, 1);
                      J := $7F;
                    end;
                  IntConst:
                    begin
                      I := ReadInts(DataStream, 2);
                      J := $7FFF;
                    end;
                  LongConst:
                    begin
                      I := ReadInts(DataStream, 4);
                      J := $7FFFFFFF;
                    end;
                end;
                // TODO : ValueLabels and Missing in STATA
{                // This is a missing value type.
                if I > (J - MissingBaseNum) then
                Begin
                  // This corresponds to Stata's ".a", ".b", and ".c"
                  if (I >= (J - 25)) and (I <= (J - 23)) then
                  begin
                    // Write all 9's, 8's or 7's as missing value.
                    TmpField.AsString[CurRec] := DupeString(IntToStr((J - I) - 16), TmpField.Length);
                    if not Assigned(TmpField.ValueLabelSet) then
                    begin
                      TmpField.ValueLabelSet := TValueLabelSet.Create(nil, ftInteger);
                      TmpField.ValueLabelSet.Name := TmpField.FieldName + '_missinglbl';
                      ValueLabels.AddValueLabelSet(TmpField.ValueLabelSet);
                    end;
//                    TmpField.ValueLabelSet.AddValueLabelPair(TmpField.AsInteger[CurRec], '', True);
//                    TmpField.CheckField.MissingValues[] := TmpField.AsString[CurRec];
                  end else
                    TmpField.IsMissing[CurRec] := true;
                end else                               }
                  TmpField.AsInteger[CurRec] := I;
              end;
            FloatConst,
            DoubleConst:
              Begin
                if TypeList[CurField] = FloatConst then
                  TmpFlt := ReadSingleMissing(StrBuf)
                else
                  TmpFlt := ReadDoubleMissing(StrBuf);
                if StrBuf <> '' then
                begin
                  if StrBuf = '-' then
                    StrBuf := ''
                  else begin
                    TmpField.AsString[CurRec] :=
                      DupeString(StrBuf, TmpField.Length - (TmpField.Decimals + 1)) + DecS + DupeString(StrBuf, TmpField.Decimals);

{                    if not Assigned(TmpField.ValueLabelSet) then
                    begin
                      TmpField.ValueLabelSet := TValueLabelSet.Create(nil, ftInteger);
                      TmpField.ValueLabelSet.Name := TmpField.FieldName + '_missinglbl';
                      ValueLabels.AddValueLabelSet(TmpField.ValueLabelSet);
                    end;
                    // TODO : ValueLabels in STATA
//                    TmpField.ValueLabelSet.AddValueLabelPair(TmpField.AsFloat[CurRec], '', True);    }
                  end;
                end else begin
                  {Date is converted from Stata's 1/1-1960 base to Lazarus's 30/12-1899 base}
                  if TmpField.FieldType in DateFieldTypes then
                    TmpFlt := TmpFlt + 21916;
                  TmpField.AsFloat[CurRec] := TmpFlt;
                end;
              End;
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
    EXCEPT
      RaiseError('Error reading data from Stata-file');
      Exit;
    END;  //try..except

    // TODO : ValueLabels in STATA
{    IF (ValueLabels.Count > 0) AND (DataStream.Position < DataStream.Size - 4) THEN
    BEGIN
      IF FileVersion = $69 THEN
      BEGIN
        {Read value labels definitions - if present}
        WHILE DataStream.Position < DataStream.Size - 2 DO
        BEGIN
          J := ReadInts(2);  //get number of entries in label
          SetLength(CharBuf, 10);
          DataStream.Read(CharBuf[0], 10); //Load label definition
          TmpValSet := ValueLabels.ValueLabelSetByName(string(CharBuf));
          SetLength(CharBuf, 8);
          FOR i := 0 TO J - 1 DO
          BEGIN
            TmpInt := ReadInts(2);
            DataStream.Read(CharBuf[0], 8);
            if Trim(TmpValSet.ValueLabel[IntToStr(TmpInt)]) <> '' then
            BEGIN
              ErrorText := Lang(23936, 'Duplicate value label name found');
              ErrorCode := EPI_FAILED;
              EpiLogger.AddError(Classname, 'ImportStata', ErrorText, 23936);
              Exit;
            END;
            TmpValSet.AddValueLabelPair(IntToStr(TmpInt), EpiUnknownStrToUTF8(string(CharBuf)));
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
          TmpValSet := ValueLabels.ValueLabelSetByName(StringFromBuffer(PChar(@CharBuf[0]), FieldNameLength));  // Get ValueLabelSet
          DataStream.Seek(3, soCurrent);                                   // byte padding

          J := ReadInts(4);                                               // Number of entries in label
          SetLength(ByteBuf, 4 * J);
          SetLength(ValBuf, 4 * J);
          NObs := ReadInts(4);                                            // Length of txt[]
          SetLength(CharBuf, NObs);
          DataStream.Read(ByteBuf[0], 4 * J);                              // Read Off[]
          DataStream.Read(ValBuf[0], 4 * J);                               // Read Val[]
          DataStream.Read(CharBuf[0], NObs);                               // Read Txt[]

          FOR I := 0 TO J - 1 DO
          BEGIN
            CurRec := Integer(ByteBuf[I * 4]);                            // CurRec holds offset value into Txt[]
            TmpInt := Integer(ValBuf[I * 4]);                             // TmpInt holds actual value for value label

            if (TmpInt >= $7FFFFFE5) then                                 // ignore valuelabels
              Continue;

            TmpValSet.AddValueLabelPair(TmpInt, StringFromBuffer(PChar(@CharBuf[CurRec]), 32000));
          END;  //for i
        END;  //while
      END;  //if stataversion 6, 7 or 8
    END;}
    // successfully loaded the file.
    Result := true;
  finally
    if Assigned(DataStream) then FreeAndNil(DataStream);
  end;
end;

function TEpiImport.ImportQES(const aFilename: string;
  var DataFile: TEpiDataFile; ActiveSection: TEpiSection; FieldPrefix: string
  ): Boolean;
var
  QH: TQesHandler;
begin
  QH := TQesHandler.Create;
  QH.FieldPrefix := FieldPrefix;
  Result := QH.QesToDatafile(aFilename, DataFile, ActiveSection);
  QH.Free;
end;

end.

