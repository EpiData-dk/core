unit UImportExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UEpiDataFile, UDataFileTypes, UEpiLog;

type

  // Export record:
  // - FileVersion:     Used in Stata and DBase file to name specific version of file.
  // - Start/EndRecord: Interger to name starting and ending record to export.
  //                    To include all use 1 as starting record and -1 as ending record.
  //                    Out of bounds is checked. Negative interval is ignored (=> no data exported)
  TEpiExportSettings = record
    FileVersion: Integer;
    StartRecord: Integer;
    EndRecord:   Integer;
  end;
  PEpiExportSettings = ^TEpiExportSettings;

const

  //  Stata Version 4    = $69;
  //  Stata Version 6    = $6C;
  //  Stata Version 7    = $6E;
  //  Stata Version 8+9  = $71;
  //  Stata Version 10   = $72;
  ExportStata4_5: TEpiExportSettings =
                    (FileVersion:  $69;
                     StartRecord:  1;
                     EndRecord:    -1);
  ExportStata6:   TEpiExportSettings =
                    (FileVersion:  $6C;
                     StartRecord:  1;
                     EndRecord:    -1);
  ExportStata7:   TEpiExportSettings =
                    (FileVersion:  $6E;
                     StartRecord:  1;
                     EndRecord:    -1);
  ExportStata8_9: TEpiExportSettings =
                    (FileVersion:  $71;
                     StartRecord:  1;
                     EndRecord:    -1);
  ExportStata10:  TEpiExportSettings =
                    (FileVersion:  $72;
                     StartRecord:  1;
                     EndRecord:    -1);

type
  // Import records:
  //
  TEpiImportSettings = record
    ForceFileVersion: Integer;
    StartRecord:      Integer;
    EndRecord:        Integer;
  end;
  PEpiImportSettings = ^TEpiImportSettings;

  TEpiTxtImportSettings = record
    ImportSettings: PEpiImportSettings;
    QESFileName:    string;
    UseQESFile:     boolean;
    FieldSeparator: Char;
    DateSeparator:  Char;
    QuoteChar:      Char;
    FixedFormat:    Boolean;
  end;
  PEpiTxtImportSettings = ^TEpiTxtImportSettings;

const
  ImportAll: TEpiImportSettings =
   (ForceFileVersion: -1;
    StartRecord:      1;
    EndRecord:        -1);

  ImportTxtGuess: TEpiTxtImportSettings = (
    ImportSettings: @ImportAll;
    QESFileName:    '';
    UseQESFile:     false;
    FieldSeparator: #0;
    DateSeparator:  #0;
    QuoteChar:      '"';
    FixedFormat:    false;
    );

type

  { TEpiImportExport }

  TEpiImportExport = class(TObject)
  private
    FOnProgress:  TProgressEvent;
    FOnTranslate: TTranslateEvent;
    FByteOrder:   TByteOrder;
    FStringMode:  TStringMode;
    DataStream:   TStream;
    function      Lang(LangCode: Integer; Const LangText: string): string;
    Function      UpdateProgress(Percent: Integer; Msg: string): TProgressResult;
    procedure     ReadBuf(var Buf: Array of Byte; Count: Integer);
    function      ReadInts(Count: Integer): Integer;
    function      ReadSingle(): Single;
    function      ReadDouble(): Double;
    function      StringFromBuffer(AChar: PChar; MaxLength: Integer): string;
    procedure     WriteBuf(Buf: Array of Byte; Count: Integer);
    procedure     WriteInts(Const Val, Count: Integer);
    procedure     WriteSingle(Val: Single);
    procedure     WriteDouble(Val: Double);
    procedure     WriteString(Const Str: string; Const Count: Integer; Terminate: Boolean = True);
    function      GuessTxtFile(DataFile: TEpiDataFile; Lines: TStrings;
                    TxtImpSetting: PEpiTxtImportSettings): boolean;
  public
    constructor   Create;
    destructor    Destroy; override;
    function      ImportStata(Const aFilename: string; var DataFile: TEpiDataFile): Boolean;
    function      ImportDBase(Const aFilename: string; var DataFile: TEpiDataFile): Boolean;
    function      ImportTXT(Const aFilename: string; var DataFile: TEpiDataFile;
                            TxtImpSetting: PEpiTxtImportSettings): Boolean;
    function      ImportXLS(Const aFilename: string; var DataFile: TEpiDataFile): Boolean;
    function      ExportStata(Const aFilename: string; Const DataFile: TEpiDataFile;
                              ExpSetting: PEpiExportSettings): Boolean;
    function      ExportDBase(Const aFilename: string; Const DataFile: TEpiDataFile): Boolean;
    function      ExportTXT(Const aFilename: string; Const DataFile: TEpiDataFile): Boolean;
    function      ExportXLS(Const aFilename: string; Const DataFile: TEpiDataFile): Boolean;
    property      OnProgress:  TProgressEvent read FOnProgress write FOnProgress;
    property      OnTranslate: TTranslateEvent read FOnTranslate write FOnTranslate;
    property      ByteOrder: TByteOrder read FByteOrder;
    property      StringMode: TStringMode read FStringMode;
  end;

implementation

uses
  UValueLabels, UEpiDataGlobals, UEpiUtils, Math, StrUtils, UDateUtils,
  FileUtil, UQesHandler, Clipbrd, UStringUtils;

  { TEpiImportExport }

function TEpiImportExport.Lang(LangCode: Integer; const LangText: string): string;
begin
  Result := LangText;
  IF Assigned(FOnTranslate) THEN
    Result := FOnTranslate(langcode, Result)
end;

function TEpiImportExport.UpdateProgress(Percent: Integer; Msg: string): TProgressResult;
begin
  Result := prNormal;
  if Assigned(FOnProgress) then
  Begin
    result := FOnProgress(Self, Percent, Msg);
  end;
end;

procedure TEpiImportExport.ReadBuf(var Buf: array of Byte;
  Count: Integer);
var
  TmpByte: Byte;
  i: integer;
begin
  DataStream.Read(Buf[0], Count);
  if ByteOrder = boBigEndian then
    for i := 0 to (Count div 2)-1 do
    begin
      TmpByte := Buf[i];
      Buf[i]  := Buf[(Count - 1) - i];
      Buf[(Count - 1) - i] := TmpByte;
    end;
end;

function TEpiImportExport.ReadInts(Count: Integer): Integer;
var
  Buf: Array[0..3] of Byte;
begin
  FillChar(Buf, 4, 0);
  ReadBuf(Buf, Count);
  Result := Integer(Buf);
end;


function TEpiImportExport.ReadSingle(): Single;
var
  Buf: Array[0..3] of Byte;
begin
  ReadBuf(Buf, 4);
  Result := Single(Buf);
end;


function TEpiImportExport.ReadDouble(): Double;
var
  Buf: Array[0..7] of Byte;
begin
  ReadBuf(Buf, 8);
  Result := Double(Buf);
end;

function TEpiImportExport.StringFromBuffer(AChar: PChar; MaxLength: Integer): string;
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
end;

procedure TEpiImportExport.WriteBuf(Buf: Array of Byte; Count: Integer);
var
  TmpByte: Byte;
  i: integer;
begin
  if ByteOrder = boBigEndian then
    for i := 0 to (Count div 2)-1 do
    begin
      TmpByte := Buf[i];
      Buf[i]  := Buf[(Count - 1) - i];
      Buf[(Count - 1) - i] := TmpByte;
    end;
  DataStream.Write(Buf[0], Count);
end;

procedure TEpiImportExport.WriteInts(Const Val, Count: Integer);
var
  TmpBuf: Array[0..3] of Byte;
  I: Integer;
begin
  FillChar(TmpBuf, 4, 0);
  for i := 0 to Count - 1 do
    TmpBuf[i] := Byte(Val shr (i * 8));
  WriteBuf(TmpBuf, Count);
end;


procedure TEpiImportExport.WriteSingle(Val: Single);
var
  FltByte: Array[0..4] of Byte absolute Val;
begin
  WriteBuf(FltByte, 4);
end;

procedure TEpiImportExport.WriteDouble(Val: Double);
var
  FltByte: Array[0..7] of Byte absolute Val;
begin
  WriteBuf(FltByte, 8);
end;

procedure TEpiImportExport.WriteString(Const Str: string; Const Count: Integer; Terminate: Boolean = True);
var
  StrBuf: PChar;
  z: integer;
begin
  z := 0;
  if Terminate then z := 0 else z := 1;
  StrBuf := StrAlloc(Count + z);
  StrPLCopy(PChar(@StrBuf[0]), Str, Count - 1 + z);
  DataStream.Write(StrBuf[0], Count);
  StrDispose(StrBuf);
end;

function TEpiImportExport.GuessTxtFile(DataFile: TEpiDataFile; Lines: TStrings;
  TxtImpSetting: PEpiTxtImportSettings): boolean;
var
  tabcount, semicoloncount, commacount,
  spacecount: Integer;
  istab, issemi, iscomma, isspace: Boolean;
  i, LineCount, FieldCount, w: Integer;
  TmpStr: string;
  TmpField: TEpiField;
  FtList: array of TFieldType;
  FieldStrings: TStrings;
  j: Integer;
  TmpFT: TFieldType;
begin
  // TODO -o Torsten: GuessTxtFile
  result := false;

  FieldStrings := nil;
  TmpField := nil;
  try
    tabcount:=0;   semicoloncount:=0; commacount:=0;   spacecount:=0;
    LineCount := Math.Min(10, Lines.Count);
    for i := 0 to LineCount - 1 do
    begin
      TmpStr := TrimRight(Lines[i]);
      inc(tabcount, StrCountChars(TmpStr, [#9]));
      inc(semicoloncount, StrCountChars(TmpStr, [';']));
      inc(commacount, StrCountChars(TmpStr, [',']));
      inc(spacecount, StrCountChars(TmpStr, [' ']));
    end;
    tabcount := Round(tabcount / LineCount);
    semicoloncount := Round(semicoloncount / LineCount);
    commacount := Round(commacount / LineCount);
    spacecount := Round(spacecount / LineCount);

    istab:=true;   issemi:=true; iscomma:=true;   isspace:=true;
    { Look for field separator char }
    for i :=0 to LineCount - 1 do
    begin
      TmpStr := TrimRight(Lines[i]);
      if trim(TmpStr)='' then continue;

      if (istab) then
      begin
        w := StrCountChars(TmpStr, [#9]);
        if w = 0 then istab := false;
        if w > tabcount then istab := false;
      end;

      if (issemi) then
      begin
        w := StrCountChars(TmpStr, [';']);
        if w = 0 then issemi := false;
        if w > semicoloncount then issemi := false;
      end;

      if (iscomma) then
      begin
        w := StrCountChars(TmpStr, [',']);
        if w = 0 then iscomma := false;
        if w > commacount then iscomma := false;
      end;

      if (isspace) then
      begin
        w := StrCountChars(TmpStr, [' ']);
        if w = 0 then isspace := false;
        if w > spacecount then isspace := false;
      end;
    end; //for

    { Priority: tab, semicolon, comma, space    }
    if istab        then begin TxtImpSetting^.FieldSeparator := #9;  FieldCount := tabcount + 1;       end
    else if issemi  then begin TxtImpSetting^.FieldSeparator := ';'; FieldCount := semicoloncount + 1; end
    else if iscomma then begin TxtImpSetting^.FieldSeparator := ','; FieldCount := commacount + 1;     end
    else if isspace then begin TxtImpSetting^.FieldSeparator := ' '; FieldCount := spacecount + 1;     end
    else begin
      DataFile.ErrorCode := EPI_IMPORT_FAILED;
      DataFile.ErrorText := Lang(0, 'Illegal formal of textfile. Fieldseparator not found.');
      Exit;
    end;

    // Guess field type.
    // Skip first line since it may contain headings/field names.
    SetLength(FtList, FieldCount);
    for i := 1 to LineCount - 1 do
    begin
      TmpStr := Lines[i];
      if Trim(TmpStr) = '' then continue;

      SplitString(TmpStr, FieldStrings, [TxtImpSetting^.FieldSeparator], [TxtImpSetting^.QuoteChar]);

      for j := 0 to FieldStrings.Count -1 do
      begin
        TmpStr := FieldStrings[j];
        if TmpStr = '.' then continue;
        FtList[j] := FindFieldType(TmpStr, FtList[j]);
      end;
    end;

    // TODO -o Torsten : Guess field names and variable labels.

    // Create Fields.
    for i := 1 to FieldCount do
    begin
      TmpField := TEpiField.Create;
      with TmpField do
      begin
        FieldType := FtList[i-1];
        FieldName := 'V' + IntToStr(i);
        FieldNo   := i;
        case FieldType of
          ftInteger:
            FieldLength := 4;
          ftFloat:
            begin
              FieldLength := 14;
              NumDecimals := 6;
            end;
          ftDate, ftEuroDate, ftYMDDate:
            FieldLength := 10;
          ftAlfa:
            FieldLength := 42;
        end;
      end;
      DataFile.AddField(TmpField);
    end;

    result := true;
  finally
    if Assigned(FieldStrings) then FreeAndNil(FieldStrings);
  end;
end;

constructor TEpiImportExport.Create;
begin

end;

destructor TEpiImportExport.Destroy;
begin
  if Assigned(DataStream) then FreeAndNil(DataStream);
end;

function TEpiImportExport.ImportStata(const aFilename: string;
  var DataFile: TEpiDataFile): Boolean;
var
  ByteBuf, ValBuf: Array of Byte;
  TypeList,
  CharBuf: Array of Char;
  NVar, NObs, CurRec, CurField,
  I, J, TmpInt: integer;
  TmpFlt: Double;
  TmpField: TEpiField;
  StrBuf: string;
  WideBuf: WideString;
  TmpValSet: TValueLabelSet;

  // Version specific variables.
  FieldNameLength, StrBaseNum, FileLabelLength,
  FmtLength: Integer;
  ByteChar, IntChar, LongChar,
  FloatChar, DoubleChar: Char;
  MissingBaseNum: Cardinal;

  function ReadSingleMissing(var MisVal: string): Single;
  var
    Buf: Array[0..3] of Byte absolute Result;
  begin
    MisVal := '';
    Result := ReadSingle();

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
    Result := ReadDouble();

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
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'ImportStata', 2, 'Filename = ' + aFilename);
  result := false;

  if Assigned(DataFile) then
    DataFile.Reset()
  else
    DataFile := TEpiDataFile.Create([eoIgnoreChecks, eoIgnoreIndex, eoIgnoreRelates, eoInMemory]);

  With DataFile do
  TRY
    FieldNaming := fnAuto;
    OrgDataType := dftStata;
    FileName := aFilename;
    UpdateProgress(0, Lang(0, 'Reading header information'));

    DataStream := TFileStream.Create(aFileName, fmOpenRead);
    FStringMode := smAnsi;

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
      ErrorText := Lang(23978, 'Unknown version of Stata-file');
      ErrorCode := EPI_NOT_VALID_STATA_FILE;
      EpiLogger.AddError(Classname, 'ImportStata', ErrorText, 23978);
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
      ErrorText := Lang(23980, 'Incorrect format of stata-file');
      ErrorCode := EPI_NOT_VALID_STATA_FILE;
      EpiLogger.AddError(Classname, 'ImportStata', ErrorText, 23980);
      Exit;
    END;
    FByteOrder := boLittleEndian;
    if ByteBuf[1] = 1 then
      FByteOrder := boBigEndian;

    // filetype: NumBuff[2] (always 1)   -  nummbuf[3] not used.
    IF ByteBuf[2]<>1 THEN
    BEGIN
      ErrorText := Lang(23980, 'Incorrect format of stata-file');
      ErrorCode := EPI_NOT_VALID_STATA_FILE;
      EpiLogger.AddError(Classname, 'ImportStata', ErrorText, 23980);
      Exit;
    END;

    // nvars (number of vars)
    nVar := ReadInts(2);
    IF nVar>800 THEN
    BEGIN
      ErrorText := Format(Lang(23982, 'The stata-file contains %d variables.~A maximum of 800 variables can be imported.'), [nVar]);
      ErrorCode := EPI_TOO_MANY_VARIABLES;
      EpiLogger.AddError(Classname, 'ImportStata', ErrorText, 23982);
      Exit;
    END;

    // nobs (number of obs)
    NObs := ReadInts(4);

    // data_label \0 terminated.
    SetLength(CharBuf, FileLabelLength);
    DataStream.Read(CharBuf[0], FileLabelLength);
    FileLabel := string(CharBuf);

    // time_stamp \0 terminated (not used in epidata)
    DataStream.Read(CharBuf[0], 18);

    // ********************************
    //         STATA DESCRIBTORS
    // ********************************
    // - typlist: the variable's types
    // - varlist: variable names
    SetLength(TypeList, NVar);
    DataStream.Read(TypeList[0], nVar);

    SetLength(CharBuf, FieldNameLength * NVar);
    DataStream.Read(CharBuf[0], FieldNameLength * NVar);

    FOR i := 0 TO NVar - 1 DO
    BEGIN
      TmpField := TEpiField.Create();
      WITH TmpField DO
      BEGIN
        Question      := '';
        FieldLength   := 0;
        NumDecimals   := 0;
        VariableLabel := '';
        FieldX        := 0;
        FieldY        := i;
        QuestX        := 1;
        QuestY        := i;
        FieldName     := '';
      END;

      // Update typelist to consts...
      if TypeList[i] = ByteChar then   TypeList[i] := ByteConst;
      if TypeList[i] = IntChar then    TypeList[i] := IntConst;
      if TypeList[i] = LongChar then   TypeList[i] := LongConst;
      if TypeList[i] = FloatChar then  TypeList[i] := FloatConst;
      if TypeList[i] = DoubleChar then TypeList[i] := DoubleConst;

      // - typelist
      TmpField.NumDecimals := 0;
      case TypeList[i] of
        ByteConst: TmpField.FieldLength := 2;
        IntConst:  TmpField.FieldLength := 4;
        LongConst: TmpField.FieldLength := 10;
        FloatConst,
        DoubleConst:
          Begin
            TmpField.FieldLength := 18;
            TmpField.NumDecimals := 4;
          End;
      end;

      IF (TypeList[i] in [IntConst, LongConst, FloatConst, DoubleConst]) THEN
        TmpField.FieldType := ftFloat
      ELSE IF Char(TypeList[i]) = ByteConst THEN
        TmpField.FieldType := ftInteger
      ELSE
      BEGIN
        if (Ord(TypeList[i]) - StrBaseNum) < 0 then
        BEGIN
          ErrorText := Lang(23984, 'Unknown variable type found in Stata-file');
          ErrorCode := EPI_NOT_VALID_STATA_FILE;
          EpiLogger.AddError(Classname, 'ImportStata', ErrorText, 23984);
          Exit;
        END;
        TmpField.FieldType := ftAlfa;
        TmpField.FieldLength := (Ord(TypeList[i]) - StrBaseNum) * 2;
      END;
      // - varlist
      StrBuf := Trim(StringFromBuffer(PChar(@CharBuf[i * FieldNameLength]), FieldNameLength));
      j := 1;
      if (not CheckVariableName(StrBuf, AlfaNumChars)) or (FieldExists(StrBuf)) then
      repeat
        StrBuf := 'V '+ IntToStr(J);
        INC(J);
      until not FieldExists(StrBuf);
      TmpField.FieldName := Trim(StrBuf);

      AddField(TmpField);
    END;

    // - Skip reading sorting list
    DataStream.Seek(2 * (NVar + 1), soCurrent);

    // - Fmtlist: list of formats of the variables
    //            only relevant for dates.
    SetLength(CharBuf, FmtLength * NVar);
    DataStream.Read(CharBuf[0], FmtLength * NVar);
    FOR i := 0 TO NVar - 1 DO
    BEGIN
      TmpField := Fields[i];
      StrBuf := Trim(StringFromBuffer(PChar(@CharBuf[i * FmtLength]), FmtLength));

      {Handle formats...}
      StrBuf := trim(AnsiUpperCase(StrBuf));
      if not (StrBuf[1] = '%') then
      BEGIN
        ErrorText := Format(Lang(23986, 'Unknown format specified for variable %s'), [TmpField.FieldName]);
        ErrorCode := EPI_NOT_VALID_STATA_FILE;
        EpiLogger.AddError(Classname, 'ImportStata', ErrorText, 23986);
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
              TmpField.FieldType   := ftEuroDate;
              TmpField.FieldLength := 10;
              // Detailed format.
              if Length(StrBuf) > (2+j) then
              begin
                if (Pos('D', StrBuf) > Pos('M', StrBuf)) or
                   (Pos('D', StrBuf) > Pos('N', StrBuf)) or
                   (Pos('D', StrBuf) > Pos('L', StrBuf)) then
                  TmpField.FieldType := ftDate;
              end;
            End;
          // Number
          '0'..'9':
            Continue;
        else
          ErrorText := Format(Lang(23986, 'Unknown format specified for variable %s'), [TmpField.FieldName]);
          ErrorCode := EPI_NOT_VALID_STATA_FILE;
          EpiLogger.AddError(Classname, 'ImportStata', ErrorText, 23986);
          Exit;
        end;
      end;
    end;


    // - lbllist: names af value label
    SetLength(CharBuf, FieldNameLength * NVar);
    DataStream.Read(CharBuf[0], FieldNameLength * NVar);
    FOR i:=0 TO nVar-1 DO
    BEGIN
      TmpField := Fields[i];
      StrBuf := Trim(StringFromBuffer(PChar(@CharBuf[i * FieldNameLength]), FieldNameLength));
      IF StrBuf <> '' THEN
      BEGIN
        TmpField.CheckField := TEpiCheckField.Create();
        TmpField.CheckField.ValueLabel := TValueLabelSet.Create;
        TmpField.ValueLabelSet.Name := StrBuf;
        ValueLabels.AddValueLabelSet(TmpField.ValueLabelSet);
      END;
    END;  //for i

    // ********************************
    //      STATA VARIABLE LABELS
    // ********************************
    SetLength(CharBuf, FileLabelLength * NVar);
    DataStream.Read(CharBuf[0], FileLabelLength * NVar);
    J := 0;
    FOR i := 0 TO nVar-1 DO
    BEGIN
      TmpField := Fields[i];
      StrBuf := Trim(StringFromBuffer(PChar(@CharBuf[i * FileLabelLength]), FileLabelLength));
      IF Length(StrBuf)>50 THEN
        StrBuf := Copy(StrBuf, 1, 48) + '..';
      J := Max(J, Length(StrBuf));
      StrBuf := StringReplace(StrBuf, '#', ' ', [rfReplaceAll]);
      StrBuf := StringReplace(StrBuf, '>', ' ', [rfReplaceAll]);
      StrBuf := StringReplace(StrBuf, '<', ' ', [rfReplaceAll]);
      StrBuf := StringReplace(StrBuf, '_', ' ', [rfReplaceAll]);
      TmpField.VariableLabel := StrBuf;
    END;

    {Make Field's question and position entryfield}
    FOR i := 0 TO nVar-1 DO
    BEGIN
      TmpField          := Fields[i];
      TmpField.Question := Format('%-10s %' + IntToStr(J) + 's', [TmpField.FieldName, TmpField.VariableLabel]);
      TmpField.FieldX   := 10 + 1 + J + 2;
    END;

    // ********************************
    //      STATA EXPANSION FIELDS
    // ********************************
    // - skip expansion fields
    SetLength(ByteBuf, 1);
    REPEAT
      DataStream.Read(ByteBuf[0], 1); //data type code
      IF FileVersion >= $6E THEN
        I := ReadInts(4)
      ELSE
        I := ReadInts(2);
      IF (ByteBuf[0] > 0) OR (I > 0) THEN
        DataStream.Seek(I, soCurrent);
    UNTIL (DataStream.Position >= DataStream.Size-1) OR ((I=0) AND (ByteBuf[0]=0));

    // ********************************
    // Commit field info to REC-file
    // structure.
    // ********************************
    Save(FileName);

    // ********************************
    //          STATA DATA
    // ********************************
    TRY
      FOR CurRec := 1 TO nObs DO
      BEGIN
        UpdateProgress(Trunc(100 * CurRec/nObs), Lang(0, 'Reading data'));
        FOR CurField := 0 TO NumFields-1 DO
        BEGIN
          TmpField := Fields[Curfield];
          Case TypeList[CurField] of
            ByteConst,
            IntConst,
            LongConst:
              begin
                Case TypeList[CurField] of
                  ByteConst:
                    begin
                      I := ReadInts(1);
                      J := $7F;
                    end;
                  IntConst:
                    begin
                      I := ReadInts(2);
                      J := $7FFF;
                    end;
                  LongConst:
                    begin
                      I := ReadInts(4);
                      J := $7FFFFFFF;
                    end;
                end;
                if I > (J - MissingBaseNum) then
                Begin
                  if (I >= (J - 25)) and (I <= (J - 23)) then
                  begin
                    // Write all 9's, 8's or 7's as missing value.
                    StrBuf := DupeString(IntToStr((J - I) - 16), TmpField.FieldLength);
                    TmpField.CheckField.MissingValues[I - (J - 25)] := StrBuf;
                  end else
                    StrBuf := '';
                end else
                  StrBuf := IntToStr(I);
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
                    TmpField.CheckField.MissingValues[9 - StrToInt(StrBuf)] :=
                      DupeString(StrBuf, TmpField.FieldLength - (TmpField.NumDecimals + 1)) + '.' + DupeString(StrBuf, TmpField.NumDecimals);
                    StrBuf := TmpField.CheckField.MissingValues[9 - StrToInt(StrBuf)];
                  end;
                end else
                  Str(TmpFlt : TmpField.FieldLength : TmpField.NumDecimals, StrBuf)
              End;
          else
            // This is a string field.
            SetLength(CharBuf, TmpField.FieldLength div 2);
            FillChar(CharBuf[0], Length(CharBuf), 0);
            DataStream.Read(CharBuf[0], TmpField.FieldLength div 2);
            StrBuf := SysToUTF8(String(CharBuf));
          end;

          IF (StrBuf <> '') AND (TmpField.Fieldtype in DateFieldTypes) THEN
          BEGIN
            {Date is converted from Stata's 1/1-1960 base to Delphi's 30/12-1899 base}
            StrBuf := EpiDateTimeToStr(StrToFloat(StrBuf) + 21916, TmpField.FieldType);
          END;  //if date variable
          TmpField.AsData := StrBuf;
        END;  //for CurField
        Write();
      END;  //for CurRec
    EXCEPT
      ErrorText := Lang(23934, 'Error reading data from Stata-file');
      ErrorCode := EPI_FAILED;
      EpiLogger.AddError(Classname, 'ImportStata', ErrorText, 23934);
      Exit;
    END;  //try..except

    IF (ValueLabels.Count > 0) AND (DataStream.Position < DataStream.Size - 4) THEN
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
            TmpValSet.AddValueLabelPair(IntToStr(TmpInt), string(CharBuf));
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
          TmpValSet := ValueLabels.ValueLabelSetByName(string(PChar(@CharBuf[0])));  // Get ValueLabelSet
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

            if (TmpInt >= $7FFFFFE5) then                                 //ignore valuelabels
              Continue;

            if Trim(TmpValSet.ValueLabel[IntToStr(TmpInt)]) <> '' then
            BEGIN
              ErrorText := Lang(23936, 'Duplicate value label name found');
              ErrorCode := EPI_FAILED;
              EpiLogger.AddError(Classname, 'ImportStata', ErrorText, 23936);
              Exit;
            END;
            TmpValSet.AddValueLabelPair(IntToStr(TmpInt),
              StringFromBuffer(PChar(@CharBuf[CurRec]), 32000));
          END;  //for i
        END;  //while
      END;  //if stataversion 6, 7 or 8
    END;
    // successfully loaded the file.
    Result := true;
  finally
    EpiLogger.DecIndent;
    if Assigned(DataStream) then FreeAndNil(DataStream);
  end;
end;

Function TEpiImportExport.ImportDBase(Const aFilename: string; var DataFile: TEpiDataFile): Boolean;
var
  ByteBuf,
  FieldLengths: Array of Byte;
  CharBuf: Array of Char;
  NObs, NVars: Integer;
  RSize: LongInt;
  HSize: LongInt;
  TmpField: TEpiField;
  TmpStr: string;
  J: Integer;
  CurRec: Integer;
  TmpInt: LongInt;
  CurField: Integer;
  C: Char;
BEGIN
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'ImportDBase', 2, 'Filename = ' + aFilename);
  Result := false;

  if Assigned(DataFile) then
    DataFile.Reset()
  else
    DataFile := TEpiDataFile.Create([eoIgnoreChecks, eoIgnoreIndex, eoIgnoreRelates, eoInMemory]);

  With DataFile do
  TRY
    FieldNaming := fnAuto;
    OrgDataType := dftStata;
    FileName := aFilename;
    UpdateProgress(0, Lang(0, 'Reading header information'));

    DataStream := TFileStream.Create(aFileName, fmOpenRead);
    FByteOrder := boLittleEndian;

    // ********************************
    //           DBASE HEADER
    // ********************************

    SetLength(ByteBuf, 4);
    DataStream.Read(ByteBuf[0], 4);
    IF not ((ByteBuf[0]{ and $7}) in [3, 4]) THEN
    BEGIN
      // TODO -o Torsten: Error Message.
      ErrorCode := EPI_NOT_VALID_DBASE_FILE;
      Exit;
    END;

    NObs := ReadInts(4);
    HSize := ReadInts(2);
    RSize := ReadInts(2);

    DataStream.Seek(32, soBeginning);

    {Read field descriptors}
    SetLength(CharBuf, 12);
    SetLength(ByteBuf, 20);
    While True do
    begin
      SetLength(FieldLengths, DataFields.Count + 1);
      DataStream.Read(CharBuf[0], 12);
      IF Ord(CharBuf[0]) = $0D THEN
        Break;

      TmpField := TEpiField.Create();
      TmpStr := Trim(StringFromBuffer(PChar(@CharBuf[0]), 11));

      J := 1;
      if (not CheckVariableName(TmpStr, AlfaNumChars)) or (FieldExists(TmpStr)) then
      repeat
        TmpStr := 'V '+ IntToStr(J);
        INC(J);
      until not FieldExists(TmpStr);
      TmpField.FieldName := TmpStr;
      DataStream.Read(ByteBuf[0], 20);

      FieldLengths[High(FieldLengths)] := ByteBuf[4];
      Case CharBuf[11] of
        'C':
          Begin
            TmpField.FieldType   := ftAlfa;
            TmpField.FieldLength := ByteBuf[4];
          End;
        'D':
          Begin
            TmpField.FieldType   := ftDate;
            TmpField.FieldLength := 10;
          End;
        'F', 'N':
          Begin
            IF (ByteBuf[5] = 0) AND (ByteBuf[4] < 5) THEN
              TmpField.FieldType := ftInteger
            ELSE
              TmpField.FieldType := ftFloat;
            IF ByteBuf[4] > 16 THEN
              ByteBuf[4] := 16;
            TmpField.FieldLength := ByteBuf[4];
            TmpField.NumDecimals := ByteBuf[5];
          End;
        'L':
          Begin
            TmpField.FieldType   := ftBoolean;
            TmpField.FieldLength := ByteBuf[4];
          End;
      else
        ErrorCode := EPI_IMPORT_FAILED;
        ErrorText := Format(Lang(0, 'Unknown Field Code: %s'), [CharBuf[11]]);
        EpiLogger.AddError(Classname, 'ImportDBase', ErrorText, 0);
        Exit;
      end;

      WITH TmpField DO
        BEGIN
          Question      := TmpStr;
          QuestX        := 1;
          QuestY        := Fields.Count + 1;
          FieldX        := 12;
          FieldY        := QuestY;
        END;  //with
      AddField(TmpField);
    end;

    Save(FileName);

    {Read data}
    DataStream.Position := HSize;
    FOR CurRec := 1 TO NObs DO
    BEGIN
      UpdateProgress((CurRec * 100) DIV NObs, 'Reading records');

      TmpInt := ReadInts(1);
      RecordState := rsNormal;
      if TmpInt = $2A then
        RecordState := rsDeleted;

      FOR CurField := 0 TO DataFields.Count - 1 DO
      with DataFields[CurField] do
      BEGIN
        SetLength(CharBuf, FieldLengths[CurField]);
        DataStream.Read(CharBuf[0], FieldLengths[CurField]);

        TmpStr := StringFromBuffer(PChar(@CharBuf[0]), FieldLengths[CurField]);
        IF (FieldType in [ftInteger, ftFloat, ftIDNUM, ftBoolean]) AND
           (Length(TmpStr) >= 1) and (TmpStr[1]='*') THEN TmpStr := '';
        IF Trim(TmpStr)<>'' THEN
        BEGIN
          CASE FieldType OF
            ftDate:
              begin
                TmpStr := Copy(TmpStr, 5, 2) + '/' + Copy(TmpStr, 7, 2) + '/' + Copy(TmpStr, 1, 4);
                EpiIsDate(TmpStr, ftDate);
              end;
            ftInteger:
              IsInteger(TmpStr);
            ftFloat:
              IsFloat(TmpStr);
            ftBoolean:
              BEGIN
                C := upCase(TmpStr[1]);

                if C in BooleanYesChars then
                  C := 'Y'
                else
                  C := 'N';
              END;  //case ftBoolean
          END;  //case
        END;  //if s<>''
        AsData := Trim(TmpStr);
      END;  //for CurField
      Write();
    END;  //for CurRec
    Result := true;
  FINALLY
    EpiLogger.DecIndent;
    If Assigned(DataStream) then FreeAndNil(DataStream);
  END;  //try..finally
END;   //ImportDBaseFile

function TEpiImportExport.ImportTXT(const aFilename: string;
  var DataFile: TEpiDataFile; TxtImpSetting: PEpiTxtImportSettings): Boolean;
var
  QESHandler: TQesHandler;
  ImportLines: TStrings;
  TmpStr: String;
  FieldLines: TStringList;
  i: Integer;
  j: Integer;
  TmpField: TEpiField;
  ok: Boolean;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'ImportTXT', 2, 'Filename = ' + BoolToStr(Trim(aFilename) = '', aFileName, 'ClipBoard'));
  result := false;

  if Assigned(DataFile) then
    DataFile.Reset()
  else
    DataFile := TEpiDataFile.Create([eoIgnoreChecks, eoIgnoreIndex, eoIgnoreRelates, eoInMemory]);

  if (TxtImpSetting = nil) then
    TxtImpSetting := @ImportTxtGuess;

  QESHandler := nil;
  ImportLines := nil;
  FieldLines := nil;

  // IMPORT.
  With DataFile do
  TRY
    FieldNaming := fnAuto;
    OrgDataType := dftStata;
    FileName := aFilename;
    UpdateProgress(0, Lang(0, 'Initializing'));

    ImportLines := TStringList.Create;

    // Importing from ClipBoard?
    if aFilename = '' then
    begin
      if Clipboard.HasFormat(CF_Text) then
      begin
        TmpStr := Clipboard.AsText;
        {$IFDEF MSWINDOWS} // When compiling for linux (GTK2) the strings are automatically split at char #10.
        TmpStr := StringReplace(TmpStr, #13#10, #1, [rfReplaceAll]);
        ImportLines.Delimiter := #1;
        {$ENDIF}
        ImportLines.StrictDelimiter := true;
        ImportLines.DelimitedText := TmpStr;
      end;
    end else
      ImportLines.LoadFromFile(aFileName);

    if ImportLines.Count = 0 then
    begin
      ErrorCode := EPI_IMPORT_FAILED;
      ErrorText := Lang(0, 'ClipBoard or File contains no data.');
      Exit;
    end;

    if TxtImpSetting^.UseQESFile then
    begin
      QESHandler := TQesHandler.Create;
      if Not QESHandler.QesToDatafile(TxtImpSetting^.QESFileName, DataFile) then
        Exit;
    end else begin
      // Guess structure based on content.
      if not GuessTxtFile(DataFile, ImportLines, TxtImpSetting) then
        Exit;
    end;

    DataFile.Save('');

    WriteLn('FieldSepator: ' + TxtImpSetting^.FieldSeparator);
    WriteLn('QuoteChar: ' + TxtImpSetting^.QuoteChar);

    FieldLines := TStringList.Create;
    for i := 0 to ImportLines.Count -1 do
    begin
      if Trim(ImportLines[i]) = '' then continue;
      WriteLn('Line to split: ' + ImportLines[i]);
      SplitString(ImportLines[i], FieldLines, [TxtImpSetting^.FieldSeparator], [TxtImpSetting^.QuoteChar]);
      WriteLn('First line split: ' + FieldLines[0]);

      if FieldLines.Count > NumDataFields then
      begin
        ErrorCode := EPI_IMPORT_FAILED;
        ErrorText := Format(Lang(0, 'Error in line %d. To many delimiters - found %d, should be %d'), [i + 1, FieldLines.Count - 1, NumDataFields - 1]);
        Exit;
      end;

      for j := 0 to FieldLines.Count -1 do
      begin
        TmpStr   := FieldLines[j];
        TmpField := DataFields[j];
        case TmpField.FieldType of
          ftInteger, ftIDNUM:
            ok := IsInteger(TmpStr);
          ftAlfa,ftUpperAlfa,
          ftSoundex,ftCrypt:
            ok := True;
          ftBoolean:
            BEGIN
              Ok := true;
              TmpStr := AnsiUpperCase(Trim(TmpStr));
              IF (Length(TmpStr) >= 1) and
                 (TmpStr[1] in BooleanYesChars) THEN
                TmpStr := 'Y'
              else
                TmpStr := 'N';
            END;
          ftFloat:
            ok := IsFloat(TmpStr);
          ftDate,ftEuroDate,ftToday,
          ftYMDDate,ftYMDToday,ftEuroToday:
            ok := EpiIsDate(TmpStr, TmpField.FieldType);
        end;
        if not ok then
        begin
          ErrorCode := EPI_IMPORT_FAILED;
          ErrorText := Format(Lang(0, 'Error in line %d') + #13#10 +
             Lang(23958, 'Data (''%s'') is not compliant with the fieldtype of field %s (%s).'),
            [i + 1, TmpStr, TmpField.FieldName, FieldTypeToFieldTypeName(TmpField.FieldType, OnTranslate)]);
          Exit;
        end;
        IF Length(TmpStr) > TmpField.FieldLength THEN
        BEGIN
          ErrorCode := EPI_IMPORT_FAILED;
          ErrorText := Format(Lang(0, 'Error in line %d') + #13#10 +
             Lang(23960, 'Data (''%s'') too wide to fit in the field %s'),
            [i + 1, TmpStr, TmpField.FieldName]);
          Exit;
        END;
        TmpField.AsData := TmpStr;
      end;
      Write();
    end;
  finally
    if Assigned(QESHandler) then FreeAndNil(QESHandler);
    if Assigned(ImportLines) then FreeAndNil(ImportLines);
    if Assigned(FieldLines) then FreeAndNil(FieldLines);
    EpiLogger.DecIndent;
  end;
end;

function TEpiImportExport.ImportXLS(const aFilename: string;
  var DataFile: TEpiDataFile): Boolean;
begin

end;

function TEpiImportExport.ExportStata(const aFilename: string;
  const DataFile: TEpiDataFile; ExpSetting: PEpiExportSettings): Boolean;
var
  ValBuf,
  ByteBuf: Array of Byte;
  TypeList,
  CharBuf: Array of Char;
  NVar, NObs,
  I, J, TmpInt: Integer;
  TmpFlt: Double;
  TmpStr: string;
  TmpChar: Char;
  WritenValueLabels: TStringList;
  UniqueValueLabels: TStringList;
  CurRec: Integer;
  CurField: Integer;
  VLblSet: TValueLabelSet;

  // Version specific variables.
  FieldNameLength, StrBaseNum, FileLabelLength,
  FmtLength: Integer;
  ByteChar, IntChar, LongChar,
  FloatChar, DoubleChar: Char;
  MissingBaseNum: Cardinal;

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
    WriteDouble(Val);
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
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'ExportStata', 2, 'Filename = ' + aFilename);
  Result := false;
  // Sanity checks:
  if Trim(aFilename) = '' then Exit;

  if not Assigned(DataFile) then Exit;

  with DataFile do
  try
    UpdateProgress(0, Lang(0, 'Writing header information'));

    DataStream := TFileStream.Create(aFileName, fmCreate);
    FileVersion := ExpSetting^.FileVersion;

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

    NVar := NumDataFields;
    NObs := NumRecords;

    // ********************************
    //           STATA HEADER
    // ********************************
    SetLength(ByteBuf, 4);
    ByteBuf[0] := FileVersion;
    ByteBuf[1] := 2;                                          // Use LOHI order of data
    ByteBuf[2] := 1;                                          // Filetype - only 1 is legal value
    ByteBuf[3] := 0;                                          // Unused
    DataStream.Write(ByteBuf[0], 4);
    WriteInts(NVar, 2);                                       // Number of Variables
    WriteInts(NObs, 4);                                       // Number of records

    IF trim(FileLabel) <> '' THEN
      TmpStr := Trim(FileLabel)
    ELSE
      TmpStr := Format('Datafile created by EpiData based on %s', [ExtractFilename(FileName)]);

    IF Length(TmpStr) > (FileLabelLength - 1) THEN
      TmpStr := Copy(TmpStr, 1, FileLabelLength - 1);

    // data_label \0 terminated.
    WriteString(TmpStr, FileLabelLength);

    // time_stamp \0 terminated (not used in epidata)
    TmpStr := FormatDateTime('dd mmm yyyy hh":"nn', Now);
    WriteString(TmpStr, 18);

    // ********************************
    //         STATA DESCRIBTORS
    // ********************************
    // - typlist: the variable's types
    SetLength(TypeList, NVar);
    SetLength(ByteBuf, NVar);
    FOR i := 0 to NVar - 1 DO
    BEGIN
      WITH DataFields[i] DO
      BEGIN
        CASE FieldType OF
          ftInteger, ftIDNUM:
            BEGIN
              IF FieldLength < 3 THEN
                TmpChar := ByteChar
              ELSE IF FieldLength < 5 THEN
                TmpChar := IntChar
              ELSE IF FieldLength < 10 THEN
                TmpChar := LongChar
              ELSE IF FieldLength >= 10 THEN
                TmpChar := DoubleChar;  //&&
            END;
          ftAlfa, ftUpperAlfa, ftCrypt:
            IF FieldLength > 80 THEN
              TmpChar := Chr(207)
            ELSE
              TmpChar := Chr(StrBaseNum + FieldLength);
          ftSoundex:
            TmpChar := CHR(StrBaseNum + 5);
          ftBoolean:
            TmpChar := ByteChar;
          ftDate, ftToday, ftEuroDate,
          ftEuroToday, ftYMDDate, ftYMDToday:
            BEGIN
              Case FieldLength of
                5: TmpChar    := CHR(StrBaseNum + FieldLength);    //Short dates is string
                8,10: TmpChar := LongChar;                         //Med. and long dates: longint
              END;
            END;
          ftFloat:
            BEGIN
              IF NumDecimals = 0 THEN
              BEGIN
                IF FieldLength < 3 THEN TmpChar := ByteChar
                ELSE IF FieldLength < 5 THEN TmpChar := IntChar
                ELSE IF FieldLength < 10 THEN TmpChar := LongChar
                ELSE TmpChar := DoubleChar;
              END ELSE
                TmpChar := DoubleChar;
            END;
        ELSE
          ErrorText := Lang(22312, 'Unknown fieldtype used in datafile.~~Export terminated.');
          ErrorCode := EPI_EXPORT_FAILED;
          EpiLogger.AddError(Classname, 'ExportStata', ErrorText, 22312);
          Exit;
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
    FOR i :=0 TO NVar - 1 DO
    BEGIN
      WITH DataFields[i] DO
      BEGIN
        TmpStr := Trim(FieldName);
        WriteString(TmpStr, FieldNameLength);
      END;   //with
    END;  //for eN

    // - srtlist: sortorder of fields}
    //   No sortorder is written, only zeros to indicated end of list}
    SetLength(ByteBuf, 2 * (NVar + 1));
    FillChar(ByteBuf[0], 2 * (NVar + 1), 0);
    DataStream.Write(ByteBuf[0], 2 * (NVar + 1));

    // - Fmtlist: list of formats of the variables
    FOR i := 0 TO NVar - 1 DO
    WITH DataFields[i] DO
    BEGIN
      CASE FieldType OF
        ftInteger, ftIDNUM:
          TmpStr := '%' + IntToStr(FieldLength) + '.0f';
        ftFloat:
          TmpStr := '%' + IntToStr(FieldLength) + '.' + IntToStr(NumDecimals) + 'f';
        ftBoolean:
          TmpStr := '%1.0f';
        ftAlfa, ftUpperAlfa, ftCrypt:
          TmpStr := '%' + IntToStr(FieldLength) + 's';
        ftSoundex:
          TmpStr := '%5s';
        ftToday, ftDate, ftEuroToday,
        ftEuroDate, ftYMDDate, ftYMDToday:
          BEGIN
            Case FieldLength of
              5:     tmpStr := '%5s';
              8,10:  tmpStr := '%d';
            END;  //case FLength
          END;  //case date
      ELSE
        ErrorCode := EPI_EXPORT_FAILED;
        ErrorText := Lang(22312, 'Unknown fieldtype used in datafile.~~Export terminated.');
        EpiLogger.AddError(Classname, 'ExportStata', ErrorText, 22312);
        Exit;
      END;   //case FeltType
      WriteString(TmpStr, FmtLength);
    END;  //for - with

    // - lbllist: names af value label
    WritenValueLabels := TStringList.Create();
    WritenValueLabels.Sorted := true;
    UniqueValueLabels := TStringList.Create();
    UniqueValueLabels.Sorted := true;
    for i := 0 to NVar - 1 do
    with DataFields[i] do
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
        // TODO -o Torsten: Change with new REC-format when integer field can have more > 4 characters.
        if not ((FieldType = ftInteger) or ((FieldType = ftFloat) and (NumDecimals = 0))) then
          TmpStr := '';

        TmpStr := UniqueValueLabelName(TmpStr, FieldNameLength);
        if TmpStr <> '' then
          UniqueValueLabels.Add(TmpStr);
      end;
      WriteString(TmpStr, FieldNameLength);
    end;

    // ********************************
    //      STATA VARIABLE LABELS
    // ********************************
    FOR i := 0 TO NVar - 1 DO
    BEGIN
      WITH DataFields[i] DO
      BEGIN
        TmpStr := Trim(VariableLabel);
        WriteString(TmpStr, FileLabelLength);
      END;  //with
    END;  //for eN

    // ********************************
    //      STATA EXPANSION FIELDS
    // ********************************
    // - skip expansion fields
    WriteInts(0, 3);
    if FileVersion >= $6E then
      WriteInts(0, 2);

    // ********************************
    //          STATA DATA
    // ********************************
    TRY
      FOR CurRec := 1 TO NObs DO
      BEGIN
        UpdateProgress((CurRec * 100) DIV NObs, 'Writing records');
        Read(CurRec);

        FOR CurField := 0 TO NVar - 1 DO
        With DataFields[CurField] do
        BEGIN
          TmpStr := AsData;

          // StrToFloat expects decimal separator to be in current locale.
          // It is always stored as "." in EpiData.
          // Placed here so it down't interfere with missingvalues.
          IF FieldType = ftFloat THEN
            TmpStr := StringReplace(TmpStr, '.', DecimalSeparator, []);

          IF trim(TmpStr)='' THEN
            TmpStr := '..';

          // Specific missing values
          IF (FileVersion >= $71) AND (FieldType in [ftInteger, ftIDNUM, ftFloat]) AND
             (NumDecimals = 0) AND (FieldLength < 10) THEN
          BEGIN
            if Assigned(CheckField) then
            begin
              IF TmpStr = CheckField.MissingValues[0] THEN TmpStr := '.a';
              IF TmpStr = CheckField.MissingValues[1] THEN TmpStr := '.b';
              IF TmpStr = CheckField.MissingValues[2] THEN TmpStr := '.c';
            end;
            IF TmpStr = CheckFile.GlobalMissingVal[0] THEN TmpStr := '.a';
            IF TmpStr = CheckFile.GlobalMissingVal[1] THEN TmpStr := '.b';
            IF TmpStr = CheckFile.GlobalMissingVal[2] THEN TmpStr := '.c';
          END;

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
                if TmpStr[1] = '.' then
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
                    {Date is converted from Delphi's 30/12-1899 base
                     to Stata's 1/1-1960 base by substracting 21916 days}
                    TmpInt := Round(EpiDateToDateTime(TmpStr, FieldType, FieldLength) - 21916)
                  else if FieldType = ftBoolean then
                    TmpInt := BoolStrToInt(TmpStr)
                  else
                    TmpInt := StrToInt(TmpStr);
                end;
                WriteInts(TmpInt, J);
              end; // Byte, Int and Long.
//          FloatConst: ( We never export to Float type.)
            DoubleConst:
              Begin
                if TmpStr[1] = '.' then
                  WriteFloat(Power(2, 1023), TmpStr)
                else
                  WriteFloat(StrToFloat(TmpStr), '-');
              End;
          else
            if (FieldType in DateFieldTypes) and not
               EpiIsDate(TmpStr, FieldType) then
            begin
              ErrorCode := EPI_EXPORT_FAILED;
              ErrorText := Format(Lang(22306, 'Illegal date found in record # %d, field %s~Export terminates.'), [CurRec, FieldName]);
              EpiLogger.AddError(Classname, 'ExportStata', ErrorText, 22306);
              Exit;
            end;
            if TmpStr = '..' then TmpStr := '';
            WriteString(TmpStr, FieldLength);
          end;
        END;  //for CurVar
      END;  //for CurObs
    EXCEPT
      ErrorCode := EPI_EXPORT_FAILED;
      ErrorText := Format(Lang(22304, 'Error occured during export of record #%d'), [CurRec]);
      EpiLogger.AddError(Classname, 'ExportStata', ErrorText, 223042);
      Exit;
    END;  //try..Except

    {Write VALUE-LABELS}
    IF FileVersion = $69 THEN
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
          WriteInts(StrToInt(VLblSet.Values[j]), 2);
          WriteString(VLblSet.Labels[j], 8);
        END;  //for j
      END;
    END ELSE BEGIN
      //write value labels in Stata ver. 6 format
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
          TmpInt := StrToInt(VLblSet.Values[J]);
          Move(TmpInt, ValBuf[J * 4], 4);
          TmpStr := VLblSet.Labels[J];
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
    END;  //for i

    Result := true;
  finally
    EpiLogger.DecIndent;
    if Assigned(DataStream) then FreeAndNil(DataStream);
    if Assigned(WritenValueLabels) then FreeAndNil(WritenValueLabels);
    if Assigned(UniqueValueLabels) then FreeAndNil(UniqueValueLabels);
  end;
end;

function TEpiImportExport.ExportDBase(Const aFilename: string; Const DataFile: TEpiDataFile): Boolean;
var
  i, dbRecLength, NObs,
  CurRec, CurField: integer;
  eYear, eMonth, eDay: Word;
  ByteBuf: Array of byte;
  TmpStr: string;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(ClassName, 'ExportDBase', 2, 'Filename = ' + aFilename);
  Result := false;

  // Sanity checks:
  if Trim(aFilename) = '' then Exit;

  if not Assigned(DataFile) then Exit;
  FByteOrder := boLittleEndian;

  with DataFile do
  try
    UpdateProgress(0, Lang(0, 'Writing header information'));

    DataStream := TFileStream.Create(aFileName, fmCreate);

    {Calculate recordlength as it is in dBase format}
    dbRecLength := 0;
    FOR i := 0 TO DataFields.Count - 1 DO
    BEGIN
      WITH DataFields[i] DO
      BEGIN
        if FieldType in DateFieldTypes then
          INC(dbRecLength, 8)
        ELSE
          INC(dbRecLength, FieldLength);
      END;  //with
    END;  //for eN

    DecodeDate(Date, eYear, eMonth, eDay);
    IF eYear>100 THEN eYear:=eYear MOD 100;

    // ********************************
    //           DBASE HEADER
    // ********************************
    WriteInts(3, 1);                                 //header offset 0 - dBase III identifier
    WriteInts(eYear, 1);                             //header offset 1 - year of last update
    WriteInts(eMonth, 1);                            //header offset 2 - month of last update
    WriteInts(eDay, 1);                              //header offset 3 - date of last update
    WriteInts(NumRecords, 4);                        //header offset 4 - Number of records
    WriteInts(32 + (DataFields.Count * 32) + 1, 2);  //header offset 8 - Header size in bytes
    WriteInts(dbRecLength + 1, 2);                   //header offset 10 - Record size in bytes

    SetLength(ByteBuf, 20);
    FillChar(ByteBuf[0], 20, 0);
    DataStream.Write(ByteBuf[0], 20);                //header offset 12 - 20 x unused bytes

    {Write field descriptions}
    SetLength(ByteBuf, 14);
    FillChar(ByteBuf[0], 14, 0);
    ByteBuf[5] := 1;
    FOR i := 0 TO DataFields.Count - 1 DO
    WITH DataFields[i] DO
    BEGIN
      // TODO -o Torsten : Check for too long field names.
      WriteString(FieldName, 11);                                  // Field name   (Bytes 0  - 10)
      CASE FieldType of                                             // Field type   (Byte  11)
        ftInteger, ftIDNUM, ftFloat:         WriteString('N', 1, False);
        ftSoundex,
        ftAlfa,ftUpperAlfa,ftCrypt:          WriteString('C', 1, False);
        ftDate,ftToday,ftYMDDate,ftYMDToday,
        ftEuroDate,ftEuroToday:              WriteString('D', 1, False);
        ftBoolean:                           WriteString('L', 1, False);
      ELSE
        WriteString('X', 1, False);
      END;
      WriteInts(0, 4);                                             // Data address (Bytes 12 - 15)  - not used.
      IF (FieldType in DateFieldTypes) THEN                        // Field type   (Byte  16)
        WriteInts(8, 1)                                            // -- all dates are length 8 in dBase
      ELSE
        WriteInts(FieldLength, 1);
      WriteInts(NumDecimals, 1);                                   // Decimal cnt. (Byte 17)
      DataStream.Write(ByteBuf[0], 14);                            // Reserveds... (Bytes 17 - 31, Byte 23 must be == 1)   
    END;  //for
    WriteInts($0D, 1);   //write Header Terminator

    {write records}
    NObs := NumRecords;
    TRY
      FOR CurRec := 1 TO NObs DO
      BEGIN
        UpdateProgress((CurRec * 100) DIV NObs, 'Writing records');
        Read(CurRec);

        IF RecordState = rsDeleted THEN
          WriteInts($2A, 1)
        ELSE
          WriteInts($20, 1);

        FOR CurField := 0 TO DataFields.Count - 1 DO
        WITH DataFields[CurField] DO
        BEGIN
          TmpStr := AsData;
          CASE FieldType of
            ftAlfa, ftUpperAlfa, ftCrypt,
            ftBoolean, ftSoundex,
            ftInteger, ftIDNUM, ftFloat:
              WriteString(Trim(TmpStr), FieldLength, False);
            ftDate, ftToday, ftEuroDate,
            ftEuroToday,ftYMDDate,ftYMDToday:
              begin
                if Trim(TmpStr) ='' then
                begin
                  ErrorCode := EPI_EXPORT_FAILED;
                  ErrorText := Format(Lang(22306, 'Illegal date found in record # %d, field %s'), [CurRec, FieldName]);
                  Exit;
                end;
                WriteString(FormatDateTime('yyyymmdd', EpiDateToDateTime(TmpStr, FieldType, FieldLength)), 8, False);
              end;
          END;   //Case
        END;  //with CurField
      END;  //for CurRec
    EXCEPT
      // TODO -o Torsten : Errormessage;
      ErrorText := Lang(0, '');
      ErrorCode := EPI_EXPORT_FAILED;
      Exit;
    END;  //try..except
    Result := True;
  finally
    EpiLogger.DecIndent;
    if Assigned(DataStream) then FreeAndNil(DataStream);
  end;
END;

function TEpiImportExport.ExportTXT(const aFilename: string;
  const DataFile: TEpiDataFile): Boolean;
begin

end;

function TEpiImportExport.ExportXLS(const aFilename: string;
  const DataFile: TEpiDataFile): Boolean;
begin

end;

end.

