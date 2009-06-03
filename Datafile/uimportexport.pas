unit UImportExport;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, UEpiDataFile, UDataFileTypes, UDebug;

type

  // Export record:
  // - FileVersion:     Used in Stata and DBase file to name specific version of file.
  // - Start/EndRecord: Interger to name starting and ending record to export. Use
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
  ExportStata4_5: TEpiExportSettings = (FileVersion:$69; StartRecord:1; EndRecord:-1);
  ExportStata6:   TEpiExportSettings = (FileVersion:$6C; StartRecord:1; EndRecord:-1);
  ExportStata7:   TEpiExportSettings = (FileVersion:$6E; StartRecord:1; EndRecord:-1);
  ExportStata8_9: TEpiExportSettings = (FileVersion:$71; StartRecord:1; EndRecord:-1);
  ExportStata10:  TEpiExportSettings = (FileVersion:$72; StartRecord:1; EndRecord:-1);

type

  { TEpiImportExport }

  TEpiImportExport = class(TObject)
  private
    FOnProgress:  TProgressEvent;
    FOnTranslate: TTranslateEvent;
    FByteOrder:   TByteOrder;
    DataStream:   TStream;
    function      Lang(LangCode: Integer; Const LangText: string): String;
    Function      UpdateProgress(Percent: Integer; Msg: String): TProgressResult;
    procedure     ReadBuf(var Buf: Array of Byte; Count: Integer);
    function      ReadInts(Count: Integer): Integer;
    function      ReadSingle(Count: Integer; var MisVal: String): Double;
    function      ReadDouble(Count: Integer; var MisVal: String): Double;
    function      StringFromBuffer(AChar: PChar; MaxLength: Integer): String;
  public
    constructor   Create;
    destructor    Destroy; override;
    function      ImportStata(Const aFilename: String; var DataFile: TEpiDataFile): Boolean;
    function      ImportDBase(Const aFilename: String; var DataFile: TEpiDataFile): Boolean;
    function      ImportTXT(Const aFilename: String; var DataFile: TEpiDataFile): Boolean;
    function      ImportXLS(Const aFilename: String; var DataFile: TEpiDataFile): Boolean;
    function      ExportStata(Const aFilename: String; Const DataFile: TEpiDataFile;
                              ExpSetting: PEpiExportSettings): Boolean;
    function      ExportTXT(Const aFilename: String; Const DataFile: TEpiDataFile): Boolean;
    function      ExportXLS(Const aFilename: String; Const DataFile: TEpiDataFile): Boolean;
    property      OnProgress:  TProgressEvent read FOnProgress write FOnProgress;
    property      OnTranslate: TTranslateEvent read FOnTranslate write FOnTranslate;
    property      ByteOrder: TByteOrder read FByteOrder;
  end;

implementation

uses
  UValueLabels, UEpiDataConstants, UEpiUtils, Math, StrUtils, UDateUtils;

  { TEpiImportExport }

function TEpiImportExport.Lang(LangCode: Integer; const LangText: string): String;
begin
  Result := LangText;
  IF Assigned(FOnTranslate) THEN
    Result := FOnTranslate(langcode, Result)
end;

function TEpiImportExport.UpdateProgress(Percent: Integer; Msg: String
  ): TProgressResult;
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


function TEpiImportExport.ReadSingle(Count: Integer;
  var MisVal: String): Double;
var
  Buf: Array[0..3] of Byte;
begin
  MisVal := '';
  FillChar(Buf, 4, 0);
  ReadBuf(Buf, Count);
  Result := Single(Buf);
end;


function TEpiImportExport.ReadDouble(Count: Integer;
  var MisVal: String): Double;
var
  Buf: Array[0..7] of Byte;
begin
  MisVal := '';
  FillChar(Buf, 8, 0);
  ReadBuf(Buf, Count);
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
  Result := Double(Buf);
end;

function TEpiImportExport.StringFromBuffer(AChar: PChar; MaxLength: Integer
  ): String;
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
  end
end;

constructor TEpiImportExport.Create;
begin

end;

destructor TEpiImportExport.Destroy;
begin

end;

function TEpiImportExport.ImportStata(const aFilename: String;
  var DataFile: TEpiDataFile): Boolean;
var
  ByteBuf, ValBuf: Array of Byte;
  TypeList,
  CharBuf: Array of Char;
  NVar, NObs, CurRec, CurField,
  I, J, TmpInt: integer;
  TmpFlt: Double;
  TmpField: TEpiField;
  StrBuf: String;
  WideBuf: WideString;
  TmpValSet: TValueLabelSet;

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

begin
  Debugger.IncIndent;
  Debugger.Add(ClassName, 'ImportStata', 2, 'Filename = ' + aFilename);
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
      Exit;
    END;

    // nvars (number of vars)
    nVar := ReadInts(2);
    IF nVar>800 THEN
    BEGIN
      ErrorText := Format(Lang(23982, 'The stata-file contains %d variables.~A maximum of 800 variables can be imported.'), [nVar]);
      ErrorCode := EPI_TOO_MANY_VARIABLES;
      Exit;
    END;

    // nobs (number of obs)
    NObs := ReadInts(4);

    // data_label \0 terminated.
    SetLength(CharBuf, FileLabelLength);
    DataStream.Read(CharBuf[0], FileLabelLength);
    FileLabel := String(CharBuf);

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
          Exit;
        END;
        TmpField.FieldType := ftAlfa;
        TmpField.FieldLength := Ord(TypeList[i]) - StrBaseNum;
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
                  TmpFlt := ReadSingle(4, StrBuf)
                else
                  TmpFlt := ReadDouble(8, StrBuf);
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
            SetLength(CharBuf, TmpField.FieldLength);
            DataStream.Read(CharBuf[0], TmpField.FieldLength);
            StrBuf := StringFromBuffer(PChar(@CharBuf[0]), TmpField.FieldLength);
          end;

          IF (StrBuf <> '') AND (TmpField.Fieldtype in DateFieldTypes) THEN
          BEGIN
            TmpFlt := StrToFloat(StrBuf) + 21916;  {Date is converted from Stata's 1/1-1960 base to Delphi's 30/12-1899 base}
            // TODO -O Torsten : Handle dates in Stata.
//              s:=mibDateToStr(tmpDate,EField.Fieldtype);
          END;  //if date variable
          TmpField.AsData := StrBuf;
        END;  //for CurField
        Write();
      END;  //for CurRec
    EXCEPT
      ErrorText := Lang(23934, 'Error reading data from Stata-file');
      ErrorCode := EPI_FAILED;
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
          TmpValSet := ValueLabels.ValueLabelSetByName(String(CharBuf));
          SetLength(CharBuf, 8);
          FOR i := 0 TO J - 1 DO
          BEGIN
            TmpInt := ReadInts(2);
            DataStream.Read(CharBuf[0], 8);
            if Trim(TmpValSet.ValueLabel[IntToStr(TmpInt)]) <> '' then
            BEGIN
              ErrorText := Lang(23936, 'Duplicate value label name found');
              ErrorCode := EPI_FAILED;
              Exit;
            END;
            TmpValSet.AddValueLabelPair(IntToStr(TmpInt), String(CharBuf));
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
          TmpValSet := ValueLabels.ValueLabelSetByName(String(PChar(@CharBuf[0])));  // Get ValueLabelSet
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
    if Assigned(DataStream) then FreeAndNil(DataStream);
  end;
end;

Function TEpiImportExport.ImportDBase(Const aFilename: String; var DataFile: TEpiDataFile): Boolean;
var
  DBaseFile: TFileStream;
  ByteBuf: Array of Byte;
  CharBuf: Array of Char;
  NObs, NVars: Integer;
  RSize: LongInt;
  HSize: LongInt;
  TmpField: TEpiField;
  TmpStr: String;
  J: Integer;
  CurRec: Integer;
  TmpInt: LongInt;
  CurField: Integer;
  C: Char;
BEGIN
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
    IF not ((ByteBuf[0] and $7) in [3, 4]) THEN
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
    While True do
    begin
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

      Case CharBuf[11] of
        'C':
          Begin
            TmpField.FieldType   := ftAlfa;
            TmpField.FieldLength := ByteBuf[5];
          End;
        'D':
          Begin
            TmpField.FieldType   :=ftDate;
            TmpField.FieldLength :=10;
          End;
        'F', 'N':
          Begin
            IF (ByteBuf[6] = 0) AND (ByteBuf[5] < 5) THEN
              TmpField.FieldType := ftInteger
            ELSE
              TmpField.FieldType := ftFloat;
            IF ByteBuf[5] > 16 THEN
              ByteBuf[5] := 16;
            TmpField.FieldLength := ByteBuf[5];
            TmpField.NumDecimals := ByteBuf[6];
          End;
        'L':
          Begin
            TmpField.FieldType   := ftBoolean;
            TmpField.FieldLength := 1;
          End;
      else
        ErrorCode := EPI_IMPORT_FAILED;
        ErrorText := Format(Lang(0, 'Unknown Field Code: %c'), [CharBuf[11]]);
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
        SetLength(CharBuf, FieldLength);
        DataStream.Read(CharBuf[0], FieldLength);

        TmpStr := StringFromBuffer(PChar(@CharBuf[0]), FieldLength);
        IF (FieldType in [ftInteger, ftFloat, ftIDNUM, ftBoolean]) AND
           (TmpStr[1]='*') THEN TmpStr := '';
        IF Trim(TmpStr)<>'' THEN
        BEGIN
          CASE FieldType OF
            ftDate:
              TmpStr := Copy(TmpStr, 5, 2) + '/' + Copy(TmpStr, 7, 2) + '/' + Copy(TmpStr, 1, 4);
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
    If Assigned(DataStream) then FreeAndNil(DataStream);
  END;  //try..finally
END;   //ImportDBaseFile

function TEpiImportExport.ImportTXT(const aFilename: String;
  var DataFile: TEpiDataFile): Boolean;
begin

end;

function TEpiImportExport.ImportXLS(const aFilename: String;
  var DataFile: TEpiDataFile): Boolean;
begin

end;

function TEpiImportExport.ExportStata(const aFilename: String;
  const DataFile: TEpiDataFile; ExpSetting: PEpiExportSettings): Boolean;
var
  ValBuf,
  ByteBuf: Array of Byte;
  TypeList,
  CharBuf: Array of Char;
  StataFile: TFileStream;
  NVar, NObs,
  I, J, TmpInt: Integer;
  TmpFlt: Double;
  TmpStr: String;
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

  procedure WriteInts(Const Val, Count: Integer);
  var
    TmpBuf: Array[0..3] of Byte;
    I: Integer;
  begin
    FillChar(TmpBuf, 4, 0);
    for i := 0 to Count - 1 do
      TmpBuf[i] := Byte(Val shr (i * 8));
    StataFile.Write(TmpBuf, Count);
  end;

  procedure WriteString(Const Str: String; Const Count: Integer);
  var
    StrBuf: PChar;
  begin
    StrBuf := StrAlloc(Count);
    StrPLCopy(PChar(@StrBuf[0]), Str, Count - 1);
    StataFile.Write(StrBuf[0], Count);
    StrDispose(StrBuf);
  end;

  procedure WriteFloat(Val: Double; Const MisVal: String);
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
    StataFile.Write(FltByte[0], 8);
  end;

  function UniqueValueLabelName(Str: String; Const Count: Integer): string;
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

  StataFile := nil;

  with DataFile do
  try
    UpdateProgress(0, Lang(0, 'Writing header information'));

    StataFile := TFileStream.Create(aFileName, fmCreate);
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
    StataFile.Write(ByteBuf[0], 4);
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

    StataFile.Write(ByteBuf[0], NVar);

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
    StataFile.Write(ByteBuf[0], 2 * (NVar + 1));

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
                  TmpInt := J - MissingBaseNum + 1;
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
               IsDate(TmpStr, FieldType) then
            begin
              ErrorCode := EPI_EXPORT_FAILED;
              ErrorText := Format(Lang(22306, 'Illegal date found in record # %d, field %s~Export terminates.'), [CurRec, FieldName]);
              Exit;
            end;

            WriteString(TmpStr, FieldLength);
          end;
        END;  //for CurVar
      END;  //for CurObs
    EXCEPT
      ErrorCode := EPI_EXPORT_FAILED;
      ErrorText := Format(Lang(22304, 'Error occured during export of record #%d'), [CurRec]);
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
        StataFile.Write(ByteBuf[0], 4 * NObs);                  // off[]
        StataFile.Write(ValBuf[0], 4 * NObs);                   // val[]
        StataFile.Write(CharBuf[0], CurRec);                    // txt[]
      END;  //write value labels in stata 6 version
    END;  //for i

    Result := true;
  finally
    if Assigned(StataFile) then FreeAndNil(StataFile);
    if Assigned(WritenValueLabels) then FreeAndNil(WritenValueLabels);
    if Assigned(UniqueValueLabels) then FreeAndNil(UniqueValueLabels);
  end;
end;

function TEpiImportExport.ExportTXT(const aFilename: String;
  const DataFile: TEpiDataFile): Boolean;
begin

end;

function TEpiImportExport.ExportXLS(const aFilename: String;
  const DataFile: TEpiDataFile): Boolean;
begin

end;

end.

