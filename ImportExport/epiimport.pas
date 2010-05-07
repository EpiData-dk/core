unit epiimport; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument,epidatafiles, epidatafilestypes;

type

  TEpiClipBoardReadHook = procedure (ClipBoardLine: TStrings) of object;

  { TEpiImport }

  TEpiImport = class(TObject)
  private
    FOnClipBoardRead: TEpiClipBoardReadHook;
    procedure   RaiseError(Const Msg: string);
  public
    constructor Create;
    destructor  Destroy; override;
    function    ImportRec(Const aFilename: string; Document: TEpiDocument = nil): TEpiDataFile;
    property    OnClipBoardRead: TEpiClipBoardReadHook read FOnClipBoardRead write FOnClipBoardRead;
  end;

implementation

uses
  FileUtil, epistringutils;

{ TEpiImport }

procedure TEpiImport.RaiseError(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

constructor TEpiImport.Create;
begin

end;

destructor TEpiImport.Destroy;
begin
  inherited Destroy;
end;

function TEpiImport.ImportRec(const aFilename: string;
  Document: TEpiDocument = nil): TEpiDataFile;
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
  i: Integer;

const
  // Convert old REC file fieldtype number to new order of fieldtypes.
  FieldTypeConversionTable: array[0..20] of TEpiFieldType = (
  //   0          1         2          3              4
  //   ftInteger, ftString, ftDate,    ftUpperAlfa,   ftCheckBox,
       ftInteger, ftString, ftMDYDate, ftUpperString, ftBoolean,

  //   5          6        7           8       9
  //   ftBoolean, ftFloat, ftPhoneNum, ftTime, ftLocalNum,
       ftBoolean, ftFloat, ftString,   ftTime, ftString,
  //   10          11          12         13        14
  //   ftToday,    ftEuroDate, ftIDNUM,   ftRes4,   ftRes5,
       ftMDYToday, ftDMYDate,  ftAutoInc, ftString, ftString,

  //   15          16           17         18        19
  //   ftQuestion, ftEuroToday, ftSoundex, ftCrypt,  ftYMDDate,
       ftString,   ftDMYToday,  ftString,  ftString, ftYMDDate,

  //   20
  //   ftYMDToday);
       ftYMDToday
  );


  function RequestPassword(Const EncryptedString: string): boolean;
  var
    S, FPassword: string;
  begin
    result := false;
{    if Assigned(FOnPassword) then
      FOnPassword(Self, rpOpen, FPassword);
    try
      S := Base64DecodeStr(EncryptedString);
      FCrypter.InitStr(FPassword, TDCP_sha1);
      FCrypter.DecryptCFBblock(S[1], S[1], Length(S));
      FCrypter.Reset;
      Result := (AnsiCompareText(FPassword, S) = 0);
    except
      DataFile.ErrorText := Lang(0, 'Fatal Error in decrypting password.');
      DataFile.ErrorCode := EPI_INVALID_PASSWORD;
      EpiLogger.AddError(ClassName, 'RequestPassword', DataFile.ErrorText, 0);
      Abort;
    end;    }
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
  result := nil;

  if aFilename = '' then exit;
  if not FileExistsUTF8(aFilename) then
    RaiseError('File does not exists');

  if Assigned(Document) then
    result := Document.DataFiles.NewDataFile
  else
    result := TEpiDataFile.Create(nil);

  with Result do
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
    if Pos('FILELABEL: ', AnsiUpperCase(TxtLine)) > 0 then
      Name.Text :=  EpiUnknownStrToUTF8(Copy(TxtLine, Pos('FILELABEL: ', AnsiUpperCase(TxtLine)) + Length('FILELABEL: ') , Length(TxtLine)));

    // - Header lines:
    Val(Copy(TxtLine, 1, Pos(' ', TxtLine)-1), HeaderLineCount, ValCode);
    if ValCode > 0 then
      RaiseError(Format('Incorrect format of file: %S', [aFilename]));

    // Read field defining header lines.
    TotFieldLength := 0;
    for CurrentLine := 1 to HeaderLineCount do
    begin
      ReadLn(TxtFile,
             TmpFieldChar, TmpName, TmpQuestX, TmpQuestY,
             TmpQuestColor, TmpFieldX, TmpFieldY, TmpFieldTypeInt, TmpLength,
             TmpFieldColor, dummy, TmpLabel);

      // Field types.
      FieldIsQuestion := false;
      if TmpFieldTypeInt >= 100 then
        // Type > 100 => float field
        TmpFieldType := ftFloat
      else begin
        // Normal field type recognition.
        TmpFieldType := FieldTypeConversionTable[TmpFieldTypeInt];
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
        with EHeading do
        begin
          Name.Text    := TmpName;
          Caption.Text := TmpLabel;
          Left         := TmpFieldX;
          Top          := TmpFieldY;
        end;
        Continue;
      end;

      EField := NewField(TmpFieldType);
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
          Name.Text := VariableLabel;
        end;
        // Ensure valid variable name.
        Name.Text := TmpName;

        // Summerize field findings.
        TotFieldLength := TotFieldLength + Length;
      end;  // With EField
    end; // For CurrentLine

    // Position for reading and check for corruptness.
    TotFieldLength := TotFieldLength + (((TotFieldLength - 1) DIV 78) + 1) * 3; { MaxRecLineLength = 78 }
    TmpLength := TextPos(TxtFile);
    CloseFile(TxtFile);

    DataStream := TMemoryStream.Create;
    DataStream.LoadFromFile(UTF8ToSys(AFilename));
    DataStream.Position := DataStream.Size;

    // Skip all lineendings / EOF chars.
    SetLength(CharBuf, 16);
    Stop := false;
    while DataStream.Position >= TmpLength do
    begin
      DataStream.Seek(-16, soCurrent);
      DataStream.Read(CharBuf[0], 16);

      i := 15;
      while i >= 0 do
      begin
        if (CharBuf[i] in ['!', '?', '^']) then
        begin
          Stop := true;
          break;
        end;
        Dec(i);
      end;
      if Stop then break;
      DataStream.Seek(-16, soCurrent);
    end;

    if DataStream.Position < TmpLength then
      TempInt := TmpLength  // This is an empty datafile!
    else
      TempInt := DataStream.Position - (16 - i) + 3; // + 3 is for "!#13#10" which all .REC file should end with??!?!?
    if ((TempInt - TmpLength) mod TotFieldLength) <> 0 then
      RaiseError(Format('Error in datafile %s. One or more records are corrupted. Size: %d, Offset: %d, TotalLength: %d, i: %d',
        [AFilename, DataStream.Size, TmpLength, TotFieldLength, i]));

    TempInt := ((TempInt - TmpLength) div TotFieldLength);
    Size := TempInt;
    DataStream.Position := TmpLength;

    SetLength(CharBuf, TotFieldLength);
    For CurRec := 0 to TempInt-1 do
    begin
      I := DataStream.Read(CharBuf[0], TotFieldLength);
      if (I <> TotFieldLength) then
        RaiseError(Format('Error reading record: %d', [CurRec+1]));

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
//        IF (fieldtype = ftCrypt) THEN
{        if false then
        begin
          EncData := Base64DecodeStr(TmpStr);
          FCrypter.DecryptCFBblock(EncData[1], EncData[1], Length(EncData));
          TmpStr := Trim(EncData);
          FCrypter.Reset;
        end; }
        AsString[CurRec] := EpiUnknownStrToUTF8(TmpStr);
        Inc(BufPos, Length);
      end;
    end;

    // TODO : Import .CHK files.
  finally
//    CloseFile(TxtFile);
    if Assigned(DataStream) then DataStream.Free;
  end;
end;

end.

