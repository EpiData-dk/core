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
  public
    constructor Create;
    destructor  Destroy; override;
    function    ImportRec(Const aFileName: string; var DataFile: TEpiDataFile;
      ImportData: boolean = true): boolean;
    property    OnClipBoardRead: TEpiClipBoardReadHook read FOnClipBoardRead write FOnClipBoardRead;
    // The RequestPasswordEvent does in this case not require a login name - since old .REC files do no support logins. It is discarded and not used.
    property    OnRequestPassword: TRequestPasswordEvent read FOnRequestPassword write FOnRequestPassword;
  end;

implementation

uses
  FileUtil, epistringutils, DCPbase64, DCPrijndael, DCPsha1;

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

const
  // Convert old REC file fieldtype number to new order of fieldtypes.
  FieldTypeConversionTable: array[0..20] of TEpiFieldType = (
  //   0           1            2           3              4
  //   ftInteger,  ftString,    ftDate,     ftUpperAlfa,   ftCheckBox,
       ftInteger,  ftString,    ftMDYDate,  ftUpperString, ftBoolean,

  //   5           6            7           8              9
  //   ftBoolean,  ftFloat,     ftPhoneNum, ftTime,        ftLocalNum,
       ftBoolean,  ftFloat,     ftString,   ftTime,        ftString,

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
    Val(Copy(TxtLine, 1, Pos(' ', TxtLine)-1), HeaderLineCount, ValCode);
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
        // Type > 100 => float field
        TmpFieldType := ftFloat
      else begin
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
          Name.Text    := TmpName;
          Caption.Text := TmpLabel;
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
          Name.Text := VariableLabel;
        end;
        // Ensure valid variable name.
        Name.Text := TmpName;

        // Summerize field findings.
        TotFieldLength := TotFieldLength + Length;
      end;  // With EField
      EField.EndUpdate;
    end; // For CurrentLine

    // Position for reading and check for corruptness.
    TotFieldLength := TotFieldLength + (((TotFieldLength - 1) DIV 78) + 1) * 3; { MaxRecLineLength = 78 }
    TmpLength := TextPos(TxtFile);
    CloseFile(TxtFile);

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
        Size := CurRec + 1; // TODO : Fix to use an upcomming ADDRECORDS method!!! This is VERY inefficient.
        I := DataStream.Read(CharBuf[0], TotFieldLength);
        if (I <> TotFieldLength) then
        begin
          break;
          // TODO : Exit gracefully when reading beyond last record...
          // Could be an imcomplete record.
        end;

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
    if Assigned(DataStream) then DataStream.Free;
  end;
  result := true;
end;

end.

