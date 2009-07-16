unit UQesHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UEpiDataFile, UDataFileTypes;

type

  { TQesHandler }

  TQesHandler = class(TObject)
  private
    FOnProgress:  TProgressEvent;
    FOnTranslate: TTranslateEvent;
    FDf:          TEpiDataFile;
    FLines:       TStringList;
    CurLine:      string;
    FCurX:        integer;
    LabelNo:      integer;
    Function      UpdateProgress(Percent: Integer; Msg: string): TProgressResult;
    function      Lang(LangCode: Integer; Const LangText: string): string;
    function      ExtractFieldName(const AText: string): string;
    function      NumValidChars(Const AText: string): integer;
  protected
    function      makeField(Ft: TFieldType; PosStart, PosEnd: Integer): TEpiField;
    function      makeLabel(LineNum: Integer): TEpiField;
    function      makeNumField(StartPos: Integer): TEpiField;
    function      makeTxtField(StartPos: Integer): TEpiField;
    function      makeOtherField(StartPos: Integer): TEpiField;
    function      makeBoolField(StartPos: Integer): TEpiField;
    function      makeUpperAlfa(StartPos: Integer): TEpiField;
    function      makeIdNum(StartPos: Integer): TEpiField;
    function      makeDate(StartPos: Integer): TEpiField;
    function      makeToday(StartPos: Integer):TEpiField;
    function      makeSoundex(StartPos: Integer): TEpiField;
    function      makeCrypt(StartPos: Integer): TEpiField;
    property      CurX: integer read FCurX write FCurX;
    property      Df: TEpiDataFile read FDf write FDf;
  public
    constructor Create;
    destructor Destroy; override;
    function   QesToDatafile(Const aStream: TStream; var DataFile: TEpiDataFile): boolean; overload;
    function   QesToDatafile(Const aLines: TStringList; var DataFile: TEpiDataFile): boolean; overload;
    function   QesToDatafile(Const aFilename: string; var DataFile: TEpiDataFile): boolean; overload;
    function   DatafileToQes(Const DataFile: TEpiDatafile; Const aFileName: string): boolean;
    property   OnProgress:  TProgressEvent read FOnProgress write FOnProgress;
    property   OnTranslate: TTranslateEvent read FOnTranslate write FOnTranslate;
  end;

implementation

uses
  UEpiLog, Math, UEpiDataGlobals, FileUtil, UEpiUtils, UStringUtils;

{ TQesHandler }

function TQesHandler.UpdateProgress(Percent: Integer; Msg: string
  ): TProgressResult;
begin
  Result := prNormal;
  if Assigned(FOnProgress) then
  Begin
    result := FOnProgress(Self, Percent, Msg);
  end;
end;

function TQesHandler.Lang(LangCode: Integer; const LangText: string): string;
begin
  Result := LangText;
  IF Assigned(FOnTranslate) THEN
    Result := FOnTranslate(langcode, Result)
end;

function TQesHandler.ExtractFieldName(const AText: string): string;
var
  i: Integer;
  TmpStr: String;
begin
  IF AText = '' THEN Result := ' ';

  if Df.FieldNaming = fnAuto then
    // EpiInfo fieldnaming style.
    Result := FirstWord(AText, MaxFieldNameLen)
  else  IF Pos('{', Result) > 0 THEN
    // Explicit force fieldname by using { and }.
    Result := ExtractStrBetween(AText, '{', '}')
  else begin
    // Normal guessing based on text.

    TmpStr := StripWordsFromString(AText, CommonWords);
    if (NumValidChars(TmpStr) = 0) and (Df.QuestFields.Count > 0) then
      // Guess no good - try to find a name in prev. non-label field
      Result := StripWordsFromString(Df.QuestFields[Df.QuestFields.Count-1].Question, CommonWords)
    else begin
      //Construct name from question
      i := 0;
      while (i < Length(TmpStr)) and (i < MaxFieldNameLen) do
      begin
        if TmpStr[i] in AlfaNumChars then
          Result := Result + TmpStr[i];
      end;
    end;
  end;

  // Sanity checks.
  if not CheckVariableName(Result, AlfaNumChars) then
    Result := 'FIELD1';
  if (Length(Result) > 0) and (Result[1] in NumChars) then
    Result := 'N' + Result;
  if Length(Result) > MaxFieldNameLen then
    Result := Copy(Result, 1, MaxFieldNameLen);
  Result := Df.CreateUniqueFieldName(Result);
end;

function TQesHandler.NumValidChars(const AText: string): integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(AText) do
    if AText[i] in AlfaNumChars then
      Inc(result);
end;

function TQesHandler.makeField(Ft: TFieldType; PosStart, PosEnd: Integer): TEpiField;
var
  Tmpstr: String;
begin
  result := TEpiField.Create;
  With result do
  begin
    FieldType     := Ft;
    FieldName     := ExtractFieldName(Copy(CurLine, 1, PosStart - 1));
    FieldLength   := PosEnd - PosStart + 1;
    // TODO: QuestX of Y!
    QuestX        := 0;
    QuestY        := 0;
    FieldX        := 0;
    FieldY        := 0;
    Question      := StringReplace(Copy(CurLine, 1, PosStart - 1), '{', '', [rfIgnoreCase, rfReplaceAll]);
    Question      := StringReplace(Question, '}', '', [rfIgnoreCase, rfReplaceAll]);
    VariableLabel := trim(Question);
    if (Df.FieldNaming = fnFirstWord) and (VariableLabel <> '') then
    begin
      Tmpstr := FirstWord(VariableLabel, Length(VariableLabel));
      VariableLabel := Copy(VariableLabel, Length(TmpStr) + 1, Length(VariableLabel));
    end;
  end;
  Df.AddField(result);
  Delete(CurLine, 1, PosEnd);
end;

function TQesHandler.makeLabel(LineNum: Integer): TEpiField;
var
  fName: string;
begin
  FName := Format('Label%d', [PostInc(LabelNo)]);

  Result := TEpiField.Create();
  WITH Result DO
  BEGIN
    FieldName    := fName;
    FieldType    := ftQuestion;
    FieldLength  := 0;
    Question     := CurLine;
    QuestX       := CurX;
    QuestY       := LineNum;
  END;
end;

function TQesHandler.makeNumField(StartPos: Integer): TEpiField;
VAR
  I:Integer;
  St, En: Integer;
  NumStr: String;
  TmpField: TEpiField;
  Tmpstr: String;
BEGIN
  result := nil;

  St := StartPos;
  WHILE (CurLine[StartPos] in ['#', ',', '.'])  AND (StartPos <= Length(CurLine)) DO
    INC(StartPos);
  En := StartPos - 1;

  IF (En - St + 1) > 18 THEN    //is numberfield longer than 18 chars?
  BEGIN
    Df.ErrorCode := EPI_QES_FAILED;
    // TODO -o Torsten : LineNum
    Df.ErrorText := Format(Lang(20420, 'Number field in line %d exceeds maximum length of 18 characters'), [0]);
    Delete(CurLine, 1, En);
    CurX := CurX + En;
    Exit;
  END;

  NumStr := Copy(CurLine, St, En - St + 1);

  if (StrCountChars(NumStr, ['.', ',']) > 1) or (NumStr[Length(NumStr)] in ['.', ',']) then
  begin
    Df.ErrorCode := EPI_QES_FAILED;
    // TODO -o Torsten : LineNum
    Df.ErrorText := Format(Lang(20422, 'Error in floating point field in line %d:'), [0]);
    Delete(CurLine, 1, En);
    Exit;
  end;

  TmpField := makeField(ftInteger, St, En);
  if (StrCountChars(NumStr, ['.', ',']) > 0) then
  begin
    Tmpstr      := BoolToStr(Pos('.', NumStr) > 0, '.', ',');
    TmpField.NumDecimals := Length(NumStr) - Pos(TmpStr, NumStr);
  end;
end;

function TQesHandler.makeTxtField(StartPos: Integer): TEpiField;
var
  St, En: Integer;
begin
  St := StartPos;

  WHILE (CurLine[StartPos] in ['_']) AND (StartPos <= Length(CurLine)) DO
    INC(StartPos);
  En := StartPos - 1;

  if (En - St) + 1 > 80 then
  begin
    Df.ErrorCode := EPI_QES_FAILED;
    // TODO -o Torsten : LineNum
    Df.ErrorText := Format(Lang(20424, 'Text field in line %d exceeds maximum length of 80 characters:'), [0]);
    Delete(CurLine, 1, En);
    Exit;
  end;

  makeField(ftAlfa, St, En);
end;

function TQesHandler.makeOtherField(StartPos: Integer): TEpiField;
var
  St, En: LongInt;
  FieldCode: String;
begin
  St := StartPos;
  En := Pos('>', CurLine);

  if En < St then
  begin
    Df.ErrorCode := EPI_QES_FAILED;
    // TODO -o Torsten : LineNum
    Df.ErrorText := Format(Lang(20432, 'A <...> field without closing-bracket in line %d:'), [0]);
    Exit;
  end;

  FieldCode := AnsiUpperCase(Copy(CurLine, St + 1, En - St));
(*  case FieldCode[1] of
    'Y': begin
           if Length(FieldCode) = 1 then
             result := makeBoolField(St)

         end;
  end;


    BEGIN
      FeltSlut:=pos('>',L);
      CodeFound:=ANSIUpperCase(COPY(L,FeltStart,FeltSlut-FeltStart+1));
      IF (CodeFound='<Y>') THEN MakeBoolean;
      IF COPY(CodeFound,1,2)='<A' THEN MakeUpperAlfa;
      IF COPY(CodeFound,1,6)='<IDNUM' THEN MakeIDNum;
      IF (COPY(CodeFound,1,6)='<MM/DD') OR
         (COPY(CodeFound,1,6)='<DD/MM') OR
         (CodeFound='<YYYY/MM/DD>') THEN MakeDate;  //&&
      IF COPY(CodeFound,1,6)='<TODAY' THEN MakeToday;
      IF COPY(CodeFound,1,2)='<S' THEN MakeSoundex;
      IF COPY(CodeFound,1,2)='<E' THEN MakeCrypt;  //&&
      IF CodeFound<>'Done' THEN
        BEGIN
          {$IFNDEF epidat}
          MidLin.Append(Format(Lang(20434),[LinNum+1]));  //'Unknown code found in line %d:'
          {$ELSE}
          MidLin.Append(Format('Unknown code found in line %d:',[LinNum+1]));
          {$ENDIF}
          MidLin.Append(Lin[LinNum]);
          MidLin.Append(' ');
          CreateIndtastningsFormError:=TRUE;
          Delete(L,1,FeltSlut);
        END;   //if CodeFound not Done
      END;  //if slut-tegn mangler   *)
end;

function TQesHandler.makeBoolField(StartPos: Integer): TEpiField;
begin

end;

function TQesHandler.makeUpperAlfa(StartPos: Integer): TEpiField;
begin

end;

function TQesHandler.makeIdNum(StartPos: Integer): TEpiField;
begin

end;

function TQesHandler.makeDate(StartPos: Integer): TEpiField;
begin

end;

function TQesHandler.makeToday(StartPos: Integer): TEpiField;
begin

end;

function TQesHandler.makeSoundex(StartPos: Integer): TEpiField;
begin

end;

function TQesHandler.makeCrypt(StartPos: Integer): TEpiField;
begin

end;

constructor TQesHandler.Create;
begin
  FLines := TStringList.Create;
  LabelNo := 1;
end;

destructor TQesHandler.Destroy;
begin
  if Assigned(FLines) then FreeAndNil(FLines);
  inherited Destroy;
end;

function TQesHandler.QesToDatafile(const aStream: TStream; var DataFile: TEpiDataFile
  ): boolean;
var
  aLines: TStringList;
begin
  aLines := TStringList.Create;
  try
    aLines.LoadFromStream(aStream);
    result := QesToDatafile(aLines, DataFile);
  finally
    if Assigned(aLines) then FreeAndNil(aLines);
  end;
end;

function TQesHandler.QesToDatafile(const aLines: TStringList;
  var Datafile: TEpiDataFile): boolean;
var
  LinNum: Integer;
  FirstPos: Integer;
  N: Int64;
  TmpField: TEpiField;
begin
  EpiLogger.IncIndent;
  EpiLogger.Add(Classname, 'QesToDatafile', 2);
  FLines.Assign(aLines);

  Result := false;
  Df := DataFile;

  // TODO -o Torsten : Sanity checks!
  if not Assigned(Df) then
    Df := TEpiDataFile.Create([eoIgnoreChecks, eoIgnoreIndex, eoIgnoreRelates, eoInMemory])
  else
    Df.Reset;

  CurX := 1;

  try
    FOR LinNum := 0 TO aLines.Count - 1 DO
    BEGIN
      UpdateProgress((LinNum * 100) div aLines.Count, Lang(20440, 'Building dataform'));
      CurLine := aLines[LinNum];

      IF Trim(CurLine) = '' THEN CurLine := '';
      WHILE Length(CurLine)>0 DO
      BEGIN
        //Check which code is first in the line
        FirstPos := MaxInt;
        N := pos('#', CurLine);
        IF (N > 0) Then FirstPos := Max(N, FirstPos);
        N := pos('_', CurLine);
        IF (N > 0) Then FirstPos := Max(N, FirstPos);
        N := pos('<', CurLine);
        IF (N > 0) Then FirstPos := Max(N, FirstPos);
        IF (FirstPos = MaxInt) AND (Trim(CurLine) <> '') THEN
          TmpField := MakeLabel(LinNum)
        ELSE
          BEGIN
            CASE CurLine[FirstPos] OF
              '#': TmpField := makeNumField(FirstPos);
              '_': TmpField := makeTxtField(FirstPos);
              '<': TmpField := makeOtherField(FirstPos);
            END;  //Case
          END;  //if
        Df.AddField(TmpField);
        IF trim(CurLine) = '' THEN CurLine := '';
      END;  //while
(*      CurTop:=CurTop+(Tallest DIV 2);
      CASE LineHeight OF
        0: CurTop:=CurTop+Tallest;              //lineheight=1
        1: CurTop:=CurTop+((Tallest*3) DIV 2);  //Lineheight=1½
        2: CurTop:=CurTop+Tallest+Tallest;      //LineHeight=2
      END; *)
    END;  //for LinNum
    IF Df.NumDataFields = 0 THEN
    BEGIN
      Df.ErrorText := Lang(20442, 'No fields found in QES-file');
      Df.ErrorCode := EPI_QES_NO_FIELDS;
      Exit;
    end;

  finally
    EpiLogger.DecIndent;
    DataFile := Df;
  end;
end;

function TQesHandler.QesToDatafile(const aFilename: string; var DataFile: TEpiDataFile
  ): boolean;
var
  aLines: TStringList;
begin
  aLines := TStringList.Create;
  try
    aLines.LoadFromFile(aFilename);
    Result := QesToDatafile(aLines, DataFile);
  finally
    if Assigned(aLines) then FreeAndNil(aLines);
  end;
end;

function TQesHandler.DatafileToQes(const DataFile: TEpiDatafile;
  const aFileName: string): boolean;
begin
  // Todo -o Torsten : Implement DatafileToQes
end;

end.

