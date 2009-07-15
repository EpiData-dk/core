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
  protected
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
  UEpiLog, Math, UEpiDataConstants, FileUtil, UEpiUtils, UStringUtils;

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
begin
  Result := AText;

  IF Result = '' THEN Result := ' ';

  if Df.FieldNaming = fnAuto then
    Result := FirstWord(Result, MaxFieldNameLen)
  else  IF Pos('{', Result) > 0 THEN
    Result := ExtractStrBetween(Result, '{', '}')
  else begin
//    Result := StripWordsFromString(Result, CommonWords);
  end;

(*
        StripCommonWords;
        IF NumValidChars(q)=0 THEN
          BEGIN
            IF df^.FieldList.Count>0 THEN
              BEGIN   //Try to find a name in prev. non-label field
                gfN:=df^.FieldList.Count;
                FoundName:=False;
                WHILE (gfN>0) AND (NOT FoundName) DO
                  BEGIN
                    IF (PeField(df^.FieldList.Items[gfN-1])^.Felttype<>ftQuestion)
                    THEN FoundName:=True;
                    IF NOT FoundName THEN DEC(gfN);
                  END;  //While
                IF FoundName
                THEN TempName:=PeField(df^.FieldList.Items[gfN-1])^.FName
                ELSE TempName:='FIELD1';
                WHILE Length(TempName)<FieldNameLen DO TempName:=TempName+' ';
              END;  //if feltliste.count>0
          END    //if numvalidchars=0
        ELSE
          BEGIN   //Construct name from question
            QIndex:=0;
            NameIndex:=0;
            WHILE (QIndex<Length(q)) AND (NameIndex<FieldNameLen) DO
              BEGIN
                INC(QIndex);
                IF (q[QIndex] in AlfaNumChars) THEN
                  BEGIN
                    INC(NameIndex);
                    TempName[NameIndex]:=q[QIndex];
                  END;  //if char in AlfaNumChars
              END;   //while
          END;   //construct name from question
      END;  //if there were no curly brackets
    END;  //if tempName=NoName

    if not CheckVariableName(Result, AlfaNumChars) then
      Result := 'FIELD1';
    if (Length(Result) > 0) and (Result[1] in NumChars) then
      Result := 'N' + Result;

  IF (TempName=' ') or (TempName=Noname) then tempname:='FIELD1';
  //IF AnsiLowercase(TempName)='date' THEN TempName:=TempName+'1';     Funktion fjernet 080404 - bør erstattes med warning
  WHILE Length(TempName)<FieldNameLen DO TempName:=TempName+' ';
  IF Length(TempName)>FieldNameLen THEN TempName:=Copy(TempName,1,FieldNameLen);
  IF NOT NameIsUnique(Tempname,df,FieldNameLen) THEN REPEAT UNTIL NameIsUnique(TempName,df,FieldNameLen);
  Case FieldNameCase OF
    fcUpper: TempName:=AnsiUpperCase(TempName);
    fcLower: TempName:=AnsiLowerCase(TempName);
  END;  //case;
  GetFieldName:=TempName;    *)
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


(*
  INC(FeltNr);
  FeltNavn:=GetFieldName(COPY(L,1,FeltStart-1));
  New(eField);
  ResetField(eField);
  AntalDecimaler:=0;
  IF AntalKomma=1 THEN
    BEGIN
      AntalDecimaler:=Length(Indhold)-Pos('.',Indhold);
      IF Pos('.',Indhold)=0 THEN
        AntalDecimaler:=Length(Indhold)-Pos(',',Indhold);
    END;
  WITH eField^ DO
    BEGIN
      FName:=FeltNavn;
      FLength:=FeltSlut-FeltStart+1;
      FNumDecimals:=AntalDecimaler;
      IF (AntalKomma=1) OR (FLength>9) THEN Felttype:=ftFloat
      ELSE Felttype:=ftInteger;
      IF FeltStart>1 THEN    //is there question before the field?
        BEGIN
          FQuestTop:=CurTop+2;
          FQuestLeft:=CurLeft;
          FQuestion:=RemoveCurly(COPY(L,1,FeltStart-1));
          FOriginalQuest:=FQuestion;
          TabsInNextField:=0;
          WHILE FQuestion[Length(FQuestion)]='@' DO
            BEGIN
              INC(TabsInNextField);
              FQuestion:=COPY(FQuestion,1,Length(FQuestion)-1);
            END;  //While
          FVariableLabel:=trim(FQuestion);
          IF (NOT df^.EpiInfoFieldNaming) AND (trim(FQuestion)<>'') THEN
            BEGIN
              s:=FirstWord(FVariableLabel);
              Delete(FVariableLabel,Pos(s,FVariableLabel),Length(s));
              FVariableLabel:=trim(FVariableLabel);
              IF df^.UpdateFieldnameInQuestion THEN
                BEGIN
                  s:=trim(FirstWord(FQuestion));
                  tt:=Pos(s,FQuestion);
                  Delete(FQuestion,tt,Length(s));
                  Insert(trim(FName),FQuestion,tt);
                  s:=trim(FirstWord(FOriginalQuest));
                  tt:=Pos(s,FOriginalQuest);
                  Delete(FOriginalQuest,tt,Length(s));
                  Insert(trim(FName),FOriginalQuest,tt);
                END;
            END;
          FQuestY:=LinNum+1;
          FQuestX:=CurX;
          INC(CurX,Length(FOriginalQuest));  // tidligere FQuestion
          {$IFNDEF epidat}
          ObjHeight:=MainForm.Canvas.TextHeight(FQuestion);
          ObjWidth:=MainForm.Canvas.TextWidth(FQuestion);
          {$ENDIF}
          INC(CurLeft,ObjWidth);
          IF ObjHeight>Tallest THEN Tallest:=ObjHeight;
        END;   //if label before field
      FLength:=FeltSlut-FeltStart+1;
      {$IFNDEF epidat}
      FFieldWidth:=(MainForm.Canvas.TextWidth('9')*(FLength+2))+6;
      {$ENDIF}
      FFieldTop:=CurTop;
      IF TabsInNextField>0 THEN
        BEGIN
          CurLeft:=((Curleft DIV EvenTabValue)+
                TabsInNextField)*EvenTabValue;
          TabsInNextField:=0;
        END;
      FFieldLeft:=CurLeft;
      FFieldY:=LinNum+1;
      FFieldX:=CurX;
      FFieldText:='';
      INC(CurLeft,FFieldWidth);
      t:=FLength;
    END;   //with eField do
  df^.FieldList.Add(eField);
  Delete(L,1,FeltSlut);
  INC(CurX,t);
    END;  //if AntalKomma>1 *)
end;

function TQesHandler.makeTxtField(StartPos: Integer): TEpiField;
begin

end;

function TQesHandler.makeOtherField(StartPos: Integer): TEpiField;
begin

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

