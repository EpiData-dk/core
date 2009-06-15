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
    FLines:       TStringList;
    FCurLine:     UTF8String;
    Function      UpdateProgress(Percent: Integer; Msg: UTF8String): TProgressResult;
    function      Lang(LangCode: Integer; Const LangText: UTF8String): UTF8String;

    function      makeLabel(): TEpiField;
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
    property      CurLine: UTF8String read FCurLine write FCurLine;
  public
    constructor Create;
    destructor Destroy; override;
    function   QesToDatafile(Const aStream: TStream; var Df: TEpiDataFile): boolean; overload;
    function   QesToDatafile(Const aLines: TStringList; var Df: TEpiDataFile): boolean; overload;
    function   QesToDatafile(Const aFilename: UTF8String; var Df: TEpiDataFile): boolean; overload;
    function   DatafileToQes(Const Df: TEpiDatafile; Const aFileName: UTF8String): boolean;
    property   OnProgress:  TProgressEvent read FOnProgress write FOnProgress;
    property   OnTranslate: TTranslateEvent read FOnTranslate write FOnTranslate;
  end;

implementation

uses
  UDebug, Math, UEpiDataConstants;

{ TQesHandler }

function TQesHandler.UpdateProgress(Percent: Integer; Msg: UTF8String
  ): TProgressResult;
begin
  Result := prNormal;
  if Assigned(FOnProgress) then
  Begin
    result := FOnProgress(Self, Percent, Msg);
  end;
end;

function TQesHandler.Lang(LangCode: Integer; const LangText: UTF8String): UTF8String;
begin
  Result := LangText;
  IF Assigned(FOnTranslate) THEN
    Result := FOnTranslate(langcode, Result)
end;

function TQesHandler.makeLabel(): TEpiField;
var
  fName: widestring;
begin
  fName := ''; //GetFieldName('Label'+IntToStr(LabelNo));
(*
  Result := TEpiField.Create();
  WITH Result DO
  BEGIN
    FieldName := fName;
    FeltType:=ftQuestion;
    FLength:=0;
    FQuestion:=L;
    FOriginalQuest:=L;
    FQuestTop:=CurTop+2;
    FQuestLeft:=CurLeft;
    FQuestY:=LinNum+1;
    FQuestX:=CurX;
    {$IFNDEF epidat}
    ObjHeight:=MainForm.Canvas.TextHeight(FQuestion);
    ObjWidth:=MainForm.Canvas.TextWidth(FQuestion);
    {$ENDIF}
    INC(CurLeft,ObjWidth);
    IF ObjHeight>Tallest THEN Tallest:=ObjHeight;
  END;
  df^.FieldList.Add(eField);
  IF Length(L)>80 THEN
    BEGIN
      Delete(L,1,80);
      INC(CurX,80);
     END
  ELSE L:=''; *)
end;

function TQesHandler.makeNumField(StartPos: Integer): TEpiField;
begin

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
end;

destructor TQesHandler.Destroy;
begin
  if Assigned(FLines) then FreeAndNil(FLines);
  inherited Destroy;
end;

function TQesHandler.QesToDatafile(const aStream: TStream; var Df: TEpiDataFile
  ): boolean;
var
  aLines: TStringList;
begin
  aLines := TStringList.Create;
  try
    aLines.LoadFromStream(aStream);
    result := QesToDatafile(aLines, df);
  finally
    if Assigned(aLines) then FreeAndNil(aLines);
  end;
end;

function TQesHandler.QesToDatafile(const aLines: TStringList;
  var Df: TEpiDataFile): boolean;
var
  LinNum: Integer;
  FirstPos: Integer;
  N: Int64;
  TmpField: TEpiField;
begin
  Debugger.IncIndent;
  Debugger.Add(Classname, 'QesToDatafile', 2);
  FLines.Assign(aLines);

  Result := false;

  // TODO -o Torsten : Sanity checks!
  if not Assigned(Df) then
    Df := TEpiDataFile.Create([eoIgnoreChecks, eoIgnoreIndex, eoIgnoreRelates, eoInMemory])
  else
    Df.Reset;

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
          TmpField := MakeLabel
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
    Debugger.DecIndent;
  end;
end;

function TQesHandler.QesToDatafile(const aFilename: UTF8String; var Df: TEpiDataFile
  ): boolean;
var
  aLines: TStringList;
begin
  aLines := TStringList.Create;
  try
    aLines.LoadFromFile(aFilename);
    Result := QesToDatafile(aLines, Df);
  finally
    if Assigned(aLines) then FreeAndNil(aLines);
  end;
end;

function TQesHandler.DatafileToQes(const Df: TEpiDatafile;
  const aFileName: UTF8String): boolean;
begin
  // Todo -o Torsten : Implement DatafileToQes
end;

end.

