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
    Function      UpdateProgress(Percent: Integer; Msg: String): TProgressResult;
    function      Lang(LangCode: Integer; Const LangText: string): String;

(*    function makeNumField(): TEpiField;
    function makeTxtField(): TEpiField;
    function makeBoolField(): TEpiField;
    function makeUpperAlfa(): TEpiField;
    function makeIdNum(): TEpiField;
    function makeDate(): TEpiField;
    function makeToday():TEpiField;
    function makeSoundex(): TEpiField;
    function makeCrypt(): TEpiField; *)
  public
    constructor Create;
    destructor Destroy; override;
    function   QesToDatafile(Const aStream: TStream; var Df: TEpiDataFile): boolean;
    function   QesToDatafile(Const aLines: TStringList; var Df: TEpiDataFile): boolean;
    function   QesToDatafile(Const aFilename: String; var Df: TEpiDataFile): boolean;
    function   DatafileToQes(Const Df: TEpiDatafile; Const aFileName: String): boolean;
    property   OnProgress:  TProgressEvent read FOnProgress write FOnProgress;
    property   OnTranslate: TTranslateEvent read FOnTranslate write FOnTranslate;
  end;

implementation

{ TQesHandler }

function TQesHandler.UpdateProgress(Percent: Integer; Msg: String
  ): TProgressResult;
begin
  Result := prNormal;
  if Assigned(FOnProgress) then
  Begin
    result := FOnProgress(Self, Percent, Msg);
  end;
end;

function TQesHandler.Lang(LangCode: Integer; const LangText: string): String;
begin
  Result := LangText;
  IF Assigned(FOnTranslate) THEN
    Result := FOnTranslate(langcode, Result)
end;

constructor TQesHandler.Create;
begin
  //
end;

destructor TQesHandler.Destroy;
begin
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
begin
  // TODO -o Torsten : Sanity checks!

  FOR LinNum := 0 TO aLines.Count - 1 DO
  BEGIN
    UpdateProgress((LinNum * 100) div aLines.Count, Lang(20440, 'Building dataform'));
(*    MainForm.ProgressBar.Position:=LinNum;
    L:=Lin[LinNum];
    CurLeft:=LeftMargin;
    CurX:=1;
    {$IFNDEF epidat}
    Tallest:=MainForm.Canvas.TextHeight('Wg');
    {$ELSE}
    Tallest:=10;
    {$ENDIF}
    IF Trim(L)='' THEN L:='';
    WHILE Length(L)>0 DO
      BEGIN
      //Check which code is first in the line
        FoersteTegn:=9999;
        n:=pos('#',L);
        IF (n<>0) AND (n<FoersteTegn) THEN FoersteTegn:=n;
        n:=pos('_',L);
        IF (n<>0) AND (n<FoersteTegn) THEN FoersteTegn:=n;
        n:=pos('<',L);
        IF (n<>0) AND (n<FoersteTegn) THEN FoersteTegn:=n;
        IF (FoersteTegn=9999) AND (Trim(L)<>'') THEN LavLabel
        ELSE
          BEGIN
            CASE L[FoersteTegn] OF
              '#': LavNrFelt;
              '_': LavTextFelt;
              '<': LavAndetFelt;
            END;  //Case
          END;  //if
        IF trim(L)='' THEN L:='';
      END;  //while
    CurTop:=CurTop+(Tallest DIV 2);
    CASE LineHeight OF
      0: CurTop:=CurTop+Tallest;              //lineheight=1
      1: CurTop:=CurTop+((Tallest*3) DIV 2);  //Lineheight=1½
      2: CurTop:=CurTop+Tallest+Tallest;      //LineHeight=2
    END;
  END;  //for LinNum
  IF df^.FieldList.Count=0 THEN
    BEGIN
      {$IFNDEF epidat}
      MidLin.Append(Lang(20442));   //'No fields found in QES-file.'
      {$ELSE}
      MidLin.Append('No fields found in QES-file');
      {$ENDIF}
      CreateIndtastningsFormError:=TRUE;
    END
  ELSE
    FOR n:=0 TO df^.FieldList.Count-1 DO
      BEGIN
//        ResetField(df^.FieldList.Items[n]);
        IF PeField(df^.FieldList.Items[n])^.FeltType<>ftQuestion
        THEN INC(df^.NumFields);
      END;  //for
  {$IFNDEF epidat}
  MainForm.ProgressBar.Visible:=False;
  MainForm.StatPanel2.Caption:='';
  MainForm.StatPanel2.Repaint;
  MainForm.Canvas.Font.Assign(OldFont);
  {$ENDIF}
  LineIn:=Midlin.text;
  MidLin.Free;
  Lin.Free;
  OldFont.Free;
  Screen.Cursor:=crDefault;
  Result:=NOT CreateIndtastningsFormError;   *)

  end;
end;

function TQesHandler.QesToDatafile(const aFilename: String; var Df: TEpiDataFile
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
  const aFileName: String): boolean;
begin
  // Todo -o Torsten : Implement DatafileToQes
end;

end.

