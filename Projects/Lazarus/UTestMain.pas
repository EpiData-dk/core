unit UTestMain;

interface

uses
  {Windows,} Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls, Grids, UEpiDataFile, UPWform,
  LResources;

type

  { TForm1 }

  TForm1 = class(TForm)
    Panel1: TPanel;
    edInputFilename: TLabeledEdit;
    OpenDialog1: TOpenDialog;
    SpeedButton1: TSpeedButton;
    Button1: TButton;
    edOutputFilename: TLabeledEdit;
    Button2: TButton;
    SpeedButton2: TSpeedButton;
    SaveDialog1: TSaveDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    sg: TStringGrid;
    TabSheet3: TTabSheet;
    Memo2: TMemo;
    TabSheet4: TTabSheet;
    memoOrigCheckFile: TMemo;
    TabSheet5: TTabSheet;
    memoIntepredCheck: TMemo;
    Panel2: TPanel;
    checkShowLabels: TCheckBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
//    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    function  DocumentDataFile:string;
    procedure LoadData(ShowAsLabels:boolean);
    procedure checkShowLabelsClick(Sender: TObject);
  private
    { Private declarations }
    procedure GetPassword(Sender: TObject; requesttype:TRequestPasswordTypes; var password:String);
  public
    { Public declarations }
    procedure ErrorMsg(s:string);
    procedure out(s:string);
  end;

var
  Form1: TForm1;

implementation


Uses
  UEpiUtils,UEpiTypes, UValueLabels, UeFields;

var
  epd: TEpiDataFile;

procedure TForm1.ErrorMsg(s:string);
begin
  MessageDlg(s,mtError,[mbOK],0);
end;

procedure TForm1.out(s:string);
begin
  memo1.Lines.append(s);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if sender=SpeedButton1 then if OpenDialog1.Execute then edInputFilename.Text:=OpenDialog1.FileName;
  if sender=SpeedButton2 then if SaveDialog1.Execute then edOutputFilename.Text:=SaveDialog1.FileName;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if assigned(epd) then FreeAndNil(epd);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
  epd:=NIL;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  s:string;
begin
  memo1.Clear;
  if assigned(epd) then FreeAndNil(epd);
  if trim(edInputFilename.Text)='' then
    begin
      ErrorMsg('No filename for input file entered');
      exit;
    end;
  if (not fileexists(edInputFilename.text)) then
    begin
      ErrorMsg('Input file does not exist');
      exit;
    end;
  epd:=TEpiDataFile.Create;
  epd.OnRequestPassword := @GetPassword;
  out('Datafile: '+edInputFilename.text);
  if epd.Open(edInputFilename.Text,[eoInMemory, oeIgnoreIndex]) then
    begin
      out('Data file opened with succes');
      out('Num fields = '+inttostr(epd.NumFields));
      out('Num data fields = '+inttostr(epd.NumDataFields));
      out('Num records = '+inttostr(epd.NumRecords));
      s:=DocumentDataFile;
      memo2.Lines.Text:=s;
      checkShowLabels.Checked:=(epd.ValueLabels.Count>0);
      //LoadData(epd.ValueLabels.count>0);
      if(epd.HasCheckFile) then
        begin
          memoOrigCheckFile.Lines.LoadFromFile(epd.ChkFilename);
          memoIntepredCheck.Lines.Text:=epd.GetCheckLines;
        end;
    end
  else
    begin
      out('Could not open data file');
      out('Errorcode: '+inttostr(epd.ErrorCode));
      out('Errortext: '+epd.ErrorText);
    end;
end;
{
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if assigned(epd) then FreeAndNil(epd);
end;    }

function TForm1.DocumentDataFile:string;
type
TCharSet=Set of Char;

var
  res: TStringList;
  FileSiz:LongInt;
  SizeUnit,tmpStr,tmpType,tmpWidth: string;
  UsesValueLabels:boolean;
  nN,nN2,nN3,FieldNumber:integer;
  QuestStr,CheckStr,ValLabelStr:ARRAY [1..25] OF String[20];
  AutoList:Tstringlist;
  aValueLabelSet: TValueLabelSet;
  aValue,aLabel,valuelabelname: string;

  Function CutString(VAR s:String; ch:TCharSet; wid:Integer):String;
  VAR
    LastOccur, cN:Integer;
  BEGIN
    IF Length(s)<=wid THEN
      BEGIN
        Result:=s;
        s:='';
      END
    ELSE
      BEGIN
        LastOccur:=0;
        FOR cN:=1 TO Wid DO
          IF (s[cN] in ch) THEN LastOccur:=cN;
        IF LastOccur=0 THEN LastOccur:=Wid;
        Result:=Copy(s,1,LastOccur);
        Delete(s,1,LastOccur);
      END;
  END;  //End CutString


begin
  result:='';
  if (not assigned(epd)) then exit;
  res:=TStringList.create;
  try
    {Write heading}
    res.Append('');
    res.Append(Format('DATAFILE: %s',[epd.RecFilename]));
    res.append('Filelabel: '+epd.Filelabel);
    res.Append('');

    {Write datafile information}
    //FileSiz:=FileSize(df^.Datfile);
    FileSiz:=epd.DatafileSize;
    SizeUnit:=' bytes';
    IF FileSiz>10000 THEN
      BEGIN
        FileSiz:=FileSiz DIV 1024;
        SizeUnit:=' kb';
      END;
    res.Append(FitLength('Filesize:',23)+IntToStr(FileSiz)+SizeUnit);
    res.Append(FitLength('Last revision:',23)+
      FormatDateTime('d. mmm yyyy t',
      FileDateToDateTime(FileAge(epd.RecFilename))));
    res.Append(FitLength('Number of fields:',23)+IntToStr(epd.NumFields));
    tmpStr:=FitLength('Number of records:',23);
    IF epd.NumRecords=-1
    THEN tmpStr:=tmpStr+'Error in datafile. Number of records cannot be counted.'
    ELSE tmpStr:=tmpStr+IntToStr(epd.NumRecords);
    res.Append(tmpStr);
    tmpStr:=FitLength('Checks applied:',23);
    IF (epd.HasCheckFile) and (not epd.ErrorInCheckFile)
    THEN tmpStr:=tmpStr+format('Yes (Last revision %s)',
      [FormatDateTime('d. mmm yyyy t',FileDateToDateTime(FileAge(epd.ChkFilename)))])
    ELSE tmpStr:=tmpStr+'No';
    res.Append(tmpStr);
    IF epd.ErrorInCheckFile THEN res.Append(cFill(' ',23)+'Warning: A checkfile exists but it contains errors.');
    res.Append('');

    {Check if value labels are used}
    UsesValueLabels:=((epd.ValueLabels.count>0) AND (not epd.ErrorInCheckFile) AND (epd.HasCheckFile));

    {Write variable information}
    res.Append('');
    res.Append('Fields in datafile:');
    res.Append('');
{
1   5          16                    38              54     61                    83
-------------------------------------------------------------------------------------------------------
NUM Name       Variable label        Type            Width  Checks                Value labels
-------------------------------------------------------------------------------------------------------
100 andekeltke Abcdefghijklmnopqrst  Integer                Must enter, repeat
           uvxyzæøp              Upper-case text        Legal: 5-10,14,45,
                                 Boolean                  46,50
                                 Floating point  7:3    Jumps:
                                 US date                  2>IFDLE
                                 Today's date             3>IUWLW

3   10         20                    15              5      20                    20
}


    IF UsesValueLabels
    THEN res.Append(Format('%3s %-10s %-20s  %-15s %-5s  %-20s  %-20s',
      ['No','Name','Variable label','Fieldtype','Width','Checks','Value labels']))
      {20628=No.
      20630=Name
      20632=Variable label
      20634=Fieldtype
      20636=Width
      20638=Checks
      20640=Value labels}
    ELSE res.Append(Format('%3s %-10s %-20s  %-15s %-5s  %-20s',
      ['No.','Name','Variable label','Fieldtype','Width','Checks']));
    IF UsesValueLabels THEN res.Append(cFill('-',102))
    ELSE res.Append(cFill('-',80));
    FieldNumber:=0;
    FOR nN:=0 TO epd.NumFields-1 DO
      BEGIN
        IF epd[nN].Fieldtype<>ftQuestion THEN
          BEGIN
            INC(FieldNumber);
            {Reset arrays}
            FOR nN2:=1 TO 25 DO
              BEGIN
                QuestStr[nN2]:='';
                CheckStr[nN2]:='';
                ValLabelStr[nN2]:='';
              END;
            {Put Var-label in array}
            tmpStr:=epd[nN].VariableLabel;
            WHILE Pos('@',tmpStr)>0 DO Delete(tmpStr,Pos('@',tmpStr),1);
            nN2:=1;
            WHILE (tmpStr<>'') AND (nN2<=25) DO
              BEGIN
                QuestStr[nN2]:=CutString(tmpStr,[' '],16);
                INC(nN2);
              END;
            IF tmpStr<>'' THEN
              BEGIN
                IF Length(tmpStr)>16 THEN tmpStr:=Copy(tmpStr,1,16);
                tmpStr:=tmpStr+'...';
              END;

            {Put Checks in array}
            IF (epd.HasCheckFile) AND (NOT epd.ErrorInCheckFile) THEN
              BEGIN
                nN2:=1;
                IF epd[nN].Index>0 THEN
                  BEGIN
                    tmpStr:='Key ';
                    IF epd.IndexIsUnique[epd[nN].Index] THEN tmpStr:=tmpStr+'unique ';
                    tmpStr:=tmpStr+IntToStr(epd[nN].Index);
                    CheckStr[nN2]:=tmpStr;
                    INC(nN2);
                  END;
                IF epd[nN].Autosearch THEN
                  BEGIN
                    tmpStr:='Autosearch ';
                    //IF FAutoList THEN tmpStr:=tmpStr+' LIST ';
                    TRY
                      autolist:=TStringList.Create;
                      autolist.CommaText:=epd[nN].AutoFields;;
                      FOR nN3:=0 TO autolist.count-1 DO tmpStr:=tmpStr+trim(epd[strtoint(autolist[nN3])].Fieldname);
                    FINALLY
                      autolist.Free;
                    END;
                    CheckStr[nN2]:=tmpStr;
                    INC(nN2);
                  END;
                IF epd[nN].MustEnter THEN
                  BEGIN
                    CheckStr[nN2]:='Must enter';
                    INC(nN2);
                  END;
                IF epd[nN].doRepeat THEN
                  BEGIN
                    CheckStr[nN2]:='Repeat';
                    INC(nN2);
                  END;
                IF epd[nN].DefaultValue<>'' THEN
                  BEGIN
                    CheckStr[nN2]:='Default value='+epd[nN].DefaultValue;
                    INC(nN2);
                  END;
                IF (epd[nN].MissingValues[0]<>'') THEN
                  BEGIN
                    CheckStr[nN2]:='Missing value='+epd[nN].MissingValues[0];
                    IF (epd[nN].MissingValues[1]<>'') THEN CheckStr[nN2]:=CheckStr[nN2]+','+epd[nN].MissingValues[1];
                    IF (epd[nN].MissingValues[2]<>'') THEN CheckStr[nN2]:=CheckStr[nN2]+','+epd[nN].MissingValues[2];
                  END;
                IF epd[nN].NoEnter THEN
                  BEGIN
                    CheckStr[nN2]:='NoEnter';
                    INC(nN2);
                  END;
                IF epd[nN].Legal<>'' THEN
                  BEGIN
                    tmpStr:='Legal: '+trim(epd[nN].Legal);
                    WHILE Pos('"',tmpStr)>0 DO
                      Delete(tmpStr,Pos('"',tmpStr),1);
                    WHILE (tmpStr<>'') AND (nN2<=25) DO
                      BEGIN
                        CheckStr[nN2]:=CutString(tmpStr,[' ',','],20);
                        INC(nN2);
                      END;
                    IF tmpStr<>'' THEN
                      BEGIN
                        IF Length(tmpStr)>17
                        THEN tmpStr:=Copy(tmpStr,1,17);
                        tmpStr:=tmpStr+'...';
                      END;
                  END;
                IF epd[nN].Jumps<>'' THEN
                  BEGIN
                    tmpStr:='Jumps: '+trim(epd[nN].Jumps);
                    WHILE Pos('"',tmpStr)>0 DO
                      Delete(tmpStr,Pos('"',tmpStr),1);
                    WHILE (tmpStr<>'') AND (nN2<=25) DO
                      BEGIN
                        CheckStr[nN2]:=CutString(tmpStr,[' ',','],20);
                        INC(nN2);
                      END;
                    IF tmpStr<>'' THEN
                      BEGIN
                        IF Length(tmpStr)>17
                        THEN tmpStr:=Copy(tmpStr,1,17);
                        tmpStr:=tmpStr+'...';
                      END;
                  END;
                IF (epd[nN].AfterCmds<>NIL) or (epd[nN].BeforeCmds<>NIL) THEN
                  BEGIN
                    IF nN2=25 THEN CheckStr[25]:='More: See Checkfile'
                    ELSE CheckStr[nN2]:='More: See Checkfile';
                  END;
              END;  //if not errorInCheckfile

            {Put value labels in array}
            IF (UsesValueLabels) AND (epd[nN].Valuelabel<>NIL) THEN
              BEGIN
                aValueLabelSet:=epd.Fields[nN].Valuelabel;
                nN2:=1;
                WHILE (nN2<=aValueLabelSet.count) AND (nN2<25) DO
                  BEGIN
                    aValue:=aValueLabelSet.Values[nN2-1];
                    aLabel:=aValueLabelSet.Labels[nN2-1];
                    IF aValue[1]<>'*' THEN
                      BEGIN
                        tmpStr:=aValue+': '+aLabel;
                        IF Length(tmpStr)>20 THEN ValLabelStr[nN2]:=Copy(tmpStr,1,20)
                        ELSE ValLabelStr[nN2]:=tmpStr;
                      END;
                    inc(nN2);
                  END;  //While
                IF (nN2<aValueLabelSet.count) AND (nN2=25)
                THEN ValLabelStr[25]:='...';
              END;

            tmpType:=epd[nN].FieldtypeName;
            tmpWidth:=IntToStr(epd[nN].Length);
            IF (epd[nN].Fieldtype=ftFloat) AND (epd[nN].NumDecimals>0)
            THEN tmpWidth:=tmpWidth+':'+IntToStr(epd[nN].NumDecimals);
            IF epd[nN].Fieldtype=ftCrypt THEN tmpWidth:=IntToStr(epd[nN].CryptEntryLength);
            {Write first line}
            IF UsesValueLabels
            THEN res.Append(Format('%3d %-10s %-20s  %-15s %-5s  %-20s  %-20s',
              [FieldNumber,epd[nN].FieldName,QuestStr[1],tmpType,tmpWidth,CheckStr[1],ValLabelStr[1]]))
            ELSE res.Append(Format('%3d %-10s %-20s  %-15s %-5s  %-20s',
              [FieldNumber,epd[nN].FieldName,QuestStr[1],tmpType,tmpWidth,CheckStr[1]]));
            nN2:=2;
            {Write next lines}
            WHILE ((QuestStr[nN2]<>'') OR (CheckStr[nN2]<>'')
            OR (ValLabelStr[nN2]<>'')) AND (nN2<=25) DO
              BEGIN
                IF UsesValueLabels
                THEN res.Append(Format('%3s %-10s %-20s  %-15s %-5s  %-20s  %-20s',
                ['','',QuestStr[nN2],'','',CheckStr[nN2],ValLabelStr[nN2]]))
                ELSE res.Append(Format('%3s %-10s %-20s  %-15s %-5s  %-20s',
                ['','',QuestStr[nN2],'','',CheckStr[nN2]]));
                INC(nN2);
              END;  //while
            res.Append('');
          END;  //if not fQuestion
      END;  //for
    IF UsesValueLabels THEN nN:=102 ELSE nN:=80;
    res.Append(cFill('-',nN));
    res.Append('');
    result:=res.Text;
  finally
    res.free;
  end;
end;

procedure TForm1.LoadData(ShowAsLabels:boolean);
var
  row,col:integer;
begin
  sg.ColCount:=epd.NumDataFields+1;
  sg.RowCount:=epd.NumRecords+1;
  
  for row:=0 to sg.RowCount-1 do
    for col:=0 to sg.ColCount-1 do
      sg.Cells[col,row]:='';
  if (not assigned(epd)) then exit;

  //Make col-headings
  for col:=0 to epd.NumFields-1 do
    if epd[col].Fieldtype<>ftQuestion then sg.Cells[col+1,0]:=epd[col].FieldName;

  for row:=1 to epd.NumRecords do
    begin
      epd.Read(row);
      sg.Cells[0,row]:=inttostr(row);
      for col:=0 to epd.NumFields-1 do
        begin
          if epd[col].Fieldtype<>ftQuestion then
            begin
              if ShowAsLabels
              then sg.Cells[col+1,row]:=epd[col].AsLabel
              else sg.Cells[col+1,row]:=epd[col].AsString;
            end;
        end;
    end;
end;

procedure TForm1.checkShowLabelsClick(Sender: TObject);
begin
  LoadData(checkShowLabels.checked);
end;

procedure TForm1.GetPassword(Sender: TObject; requesttype:TRequestPasswordTypes; var password:String);
begin
  formPW:=TformPW.Create(self);
  try
    formPW.Filename:=epd.RecFilename;
    if requesttype=rpOpen then formPW.DoublePW:=false else formPW.DoublePW:=true;
    if formPW.ShowModal=mrOK then password:=formPW.editPW1.Text;
  finally
    formPW.free;
  end;
end;

initialization
  {$i UTestMain.lrs}
  {$i UTestMain.lrs}

end.
