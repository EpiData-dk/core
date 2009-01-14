unit UImport;

interface

uses
  UEpiDataFile;

type

  TEpiImport = class(TObject)
  private
    procedure LoadRec(const FileName: string; DataFile: TEpiDataFile);
  protected

  public
    procedure Load(const FileName: string; DataFile: TEpiDataFile);
    procedure LoadStata(const FileName: string; DataFile: TEpiDataFile);
    procedure LoadTXT(const FileName: string; DataFile: TEpiDataFile);
    procedure LoadSAS(const FileName: string; DataFile: TEpiDataFile);
    procedure LoadSPSS(const FileName: string; DataFile: TEpiDataFile);
    procedure LoadXLS(const FileName: string; DataFile: TEpiDataFile);
    constructor Create;
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils, UEFields, Classes, UEpiDataConstants, UEpiTypes, UEpiUtils,
  UValueLabels, Math;
{ TEpiImport }

constructor TEpiImport.Create;
begin

end;

destructor TEpiImport.Destroy;
begin

  inherited;
end;

procedure TEpiImport.Load(const FileName: string; DataFile: TEpiDataFile);
var
  ext: string;
begin
  if FileName<>'' then
    ext:=AnsiLowerCase(ExtractFileExt(FileName));
  if (ext='.rec') or (ext='') then LoadRec(FileName, DataFile)
  else if ext='dta' then LoadStata(FileName, DataFile)
  else if ext='txt' then LoadTXT(FileName, DataFile)
  else if ext='sas' then LoadSAS(FileName, DataFile)
  else if ext='sps' then LoadSPSS(FileName, DataFile)
  else if ext='xls' then LoadXLS(FileName, DataFile)
  else begin
      //error('Filetype `'+ext+'` not supported');
    exit;
  end;
end;

procedure TEpiImport.LoadRec(const FileName: string;
  DataFile: TEpiDataFile);
begin

end;

procedure TEpiImport.LoadSAS(const FileName: string;
  DataFile: TEpiDataFile);
begin

end;

procedure TEpiImport.LoadSPSS(const FileName: string;
  DataFile: TEpiDataFile);
begin

end;

procedure TEpiImport.LoadStata(const FileName: string; DataFile: TEpiDataFile);
var
  eField: TEField;
  n,n2,t,CurField,CurRec: integer;
  s, s2: string;
  tmpDate: TDateTime;
  UserAborts:Boolean;          //Flag to indicate that user presses ESC to Abort
  tmpSmallInt: SmallInt;
  SmallIntBuff: Array[0..1] OF Byte absolute tmpSmallInt;
  F: TextFile;
  StataFile: TFileStream;
  StataFilename,RECFilename: TFilename;
  buff: Array[0..50000] OF Char;
  NumBuff: Array[0..7] OF Byte;
  typList: Array[0..800] OF Char;
  StataVersion: Byte;
  pByte: ^BYTE;
  pSmallInt: ^SmallInt;
  pShortInt: ^ShortInt;
  pWord: ^Word;
  pLongInt: ^LongInt;
  pDouble: ^Double;
  n3, NameLength:Integer;
  LOHI,HasValueLabels:Boolean;
  HeaderSize,nVar,nObs,MaxVarLabel,MaxFNameWidth: Integer;
  tmpByte:   Byte;
  tmpWord:   Word;
  tmpInt:    SmallInt;
  tmpShortInt: ShortInt;
  tmpLong:   LongInt;
  tmpSingle: Single;
  tmpDouble: Double;
  tmpS,tmpS2:      String;
//    FirstLabelRec,NextLabelRec,tmpLabelRec: PLabelRec;
  FirstLabel:Boolean;
  ChkLin: TStringList;
  CutsDecimals,OkToCutDecimals: Boolean;
  ByteChar,intChar,LongChar,FloatChar,DoubleChar: Char;
  StrBaseNumber: Byte;
  MisVal: String;
  MisWidth: Integer;

  function ReadByte(p: Integer): shortInt;
  var
    pShortInt: ^ShortInt;
  begin
    New(pShortInt);
    pShortInt^:=0;
    Statafile.Position:=p;
    StataFile.Read(pShortInt^,1);
    Result:=pShortInt^;
    Dispose(pShortInt);
  end;

  function ReadDbWord(p: Integer): LongInt;
  VAR
    tmpValue: LongInt;
    tBuff: Array[0..3] OF byte absolute tmpValue;
    tmpByte: Byte;
  BEGIN
    StataFile.Position:=p;
    StataFile.Read(tBuff,4);
    IF NOT LOHI THEN
    BEGIN
      tmpByte:=tBuff[0];
      tBuff[0]:=tBuff[3];
      tBuff[3]:=tmpByte;
      tmpByte:=tBuff[1];
      tBuff[1]:=tBuff[2];
      tBuff[2]:=tmpByte;
    END;
    Result:=tmpValue;
  end;

  function ReadDouble(p: Integer): Double;
  VAR
    tmpValue: Double;
    b: Array[0..7] of byte absolute tmpValue;
    tmpByte: Byte;
  BEGIN
    MisVal:='';
    StataFile.Position:=p;
    StataFile.Read(b,8);
    IF NOT LOHI THEN
    BEGIN
      tmpByte:=b[0];
      b[0]:=b[7];
      b[7]:=tmpByte;
      tmpByte:=b[1];
      b[1]:=b[6];
      b[6]:=tmpByte;
      tmpByte:=b[2];
      b[2]:=b[5];
      b[5]:=tmpByte;
      tmpByte:=b[3];
      b[3]:=b[4];
      b[4]:=tmpByte;
    END;
    IF (b[0]=0) AND (b[1]=0) AND (b[2]=0) AND (b[3]=0) AND (b[4]=0) AND (b[5]=0) AND (b[6]=$E0) AND (b[7]=$7F) THEN MisVal:='.';
    IF (b[0]=0) AND (b[1]=0) AND (b[2]=0) AND (b[3]=0) AND (b[4]=0) AND (b[5]=01) AND (b[6]=$E0) AND (b[7]=$7F) THEN MisVal:='.a';
    IF (b[0]=0) AND (b[1]=0) AND (b[2]=0) AND (b[3]=0) AND (b[4]=0) AND (b[5]=02) AND (b[6]=$E0) AND (b[7]=$7F) THEN MisVal:='.b';
    IF (b[0]=0) AND (b[1]=0) AND (b[2]=0) AND (b[3]=0) AND (b[4]=0) AND (b[5]=03) AND (b[6]=$E0) AND (b[7]=$7F) THEN MisVal:='.c';
    Result:=tmpValue;
  end;

  function ReadSingle(p: Integer): Single;
  VAR
    tmpValue: Single;
    b: Array[0..3] of byte absolute tmpValue;
    tmpByte: Byte;
  BEGIN
    MisVal:='';
    StataFile.Position:=p;
    StataFile.Read(b,4);
    IF NOT LOHI THEN
      BEGIN
        tmpByte:=b[0];
        b[0]:=b[3];
        b[3]:=tmpByte;
        tmpByte:=b[1];
        b[1]:=b[2];
        b[2]:=tmpByte;
      END;
    IF (b[0]=0) AND (b[1]=0) AND (b[2]=0) AND (b[3]=$7F) THEN MisVal:='.';
    IF (b[0]=0) AND (b[1]=8) AND (b[2]=0) AND (b[3]=$7F) THEN MisVal:='.a';
    IF (b[0]=0) AND (b[1]=$10) AND (b[2]=0) AND (b[3]=$7F) THEN MisVal:='.b';
    IF (b[0]=0) AND (b[1]=$18) AND (b[2]=0) AND (b[3]=$7F) THEN MisVal:='.c';
    Result:=tmpValue;
  end;

  function ReadWord(p: Integer): Word;
  VAR
    tmpValue: Word;
    tBuff: Array[0..1] OF Byte absolute tmpValue;
    tmpByte: Byte;
  BEGIN
    Statafile.Position:=p;
    StataFile.read(tBuff,2);
    IF NOT LOHI THEN
      BEGIN
        tmpByte:=tBuff[0];
        tBuff[0]:=tBuff[1];
        tBuff[1]:=tmpByte;
      END;
    Result:=tmpValue;
  end;

  function vltInteger(p: Integer): LongInt;
  VAR
    tmpValue: LongInt;
    tBuff: Array[0..3] OF Byte absolute tmpValue;
    vn: Integer;
    tmpByte: Byte;
  BEGIN
    MisVal:='';
    FOR vn:=0 TO 3 DO
      tBuff[vn]:=ORD(buff[p+vn]);
    IF NOT LOHI THEN
      BEGIN
        tmpByte:=tBuff[0];
        tBuff[0]:=tBuff[3];
        tBuff[3]:=tmpByte;
        tmpByte:=tBuff[1];
        tBuff[1]:=tBuff[2];
        tBuff[2]:=tmpByte;
      END;
    IF tmpValue>=$7fffffe5 THEN MisVal:='.';
    Result:=tmpValue;
  end;

  function vltString(p: Integer): String;
  VAR
    vs: String;
    vn: Integer;
  BEGIN
    vn:=p;
    vs:='';
    WHILE (vn<SizeOf(buff)-1) AND (buff[vn]<>#0) DO
      BEGIN
        vs:=vs+buff[vn];
        INC(vn);
      END;
    Result:=vs;
  end;

begin
  StataFile:=NIL;
  if DataFile = nil then
    exit;

  TRY
    Statafile:=TFileStream.Create(StataFilename,fmOpenRead);

    DataFile.EpiInfoFieldNaming:=False;
    DataFile.RECFilename:=RECFilename;

    StataFile.Position:=0;
    StataFile.Read(NumBuff,3);
    IF NumBuff[0]=$69 THEN
      BEGIN
        HeaderSize:=60;
        StataVersion:=4;
      END
    ELSE IF NumBuff[0]=$6C THEN
      BEGIN
        HeaderSize:=109;
        StataVersion:=6;
      END
    ELSE IF NumBuff[0]=$6E THEN
      BEGIN
        HeaderSize:=109;
        StataVersion:=7;
      END
    ELSE IF NumBuff[0]=$71 THEN   //&&
      BEGIN
        HeaderSize:=109;
        StataVersion:=8;
      END
    ELSE
      BEGIN
        DataFile.Error(EPI_NOT_VALID_STATA_FILE, DataFile.Lang(23978, 'Unknown version of Stata-file'));  //'Unknown version of Stata-file'
        Exit;
      END;

    IF NumBuff[1]=1 THEN LOHI:=False
    ELSE IF NumBuff[1]=2 THEN LOHI:=True
    ELSE
      BEGIN
        DataFile.Error(EPI_NOT_VALID_STATA_FILE, DataFile.Lang(23980, 'Incorrect format of stata-file'));  //'Incorrect format of stata-file'
        Exit;
      END;

    IF NumBuff[2]<>1 THEN
      BEGIN
        DataFile.Error(EPI_NOT_VALID_STATA_FILE, DataFile.Lang(23980, 'Incorrect format of stata-file'));  //'Incorrect format of stata-file'
        Exit;
      END;

    nVar:=ReadWord(4);
    IF nVar>800 THEN
      BEGIN
        DataFile.Error(EPI_TOO_MANY_VARIABLES, Format(DataFile.Lang(23982, 'The stata-file contains %d variables.~A maximum of 800 variables can be imported.'),[nVar]));  //'The stata-file contains %d variables.~A maximum of 800 variables can be imported.'
        Exit;
      END;

    FOR n:=1 TO nVar DO
      BEGIN
        eField := TeField.Create;
        WITH eField DO
          BEGIN
            Question:='';
            OriginalQuest:='';
            Length:=0;
            NumDecimals:=0;
            VariableLabel:='';
            FieldX:=0;
            FieldY:=n;
            QuestX:=1;
            QuestY:=n;
            FieldName:='          ';
          END;
        DataFile.AddField(eField);
      END;  //for N


    nObs:=ReadDbWord(6);
    FillChar(buff,SizeOf(buff),0);
    StataFile.Position:=10;
    IF StataVersion=4 THEN StataFile.Read(buff,32) ELSE StataFile.Read(buff,81);
    DataFile.FileLabel:=StrPas(buff);

    {Read typlist - the variable's types}
    IF StataVersion=8 THEN  //&&
      BEGIN
        ByteChar:=#251;
        IntChar:=#252;
        LongChar:=#253;
        FloatChar:=#254;
        DoubleChar:=#255;
      END
    ELSE
      BEGIN
        ByteChar:='b';
        IntChar:='i';
        LongChar:='l';
        FloatChar:='f';
        DoubleChar:='d';
      END;
    IF StataVersion=8 THEN StrBaseNumber:=0 ELSE StrBaseNumber:=$7F;  //&&
    FillChar(typList,sizeOf(typList),0);
    StataFile.Position:=HeaderSize;
    StataFile.Read(typList,nVar);
    FOR n:=0 TO nVar-1 DO
      BEGIN
        EField := DataFile.Fields[n];
        IF (typList[n]=DoubleChar) OR (typList[n]=FloatChar)
        OR (typList[n]=LongChar) OR (typList[n]=IntChar) THEN EField.Fieldtype:=ftFloat
        ELSE IF typList[n]=ByteChar THEN EField.Fieldtype:=ftInteger
  //          ELSE IF ORD(typList[n])>$7F THEN    &&
  //            BEGIN
  //              EField.Felttype:=ftAlfa;
  //              EField.FLength:=ORD(typList[n])-$7F;
  //            END
        ELSE
          BEGIN
            IF StataVersion=8 THEN              //&&
              BEGIN
                EField.Fieldtype:=ftAlfa;
                EField.Length:=ORD(typList[n]);
              END
            ELSE
              BEGIN
                IF ORD(typList[n])>$7F THEN
                  BEGIN
                    EField.Fieldtype:=ftAlfa;
                    EField.Length:=ORD(typList[n])-$7F;
                  END
                ELSE
                  BEGIN
                    DataFile.Error(EPI_NOT_VALID_STATA_FILE, DataFile.Lang(23984, 'Unknown variable type found in Stata-file'));  //'Unknown variable type found in Stata-file'
                    Exit;
                  END;
              END;
          END;
      END;  //for

    {Read varlist - list of variable names}
    Fillchar(buff,sizeOf(buff),0);
    MaxFNameWidth:=0;
    StataFile.Position:=HeaderSize+nVar;
    IF StataVersion>=7 THEN NameLength:=33 ELSE NameLength:=9;   //&&
    StataFile.Read(buff,NameLength*nVar);
    FOR n:=0 TO nVar-1 DO
      BEGIN
        EField := DataFile.Fields[n];
        tmpS:=cFill(' ',NameLength);
        t:=0;
        WHILE buff[(n*NameLength)+t]<>#0 DO
          BEGIN
            tmpS[t+1]:=buff[(n*NameLength)+t];
            INC(t);
          END;  //while
        tmpS2:=tmpS;
        tmpS:='';
        FOR t:=1 TO Length(tmpS2) DO
          IF (tmpS2[t] in AlfaNumChars) THEN tmpS:=tmpS+tmpS2[t];
        IF Length(tmpS)>FieldNameLen THEN tmpS:=Copy(tmpS,1,FieldnameLen);
        IF NOT NameIsUnique(tmpS, DataFile, FieldnameLen) THEN REPEAT UNTIL NameIsUnique(tmpS, DataFile, FieldnameLen);
        // TODO -o Torsten : Hvad er dette???
{        CASE FieldNameCase OF
          fcUpper: tmpS:=AnsiUpperCase(tmpS);
          fcLower: tmpS:=AnsiLowerCase(tmpS);
        END;  //case     }
        EField.FieldName:=trim(tmpS);
        IF Length(trim(EField.FieldName))>MaxFNameWidth THEN MaxFNameWidth:=Length(trim(EField.FieldName));
        IF EField.Length>80 THEN
          BEGIN
            DataFile.Error(EPI_FIELDS_TOO_LONG, Format(DataFile.Lang(23914, 'The variable %s is a text variable with more than 80 characters.')+#13+  //'The variable %s is a text variable with more than 80 characters.'
            DataFile.Lang(23916, 'This variable cannot be imported to EpiData.'),[EField.FieldName]));   //'This variable cannot be imported to EpiData.'
            Exit;
          END;

      END;  //for n

    {Read fmtlist - list of formats of the variables}
    Fillchar(buff,sizeOf(buff),0);
    StataFile.Position:=HeaderSize+nVar+(NameLength*nVar)+(2*(nVar+1));
    StataFile.Read(buff,12*nVar);
    FOR n:=0 TO nVar-1 DO
      BEGIN
        EField:=DataFile.Fields[n];
        tmpS:='            ';
        t:=0;
        WHILE buff[(n*12)+t]<>#0 DO
          BEGIN
            tmpS[t+1]:=buff[(n*12)+t];
            INC(t);
          END;  //while
        {Handle formats...}
        tmpS:=trim(AnsiUpperCase(tmpS));
        IF tmpS[Length(tmpS)]='S' THEN EField.Fieldtype:=ftAlfa
        ELSE IF Copy(tmpS,1,2)='%D' THEN
          BEGIN
            {format is a date}
            EField.Fieldtype:=ftEuroDate;
            EField.Length:=10;
            IF length(tmpS)>2 THEN
              BEGIN
                {date has a detailed format - check if month is before date}
                s:=Copy(tmpS,3,Length(tmpS));
                FOR t:=1 TO Length(s) DO
                  IF Pos(s[n],'MLN')>0 THEN s[n]:='M';
                IF (pos('D',s)>0) AND (pos('D',s)>Pos('M',s)) THEN EField.Fieldtype:=ftDate;
              END;
          END  //if date
        ELSE
          BEGIN
            {format is numeric}
            t:=Pos('.',tmpS);
            IF t=0 THEN
              BEGIN
                DataFile.Error(EPI_NOT_VALID_STATA_FILE, Format(DataFile.Lang(23986, 'Unknown format specified for variable %s'),[EField.FieldName]));   //'Unknown format specified for variable %s'
                Exit;
              END;
            s:='';
            DEC(t);
            WHILE (t>1) AND (tmpS[t] in NumChars) DO
              BEGIN
                s:=tmpS[t]+s;
                DEC(t);
              END;
            t:=Pos('.',tmpS)+1;
            s2:='';
            WHILE (t<Length(tmpS)) AND (tmpS[t] in NumChars) DO
              BEGIN
                s2:=tmpS[t]+s2;
                INC(t);
              END;
            TRY
              EField.Length:=StrToInt(s);
              EField.NumDecimals:=StrToInt(s2);
            EXCEPT
              DataFile.Error(EPI_NOT_VALID_STATA_FILE, Format(DataFile.Lang(23986, 'Unknown format specified for variable %s'),[EField.FieldName]));   //'Unknown format specified for variable %s'
              Exit;
            END;  //try..except
            IF typList[n]=ByteChar THEN   //&&
                   BEGIN    //&&
                     EField.Length:=3;
                     EField.NumDecimals:=0;
                   END
            ELSE IF typList[n]=IntChar THEN   //&&
                   BEGIN   //&&
                     EField.Length:=5;
                     EField.NumDecimals:=0;
                   END
            ELSE IF typList[n]=LongChar THEN   //&&
                   BEGIN    //&&
                     EField.Length:=11;
                     EField.NumDecimals:=0;
                   END
            ELSE IF (typList[n]=FloatChar) OR (typList[n]=DoubleChar) THEN
                  BEGIN
                    IF (tmpS[Length(tmpS)]<>'F') AND (NOT (EField.Fieldtype in [ftDate,ftEuroDate,ftYMDDate])) THEN   //&&
                      BEGIN
                        EField.Length:=18;
                        EField.NumDecimals:=4;
                      END;
                  END;
          END;  //if numeric
      END;  //for n

    {Read lbllist - names af value label}
    Fillchar(buff,12*nVar,0);
    HasValueLabels:=False;
    StataFile.Read(buff,NameLength*nVar);
    FOR n:=0 TO nVar-1 DO
      BEGIN
        EField := DataFile.Fields[n];
//        EField.Valuelabel:=nil;
        tmpS:=cFill(' ',NameLength);
        t:=0;
        WHILE buff[(n*NameLength)+t]<>#0 DO
          BEGIN
            tmpS[t+1]:=buff[(n*NameLength)+t];
            INC(t);
          END;  //while
        tmpS:=trim(tmpS);
        IF tmpS<>'' THEN
          BEGIN
            EField.Valuelabel := TValueLabelSet.Create;
            EField.Valuelabel.Name := tmpS;
            DataFile.ValueLabels.AddValueLabelSet(eField.Valuelabel);
            HasValueLabels:=True;
          END;
      END;  //for n

    {Read variable labels}
    MaxVarLabel:=0;
    FillChar(buff,sizeOf(Buff),0);
    IF StataVersion=4 THEN t:=32 ELSE t:=81;
    FOR n:=0 TO nVar-1 DO
      BEGIN
        EField:=DataFile.Fields[n];
        FillChar(buff,t,0);
        StataFile.Read(buff,t);
        s:=StrPas(buff);
        IF Length(s)>50 THEN s:=Copy(s,1,48)+'..';
        IF Length(s)>MaxVarLabel THEN MaxVarLabel:=Length(s);
        FOR n2:=1 TO Length(s) DO
          IF (s[n2] in ['#','<','>','_']) THEN s[n2]:=' ';
        IF trim(AnsiUpperCase(EField.VariableLabel))=trim(AnsiUpperCase(EField.FieldName))
          THEN EField.VariableLabel:=''
          ELSE EField.VariableLabel:=s;
      END;  //for n

    {Make Field's question and position entryfield}
    t:=10+1+MaxVarLabel+2;
    s:='%-10s %'+IntToStr(MaxVarLabel)+'s';
    FOR n:=0 TO nVar-1 DO
      BEGIN
        EField:=DataFile.Fields[n];
        EField.OriginalQuest:=Format(s,[EField.FieldName,EField.VariableLabel]);
        EField.FieldX:=t;
      END;  //for n

    {Read - and skip - expansion fields}
    New(pByte);
    REPEAT
      StataFile.Read(pByte^,1);   //data type code
      IF StataVersion>=7 THEN tmpWord:=ReadDbWord(StataFile.Position)   //&&
      ELSE tmpWord:=ReadWord(StataFile.Position);
      IF (pByte^<>0) OR (tmpWord<>0) THEN StataFile.Read(buff,tmpWord);
    UNTIL (StataFile.Position>=StataFile.Size-1) OR ( (pByte^=0) AND (tmpWord=0) );
    Dispose(pByte);

    {Datafile description is read - now make the datafile}
    // TODO : Do we want to do this???
    {IF NOT PeekCreateDatafile(df) THEN
      BEGIN
        DataFile.Error(?, Format(DataFile.Lang((20416)+#13+  //20416=The datafile with the name %s cannot be created.
        DataFile.Lang((23910),[DataFile.RECFilename]));   //'Import terminates'
        Exit;
      END;}

    {Read data}
    TRY
      TRY
      CutsDecimals:=False;
      OKToCutDecimals:=False;
      AssignFile(F,DataFile.RECFilename);
      Append(F);
      FOR CurRec:=1 TO nObs DO
        BEGIN
{          IF ProgressStep(nObs,CurRec) THEN
            BEGIN
              ProgressForm.pBar.Position:=CurRec;
              ProgressForm.pLabel.Caption:=Format(' '+DataFile.Lang((23920),[CurRec,nObs]);  //'Importing record no. %d of %d'
              Application.ProcessMessages;
            END;}
          FOR CurField:=0 TO DataFile.NumDataFields-1 DO
            BEGIN
              EField:=DataFile.Fields[Curfield];
              IF typList[CurField]=ByteChar THEN   //&&
                BEGIN    //&&
                  tmpshortInt:=ReadByte(StataFile.Position);
                  IF StataVersion<8 THEN
                    BEGIN
                      IF tmpShortInt=$7F THEN s:='' ELSE s:=IntToStr(tmpShortInt);
                    END
                  ELSE
                    BEGIN
                      IF tmpShortInt<=100 THEN s:=IntToStr(tmpShortInt)
                      ELSE IF tmpShortInt=102 THEN s:=cFill('9',EField.Length)   // missing value .a
                      ELSE IF tmpShortInt=103 THEN s:=cFill('8',EField.Length)   // missing value .b
                      ELSE IF tmpShortInt=104 THEN s:=cFill('7',EField.Length)   // missing value .c
                      ELSE s:='';   // missing value . and missing values .d - .z
                      IF (tmpShortInt>=102) AND (tmpShortInt<=104) THEN
                        BEGIN
                          EField.MissingValues[0]:=cFill('9',EField.Length);
                          EField.MissingValues[1]:=cFill('8',EField.Length);
                          EField.MissingValues[2]:=cFill('7',EField.Length);
                        END;
                    END
                END
              ELSE IF typList[CurField]=IntChar THEN
                BEGIN   //&&
                  tmpInt:=ReadWord(Statafile.Position);
                  IF StataVersion<8 THEN
                    BEGIN
                      IF tmpInt=$7FFF THEN s:='' ELSE s:=IntToStr(tmpInt);
                    END
                  ELSE
                    BEGIN
                      IF tmpInt<=$7FE4 THEN s:=IntToStr(tmpInt)
                      ELSE IF tmpInt=$7FE6 THEN s:=cFill('9',EField.Length)   // missing value .a
                      ELSE IF tmpInt=$7FE7 THEN s:=cFill('8',EField.Length)   // missing value .b
                      ELSE IF tmpInt=$7FE8 THEN s:=cFill('7',EField.Length)   // missing value .c
                      ELSE s:='';   // missing value . and missing values .d - .z
                      IF (tmpInt>=$7FE6) AND (tmpInt<=$7FE8) THEN
                        BEGIN
                          EField.MissingValues[0]:=cFill('9',EField.Length);
                          EField.MissingValues[1]:=cFill('8',EField.Length);
                          EField.MissingValues[2]:=cFill('7',EField.Length);
                        END;
                    END
                END
              ELSE IF typList[CurField]=LongChar THEN
                BEGIN     //&&
                  tmpLong:=ReadDbWord(Statafile.Position);
                  IF StataVersion<8 THEN
                    BEGIN
                      IF tmpLong=$7FFFFFFF THEN s:='' ELSE s:=IntToStr(tmpLong);
                    END
                  ELSE
                    BEGIN
                      IF tmpLong<=$7fffffe4 THEN s:=IntToStr(tmpLong)
                      ELSE IF tmpLong=$7fffffe6 THEN s:=cFill('9',EField.Length)   // missing value .a
                      ELSE IF tmpLong=$7fffffe7 THEN s:=cFill('8',EField.Length)   // missing value .b
                      ELSE IF tmpLong=$7fffffe8 THEN s:=cFill('7',EField.Length)   // missing value .c
                      ELSE s:='';   // missing value . and missing values .d - .z
                      IF (tmpShortInt>=$7fffffe6) AND (tmpShortInt<=$7fffffe8) THEN
                        BEGIN
                          EField.MissingValues[0]:=cFill('9',EField.Length);
                          EField.MissingValues[1]:=cFill('8',EField.Length);
                          EField.MissingValues[2]:=cFill('7',EField.Length);
                        END;
                    END
                END
              ELSE IF typList[CurField]=FloatChar THEN
                BEGIN    //&&
                  tmpSingle:=ReadSingle(StataFile.Position);
                  IF StataVersion<8 THEN
                    BEGIN
                      IF tmpSingle=Power(2,127) THEN s:=''
                      ELSE Str(tmpSingle:EField.Length:EField.NumDecimals,s);
                      IF (EField.NumDecimals>4) AND (INT(tmpSingle*10000)/10000 <> tmpSingle) THEN CutsDecimals:=True;
                    END
                  ELSE
                    BEGIN
                      IF tmpSingle<Power(2,127) THEN
                        BEGIN
                          Str(tmpSingle:EField.Length:EField.NumDecimals,s);
                          IF (EField.NumDecimals>4) AND (INT(tmpSingle*10000)/10000 <> tmpSingle) THEN CutsDecimals:=True;
                        END
                      ELSE IF MisVal='.' THEN s:=''
                      ELSE IF MisVal='.a' THEN s:=cFill('9',EField.Length-1-EField.NumDecimals)+'.'+cFill('9',EField.NumDecimals)
                      ELSE IF MisVal='.b' THEN s:=cFill('8',EField.Length-1-EField.NumDecimals)+'.'+cFill('8',EField.NumDecimals)
                      ELSE IF MisVal='.c' THEN s:=cFill('7',EField.Length-1-EField.NumDecimals)+'.'+cFill('7',EField.NumDecimals)
                      ELSE s:='';
                      IF (MisVal='.a') OR (MisVal='.b') OR (MisVal='.c') THEN
                        BEGIN
                          IF EField.NumDecimals>0 THEN
                            BEGIN
                              EField.MissingValues[0]:=cFill('9',EField.Length-1-EField.NumDecimals)+'.'+cFill('9',EField.NumDecimals);
                              EField.MissingValues[1]:=cFill('8',EField.Length-1-EField.NumDecimals)+'.'+cFill('8',EField.NumDecimals);
                              EField.MissingValues[2]:=cFill('7',EField.Length-1-EField.NumDecimals)+'.'+cFill('7',EField.NumDecimals);
                            END
                          ELSE
                            BEGIN
                              EField.MissingValues[0]:=cFill('9',EField.Length-1-EField.NumDecimals);
                              EField.MissingValues[1]:=cFill('8',EField.Length-1-EField.NumDecimals);
                              EField.MissingValues[2]:=cFill('7',EField.Length-1-EField.NumDecimals);
                            END;
                        END;
                    END;  //if stata8
                END   //if FloatChar
              ELSE IF typList[CurField]=DoubleChar THEN
                BEGIN   //&&
                  tmpDouble:=ReadDouble(StataFile.Position);
                  IF StataVersion<8 THEN
                    BEGIN
                      IF tmpDouble=Power(2,1023) THEN s:=''
                      ELSE
                        BEGIN
                          Str(tmpDouble:EField.Length:EField.NumDecimals,s);
                          IF (EField.NumDecimals>4) AND ((INT(tmpDouble*10000))/10000 <> tmpDouble) THEN CutsDecimals:=True;
                        END
                    END  //if ver<8
                  ELSE
                    BEGIN
                      IF tmpDouble<Power(2,1023) THEN
                        BEGIN
                          Str(tmpDouble:EField.Length:EField.NumDecimals,s);
                          IF (EField.NumDecimals>4) AND ((INT(tmpDouble*10000))/10000 <> tmpDouble) THEN CutsDecimals:=True;
                        END
                      ELSE IF MisVal='.a' THEN s:=cFill('9',EField.Length-1-EField.NumDecimals)+'.'+cFill('9',EField.NumDecimals)
                      ELSE IF MisVal='.b' THEN s:=cFill('8',EField.Length-1-EField.NumDecimals)+'.'+cFill('8',EField.NumDecimals)
                      ELSE IF MisVal='.c' THEN s:=cFill('7',EField.Length-1-EField.NumDecimals)+'.'+cFill('7',EField.NumDecimals)
                      ELSE s:='';
                      IF (MisVal='.a') OR (MisVal='.b') OR (MisVal='.c') THEN
                        BEGIN
                          IF EField.NumDecimals>0 THEN
                            BEGIN
                              EField.MissingValues[0]:=cFill('9',EField.Length-1-EField.NumDecimals)+'.'+cFill('9',EField.NumDecimals);
                              EField.MissingValues[1]:=cFill('8',EField.Length-1-EField.NumDecimals)+'.'+cFill('8',EField.NumDecimals);
                              EField.MissingValues[2]:=cFill('7',EField.Length-1-EField.NumDecimals)+'.'+cFill('7',EField.NumDecimals);
                            END
                          ELSE
                            BEGIN
                              EField.MissingValues[0]:=cFill('9',EField.Length-1-EField.NumDecimals);
                              EField.MissingValues[1]:=cFill('8',EField.Length-1-EField.NumDecimals);
                              EField.MissingValues[2]:=cFill('7',EField.Length-1-EField.NumDecimals);
                            END;
                        END;
                    END;  //if stata8
                END  //if DoubleChar
              ELSE
                BEGIN
                  IF (StataVersion<8) AND (ORD(typList[CurField])<=$7F) THEN    //&&
                    BEGIN
                      DataFile.Error(EPI_NOT_VALID_STATA_FILE,
                        format(DataFile.Lang(23922, 'Unknown dataformat %s found'),[typList[Curfield]]));   //'Unknown dataformat %s found'
                      Exit;
                    END;
                END;
              IF ( (StataVersion=8) AND (ORD(typList[CurField])<$F5) )
              OR ( (StataVersion<8) AND (ORD(typList[CurField])>$7F) ) THEN  //&&
                BEGIN
                  FillChar(buff,sizeOf(buff),0);
                  StataFile.Read(buff,EField.Length);
                  s:=StrPas(buff);
                END;  //if string variable
              IF (s<>'') AND ( EField.Fieldtype in [ftDate,ftEuroDate,ftYMDDate]) THEN
                BEGIN
                  tmpDate:=StrToFloat(s)+21916;  {Date is converted from Stata's 1/1-1960 base to Delphi's 30/12-1899 base}
                  s:=mibDateToStr(tmpDate,EField.Fieldtype);
                END;  //if date variable

              IF Length(s)>EField.Length THEN
                BEGIN
                  tmpS:=Format(DataFile.Lang(23924, 'Error in record %d, field %s: data is too long to fit the field in EpiData'),[CurRec,EField.FieldName]);   //'Error in record %d, field %s: data is too long to fit the field in EpiData'
                  IF (TypList[CurField]=FloatChar) OR (TypList[CurField]=DoubleChar)
                  THEN tmpS:=tmpS+#13#13+DataFile.Lang(23926, 'Try to use a fixed format (e.g. %9.2f) in Stata to optimize the fieldsize');   //'Try to use a fixed format (e.g. %9.2f) in Stata to optimize the fieldsize'
                  DataFile.Error(EPI_FAILED, tmpS);
                  Exit;
                END
              ELSE IF (NOT OKToCutDecimals) AND (CutsDecimals) THEN
                BEGIN
                  tmpS:=Format(DataFile.Lang(23928, 'Data in record %d, field %s will be rounded to 4 decimals after the decimalpoint'),    //'Data in record %d, field %s will be rounded to 4 decimals after the decimalpoint'
                  [CurRec,EField.FieldName])+#13#13+DataFile.Lang(23930, 'Continue import?');   //'Continue import?'
                  // TODO : Change with new field convension (Integer can be longer then 4 digits)
                  //IF WarningDlg(tmpS)=mrCancel THEN Exit ELSE OKToCutDecimals:=True;
                END;
              EField.AsString:=s;

            END;  //for CurField
          DataFile.Write(CurRec);
//          WriteNextRecord(df,F);

{          IF UserAborts THEN
            BEGIN
              IF eDlg(DataFile.Lang((23932),mtConfirmation,[mbYes,mbNo],0)=mrYes   //'Abort import?'
              THEN
                BEGIN
                  CloseFile(F);
                  tmpBool:=DeleteFile(DataFile.RECFilename);
                  Exit;
                END
              ELSE UserAborts:=False;
            END;  //if UserAborts    }

        END;  //for CurRec
      EXCEPT
        DataFile.Error(EPI_FAILED, DataFile.Lang(23934, 'Error reading data from Stata-file'));   //'Error reading data from Stata-file'
        Exit;
      END;  //try..except
    FINALLY
      {$I-}
      CloseFile(F);
      {$I+}
//      EnableTaskWindows(WindowList);
//      ProgressForm.Free;
    END;  //try.Except

    IF (HasValueLabels) AND (StataFile.Position<StataFile.Size-4) THEN
      BEGIN
        IF StataVersion=4 THEN
          BEGIN
            {Read value labels definitions - if present}
            WHILE StataFile.Position<StataFile.Size-2 DO
              BEGIN
                FillChar(buff,sizeOf(buff),0);
                t:=ReadWord(StataFile.Position);  //get number of entries in label
                StataFile.Read(buff,10);   //11+(10*t));   //Load label definition
                s:=StrPas(buff);
                Fillchar(buff,10,0);
                StataFile.Read(buff,10*t);
                FOR n:=1 TO t DO
                  BEGIN
                    IF LOHI THEN
                      BEGIN
                        SmallIntBuff[0]:=ORD(buff[(n-1)*2]);
                        SmallIntBuff[1]:=ORD(buff[((n-1)*2)+1]);
                      END
                    ELSE
                      BEGIN
                        SmallIntBuff[1]:=ORD(buff[(n-1)*2]);
                        SmallIntBuff[0]:=ORD(buff[((n-1)*2)+1]);
                      END;
                    n2:=0;
                    WHILE (n2<8) AND (buff[(t*2)+((n-1)*8)+n2]<>#0) DO
                      BEGIN
                        tmpS2[n2+1]:=buff[(t*2)+((n-1)*8)+n2];
                        INC(n2);
                      END;
                    if Trim(DataFile.ValueLabels.ValueLabelSetByName(s).ValueLabel[IntToStr(tmpSmallInt)]) <> '' then
                      BEGIN
                        DataFile.Error(EPI_FAILED, DataFile.Lang(23936, 'Duplicate value label name found'));  //'Duplicate value label name found'
                        Exit;
                      END;
                    DataFile.ValueLabels.ValueLabelSetByName(s).AddValueLabelPair(IntToStr(tmpSmallInt), tmpS2);
                  END;  //for n
              END;  //if not end of StataFile
          END  //if stataversion 4
        ELSE
          BEGIN
            {Value labels for stata version 6, 7 and 8}
            WHILE StataFile.Position<StataFile.Size-4 DO
              BEGIN
                FillChar(buff,sizeOf(buff),0);
                n:=ReadDbWord(StataFile.Position);     //Length of value_label_table (vlt)
                StataFile.Read(buff,NameLength+3);     //Read label-name+3 byte padding
                s:=StrPas(buff);                       //s now contains labelname
                FillChar(buff,NameLength+3,0);
                StataFile.Read(buff,n);           //Load value_label_table into buffer
                n:=vltInteger(0);                 //Number of entries in label
                MisWidth:=0;
                FOR t:=0 TO n-1 DO
                BEGIN
                  tmpLong:=vltInteger(4+4+(4*n)+(4*t));   //read the value
                  IF NOT ( (tmpLong=$7FFFFFE5) OR (tmpLong>$7FFFFFE8) ) THEN  //ignore valuelabels where value = . or value > .c
                  BEGIN
                    IF (tmpLong>=$7FFFFFE6) AND (tmpLong<=$7FFFFFE8) THEN       //Missing values
                    BEGIN
                      IF MisWidth=0 THEN
                      BEGIN
                        //Find first field that uses this label to obtain the field's width
                        n3:=0;
                        WHILE (n3<DataFile.NumDataFields-1) AND
                          (DataFile.Fields[n3].Valuelabel.Name<>s) DO INC(n3);
                        EField:=DataFile.Fields[n3];
                        IF EField.NumDecimals=0
                        THEN MisWidth:=EField.Length
                        ELSE MisWidth:=EField.Length-1-EField.NumDecimals;
                      END;  //if MisWidth=0
                      CASE tmpLong OF
                        $7fffffe6: tmpS2:=cFill('9',MisWidth);   // .a
                        $7fffffe7: tmpS2:=cFill('8',MisWidth);   // .b
                        $7fffffe8: tmpS2:=cFill('7',MisWidth);   // .c
                      ELSE
                        tmpS2:='.';
                      END;  //case
                    END ELSE //if value=.a, .b or .c
                      tmpS2 := IntToStr(vltInteger(4+4+(4*n)+(4*t)));
                    DataFile.ValueLabels.ValueLabelSetByName(s).AddValueLabelPair(tmpS2, vltString(4+4+(4*n)+(4*n)+vltInteger(4+4+(4*t))));
                  END;  //if not . or >.c
                END;  //for t
              END;  //while
          END;  //if stataversion 6, 7 or 8

        IF DataFile.ChkTopComments=NIL THEN DataFile.ChkTopComments:=TSTringList.Create;
        DataFile.ChkTopComments.Append(Format('* '+DataFile.Lang(23938, 'Checkfile created from import of the stata-file %s'),[StataFilename]));   //'Checkfile created from import of the stata-file %s'
        ChkLin:=TStringList.Create;
        // TODO : Ret når michael har lavet public.
//        DataFile.ChecksToStrings(df,ChkLin);
        DataFile.CHKFilename:=ChangeFileExt(DataFile.RECFilename,'.chk');
        // TODO -o Torsten : Lave et OnFileExistsEvent...
{        IF FileExists(DataFile.CHKFilename) THEN
          BEGIN
            n:=eDlg(Format(DataFile.Lang((23940)+#13+   //'The stata-file contains valuelabels which will be imported to a checkfile'
            DataFile.Lang((23942),[DataFile.CHKFilename]),   //'Do you want to overwrite the existing checkfile %s?'
            mtWarning,[mbYes,mbNo,mbCancel],0);  
            CASE n OF
              mrYes: ChkLin.SaveTofile(DataFile.CHKFilename);
              mrNo:  HasValueLabels:=False;
              mrCancel: Exit;
            END;  //case
          END  //if Chkfile fileExists
        ELSE ChkLin.SaveToFile(DataFile.CHKFilename);       }
        ChkLin.Free;
                                    
      END  //if hasValueLabels
    ELSE HasValueLabels:=False;
{
    Screen.Cursor:=crDefault;
    s:=Format(DataFile.Lang((23944),[StataFilename]);   //'Datafile created by importing stata file %s'
    AddToNotesFile(df,s);
    IF NOT HasValueLabels
    THEN eDlg(Format(DataFile.Lang((23946),  //'Stata-file %s is imported ~to %s~~%d records were imported'
    [StataFilename,DataFile.RECFilename,DataFile.NumRecords]),mtInformation,[mbOK],0)
    ELSE eDlg(Format(DataFile.Lang((23948),  //'Stata-file %s has been imported~to %s~~Valuelabels are imported to the checkfile %s~~%d records were imported'
      [StataFilename,DataFile.RECFilename,DataFile.CHKFilename,DataFile.NumRecords]),mtInformation,[mbOK],0); 

    AddToRecentFiles(DataFile.RECFilename);    
                                               }
  finally
    //
  end;
end;

procedure TEpiImport.LoadTXT(const FileName: string;
  DataFile: TEpiDataFile);
begin

end;

procedure TEpiImport.LoadXLS(const FileName: string;
  DataFile: TEpiDataFile);
begin

end;

end.
