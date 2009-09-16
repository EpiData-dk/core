unit UCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, UEpiDataFile, UDataFileTypes, UImportExport;

  function  TinyDocumentation(Df: TEpiDatafile): TStrings;
  function  DocumentDataFile(Df: TEpiDatafile): TStrings;
  function  LoadDataFile(var Df: TEpiDataFile; Const FileName: string; IgnoreChecks: Boolean;
                         ShowProgress: TProgressEvent; GetPassword: TRequestPasswordEvent): boolean;
  function  SaveDataFile(Df: TEpiDataFile; Const FileName: string; IgnoreChecks: Boolean;
                         ShowProgress: TProgressEvent; GetPassword: TRequestPasswordEvent;
                         ExportSettings: Pointer): boolean;
  procedure SetFilter(aDialog: TOpenDialog);

implementation

uses
  UValueLabels, SysUtils, UStringUtils, StrUtils,
  UEpiDataGlobals, uimportform, Controls, UEpiUtils;

procedure SetFilter(aDialog: TOpenDialog);
begin
  aDialog.Filter := 'Supported data files|*.recxml;*.rec;*.dta;*.txt;*.csv;*.dbf;*.ods|'
                  + 'EpiData XML Data file (*.recxml)|*.recxml|'
                  + 'EpiData data file (*.rec)|*.rec|'
                  + 'Stata file (*.dta)|*.dta|'
                  + 'Text file (*.txt,*.csv)|*.txt|'
                  + 'dBase file (*.dbf)|*.dbf|'
                  + 'Open Document Spreadsheep (*.ods)|*.ods|'
                  + 'All files (*.*)|*.*';
  aDialog.FilterIndex := 0;
end;

function  TinyDocumentation(Df: TEpiDatafile): TStrings;
begin
  Result := TStringList.Create();
  if Df.ErrorCode <> EPI_OK then
  begin
    Result.Add('Failed to load file!');
    Result.Add(Df.ErrorText);
    if Df.ErrorCode <> EPI_CHECKFILE_ERROR then
      exit;
  end else
    Result.Add('Data file opened with succes');

  Result.Add('Num fields = ' + inttostr(Df.NumFields));
  Result.Add('Num data fields = ' + inttostr(Df.NumDataFields));
  Result.Add('Num records = ' + inttostr(Df.Size));
end;

function  LoadDataFile(var Df: TEpiDataFile; Const FileName: string; IgnoreChecks: Boolean;
                       ShowProgress: TProgressEvent; GetPassword: TRequestPasswordEvent): boolean;
var
  LoadOptions: TEpiDataFileOptions;
  Importer: TEpiImportExport;
  S: string;
  impform: TImportForm;
begin
  if Assigned(Df) then FreeAndNil(Df);

  LoadOptions := [];
  If IgnoreChecks then
    Include(LoadOptions, eoIgnoreChecks);

  if (AnsiUpperCase(ExtractFileExt(FileName)) <> '.REC') and
     (AnsiUpperCase(ExtractFileExt(FileName)) <> '.RECXML') then
  begin
    Importer := TEpiImportExport.Create;
    Importer.OnProgress := ShowProgress;
    S := Trim(AnsiUpperCase(ExtractFileExt(FileName)));
    if S = '.DTA' then
      Result := Importer.ImportStata(FileName, Df)
    else if S = '.DBF' then
      Result := Importer.ImportDBase(FileName, Df)
    else if S = '.ODS' then
      Result := Importer.ImportSpreadSheet(FileName, Df)
    else if (S = '.TXT') or (S = '.CSV') or (S='') then
    begin
      impform := TImportForm.Create(nil);
      if impform.ShowModal = mrCancel then exit;
      Result := Importer.ImportTXT(FileName, Df, impform.ImportSetting);
      FreeAndNil(impform);
    end;
    FreeAndNil(Importer);
  end else begin
    Df := TEpiDataFile.Create();
    Df.OnProgress := ShowProgress;
    Df.OnPassword := GetPassword;
    Result := Df.Open(FileName, LoadOptions);
  end;
end;

function  SaveDataFile(Df: TEpiDataFile; Const FileName: string; IgnoreChecks: Boolean;
                       ShowProgress: TProgressEvent; GetPassword: TRequestPasswordEvent;
                       ExportSettings: Pointer): boolean;
var
  OutDf: TEpiDataFile;
  I, J: Integer;
  TmpField: TEpiField;
  S: string;
  Exporter: TEpiImportExport;
  SaveOptions: TEpiDataFileOptions;
begin
  if (AnsiUpperCase(ExtractFileExt(FileName)) <> '.REC') and
     (AnsiUpperCase(ExtractFileExt(FileName)) <> '.RECXML') then
  begin
    Exporter := TEpiImportExport.Create();
    Exporter.OnProgress := ShowProgress;
    S := AnsiUpperCase(ExtractFileExt(FileName));
    if S = '.DTA' then
      result:= Exporter.ExportStata(FileName, Df, PEpiStataExportSettings(ExportSettings))
    else if S = '.DBF' then
      result := Exporter.ExportDBase(FileName, Df)
    else if (S = '.CSV') or (Trim(FileName) = '') then
      result := Exporter.ExportTXT(FileName, Df, @ExportTxtStandard)
    else if (S = '.XLS') or (S = '.ODS') then
      result := Exporter.ExportSpreadSheet(FileName, Df, PEpiSpreadSheetSettings(ExportSettings));
    FreeAndNil(Exporter);
    Exit;
  end;

  SaveOptions := [];
  If IgnoreChecks then
    Include(SaveOptions, eoIgnoreChecks);

  OutDf := TEpiDataFile.Create(Df.Size);
  OutDf.OnPassword   := GetPassword;
  OutDf.Filelabel    := Df.FileLabel;
  OutDf.ValueLabels.Assign(Df.ValueLabels);

  for i := 0 to Df.NumFields - 1 do
  begin
    TmpField := TEpiField.CreateField(Df[i].FieldType, Df[i].Size);
    OutDf.AddField(TmpField);
    Df[i].Clone(TmpField);
  end;

  OutDf.Save(FileName);
  FreeAndNil(OutDf);
end;

function  DocumentDataFile(Df: TEpiDatafile): TStrings;
var
  res: TStringList;
  FileSiz:LongInt;
  SizeUnit,tmpStr,tmpType,tmpWidth: string;
  UsesValueLabels:boolean;
  nN,nN2,nN3,FieldNumber:integer;
  QuestStr,CheckStr,ValLabelStr:ARRAY [1..25] OF string[20];
  AutoList:Tstringlist;
  aValueLabelSet: TValueLabelSet;
  aValue,aLabel,valuelabelname: string;
  epd: TEpiDatafile;

  Function CutString(VAR s:string; ch:TCharSet; wid:Integer):string;
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
  if (not assigned(Df)) then exit;
  epd := df;
  res:=TStringList.create;
  try
    {Write heading}
    res.Append('');
    TMPSTR := epd.Filename;
    res.Append(Format('DATAFILE: %s',[tmpstr]));
(*    TMPSTR := Utf8ToAnsi(epd.Filelabel);
    res.append('Filelabel: ' + TMPSTR);
    res.Append('');

    {Write datafile information}
    if Trim(epd.FileName) <> '' then
      tmpstr := FitLength('Last revision:',23)+
        FormatDateTime('d. mmm yyyy t',
        FileDateToDateTime(FileAge(epd.Filename)));
    res.Append(tmpstr);    *)
    res.Append(FitLength('Number of fields:',23)+IntToStr(epd.NumFields));
    tmpStr:=FitLength('Number of records:',23);
    IF epd.Size=-1
    THEN tmpStr:=tmpStr+'Error in datafile. Number of records cannot be counted.'
    ELSE tmpStr:=tmpStr+IntToStr(epd.Size);
    res.Append(tmpStr);
    tmpStr:=FitLength('Checks applied: ',23);
    IF (epd.CheckFile.HasCheckFile) and (not epd.CheckFile.ErrorInFile) THEN
      tmpStr := tmpStr + format('Yes (Last revision %s)',
                [FormatDateTime('d. mmm yyyy t', FileDateToDateTime(FileAge(epd.CheckFile.FileName)))])
    ELSE
      tmpStr := tmpStr + 'No';
    res.Append(tmpStr);
    IF epd.CheckFile.ErrorInFile THEN res.Append(DupeString(' ',23)+'Warning: A checkfile exists but it contains errors.');
    res.Append('');

    {Check if value labels are used}
    UsesValueLabels:=((epd.ValueLabels.count>0));

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
    IF UsesValueLabels THEN res.Append(DupeString('-',102))
    ELSE res.Append(DupeString('-',80));
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
            IF (epd.CheckFile.HasCheckFile) AND (NOT epd.CheckFile.ErrorInFile)
               and (Assigned(Epd[nN].CheckField)) THEN
              BEGIN
                nN2:=1;
{                IF epd[nN].Index>0 THEN
                  BEGIN
                    tmpStr:='Key ';
                    IF epd.IndexIsUnique[epd[nN].Index] THEN tmpStr:=tmpStr+'unique ';
                    tmpStr:=tmpStr+IntToStr(epd[nN].Index);
                    CheckStr[nN2]:=tmpStr;
                    INC(nN2);
                  END;         }
                IF epd[nN].CheckField.AutoSearch THEN
                  BEGIN
                    tmpStr:='Autosearch ';
                    //IF FAutoList THEN tmpStr:=tmpStr+' LIST ';
                    TRY
                      autolist:=TStringList.Create;
                      autolist.CommaText:=epd[nN].CheckField.AutoFields;;
                      FOR nN3:=0 TO autolist.count-1 DO tmpStr:=tmpStr+trim(autolist[nN3]) + ' ';
                    FINALLY
                      autolist.Free;
                    END;
                    CheckStr[nN2]:=tmpStr;
                    INC(nN2);
                  END;
                IF epd[nN].CheckField.MustEnter THEN
                  BEGIN
                    CheckStr[nN2]:='Must enter';
                    INC(nN2);
                  END;
                IF epd[nN].CheckField.doRepeat THEN
                  BEGIN
                    CheckStr[nN2]:='Repeat';
                    INC(nN2);
                  END;
                IF epd[nN].CheckField.DefaultValue<>'' THEN
                  BEGIN
                    CheckStr[nN2]:='Default value='+epd[nN].CheckField.DefaultValue;
                    INC(nN2);
                  END;
                IF (epd[nN].CheckField.MissingValues[0]<>'') THEN
                  BEGIN
                    CheckStr[nN2]:='Missing value='+epd[nN].CheckField.MissingValues[0];
                    IF (epd[nN].CheckField.MissingValues[1]<>'') THEN CheckStr[nN2]:=CheckStr[nN2]+','+epd[nN].CheckField.MissingValues[1];
                    IF (epd[nN].CheckField.MissingValues[2]<>'') THEN CheckStr[nN2]:=CheckStr[nN2]+','+epd[nN].CheckField.MissingValues[2];
                  END;
                IF epd[nN].CheckField.NoEnter THEN
                  BEGIN
                    CheckStr[nN2]:='NoEnter';
                    INC(nN2);
                  END;
                IF epd[nN].CheckField.Legal<>'' THEN
                  BEGIN
                    tmpStr:='Legal: '+trim(epd[nN].CheckField.Legal);
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
                IF epd[nN].CheckField.Jumps<>'' THEN
                  BEGIN
                    tmpStr:='Jumps: '+trim(epd[nN].CheckField.Jumps);
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
                IF (epd[nN].CheckField.AfterCmds<>NIL) or (epd[nN].CheckField.BeforeCmds<>NIL) THEN
                  BEGIN
                    IF nN2=25 THEN CheckStr[25]:='More: See Checkfile'
                    ELSE CheckStr[nN2]:='More: See Checkfile';
                  END;
              END;  //if not errorInCheckfile

            {Put value labels in array}
            IF (UsesValueLabels) AND (epd[nN].ValueLabelSet<>NIL) THEN
              BEGIN
                aValueLabelSet:=epd[nN].ValueLabelSet;
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
            tmpType:=FieldTypeToFieldTypeName(epd[nN].FieldType, nil);
            tmpWidth:=IntToStr(epd[nN].FieldLength);
            IF (epd[nN].Fieldtype=ftFloat) AND (epd[nN].NumDecimals>0)
            THEN tmpWidth:=tmpWidth+':'+IntToStr(epd[nN].NumDecimals);
//            IF epd[nN].Fieldtype=ftCrypt THEN tmpWidth:=IntToStr(epd[nN].CryptLength);
            {Write first line}
            IF UsesValueLabels
            THEN res.Append(Format('%3d %-10s %-20s  %-15s %-5s  %-20s  %-20s',
              [FieldNumber,epd[nN].FieldName,QuestStr[1],tmpType,tmpWidth,CheckStr[1],ValLabelStr[1]]))
            ELSE res.Append(Format('%3d %-10s %-20s  %-15s %-5s  %-20s',
              [FieldNumber,epd[nN].FieldName,QuestStr[1],tmpType,tmpWidth,CheckStr[1]]));
            nN2:=2;
            {Write next lines}
            WHILE (nN2<=25) and ((QuestStr[nN2]<>'') OR (CheckStr[nN2]<>'')
            OR (ValLabelStr[nN2]<>'')) DO
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
    res.Append(DupeString('-', nN));
    res.Append('');
    result := res;
  finally
    //
  end;
end;

end.
