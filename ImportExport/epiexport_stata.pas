unit epiexport_stata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles,
  epiexportsettings, epieximtypes;

type

  { TEpiStataExport }

  TEpiStataExport = class
  private
    type
      T
  private
    FDataFile: TEpiDataFile;
    FDataFileSetting: TEpiExportDatafileSettings;
    FStataSettings: TEpiStataExportSetting;
    FStataVersion: TEpiStataVersion;
    FStream: TStream;
  private
    function EncodeString(Const S: String): string;

  private
    procedure WriteStartTag(Const TagName: string);
    procedure WriteEndTag(Const TagName: string);

    // Read text content in tag
    procedure  WriteAsString(Const S: string); overload;
    procedure  WriteAsString(Const S: string; Const Len: Integer); overload;
    procedure  WriteByte(Value: Byte);
    procedure  WriteWord(Value:  Word);
    procedure  WriteDWord(Value: DWord);
    procedure  Write6Word(Value: QWord); // The very-very unusual 6-byte integer, used by the StrLs in Stata 14
    procedure  WriteQWord(Value: QWord);

  private
    procedure SetupFields;

  private
    { .dta section loaders }
    procedure WriteHeader;
    procedure WriteMap;
    procedure WriteVariableTypes;
    procedure WriteVariableNames;
    procedure WriteSortList;
    procedure WriteFormat;
    procedure WriteValueLabelNames;
    procedure WriteVariabelLabels;
    procedure WriteCharacteristics;
    procedure WriteData;
    procedure WriteStrls;
    procedure WriteValueLabels;
    procedure WriteValueLabel;
  public
    function  ExportStata(StataSettings: TEpiStataExportSetting): boolean;
  end;

implementation

uses
  epistringutils, epidatafilestypes, LConvEncoding;

{ TEpiStataExport }

function TEpiStataExport.EncodeString(const S: String): string;
begin
  result := ConvertEncoding(
              S,
              'utf8',
              EpiEncodingToString[FStataSettings.Encoding]
            );
end;

procedure TEpiStataExport.WriteStartTag(const TagName: string);
begin
  WriteAsString('<' + TagName + '>');
end;

procedure TEpiStataExport.WriteEndTag(const TagName: string);
begin
  WriteAsString('</' + TagName + '>');
end;

procedure TEpiStataExport.WriteAsString(const S: string);
begin
  WriteAsString(S, Length(S));
end;

procedure TEpiStataExport.WriteAsString(const S: string; const Len: Integer);
var
  S1: String;
  L: Integer;
begin
  S1 := S;

  L := Length(S1);
  if (L < Len) then
  begin
    SetLength(S1, Len);
    FillChar(S1[L + 1], Len - L, #0);
  end;

  FStream.Write(S1[1], Len);
end;

procedure TEpiStataExport.WriteByte(Value: Byte);
begin
  FStream.WriteByte(Value);
end;

procedure TEpiStataExport.WriteWord(Value: Word);
begin
  FStream.WriteWord(Value);
end;

procedure TEpiStataExport.WriteDWord(Value: DWord);
begin
  FStream.WriteDWord(Value);
end;

procedure TEpiStataExport.Write6Word(Value: QWord);
begin
  FStream.WriteQWord(Value);
end;

procedure TEpiStataExport.WriteQWord(Value: QWord);
begin
  FStream.WriteQWord(Value);
end;

procedure TEpiStataExport.SetupFields;
var
  F: TEpiField;
begin
  for F in FDataFile.Fields do
  begin

  end;
end;

procedure TEpiStataExport.WriteHeader;
var
  S: String;
begin
  // HEADER
  WriteStartTag('header');

  // <release>
  WriteStartTag('release');
  WriteAsString(IntToStr(Integer(FStataSettings.Version)));
  WriteEndTag('release');

  // <byteorder>
  WriteStartTag('bytorder');
  {$IFDEF ENDIAN_LITTLE}
  WriteAsString('LSF');
  {$ELSE}
  WriteAsString('MSF');
  {$ENDIF}
  WriteEndTag('bytorder');

  // <K>  = Number of fields
  WriteStartTag('K');
  WriteWord(FDataFile.Fields.Count);
  WriteEndTag('K');

  // <N>  = Number of observartions/records
  WriteStartTag('N');
  case FStataVersion of
    dta13: WriteDWord(FDataFile.Size);
    dta14: WriteQWord(FDataFile.Size);
  end;
  WriteEndTag('N');

  // <label>  = EpiData DataFile Caption
  WriteStartTag('label');
  case FStataVersion of
    dta13:
      begin
        S := EpiCutString(EncodeString(FDataFile.Caption.Text), 80);
        WriteByte(Length(S));
      end;
    dta14:
      begin
        S := EpiCutString(FDataFile.Caption.Text, 80);
        WriteWord(Length(S));
      end;
  end;
  if Length(S) > 0 then
    WriteAsString(S);
  WriteEndTag('label');

  // <timestamp> - we ignore the timestamp.
  WriteStartTag('timestamp');
  WriteByte(17);
  WriteAsString(FormatDateTime('dd mmm yyyy hh":"nn', Now));
  WriteEndTag('timestamp');

  WriteEndTag('header');
end;

procedure TEpiStataExport.WriteMap;
begin
  WriteStartTag('map');

  //1.       <stata_data>, definitionally 0
  WriteQWord(0);
  //2.       <map>
  WriteQWord(0);
  //3.       <variable_types>
  WriteQWord(0);
  // 4.       <varnames>
  WriteQWord(0);
  // 5.       <sortlist>
  WriteQWord(0);
  // 6.       <formats>
  WriteQWord(0);
  // 7.       <value_label_names>
  WriteQWord(0);
  // 8.       <variable_labels>
  WriteQWord(0);
  // 9.       <characteristics>
  WriteQWord(0);
  // 10.       <data>
  WriteQWord(0);
  // 11.       <strls>
  WriteQWord(0);
  // 12.       <value_labels>
  WriteQWord(0);
  // 13.       </stata_data>
  WriteQWord(0);
  // 14.       end-of-file
  WriteQWord(0);

  WriteEndTag('map');
end;

procedure TEpiStataExport.WriteVariableTypes;
var
  F: TEpiField;
begin
  WriteStartTag('variable_types');

  for F in FDataFile.Fields do
    with F do
      case FieldType of
        ftBoolean:
          WriteWord(StataByteConstXML);

        ftInteger,
        ftAutoInc:
          begin
            if Length <= 2 then
              WriteWord(StataByteConstXML)
            else if Length <= 4 then
              WriteWord(StataIntConstXML)
            else if Length <= 9 then
              WriteWord(StataLongConstXML)
            else if Length >= 10 then
              WriteWord(StataDoubleConstXML)
          end;

        ftString,
        ftUpperString:
          begin
            if (FStataSettings.Version >= dta14) and
               (Length > (2045 div 4))
            then
              WriteWord(StataStrLsConstXML)
            else
              WriteWord(Length);
          end;


        ftDMYDate, ftMDYDate, ftYMDDate,
        ftDMYAuto, ftMDYAuto, ftYMDAuto:
          WriteWord(StataLongConstXML);

        ftTime, ftTimeAuto,
        ftFloat:
          WriteWord(StataDoubleConstXML);
      end;

  WriteEndTag('variable_types');
end;

procedure TEpiStataExport.WriteVariableNames;
var
  Len: Integer;
  F: TEpiField;
  S: String;
begin
  WriteStartTag('varnames');

  case FStataVersion of
    //  32 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := (32 * 1) + 1;
    dta14: Len := (32 * 4) + 1;
  end;

  for F in FDataFile.Fields do
    begin
      case FStataSettings.Version of
        dta13: S := EncodeString(F.Name);
        dta14: S := F.Name;
      end;

      WriteAsString(S, Len);
    end;

  WriteEndTag('varnames');
end;

procedure TEpiStataExport.WriteSortList;
var
  i: Integer;
begin
  WriteStartTag('sortlist');

  for i := 1 to FDataFile.Fields.Count + 1 do
    WriteWord(0);

  WriteEndTag('sortlist');
end;

procedure TEpiStataExport.WriteFormat;
var
  S: String;
  Len: Integer;
  F: TEpiField;
begin
  // <format>
  WriteStartTag('formats');

  case FStataVersion of
    dta13: Len := 49;
    dta14: Len := 57;
  end;

  for F in FDataFile.Fields do
    begin
      case F.FieldType of
        ftInteger, ftAutoInc:
          S := '%' + IntToStr(F.Length) + '.0f';
        ftFloat:
          S := '%' + IntToStr(F.Length) + '.' + IntToStr(F.Decimals) + 'f';
        ftBoolean:
          S := '%1.0f';
        ftString, ftUpperString:
          S := '%' + IntToStr(F.Length) + 's';
        ftDMYDate, ftMDYDate, ftYMDDate,
        ftDMYAuto, ftMDYAuto, ftYMDAuto:
          S := '%d';
        ftTime, ftTimeAuto:
          if FStataSettings.Version >= dta10 then
            begin
              // Stata 10 supports a new time format!
              S := '%tcHH:MM:SS';
            end
          else
            S := '%6.5f';
      end;

      WriteAsString(S, Len);
    end;

  WriteEndTag('formats');
end;

procedure TEpiStataExport.WriteValueLabelNames;
var
  Len: Integer;
  F: TEpiField;
  S: String;
begin
  WriteStartTag('value_label_names');

  case FStataVersion of
    //  32 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := (32 * 1) + 1; // 33
    dta14: Len := (32 * 4) + 1; // 129
  end;

  for F in FDataFile.Fields do
    begin
      S := '';
      if Assigned(F.ValueLabelSet) then
        S := F.ValueLabelSet.Name;

      if FStataVersion = dta13 then
        S := EncodeString(S);

      WriteAsString(S, Len);
    end;

  WriteEndTag('value_label_names');
end;

procedure TEpiStataExport.WriteVariabelLabels;
var
  Len: Integer;
  F: TEpiField;
  S: String;
begin
  WriteStartTag('variable_labels');

  case FStataVersion of
    //  80 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := (80 * 1) + 1; // 81
    dta14: Len := (80 * 4) + 1; // 321
  end;

  for F in FDataFile.Fields do
    begin
      S := '';
      if Assigned(F.ValueLabelSet) then
        S := F.Question.Text;

      if FStataVersion = dta13 then
        S := EncodeString(S);

      WriteAsString(S, Len);
    end;

  WriteEndTag('variable_labels');
end;

procedure TEpiStataExport.WriteCharacteristics;
var
  Len: Integer;
  S: String;
  F: TEpiField;
  I: Integer;
  j: Integer;
begin
  WriteStartTag('characteristics');

  case FStataVersion of
    //  32 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := (32 * 1) + 1; // 81
    dta14: Len := (32 * 4) + 1; // 321
  end;

  // Write export lines to datafile notes first.
  if FStataSettings.ExportLines.Count > 0 then
  begin
    WriteStartTag('ch');

    // We start out by writing the length of the notes in a "special" characteristic called 'note0'
    S := IntToStr(FStataSettings.ExportLines.Count);

    // I = len  (sum of 2 * 33 + length(TmpStr)
    I := 2 * Len + Length(S) + 1;
    WriteDWord(I);
    WriteAsString('_dta', 33);
    WriteAsString('note0', 33);
    WriteAsString(S, Length(S) + 1);
    WriteEndTag('ch');
  end;

  for j := 0 to FStataSettings.ExportLines.Count - 1 do
  begin
    WriteStartTag('ch');

    S := FStataSettings.ExportLines[j];
    I := Len +                 // First variable name or _dta for notes regarding the dataset.
         Len +                 // Character name, in our case 'noteX'
         Length(S) + 1;
    WriteDWord(I);
    WriteAsString('_dta', 33);
    WriteAsString('note' + IntToStr(j+1), 33);
    WriteAsString(S, Length(S) + 1);

    WriteEndTag('ch');
  end;

  // Then write notes for individual fields.
  for F in FDataFile.Fields do
  begin
    if not (F.FieldType in TimeFieldTypes) then
      Continue;

    WriteStartTag('ch');

    I := 2 * Len + 2;  // 2 = 1 char for "1" and 1 char for #0;
    WriteDWord(I);
    WriteAsString(F.Name, 33);
    WriteAsString('note0', 33);
    WriteAsString('1', 2);

    WriteEndTag('ch');
    WriteStartTag('ch');

    S := 'Time variable: Formatted with %tcHH:MM:SS. See "help dates_and_times, marker(formatting)" for details. Date coded as Jan. 1st 1960.';
    WriteByte(1);
    I := 2*33 + Length(S) + 1;
    WriteDWord(I);
    WriteAsString(F.Name, 33);
    WriteAsString('note1', 33);
    WriteAsString(S, Length(S) + 1);

    WriteEndTag('ch');
  end;

  WriteEndTag('characteristics');
end;

procedure TEpiStataExport.WriteData;
begin

end;

procedure TEpiStataExport.WriteStrls;
begin

end;

procedure TEpiStataExport.WriteValueLabels;
begin

end;

procedure TEpiStataExport.WriteValueLabel;
begin

end;

function TEpiStataExport.ExportStata(StataSettings: TEpiStataExportSetting
  ): boolean;
var
  i: Integer;
begin
  FStataSettings := StataSettings;
  FStataVersion  := FStataSettings.Version;

  for i := 0 to StataSettings.DatafileSettings.Count - 1 do
  begin
    FDataFileSetting := TEpiExportDatafileSettings(StataSettings.DatafileSettings[i]);
    FDataFile        := StataSettings.Doc.DataFiles.GetDataFileByName(FDataFileSetting.DatafileName);
    FStream          := FDataFileSetting.ExportStream;

    SetupFields;

    WriteStartTag('stata_dta');

    WriteHeader;
    WriteMap;
    WriteVariableTypes;
    WriteVariableNames;
    WriteSortList;
    WriteFormat;
    WriteValueLabelNames;
    WriteVariabelLabels;
    WriteCharacteristics;
    WriteData;
    WriteStrls;
    WriteValueLabels;

    WriteEndTag('stata_dta');
  end;
end;

end.

