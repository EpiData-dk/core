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
    FDataFile: TEpiDataFile;
    FDataFileSetting: TEpiExportDatafileSettings;
    FStataSettings: TEpiStataExportSetting;
    FStream: TStream;
  private
    function EncodeString(Const S: String): string;

  private
    procedure WriteStartTag(Const TagName: string);
    procedure WriteEndTag(Const TagName: string);

    // Read text content in tag
    procedure  WriteAsString(Const S: string);
    procedure  WriteByte(Value: Byte);
    procedure  WriteWord(Value:  Word);
    procedure  WriteDWord(Value: DWord);
    procedure  Write6Word(Value: QWord); // The very-very unusual 6-byte integer, used by the StrLs in Stata 14
    procedure  WriteQWord(Value: QWord);

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
    procedure WriteCharacteristic;
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
  FStream.Write(S[1], Length(S));
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
  case FStataSettings.Version of
    dta13: WriteDWord(FDataFile.Size);
    dta14: WriteQWord(FDataFile.Size);
  end;
  WriteEndTag('N');

  // <label>  = EpiData DataFile Caption
  WriteStartTag('label');
  case FStataSettings.Version of
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

  case FStataSettings.Version of
    //  32 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := (32 * 1) + 1;
    dta14: Len := (32 * 4) + 1;
  end;

  for F in FDataFile.Fields do
    begin
      SetLength(S, Len);
      FillChar(S, Len, #0);
      case FStataSettings.Version of
        dta13: S := EncodeString(F.Name);
        dta14: S := F.Name;
      end;

      WriteAsString(S);
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
begin

end;

procedure TEpiStataExport.WriteValueLabelNames;
begin

end;

procedure TEpiStataExport.WriteVariabelLabels;
begin

end;

procedure TEpiStataExport.WriteCharacteristics;
begin

end;

procedure TEpiStataExport.WriteCharacteristic;
begin

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

  for i := 0 to StataSettings.DatafileSettings.Count - 1 do
  begin
    FDataFileSetting := TEpiExportDatafileSettings(StataSettings.DatafileSettings[i]);
    FDataFile        := StataSettings.Doc.DataFiles.GetDataFileByName(FDataFileSetting.DatafileName);

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

