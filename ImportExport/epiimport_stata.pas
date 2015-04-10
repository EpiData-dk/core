unit epiimport_stata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epidatafiles, epieximtypes;

type

  { TEpiStataImport }

  TEpiStataImport = class
  private
    { Internal variables }
    FDataFile: TEpiDataFile;
    FStream:   TStream;
    FStataVersion:    TEpiStataVersion;
    FByteOrder:       TEpiStataEndian;
    FFieldCount:      Word;
    FObsCount:        QWord;

    FVariableTypes:   Array of Word;
    FVariableNames:   TStrings;
    FVariableFormats: TStrings;
    FVariableLabels:  TStrings;

    FValuelabelNames: TStrings;

  private
    { Errors }
    procedure DoError(Const Msg: String);
  private
    { Stream Read functions }

    // Reads from the stream a start-/end tag with the given name. If not found reports an error.
    procedure ReadStartTag(Const Name: String);
    procedure ReadEndTag(Const Name: String);

    // Read text content in tag
    function  ReadAsString(Length: Integer): string;
    function  ReadByte:  Byte;
    function  ReadWord:  Word;
    function  ReadDWord: DWord;
    function  ReadQWord: QWord;

  private
    { .dta section loaders }
    procedure ReadHeader;
    procedure ReadMap;
    procedure ReadVariableTypes;
    procedure ReadVariableNames;
    procedure ReadSortList;
    procedure ReadFormat;
    procedure ReadValueLabelNames;
    procedure ReadVariabelLabels;
    procedure ReadCharacteristics;
    procedure ReadCharacteristic;
    procedure ReadData;
    procedure ReadStrls;
    procedure ReadValueLabels;
  public
    function    ImportStata(
                  Const DataStream: TStream;
                  Const Doc: TEpiDocument;
                  var DataFile: TEpiDataFile;
                  ImportData: boolean = true
                ): Boolean; overload;
  private
    FOnProgress: TEpiProgressEvent;
  public
    property    OnProgress: TEpiProgressEvent read FOnProgress write FOnProgress;
  end;


implementation

uses
  epistringutils;

{ TEpiStataImport }

procedure TEpiStataImport.DoError(const Msg: String);
begin
  Raise Exception.Create(Msg);
end;

procedure TEpiStataImport.ReadStartTag(const Name: String);
var
  TagName: String;
  S: String;
  Len: Integer;
begin
  TagName := '<' + Name + '>';
  Len :=  Length(TagName);
  S := ReadAsString(Len);

  if (S <> TagName)
  then
    DoError('Tag not found: ' + TagName);
end;

procedure TEpiStataImport.ReadEndTag(const Name: String);
var
  TagName: String;
  S: String;
  Len: Integer;
begin
  TagName := '</' + Name + '>';
  Len :=  Length(TagName);
  S := ReadAsString(Len);

  if (S <> TagName)
  then
    DoError('Tag not found: ' + TagName);
end;

function TEpiStataImport.ReadAsString(Length: Integer): string;
var
  Buffer: Array of char;
begin
  if (Length = 0) then
    Exit('');

  SetLength(Buffer, Length + 1);
  FStream.Read(Buffer[0], Length);
  Buffer[Length] := #0;

  Result := String(Buffer);
end;

function TEpiStataImport.ReadByte: Byte;
begin
  result := FStream.ReadByte;
end;

function TEpiStataImport.ReadWord: Word;
begin
  Result := FStream.ReadWord;

  case FByteOrder of
    eseLittleEndian: Result := LEtoN(Result);
    eseBigEndian:    Result := BEtoN(Result);
  end;
end;

function TEpiStataImport.ReadDWord: DWord;
begin
  Result := FStream.ReadDWord;

  case FByteOrder of
    eseLittleEndian: Result := LEtoN(Result);
    eseBigEndian:    Result := BEtoN(Result);
  end;
end;

function TEpiStataImport.ReadQWord: QWord;
begin
  Result := FStream.ReadQWord;

  case FByteOrder of
    eseLittleEndian: Result := LEtoN(Result);
    eseBigEndian:    Result := BEtoN(Result);
  end;
end;

procedure TEpiStataImport.ReadHeader;
var
  S: String;
  Len: Word;
begin
  // HEADER
  ReadStartTag('header');

  // <release>
  ReadStartTag('release');
  FStataVersion := TEpiStataVersion(StrToInt(ReadAsString(3)));
  if not (FStataVersion in [dta13, dta14]) then
    DoError('Unknown Stata Version');
  ReadEndTag('release');


  // <byteorder>
  ReadStartTag('byteorder');
  S := ReadAsString(3);
  if S = 'MSF' then
    FByteOrder := eseBigEndian
  else if S = 'LSF' then
    FByteOrder := eseLittleEndian
  else
    DoError('Unknown Byteorder');
  ReadEndTag('byteorder');

  // <K>  = Number of fields
  ReadStartTag('K');
  FFieldCount := ReadWord;
  ReadEndTag('K');

  // <N>  = Number of observartions/records
  ReadStartTag('N');
  case FStataVersion of
    dta13: FObsCount := ReadDWord;
    dta14: FObsCount := ReadQWord;
  end;
  ReadEndTag('N');


  // <label>  = EpiData DataFile Caption
  ReadStartTag('label');
  Case FStataVersion of
    dta13: Len := ReadByte;
    dta14: Len := ReadWord;
  end;

  S := ReadAsString(Len);
  if (FStataVersion = dta13) then
    S := EpiUnknownStrToUTF8(S);

  FDataFile.Caption.Text := S;
  ReadEndTag('label');

  // <timestamp> - we ignore the timestamp.
  ReadStartTag('timestamp');
  Len := ReadByte;
  ReadAsString(Len);
  ReadEndTag('timestamp');

  ReadEndTag('header');
end;

procedure TEpiStataImport.ReadMap;
begin
  // <map> - a list of indices referencing individual sections for the stream.
  // Currently we are not using these.

  ReadStartTag('map');

  //1.       <stata_data>, definitionally 0
  ReadQWord;
  //2.       <map>
  ReadQWord;
  //3.       <variable_types>
  ReadQWord;
  // 4.       <varnames>
  ReadQWord;
  // 5.       <sortlist>
  ReadQWord;
  // 6.       <formats>
  ReadQWord;
  // 7.       <value_label_names>
  ReadQWord;
  // 8.       <variable_labels>
  ReadQWord;
  // 9.       <characteristics>
  ReadQWord;
  // 10.       <data>
  ReadQWord;
  // 11.       <strls>
  ReadQWord;
  // 12.       <value_labels>
  ReadQWord;
  // 13.       </stata_data>
  ReadQWord;
  // 14.       end-of-file
  ReadQWord;

  ReadEndTag('map');
end;

procedure TEpiStataImport.ReadVariableTypes;
var
  i: Integer;
begin
  SetLength(FVariableTypes, FFieldCount);

  // <variable_types>
  ReadStartTag('variable_types');

  for i := 0 to FFieldCount - 1 do
    FVariableTypes[i] := ReadWord;

  ReadEndTag('variable_types');
end;

procedure TEpiStataImport.ReadVariableNames;
var
  Len: Integer;
  i: Integer;
begin
  FVariableNames := TStringList.Create;

  // <varnames>
  ReadStartTag('varnames');

  case FStataVersion of
    //  32 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := (32 * 1) + 1;
    dta14: Len := (32 * 4) + 1;
  end;

  for i := 0 to FFieldCount - 1 do
    FVariableNames.Add(ReadAsString(Len));

  if (FStataVersion = dta13) then
    EpiUnknownStringsToUTF8(FVariableNames);

  ReadEndTag('varnames');
end;

procedure TEpiStataImport.ReadSortList;
var
  i: Integer;
begin
  // <sortlist>  - Not used in epidata.
  ReadStartTag('sortlist');
  for i := 1 to FFieldCount + 1 do
    ReadWord;
  ReadEndTag('sortlist');
end;

procedure TEpiStataImport.ReadFormat;
var
  Len: Integer;
  i: Integer;
begin
  FVariableFormats := TStringList.Create;

  // <format>
  ReadStartTag('formats');

  case FStataVersion of
    //  32 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := 49;
    dta14: Len := 57;
  end;

  for i := 0 to FFieldCount - 1 do
    FVariableFormats.Add(ReadAsString(Len));

  if (FStataVersion = dta13) then
    EpiUnknownStringsToUTF8(FVariableFormats);

  ReadEndTag('formats');
end;

procedure TEpiStataImport.ReadValueLabelNames;
var
  Len: Integer;
  i: Integer;
begin
  FValuelabelNames := TStringList.Create;

  // <value_label_names>
  ReadStartTag('value_label_names');

  case FStataVersion of
    //  32 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := (32 * 1) + 1; // 33
    dta14: Len := (32 * 4) + 1; // 129
  end;

  for i := 0 to FFieldCount - 1 do
    FValuelabelNames.Add(ReadAsString(Len));

  if (FStataVersion = dta13) then
    EpiUnknownStringsToUTF8(FValuelabelNames);

  ReadEndTag('value_label_names');
end;

procedure TEpiStataImport.ReadVariabelLabels;
var
  Len: Integer;
  i: Integer;
begin
  FVariableLabels := TStringList.Create;

  // <value_label_names>
  ReadStartTag('variable_labels');

  case FStataVersion of
    //  80 Characters (* 4 for UTF-8) and a terminal #0
    dta13: Len := (80 * 1) + 1; // 81
    dta14: Len := (80 * 4) + 1; // 321
  end;

  for i := 0 to FFieldCount - 1 do
    FVariableLabels.Add(ReadAsString(Len));

  if (FStataVersion = dta13) then
    EpiUnknownStringsToUTF8(FVariableLabels);

  ReadEndTag('variable_labels');
end;

procedure TEpiStataImport.ReadCharacteristics;
begin
  ReadStartTag('variable_types');
  ReadEndTag('variable_types');

end;

procedure TEpiStataImport.ReadCharacteristic;
begin
  ReadStartTag('ch');
  ReadEndTag('ch');
end;

procedure TEpiStataImport.ReadData;
begin
  ReadStartTag('variable_types');
  ReadEndTag('variable_types');

end;

procedure TEpiStataImport.ReadStrls;
begin
  ReadStartTag('variable_types');
  ReadEndTag('variable_types');

end;

procedure TEpiStataImport.ReadValueLabels;
begin
  ReadStartTag('variable_types');
  ReadEndTag('variable_types');

end;

function TEpiStataImport.ImportStata(const DataStream: TStream;
  const Doc: TEpiDocument; var DataFile: TEpiDataFile; ImportData: boolean
  ): Boolean;
begin
  FStream := DataStream;
  FStream.Position := 0;

  if not Assigned(DataFile) then
    DataFile := Doc.DataFiles.NewDataFile;

  FDataFile := DataFile;
  FDataFile.Fields.Sorted := false;

  try
    ReadStartTag('stata_dta');

    ReadHeader;
    ReadMap;
    ReadVariableTypes;
    ReadVariableNames;
    ReadSortList;
    ReadFormat;
    ReadValueLabelNames;
    ReadVariabelLabels;
    ReadCharacteristics;
    ReadData;
    ReadStrls;
    ReadValueLabels;

    ReadEndTag('stata_dta');
  finally
  end;
end;

end.

