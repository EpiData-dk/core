unit epilogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, epidatafilestypes, epicustombase, epitools_search,
  Laz2_DOM;

type
  TEpiLogEntry = (
    ltNone,            // The "empty" entry in the EnumField
    ltSuccessLogin,    // A succesfull login
    ltFailedLogin,     // An unsuccessfull login
    ltSearch,          // Search performed
    ltNewRecord,       // New Record added
    ltEditRecord,      // Edited an existing record
    ltViewRecord,      // Changed record no. in viewer
    ltPack,            // Packed datafiles
    ltAppend           // Appended data to datafiles
  );


  { TEpiEnumField }

  TEpiEnumField = class(TEpiField)
  private
    FData: array of TEpiLogEntry;
  protected
    function GetAsEnum(const Index: Integer): TEpiLogEntry; virtual;
    procedure SetAsEnum(const Index: Integer; AValue: TEpiLogEntry); virtual;
    procedure SetCapacity(AValue: Integer); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    function GetCapacity: Integer; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetHasDefaultValue(const AValue: boolean); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetAsTime(const index: Integer; const AValue: EpiTime); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsDateTime(const index: Integer; const AValue: EpiDateTime); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure MovePackData(const SrcIdx, DstIdx, Count: integer); override;
    function GetIsMissing(const index: Integer): boolean; override;
    function GetHasDefaultValue: boolean; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetAsTime(const index: Integer): EpiTime; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsDateTime(const index: Integer): EpiDateTime; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsBoolean(const index: Integer): EpiBool; override;
    procedure DoSetDefaultValueAsString(const AValue: string); override;
    function DoGetDefaultValueAsString: string; override;
    function DoCompare(i, j: integer): integer; override;
  public
    procedure Exchange(i, j: integer); override;
    procedure ResetDefaultValue; override;
    procedure ResetData; override;
    function FormatString(const FillSpace: boolean = false): string; override;
    property AsEnum[Const Index: Integer]: TEpiLogEntry read GetAsEnum write SetAsEnum;
  public
    constructor Create(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType); override;
    class function DefaultMissing: TEpiLogEntry;
  end;

  { TEpiLog }

  TEpiLog = class(TEpiDataFile)
  private
    FUserNames:      TEpiField;
    FTime:           TEpiField;
    FCycle:          TEpiField;
    FType:           TEpiEnumField;
    FDataFileNames:  TEpiField;
    FLogContent:     TEpiField;
  public
    constructor Create(AOwner: TEpiCustomBase; const aSize: integer = 0);
  end;

  { TEpiLogger }

  TEpiLogger = class(TEpiCustomBase)
  private
    FLogDatafile: TEpiLog;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    function    XMLName: string; override;
    function    SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;

  { Logging properties }
  private
    FUserName: string;
    FDatafile: TEpiDataFile;
    procedure SetUserName(AValue: string);
    procedure SetDatafile(AValue: TEpiDataFile);
  public
    property   Datafile: TEpiDataFile read FDatafile write SetDatafile;
    property   UserName: string read FUserName write SetUserName;

  { Logging methods }
  public
    procedure  LogLoginSuccess();
    procedure  LogLoginFail();
    procedure  LogSearch(Search: TEpiSearch);
    procedure  LogRecordNew();
    procedure  LogRecordEdit(EditedFields: TEpiFields);
    procedure  LogRecordView(RecordNo: Integer);
    procedure  LogPack();
    procedure  LogAppend();
  end;

implementation

uses
  typinfo, epidocument;

constructor TEpiLogger.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

function TEpiLogger.XMLName: string;
begin
  Result := inherited XMLName;
end;

function TEpiLogger.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);
end;

procedure TEpiLogger.LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap
  );
begin
  inherited LoadFromXml(Root, ReferenceMap);
end;

procedure TEpiLogger.SetUserName(AValue: string);
begin
  if FUserName = AValue then Exit;
  FUserName := AValue;
end;

procedure TEpiLogger.SetDatafile(AValue: TEpiDataFile);
begin
  if FDatafile = AValue then Exit;
  FDatafile := AValue;
end;

procedure TEpiLogger.LogLoginSuccess;
begin

end;

procedure TEpiLogger.LogLoginFail;
begin

end;

procedure TEpiLogger.LogSearch(Search: TEpiSearch);
begin

end;

procedure TEpiLogger.LogRecordNew;
begin

end;

procedure TEpiLogger.LogRecordEdit(EditedFields: TEpiFields);
begin

end;

procedure TEpiLogger.LogRecordView(RecordNo: Integer);
begin

end;

procedure TEpiLogger.LogPack;
begin

end;

procedure TEpiLogger.LogAppend;
begin

end;

{ TEpiEnumField }

function TEpiEnumField.GetAsEnum(const Index: Integer): TEpiLogEntry;
begin
  CheckIndex(Index);
  Result := FData[Index];
end;

procedure TEpiEnumField.SetAsEnum(const Index: Integer; AValue: TEpiLogEntry);
begin
  CheckIndex(Index);
  FData[Index] := AValue;

end;

function TEpiEnumField.GetAsString(const index: Integer): EpiString;
begin
  Result := GetEnumName(TypeInfo(TEpiLogEntry), AsInteger[Index]);
end;

function TEpiEnumField.DoCompare(i, j: integer): integer;
begin
  result := 0;
end;

function TEpiEnumField.DoGetDefaultValueAsString: string;
begin
  result := '';
end;

procedure TEpiEnumField.DoSetDefaultValueAsString(const AValue: string);
begin
  //
end;

function TEpiEnumField.GetAsBoolean(const index: Integer): EpiBool;
begin

end;

function TEpiEnumField.GetAsDate(const index: Integer): EpiDate;
begin

end;

function TEpiEnumField.GetAsDateTime(const index: Integer): EpiDateTime;
begin

end;

function TEpiEnumField.GetAsFloat(const index: Integer): EpiFloat;
begin

end;

function TEpiEnumField.GetAsTime(const index: Integer): EpiTime;
begin

end;

function TEpiEnumField.GetAsValue(const index: Integer): EpiVariant;
begin

end;

function TEpiEnumField.GetHasDefaultValue: boolean;
begin

end;

function TEpiEnumField.GetIsMissing(const index: Integer): boolean;
begin

end;

procedure TEpiEnumField.MovePackData(const SrcIdx, DstIdx, Count: integer);
begin

end;

procedure TEpiEnumField.SetAsBoolean(const index: Integer; const AValue: EpiBool
  );
begin

end;

procedure TEpiEnumField.SetAsDate(const index: Integer; const AValue: EpiDate);
begin

end;

procedure TEpiEnumField.SetAsDateTime(const index: Integer;
  const AValue: EpiDateTime);
begin

end;

procedure TEpiEnumField.SetAsFloat(const index: Integer; const AValue: EpiFloat
  );
begin

end;

procedure TEpiEnumField.SetAsTime(const index: Integer; const AValue: EpiTime);
begin

end;

procedure TEpiEnumField.SetAsValue(const index: Integer;
  const AValue: EpiVariant);
begin

end;

procedure TEpiEnumField.SetHasDefaultValue(const AValue: boolean);
begin

end;

procedure TEpiEnumField.SetIsMissing(const index: Integer; const AValue: boolean
  );
begin

end;

function TEpiEnumField.GetCapacity: Integer;
begin
  result := System.Length(FData);
end;

function TEpiEnumField.GetAsInteger(const index: Integer): EpiInteger;
begin
  Result := EpiInteger(AsEnum[Index]);
end;

procedure TEpiEnumField.SetAsString(const index: Integer;
  const AValue: EpiString);
begin
  SetAsInteger(Index, GetEnumValue(TypeInfo(TEpiLogEntry), AValue));
end;

procedure TEpiEnumField.SetCapacity(AValue: Integer);
var
  i: LongInt;
begin
  if AValue = Capacity then exit;
  System.SetLength(FData, AValue);
  for i := Capacity to AValue-1 do
    FData[i] := DefaultMissing;
  FCapacity := AValue;
end;

procedure TEpiEnumField.SetAsInteger(const index: Integer;
  const AValue: EpiInteger);
begin
  SetAsEnum(Index, TEpiLogEntry(AValue));
end;

procedure TEpiEnumField.Exchange(i, j: integer);
var
  Tmp: TEpiLogEntry;
begin
  Tmp := AsEnum[I];
  AsEnum[J] := AsEnum[I];
  AsEnum[I] := Tmp;
end;

function TEpiEnumField.FormatString(const FillSpace: boolean): string;
begin

end;

procedure TEpiEnumField.ResetData;
begin
  FillByte(FData[0], Length, 0);
end;

procedure TEpiEnumField.ResetDefaultValue;
begin

end;

constructor TEpiEnumField.Create(AOwner: TEpiCustomBase;
  AFieldType: TEpiFieldType);
begin
  inherited Create(AOwner, AFieldType);
end;

class function TEpiEnumField.DefaultMissing: TEpiLogEntry;
begin
  result := ltNone;
end;

{ TEpiLog }

constructor TEpiLog.Create(AOwner: TEpiCustomBase; const aSize: integer);
begin
  inherited Create(AOwner, aSize);

  FUserNames := Fields.NewField(ftString);
  FTime      := Fields.NewField(ftTime);
  FCycle     := Fields.NewField(ftInteger);
  FType      := TEpiEnumField.Create(nil, ftBoolean);
  MainSection.Fields.AddItem(FType);
end;

end.

