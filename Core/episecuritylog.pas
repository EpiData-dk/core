unit episecuritylog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epidatafiles, epivaluelabels, epidatafilerelations, Laz2_DOM,
  epidatafilestypes;

type

  { TEpiSecurityDatafileRelation }

  TEpiSecurityDatafileRelation = class(TEpiMasterRelation)
  public
    constructor Create(AOwner: TEpiCustomBase); override;
  end;

  { TEpiSecurityDatafileDetailRelation }

  TEpiSecurityDatafileDetailRelation = class(TEpiDetailRelation)
  public
    constructor Create(AOwner: TEpiCustomBase); override;
  end;

  { TEpiSecurityValuelabelSet }

  TEpiSecurityValuelabelSet = class(TEpiValueLabelSet)
  public
    constructor Create(AOwner: TEpiCustomBase); override;
  end;

  { TEpiSecurityDatafile }

  TEpiSecurityDatafile = class(TEpiDataFile)
  private
    FID:             TEpiField;       // AutoInc ID and Key Variable
    FUserName:       TEpiField;       // Username for the log entry
    FDate:           TEpiField;       // Time of log entry
    FTime:           TEpiField;       // Time of log entry
    FCycle:          TEpiField;       // Cycly no fo the log entry
    FLogType:        TEpiField;       //
    FDataFileName:   TEpiField;       // Name of datafile for log entry (if applicable)
//    FKeyFieldValues: TEpiField;       // Commaseperated string with Field=Value entries of key field values.
//    FDataContent:    TEpiField;       // Holder for a list of TDataLogEntry's if Type = ltEditRecord
    FLogContent:     TEpiField;       // String holder for other data in log entry, content depends on log type.
  public
    constructor Create(AOwner: TEpiCustomBase; const ASize: integer = 0); override;
    destructor Destroy; override;
    procedure LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function  NewRecords(const Count: Cardinal = 1): integer; override;
    function  NewField(FieldType: TEpiFieldType): TEpiField; override;
    property  ID: TEpifield read FID;
    property  UserName: TEpiField read FUserName;
    property  Date: TEpiField read FDate;
    property  Time: TEpiField read FTime;
    property  Cycle: TEpiField read FCycle;
    property  LogType: TEpiField read FLogType;
    property  DataFileName: TEpiField read FDataFileName;
//    property  KeyFieldValues: TEpiField read FKeyFieldValues;
    property  LogContent: TEpiField read FLogContent;
  end;

  { TEpiSecurityDataEventLog }

  TEpiSecurityDataEventLog = class(TEpiDataFile)
  private
    FID:           TEpiField;  // ID linked with FID on TEpiSecurityDatafile
    FVariableName: TEpiField;
    FBeforeValue:  TEpiField;
    FAfterValue:   TEpiField;
  public
    constructor Create(AOwner: TEpiCustomBase; const ASize: integer = 0); override;
    procedure LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function NewField(FieldType: TEpiFieldType): TEpiField; override;
    property ID:           TEpifield read FID;
    property VariableName: TEpiField read FVariableName;
    property BeforeValue:  TEpiField read FBeforeValue;
    property AfterValue:   TEpiField read FAfterValue;
  end;

  { TEpiSecurityKeyFieldLog }

  TEpiSecurityKeyFieldLog = class(TEpiDataFile)
  private
    FID:           TEpiField;  // ID linked with FID on TEpiSecurityDatafile
    FVariableName: TEpiField;
    FKeyValue:     TEpifield;
  public
    constructor Create(AOwner: TEpiCustomBase; const ASize: integer = 0); override;
    procedure LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    function NewField(FieldType: TEpiFieldType): TEpiField; override;
    property ID:           TEpifield read FID;
    property VariableName: TEpiField read FVariableName;
    property KeyValue:     TEpiField read FKeyValue;
  end;

implementation

uses
  epilogger;

{ TEpiSecurityKeyFieldLog }

constructor TEpiSecurityKeyFieldLog.Create(AOwner: TEpiCustomBase;
  const ASize: integer);
begin
  inherited Create(AOwner, ASize);
  FProtectedItem := True;

  FID               := NewField(ftInteger);
  FID.Name          := 'ID';
  KeyFields.AddItem(FID);

  FVariableName     := NewField(ftString);
  FVariableName.Name := 'VarName';

  FKeyValue          := NewField(ftString);
  FKeyValue.Name     := 'KeyValue';
end;

procedure TEpiSecurityKeyFieldLog.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);
  FID           := Fields.FieldByName['ID'];
  FVariableName := Fields.FieldByName['VarName'];
  FKeyValue     := Fields.FieldByName['KeyValue'];
end;

function TEpiSecurityKeyFieldLog.NewField(FieldType: TEpiFieldType): TEpiField;
begin
  Result := inherited NewField(FieldType);
  Result.Top := Fields.Count * 10;
  Result.Left := 50;
end;

{ TEpiSecurityDatafileDetailRelation }

constructor TEpiSecurityDatafileDetailRelation.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FProtectedItem := true;
end;

{ TEpiSecurityDataEventLog }

constructor TEpiSecurityDataEventLog.Create(AOwner: TEpiCustomBase;
  const ASize: integer);
begin
  inherited Create(AOwner, ASize);
  FProtectedItem := True;

  FID               := NewField(ftInteger);
  FID.Name          := 'ID';
  KeyFields.AddItem(FID);

  FVariableName     := NewField(ftString);
  FVariableName.Name := 'VarName';

  FBeforeValue       := NewField(ftString);
  FBeforeValue.Name  := 'BeforeValue';

  FAfterValue        := NewField(ftString);
  FAfterValue.Name   := 'AfterValue';
end;

procedure TEpiSecurityDataEventLog.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);
  FID           := Fields.FieldByName['ID'];
  FVariableName := Fields.FieldByName['VarName'];
  FBeforeValue  := Fields.FieldByName['BeforeValue'];
  FAfterValue   := Fields.FieldByName['AfterValue'];
end;

function TEpiSecurityDataEventLog.NewField(FieldType: TEpiFieldType): TEpiField;
begin
  Result := inherited NewField(FieldType);
  Result.Top := Fields.Count * 10;
  Result.Left := 50;
end;

{ TEpiSecurityDatafileRelation }

constructor TEpiSecurityDatafileRelation.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FProtectedItem := true;
end;


{ TEpiSecurityValuelabelSet }

constructor TEpiSecurityValuelabelSet.Create(AOwner: TEpiCustomBase);
var
  LE: TEpiLogEntry;
  VL: TEpiIntValueLabel;
begin
  inherited Create(AOwner);
  FProtectedItem := true;
  LabelType := ftInteger;

  for LE in TEpiLogEntry do
  begin
    VL := TEpiIntValueLabel(NewValueLabel);

    VL.Value := Word(LE);
    VL.TheLabel.Text := EpiLogEntryText[LE];
  end;
end;

{ TEpiSecurityDatafile }

constructor TEpiSecurityDatafile.Create(AOwner: TEpiCustomBase;
  const ASize: integer);
begin
  inherited Create(AOwner, ASize);
  FProtectedItem := true;

  FID                  := NewField(ftAutoInc);
  FID.Name             := 'ID';
  KeyFields.AddItem(FID);

  FUserName            := NewField(ftString);
  FUserName.Name       := 'UserName';

  FDate                := NewField(ftDMYDate);
  FDate.Name           := 'Date';

  FTime                := NewField(ftTime);
  FTime.Name           := 'Time';

  FCycle               := NewField(ftInteger);
  FCycle.Name          := 'Cycle';

  FLogType             := NewField(ftInteger);
  FLogType.Name        := 'LogType';

  FDataFileName        := NewField(ftString);
  FDataFileName.Name   := 'DataFormName';

//  FKeyFieldValues      := NewField(ftString);
//  FKeyFieldValues.Name := 'KeyFieldValues';

//  FDataContent         := NewField(ftInteger);
//  FDataContent.Name    := 'DataContent';

  FLogContent          := NewField(ftString);
  FLogContent.Name     := 'LogContent';
end;

destructor TEpiSecurityDatafile.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiSecurityDatafile.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);

  // Since all fields are destroyed and recreated during load (they belong to main section)
  // we must re-assign the builtin variable after load.
  FID           := Fields.FieldByName['ID'];
  FUserName     := Fields.FieldByName['UserName'];
  FDate         := Fields.FieldByName['Date'];
  FTime         := Fields.FieldByName['Time'];
  FCycle        := Fields.FieldByName['Cycle'];
  FLogType      := Fields.FieldByName['LogType'];
  FDataFileName := Fields.FieldByName['DataFormName'];
  FLogContent   := Fields.FieldByName['LogContent'];
end;


function TEpiSecurityDatafile.NewRecords(const Count: Cardinal): integer;
begin
  Result := inherited NewRecords(Count);

  // During load do nothing.
  if (not IsLoadingRecords) then
    ID.AsInteger[Result] := Result;
end;

function TEpiSecurityDatafile.NewField(FieldType: TEpiFieldType): TEpiField;
begin
  Result := inherited NewField(FieldType);
  Result.Top := Fields.Count * 10;
  Result.Left := 50;
end;

end.

