unit episecuritylog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epidatafiles, epivaluelabels, epidatafilerelations;

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
    FKeyFieldValues: TEpiField;       // Commaseperated string with Field=Value entries of key field values.
    FDataContent:    TEpiField;       // Holder for a list of TDataLogEntry's if Type = ltEditRecord
    FLogContent:     TEpiField;       // String holder for other data in log entry, content depends on log type.
  public
    constructor Create(AOwner: TEpiCustomBase; const ASize: integer = 0); override;
    destructor Destroy; override;
    function NewRecords(const Count: Cardinal = 1): integer; override;
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
    property ID:           TEpifield read FID;
    property VariableName: TEpiField read FVariableName;
    property KeyValue:     TEpiField read FKeyValue;
  end;

implementation

uses
  epidatafilestypes, epilogger;

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

  FKeyFieldValues      := NewField(ftString);
  FKeyFieldValues.Name := 'KeyFieldValues';

  FDataContent         := NewField(ftInteger);
  FDataContent.Name    := 'DataContent';

  FLogContent          := NewField(ftString);
  FLogContent.Name     := 'LogContent';
end;

destructor TEpiSecurityDatafile.Destroy;
begin
  inherited Destroy;
end;

function TEpiSecurityDatafile.NewRecords(const Count: Cardinal): integer;
begin
  Result := inherited NewRecords(Count);

  ID.AsInteger[Result] := Result;
end;

end.

