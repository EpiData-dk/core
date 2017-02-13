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

  { TEpiSecurityValuelabelSet }

  TEpiSecurityValuelabelSet = class(TEpiValueLabelSet)
  public
    constructor Create(AOwner: TEpiCustomBase); override;
  end;

  { TEpiSecurityDatafile }

  TEpiSecurityDatafile = class(TEpiDataFile)
  private
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
    property  UserName: TEpiField read FUserName;
    property  Date: TEpiField read FDate;
    property  Time: TEpiField read FTime;
    property  Cycle: TEpiField read FCycle;
    property  LogType: TEpiField read FCycle;
    property  DataFileNames: TEpiField read FDataFileName;
    property  KeyFieldValues: TEpiField read FKeyFieldValues;
    property  LogContent: TEpiField read FLogContent;
  end;

implementation

uses
  epidatafilestypes, epilogger;

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

  FUserName       := NewField(ftString);
  FDate           := NewField(ftDMYDate);
  FTime           := NewField(ftTime);
  FCycle          := NewField(ftInteger);
  FLogType        := NewField(ftInteger);
  FDataFileName   := NewField(ftString);
  FKeyFieldValues := NewField(ftString);
  FDataContent    := NewField(ftInteger);
  FLogContent     := NewField(ftString);
end;

destructor TEpiSecurityDatafile.Destroy;
begin
  inherited Destroy;
end;

end.

