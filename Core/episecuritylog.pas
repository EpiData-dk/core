unit episecuritylog;

{$codepage utf8}
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
  private
    FInitialized: Boolean;
  protected
    procedure SetLanguage(const LangCode: string; const DefaultLanguage: boolean); override;
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
  end;

  { TEpiCustomSecurityDatafile }

  TEpiCustomSecurityDatafile = class(TEpiDataFile)
  public
    function NewField(FieldType: TEpiFieldType): TEpiField; override;
  end;

  { TEpiSecurityDatafile }

  TEpiSecurityDatafile = class(TEpiCustomSecurityDatafile)
  private
    FID:             TEpiField;       // AutoInc ID and Key Variable
    FUserName:       TEpiField;       // Username for the log entry
    FDate:           TEpiField;       // Time of log entry
    FTime:           TEpiField;       // Time of log entry
    FCycle:          TEpiField;       // Cycly no fo the log entry
    FLogType:        TEpiField;       // Category (valuelabel) for the type of log entry
    FMachineName:    TEpiField;       // Current machine making the log entry
    FFilename:       TEpiField;       // Location of the file
    FDataFileName:   TEpiField;       // Name of datafile for log entry (if applicable)
    FLogContent:     TEpiField;       // String holder for other data in log entry, content depends on log type.
  private
    function InternalCreateField(FieldType: TEpiFieldType; const AName, ACaption: UTF8String): TEpiField;
  public
    constructor Create(AOwner: TEpiCustomBase; const ASize: integer = 0); override;
    destructor Destroy; override;
    procedure LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure FixupReferences(EpiClassType: TEpiCustomBaseClass; ReferenceType: Byte; const ReferenceId: string); override;
    function  NewRecords(const Count: Cardinal = 1): integer; override;
    property  ID: TEpifield read FID;
    property  UserName: TEpiField read FUserName;
    property  Date: TEpiField read FDate;
    property  Time: TEpiField read FTime;
    property  Cycle: TEpiField read FCycle;
    property  LogType: TEpiField read FLogType;
    property  MachineName: TEpiField read FMachineName;
    property  Filename: TEpiField read FFilename;
    property  DataFileName: TEpiField read FDataFileName;
    property  LogContent: TEpiField read FLogContent;
  end;

  { TEpiSecurityDataEventLog }

  TEpiSecurityDataEventLog = class(TEpiCustomSecurityDatafile)
  private
    FID:           TEpiField;  // ID linked with FID on TEpiSecurityDatafile
    FVariableName: TEpiField;
    FBeforeValue:  TEpiField;
    FAfterValue:   TEpiField;
  public
    constructor Create(AOwner: TEpiCustomBase; const ASize: integer = 0); override;
    procedure LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    property ID:           TEpifield read FID;
    property VariableName: TEpiField read FVariableName;
    property BeforeValue:  TEpiField read FBeforeValue;
    property AfterValue:   TEpiField read FAfterValue;
  end;

  { TEpiSecurityKeyFieldLog }

  TEpiSecurityKeyFieldLog = class(TEpiCustomSecurityDatafile)
  private
    FID:           TEpiField;  // ID linked with FID on TEpiSecurityDatafile
    FVariableName: TEpiField;
    FKeyValue:     TEpifield;
  public
    constructor Create(AOwner: TEpiCustomBase; const ASize: integer = 0); override;
    procedure LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    property ID:           TEpifield read FID;
    property VariableName: TEpiField read FVariableName;
    property KeyValue:     TEpiField read FKeyValue;
  end;

implementation

uses
  epilogger, epidocument;

{ TEpiCustomSecurityDatafile }

function TEpiCustomSecurityDatafile.NewField(FieldType: TEpiFieldType
  ): TEpiField;
begin
  Result := inherited NewField(FieldType);
  Result.Top := Fields.Count * 30;
  Result.Left := 100;
end;

{ TEpiSecurityKeyFieldLog }

constructor TEpiSecurityKeyFieldLog.Create(AOwner: TEpiCustomBase;
  const ASize: integer);
begin
  inherited Create(AOwner, ASize);
  FProtectedItem := True;

  FID               := NewField(ftInteger);
  FID.Name          := 'ID';
  FID.Question.Text := 'ID';
  KeyFields.AddItem(FID);

  FVariableName     := NewField(ftString);
  FVariableName.Name := 'VarName';
  FVariableName.Question.Text := 'Variable Name';

  FKeyValue          := NewField(ftString);
  FKeyValue.Name     := 'KeyValue';
  FKeyValue.Question.Text := 'Key Value';
end;

procedure TEpiSecurityKeyFieldLog.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
begin
  inherited LoadFromXml(Root, ReferenceMap);
  FID           := Fields.FieldByName['ID'];
  FVariableName := Fields.FieldByName['VarName'];
  FKeyValue     := Fields.FieldByName['KeyValue'];
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
  FID.Question.Text := 'ID';
  KeyFields.AddItem(FID);

  FVariableName     := NewField(ftString);
  FVariableName.Name := 'VarName';
  FVariableName.Question.Text := 'Variable Name';

  FBeforeValue       := NewField(ftString);
  FBeforeValue.Name  := 'BeforeValue';
  FBeforeValue.Question.Text := 'Value Before';

  FAfterValue        := NewField(ftString);
  FAfterValue.Name   := 'AfterValue';
  FAfterValue.Question.Text := 'Value After';
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

{ TEpiSecurityDatafileRelation }

constructor TEpiSecurityDatafileRelation.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FProtectedItem := true;
end;


{ TEpiSecurityValuelabelSet }

procedure TEpiSecurityValuelabelSet.SetLanguage(const LangCode: string;
  const DefaultLanguage: boolean);
var
  LE: TEpiLogEntry;
  VL: TEpiIntValueLabel;
begin
  inherited SetLanguage(LangCode, DefaultLanguage);

  // A little hacky. But adding the content of the to the Set during create
  // is not posible since the language is not applied at that time. Hence the
  // labels in a single VL is not correctly set.
  if FInitialized then exit;

  for LE in TEpiLogEntry do
  begin
    VL := TEpiIntValueLabel(NewValueLabel);
    VL.Value := Word(LE);
    VL.TheLabel.Text := EpiLogEntryText[LE];
  end;

  FInitialized := true;
end;

function TEpiSecurityValuelabelSet.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  // if we already have initialized the valuelabels, then
  // clear them and do the clone instead
  if TEpiSecurityValuelabelSet(Dest).FInitialized then
    TEpiSecurityValuelabelSet(Dest).ClearAndFree;
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
end;

constructor TEpiSecurityValuelabelSet.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  ItemOwner := true;
  FProtectedItem := true;
  LabelType := ftInteger;
  FInitialized := false;
end;

{ TEpiSecurityDatafile }

function TEpiSecurityDatafile.InternalCreateField(FieldType: TEpiFieldType;
  const AName, ACaption: UTF8String): TEpiField;
begin
  result               := NewField(FieldType);
  result.Name          := AName;
  result.Question.Text := ACaption;
end;

constructor TEpiSecurityDatafile.Create(AOwner: TEpiCustomBase;
  const ASize: integer);
begin
  inherited Create(AOwner, ASize);
  FProtectedItem := true;

  FID := InternalCreateField(ftAutoInc, 'ID', 'ID');
  KeyFields.AddItem(FID);

  FUserName     := InternalCreateField(ftAutoInc, 'UserName',    'User Name');
  FDate         := InternalCreateField(ftDMYDate, 'Date',        'Date');
  FTime         := InternalCreateField(ftTime,    'Time',        'Time');
  FCycle        := InternalCreateField(ftInteger, 'Cycle',       'Cycle No');
  FLogType      := InternalCreateField(ftInteger, 'LogType',     'Log Type');
  // Version 6:
  FMachineName  := InternalCreateField(ftString,  'MachineName', 'Machine Name');
  // Version 6:
  FFilename     := InternalCreateField(ftString,  'Filename',    'Filename location');
  // Change from DataFormName in v5 to DatasetName in v6
  FDataFileName := InternalCreateField(ftString,  'DatasetName', 'Dataset Name');
  FLogContent   := InternalCreateField(ftString,  'LogContent',  'Additional Content');
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

  // In version 6 - two additional fields were introduced, since they do not exists in v5 they are NOT loaded and must be recreated.
  if (TEpiDocument(RootOwner).Version < 6) then
    begin
      FMachineName  := InternalCreateField(ftString,  'MachineName', 'Machine Name');
      FFilename     := InternalCreateField(ftString,  'Filename',    'Filename location');
      // Add a fixup reference, such that old data can be placed into the new fields correctly.
      ReferenceMap.AddFixupReference(Self, TEpiSecurityDatafile, 0, '');
    end
  else
    begin
      FMachineName  := Fields.FieldByName['MachineName'];
      FFilename     := Fields.FieldByName['Filename'];
    end;
end;

procedure TEpiSecurityDatafile.FixupReferences(
  EpiClassType: TEpiCustomBaseClass; ReferenceType: Byte;
  const ReferenceId: string);
var
  i: Integer;
begin
  inherited FixupReferences(EpiClassType, ReferenceType, ReferenceId);

  if (EpiClassType <> TEpiSecurityDatafile) then exit;
  // currently there is only a single type of reference fixup to do - replace some of the data into the new fields
  if (ReferenceType <> 0) then exit;


  for i := 0 to Size - 1 do
    begin
      if (TEpiLogEntry(LogType.AsInteger[i]) in [ltFailedLogin, ltBlockedLogin]) then
        begin
          MachineName.AsString[i] := LogContent.AsString[i];
          LogContent.IsMissing[i] := true;
        end;
    end;
end;


function TEpiSecurityDatafile.NewRecords(const Count: Cardinal): integer;
begin
  Result := inherited NewRecords(Count);

  // During load do nothing.
  if (not IsLoadingRecords) then
    if (Result > 0) then
      ID.AsInteger[Result] := ID.AsInteger[Result - 1] + 1
    else
      ID.AsInteger[Result] := 0;
end;

end.

