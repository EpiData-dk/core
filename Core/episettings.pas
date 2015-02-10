unit episettings;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, Laz2_DOM, epidatafilestypes;

type

  { TEpiXMLSettings }

  // esce = Epi Setting Change Event
  TEpiSettingChangeEvent = (
    esceDateSep, esceTimeSep, esceDecSep, esceScramble
  );

  TEpiXMLSettings = class(TEpiCustomBase)
  private
    FDateSeparator: string;
    FDecimalSeparator: string;
    FFormatSettings: TFormatSettings;
    FMissingString: string;
    FTimeSeparator: string;
    procedure SetDateSeparator(const AValue: string);
    procedure SetDecimalSeparator(const AValue: string);
    procedure SetTimeSeparator(const AValue: string);
    procedure AssignValues(Src: TEpiXMLSettings);
  protected
    function    SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property    DateSeparator: string read FDateSeparator write SetDateSeparator;
    property    TimeSeparator: string read FTimeSeparator write SetTimeSeparator;
    property    DecimalSeparator: string read FDecimalSeparator write SetDecimalSeparator;
    property    FormatSettings: TFormatSettings read FFormatSettings;
  { Cloning }
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  end;

  { TEpiProjectSettings }

  TEpiProjectSettingChangeEvent = (
    epceFieldName, epceFieldBorder, epceBackupInterval, epceBackupShutdown, epceAutoIncStart
  );

  TEpiProjectSettings = class(TEpiCustomBase)
  private
    FAutoIncStartValue: EpiInteger;
    FBackupInterval: Integer;
    FBackupOnShutdown: Boolean;
    FShowFieldBorders: Boolean;
    FShowFieldNames: Boolean;
    procedure   SetAutoIncStartValue(const AValue: EpiInteger);
    procedure   SetBackupInterval(const AValue: Integer);
    procedure   SetBackupOnShutdown(const AValue: Boolean);
    procedure   SetShowFieldBorders(const AValue: Boolean);
    procedure   SetShowFieldNames(const AValue: Boolean);
    procedure   AssignValues(Src: TEpiProjectSettings);
  protected
    function SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
    property    ShowFieldNames: Boolean read FShowFieldNames write SetShowFieldNames;
    property    ShowFieldBorders: Boolean read FShowFieldBorders write SetShowFieldBorders;
    property    BackupInterval: Integer read FBackupInterval write SetBackupInterval;
    property    BackupOnShutdown: Boolean read FBackupOnShutdown write SetBackupOnShutdown;
    property    AutoIncStartValue: EpiInteger read FAutoIncStartValue write SetAutoIncStartValue;
  { Cloning }
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  end;

implementation

uses
  epidocument;

{ TEpiXMLSettings }

procedure TEpiXMLSettings.SetDateSeparator(const AValue: string);
var
  Val: String;
begin
  if FDateSeparator = AValue then exit;
  Val := FDateSeparator;
  FDateSeparator := AValue;
  FFormatSettings.DateSeparator := FDateSeparator[1];
  DoChange(eegXMLSetting, Word(esceDateSep), @Val);
end;

procedure TEpiXMLSettings.SetDecimalSeparator(const AValue: string);
var
  Val: String;
begin
  if FDecimalSeparator = AValue then exit;
  Val := FDecimalSeparator;
  FDecimalSeparator := AValue;
  FFormatSettings.DecimalSeparator := FDecimalSeparator[1];
  DoChange(eegXMLSetting, Word(esceDecSep), @Val);
end;

procedure TEpiXMLSettings.SetTimeSeparator(const AValue: string);
var
  Val: String;
begin
  if FTimeSeparator = AValue then exit;
  Val := FTimeSeparator;
  FTimeSeparator := AValue;
  FFormatSettings.TimeSeparator := TimeSeparator[1];
  DoChange(eegXMLSetting, Word(esceTimeSep), @Val);
end;

procedure TEpiXMLSettings.AssignValues(Src: TEpiXMLSettings);
begin
  FDateSeparator     := Src.FDateSeparator;
  FDecimalSeparator  := Src.FDecimalSeparator;
  FFormatSettings    := Src.FFormatSettings;
  FMissingString     := Src.FMissingString;
  FTimeSeparator     := Src.FTimeSeparator;
end;

function TEpiXMLSettings.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, rsDateSep, DateSeparator);
  SaveDomAttr(Result, rsTimeSep, TimeSeparator);
  SaveDomAttr(Result, rsDecSep,  DecimalSeparator);
end;

constructor TEpiXMLSettings.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);

  FFormatSettings := DefaultFormatSettings;
  FFormatSettings.ShortDateFormat := 'YYYY/MM/DD HH:NN:SS';
  FFormatSettings.LongDateFormat  := 'YYYY/MM/DD HH:NN:SS';
  FFormatSettings.ShortTimeFormat := 'HH:NN:SS';
  FFormatSettings.LongTimeFormat  := 'HH:NN:SS';

  DateSeparator := '/';
  TimeSeparator := '.';
  DecimalSeparator := ',';
end;

destructor TEpiXMLSettings.Destroy;
begin
  inherited Destroy;
end;

function TEpiXMLSettings.XMLName: string;
begin
  Result := rsSettings;
end;

procedure TEpiXMLSettings.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
begin
  // Root = <Settings>
  DateSeparator    := LoadAttrString(Root, rsDateSep)[1];
  TimeSeparator    := LoadAttrString(Root, rsTimeSep)[1];
  DecimalSeparator := LoadAttrString(Root, rsDecSep)[1];
end;

procedure TEpiXMLSettings.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);

  AssignValues(TEpiXMLSettings(AEpiCustomBase));
end;

function TEpiXMLSettings.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  TEpiXMLSettings(Result).AssignValues(Self);
end;

{ TEpiProjectSettings }

procedure TEpiProjectSettings.SetShowFieldBorders(const AValue: Boolean);
var
  Val: Boolean;
begin
  if FShowFieldBorders = AValue then exit;
  Val := FShowFieldBorders;
  FShowFieldBorders := AValue;
  DoChange(eegProjectSettings, Word(epceFieldBorder), @Val);
end;

procedure TEpiProjectSettings.SetBackupInterval(const AValue: Integer);
var
  Val: LongInt;
begin
  if FBackupInterval = AValue then exit;
  Val := FBackupInterval;
  FBackupInterval := AValue;
  DoChange(eegProjectSettings, Word(epceBackupInterval), @Val);
end;

procedure TEpiProjectSettings.SetAutoIncStartValue(const AValue: EpiInteger);
var
  Val: EpiInteger;
begin
  if FAutoIncStartValue = AValue then exit;
  Val := FAutoIncStartValue;
  FAutoIncStartValue := AValue;
  DoChange(eegProjectSettings, Word(epceAutoIncStart), @Val);
end;

procedure TEpiProjectSettings.SetBackupOnShutdown(const AValue: Boolean);
var
  Val: Boolean;
begin
  if FBackupOnShutdown = AValue then exit;
  Val := FBackupOnShutdown;
  FBackupOnShutdown := AValue;
  DoChange(eegProjectSettings, Word(epceBackupShutdown), @Val);
end;

procedure TEpiProjectSettings.SetShowFieldNames(const AValue: Boolean);
var
  Val: Boolean;
begin
  if FShowFieldNames = AValue then exit;
  Val := FShowFieldNames;
  FShowFieldNames := AValue;
  DoChange(eegProjectSettings, Word(epceFieldName), @Val);
end;

procedure TEpiProjectSettings.AssignValues(Src: TEpiProjectSettings);
begin
  FAutoIncStartValue := Src.FAutoIncStartValue;
  FBackupInterval    := Src.FBackupInterval;
  FBackupOnShutdown  := Src.FBackupOnShutdown;
  FShowFieldBorders  := Src.FShowFieldBorders;
  FShowFieldNames    := Src.FShowFieldNames;
end;

function TEpiProjectSettings.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveDomAttr(Result, rsAutoIncStart,        AutoIncStartValue);
  SaveDomAttr(Result, rsTimedBackupInterval, BackupInterval);
  SaveDomAttr(Result, rsBackupOnShutdown,    BackupOnShutdown);
  SaveDomAttr(Result, rsShowFieldNames,      ShowFieldNames);
  SaveDomAttr(Result, rsShowFieldBorders,    ShowFieldBorders);
end;

constructor TEpiProjectSettings.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);

  FShowFieldBorders := true;
  FShowFieldNames   := false;
  FBackupInterval   := 10;
  FAutoIncStartValue := 1;
end;

destructor TEpiProjectSettings.Destroy;
begin
  inherited Destroy;
end;

function TEpiProjectSettings.XMLName: string;
begin
  Result := rsProjectSettings;
end;

procedure TEpiProjectSettings.LoadFromXml(Root: TDOMNode;
  ReferenceMap: TEpiReferenceMap);
var
  I: Integer;
  B: Boolean;
begin
  AutoIncStartValue := LoadAttrInt(Root, rsAutoIncStart, AutoIncStartValue, false);
  BackupInterval    := LoadAttrInt(Root, rsTimedBackupInterval, BackupInterval, false);
  BackupOnShutdown  := LoadAttrBool(Root,rsBackupOnShutdown, BackupOnShutdown, false);
  ShowFieldNames    := LoadAttrBool(Root, rsShowFieldNames, ShowFieldNames, false);
  ShowFieldBorders  := LoadAttrBool(Root, rsShowFieldBorders, ShowFieldBorders, false);
end;

procedure TEpiProjectSettings.Assign(const AEpiCustomBase: TEpiCustomBase);
begin
  inherited Assign(AEpiCustomBase);

  AssignValues(TEpiProjectSettings(AEpiCustomBase));
end;

function TEpiProjectSettings.DoClone(AOwner: TEpiCustomBase;
  Dest: TEpiCustomBase; ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);

  TEpiProjectSettings(Result).AssignValues(Self);
end;

end.

