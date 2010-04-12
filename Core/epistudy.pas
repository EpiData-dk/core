unit epistudy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, DOM;

type

  { TEpiStudy }

  TEpiStudyChangeEventType = (
    esceSetAbstract, esceSetAuthor, esceSetCreator,
    esceSetDesc, esceSetLanguage, esceSetModified, esceSetOwner,
    esceSetProtocol, esceSetProv, esceSetRef, esceSetRightsHolder,
    esceSetSource, esceSetSpatial, esceSetSubject,
    esceSetTemporal, esceSetTitle
  );


  TEpiStudy = class(TEpiCustomItem)
  private
    FAbstractText: string;
    FAuthor: string;
    FCreated: TDateTime;
    FCreator: string;
    FDescription: string;
    FLanguage: string;
    FModifiedDate: TDateTime;
    FStudyOwner: string;
    FProtocol: string;
    FProvenance: string;
    FReferences: string;
    FRightsHolder: string;
    FSource: string;
    FSpatial: string;
    FSubject: string;
    FTemporal: string;
    FTitle: string;
    procedure SetAbstractText(const AValue: string);
    procedure SetAuthor(const AValue: string);
    procedure SetCreator(const AValue: string);
    procedure SetDescription(const AValue: string);
    procedure SetLanguage(const AValue: string);
    procedure SetModifiedDate(const AValue: TDateTime);
    procedure SetStudyOwner(const AValue: string);
    procedure SetProtocol(const AValue: string);
    procedure SetProvenance(const AValue: string);
    procedure SetReferences(const AValue: string);
    procedure SetRightsHolder(const AValue: string);
    procedure SetSource(const AValue: string);
    procedure SetSpatial(const AValue: string);
    procedure SetSubject(const AValue: string);
    procedure SetTemporal(const AValue: string);
    procedure SetTitle(const AValue: string);
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: Integer); override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    Property   Protocol: string read FProtocol write SetProtocol;
    Property   AbstractText: string read FAbstractText write SetAbstractText;
    Property   Author: string read FAuthor write SetAuthor;
    Property   StudyOwner: string read FStudyOwner write SetStudyOwner;
    Property   Created: TDateTime read FCreated;
    Property   Creator: string read FCreator write SetCreator;
    Property   Description: string read FDescription write SetDescription;
    Property   Language: string read FLanguage write SetLanguage;
    Property   ModifiedDate: TDateTime read FModifiedDate write SetModifiedDate;
    Property   Provenance: string read FProvenance write SetProvenance;
    Property   References: string read FReferences write SetReferences;
    Property   RightsHolder: string read FRightsHolder write SetRightsHolder;
    Property   Source: string read FSource write SetSource;
    Property   Spatial: string read FSpatial write SetSpatial;
    Property   Subject: string read FSubject write SetSubject;
    Property   Temporal: string read FTemporal write SetTemporal;
    Property   Title: string read FTitle write SetTitle;
  end;

implementation

uses
  epistringutils;

const
  rsStudy = 'Study';
  rsProtocol =  'Protocol';
  rsAbstract = 'Abstract';
  rsAuthor = 'Author';
  rsOwner = 'Owner';
  rsCreated = 'Created';
  rsCreator = 'Creator';
  rsDescription = 'Description';
  rsLanguage = 'Language';
  rsModified = 'Modified';
  rsProvenance = 'Provenance';
  rsReferences = 'References';
  rsRightsholder = 'Rightsholder';
  rsSource = 'Source';
  rsSubject = 'Subject';
  rsTemporal = 'Temporal';
  rsTitle = 'Title';


{ TEpiStudy }

procedure TEpiStudy.SetAbstractText(const AValue: string);
var
  Val: String;
begin
  if FAbstractText = AValue then exit;
  Val := FAbstractText;
  FAbstractText := AValue;
  DoChange(Word(eegStudy), Word(esceSetAbstract), @Val);
end;

procedure TEpiStudy.SetAuthor(const AValue: string);
var
  Val: String;
begin
  if FAuthor = AValue then exit;
  Val := FAuthor;
  FAuthor := AValue;
  DoChange(Word(eegStudy), Word(esceSetAuthor), @Val);
end;

procedure TEpiStudy.SetCreator(const AValue: string);
var
  Val: String;
begin
  if FCreator = AValue then exit;
  Val := FCreator;
  FCreator := AValue;
  DoChange(Word(eegStudy), Word(esceSetCreator), @Val);
end;

procedure TEpiStudy.SetDescription(const AValue: string);
var
  Val: String;
begin
  if FDescription = AValue then exit;
  Val := FDescription;
  FDescription := AValue;
  DoChange(Word(eegStudy), Word(esceSetDesc), @Val);
end;

procedure TEpiStudy.SetLanguage(const AValue: string);
var
  Val: String;
begin
  if FLanguage = AValue then exit;
  Val := FLanguage;
  FLanguage := AValue;
  DoChange(Word(eegStudy), Word(esceSetLanguage), @Val);
end;

procedure TEpiStudy.SetModifiedDate(const AValue: TDateTime);
var
  Val: Double;
begin
  if FModifiedDate = AValue then exit;
  Val := FModifiedDate;
  FModifiedDate := AValue;
  DoChange(Word(eegStudy), Word(esceSetModified), @Val);
end;

procedure TEpiStudy.SetStudyOwner(const AValue: string);
var
  Val: String;
begin
  if FStudyOwner = AValue then exit;
  Val := FStudyOwner;
  FStudyOwner := AValue;
  DoChange(Word(eegStudy), Word(esceSetOwner), @Val);
end;

procedure TEpiStudy.SetProtocol(const AValue: string);
var
  Val: String;
begin
  if FProtocol = AValue then exit;
  Val := FProtocol;
  FProtocol := AValue;
  DoChange(Word(eegStudy), Word(esceSetProtocol), @Val);
end;

procedure TEpiStudy.SetProvenance(const AValue: string);
var
  Val: String;
begin
  if FProvenance = AValue then exit;
  Val := FProvenance;
  FProvenance := AValue;
  DoChange(Word(eegStudy), Word(esceSetProv), @Val);
end;

procedure TEpiStudy.SetReferences(const AValue: string);
var
  Val: String;
begin
  if FReferences = AValue then exit;
  Val := FReferences;
  FReferences := AValue;
  DoChange(Word(eegStudy), Word(esceSetRef), @Val);
end;

procedure TEpiStudy.SetRightsHolder(const AValue: string);
var
  Val: String;
begin
  if FRightsHolder = AValue then exit;
  Val := FRightsHolder;
  FRightsHolder := AValue;
  DoChange(Word(eegStudy), Word(esceSetRightsHolder), @Val);
end;

procedure TEpiStudy.SetSource(const AValue: string);
var
  Val: String;
begin
  if FSource = AValue then exit;
  Val := FSource;
  FSource := AValue;
  DoChange(Word(eegStudy), Word(esceSetSource), @Val);
end;

procedure TEpiStudy.SetSpatial(const AValue: string);
var
  Val: String;
begin
  if FSpatial = AValue then exit;
  Val := FSpatial;
  FSpatial := AValue;
  DoChange(Word(eegStudy), Word(esceSetSpatial), @Val);
end;

procedure TEpiStudy.SetSubject(const AValue: string);
var
  Val: String;
begin
  if FSubject = AValue then exit;
  Val := FSubject;
  FSubject := AValue;
  DoChange(Word(eegStudy), Word(esceSetSubject), @Val);
end;

procedure TEpiStudy.SetTemporal(const AValue: string);
var
  Val: String;
begin
  if FTemporal = AValue then exit;
  Val := FTemporal;
  FTemporal := AValue;
  DoChange(Word(eegStudy), Word(esceSetTemporal), @Val);
end;

procedure TEpiStudy.SetTitle(const AValue: string);
var
  Val: String;
begin
  if FTitle = AValue then exit;
  Val := FTitle;
  FTitle := AValue;
  DoChange(Word(eegStudy), Word(esceSetTitle), @Val);
end;

constructor TEpiStudy.Create(AOwner: TEpiCustomBase);
begin
  Inherited Create(AOwner);
  FCreated := Now;
  FModifiedDate := FCreated;
end;

destructor TEpiStudy.Destroy;
begin
  inherited Destroy;
end;

procedure TEpiStudy.SaveToStream(St: TStream; Lvl: Integer);
var
  S: String;
begin
  Inc(Lvl);
  S :=
    SaveNode(Lvl, rsProtocol, Protocol) +
    SaveNode(Lvl, rsAbstract, AbstractText) +
    SaveNode(Lvl, rsAuthor, Author) +
    SaveNode(Lvl, rsOwner, StudyOwner) +
    SaveNode(Lvl, rsCreated, Created) +
    SaveNode(Lvl, rsCreator, Creator) +
    SaveNode(Lvl, rsDescription, Description) +
    SaveNode(Lvl, rsLanguage, Language) +
    SaveNode(Lvl, rsModified, ModifiedDate) +
    SaveNode(Lvl, rsProvenance, Provenance) +
    SaveNode(Lvl, rsReferences, References) +
    SaveNode(Lvl, rsRightsholder, RightsHolder) +
    SaveNode(Lvl, rsSource, Source) +
    SaveNode(Lvl, rsSubject, Subject) +
    SaveNode(Lvl, rsTemporal, Temporal) +
    SaveNode(Lvl, rsTitle, Title);
  Dec(Lvl);
  S := SaveSection(Lvl, rsStudy, Id, S);
  SaveStream(St, S);
end;

procedure TEpiStudy.LoadFromXml(Root: TDOMNode);
begin
  // Root = <Study>
  Protocol     := LoadNodeString(Root, rsProtocol);
  AbstractText := LoadNodeString(Root, rsAbstract);
  Author       := LoadNodeString(Root, rsAuthor);
  StudyOwner   := LoadNodeString(Root, rsOwner);
  // Access FCreated directly.
  FCreated     := LoadNodeDateTime(Root, rsCreated);
  Creator      := LoadNodeString(Root, rsCreator);
  Description  := LoadNodeString(Root, rsDescription);
  Language     := LoadNodeString(Root, rsLanguage);
  ModifiedDate := LoadNodeDateTime(Root, rsModified);
  Provenance   := LoadNodeString(Root, rsProvenance);
  References   := LoadNodeString(Root, rsReferences);
  Rightsholder := LoadNodeString(Root, rsRightsholder);
  Source       := LoadNodeString(Root, rsSource);
  Subject      := LoadNodeString(Root, rsSubject);
  Temporal     := LoadNodeString(Root, rsTemporal);
  Title        := LoadNodeString(Root, rsTitle);
end;

end.

