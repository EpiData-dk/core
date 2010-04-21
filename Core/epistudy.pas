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


  TEpiStudy = class(TEpiCustomBase)
  private
    FAbstractText: TEpiTranslatedText;
    FAuthor: string;
    FCreated: TDateTime;
    FCreator: TEpiTranslatedText;
    FDescription: TEpiTranslatedText;
    FLanguage: string;
    FModifiedDate: TDateTime;
    FStudyOwner: TEpiTranslatedText;
    FProtocol: TEpiTranslatedText;
    FProvenance: TEpiTranslatedText;
    FReferences: TEpiTranslatedText;
    FRightsHolder: TEpiTranslatedText;
    FSource: TEpiTranslatedText;
    FSpatial: TEpiTranslatedText;
    FSubject: TEpiTranslatedText;
    FTemporal: TEpiTranslatedText;
    FTitle: TEpiTranslatedText;
    function GetAbstractText: string;
    function GetCreator: string;
    function GetDescription: string;
    function GetProtocol: string;
    function GetProvenance: string;
    function GetReferences: string;
    function GetRightsHolder: string;
    function GetSource: string;
    function GetSpatial: string;
    function GetStudyOwner: string;
    function GetSubject: string;
    function GetTemporal: string;
    function GetTitle: string;
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
    function   XMLName: string; override;
    function   SaveToXml(Content: String; Lvl: integer): string; override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    Property   Protocol: string read GetProtocol write SetProtocol;
    Property   AbstractText: string read GetAbstractText write SetAbstractText;
    Property   Author: string read FAuthor write SetAuthor;
    Property   StudyOwner: string read GetStudyOwner write SetStudyOwner;
    Property   Created: TDateTime read FCreated;
    Property   Creator: string read GetCreator write SetCreator;
    Property   Description: string read GetDescription write SetDescription;
    Property   Language: string read FLanguage write SetLanguage;
    Property   ModifiedDate: TDateTime read FModifiedDate write SetModifiedDate;
    Property   Provenance: string read GetProvenance write SetProvenance;
    Property   References: string read GetReferences write SetReferences;
    Property   RightsHolder: string read GetRightsHolder write SetRightsHolder;
    Property   Source: string read GetSource write SetSource;
    Property   Spatial: string read GetSpatial write SetSpatial;
    Property   Subject: string read GetSubject write SetSubject;
    Property   Temporal: string read GetTemporal write SetTemporal;
    Property   Title: string read GetTitle write SetTitle;
  end;

implementation

uses
  epistringutils;

{ TEpiStudy }

function TEpiStudy.GetAbstractText: string;
begin
  result := FAbstractText.Text;
end;

function TEpiStudy.GetCreator: string;
begin
  result := FCreator.Text;
end;

function TEpiStudy.GetDescription: string;
begin
  result := FDescription.Text;
end;

function TEpiStudy.GetProtocol: string;
begin
  result := FProtocol.Text;
end;

function TEpiStudy.GetProvenance: string;
begin
  result := FProvenance.Text;
end;

function TEpiStudy.GetReferences: string;
begin
  result := FReferences.Text;
end;

function TEpiStudy.GetRightsHolder: string;
begin
  result := FRightsHolder.Text;
end;

function TEpiStudy.GetSource: string;
begin
  result := FSource.Text;
end;

function TEpiStudy.GetSpatial: string;
begin
  result := FSpatial.Text;
end;

function TEpiStudy.GetStudyOwner: string;
begin
  result := FStudyOwner.Text;
end;

function TEpiStudy.GetSubject: string;
begin
  result := FSubject.Text;
end;

function TEpiStudy.GetTemporal: string;
begin
  result := FTemporal.Text;
end;

function TEpiStudy.GetTitle: string;
begin
  result := FTitle.Text;
end;

procedure TEpiStudy.SetAbstractText(const AValue: string);
var
  Val: String;
begin
  if FAbstractText.Text = AValue then exit;
  Val := FAbstractText.Text;
  FAbstractText.Text := AValue;
  DoChange(eegStudy, Word(esceSetAbstract), @Val);
end;

procedure TEpiStudy.SetAuthor(const AValue: string);
var
  Val: String;
begin
  if FAuthor = AValue then exit;
  Val := FAuthor;
  FAuthor := AValue;
  DoChange(eegStudy, Word(esceSetAuthor), @Val);
end;

procedure TEpiStudy.SetCreator(const AValue: string);
var
  Val: String;
begin
  if FCreator.Text = AValue then exit;
  Val := FCreator.Text;
  FCreator.Text := AValue;
  DoChange(eegStudy, Word(esceSetCreator), @Val);
end;

procedure TEpiStudy.SetDescription(const AValue: string);
var
  Val: String;
begin
  if FDescription.Text = AValue then exit;
  Val := FDescription.Text;
  FDescription.Text := AValue;
  DoChange(eegStudy, Word(esceSetDesc), @Val);
end;

procedure TEpiStudy.SetLanguage(const AValue: string);
var
  Val: String;
begin
  if FLanguage = AValue then exit;
  Val := FLanguage;
  FLanguage := AValue;
  DoChange(eegStudy, Word(esceSetLanguage), @Val);
end;

procedure TEpiStudy.SetModifiedDate(const AValue: TDateTime);
var
  Val: Double;
begin
  if FModifiedDate = AValue then exit;
  Val := FModifiedDate;
  FModifiedDate := AValue;
  DoChange(eegStudy, Word(esceSetModified), @Val);
end;

procedure TEpiStudy.SetStudyOwner(const AValue: string);
var
  Val: String;
begin
  if FStudyOwner.Text = AValue then exit;
  Val := FStudyOwner.Text;
  FStudyOwner.Text := AValue;
  DoChange(eegStudy, Word(esceSetOwner), @Val);
end;

procedure TEpiStudy.SetProtocol(const AValue: string);
var
  Val: String;
begin
  if FProtocol.Text = AValue then exit;
  Val := FProtocol.Text;
  FProtocol.Text := AValue;
  DoChange(eegStudy, Word(esceSetProtocol), @Val);
end;

procedure TEpiStudy.SetProvenance(const AValue: string);
var
  Val: String;
begin
  if FProvenance.Text = AValue then exit;
  Val := FProvenance.Text;
  FProvenance.Text := AValue;
  DoChange(eegStudy, Word(esceSetProv), @Val);
end;

procedure TEpiStudy.SetReferences(const AValue: string);
var
  Val: String;
begin
  if FReferences.Text = AValue then exit;
  Val := FReferences.Text;
  FReferences.Text := AValue;
  DoChange(eegStudy, Word(esceSetRef), @Val);
end;

procedure TEpiStudy.SetRightsHolder(const AValue: string);
var
  Val: String;
begin
  if FRightsHolder.Text = AValue then exit;
  Val := FRightsHolder.Text;
  FRightsHolder.Text := AValue;
  DoChange(eegStudy, Word(esceSetRightsHolder), @Val);
end;

procedure TEpiStudy.SetSource(const AValue: string);
var
  Val: String;
begin
  if FSource.Text = AValue then exit;
  Val := FSource.Text;
  FSource.Text := AValue;
  DoChange(eegStudy, Word(esceSetSource), @Val);
end;

procedure TEpiStudy.SetSpatial(const AValue: string);
var
  Val: String;
begin
  if FSpatial.Text = AValue then exit;
  Val := FSpatial.Text;
  FSpatial.Text := AValue;
  DoChange(eegStudy, Word(esceSetSpatial), @Val);
end;

procedure TEpiStudy.SetSubject(const AValue: string);
var
  Val: String;
begin
  if FSubject.Text = AValue then exit;
  Val := FSubject.Text;
  FSubject.Text := AValue;
  DoChange(eegStudy, Word(esceSetSubject), @Val);
end;

procedure TEpiStudy.SetTemporal(const AValue: string);
var
  Val: String;
begin
  if FTemporal.Text = AValue then exit;
  Val := FTemporal.Text;
  FTemporal.Text := AValue;
  DoChange(eegStudy, Word(esceSetTemporal), @Val);
end;

procedure TEpiStudy.SetTitle(const AValue: string);
var
  Val: String;
begin
  if FTitle.Text = AValue then exit;
  Val := FTitle.Text;
  FTitle.Text := AValue;
  DoChange(eegStudy, Word(esceSetTitle), @Val);
end;

constructor TEpiStudy.Create(AOwner: TEpiCustomBase);
begin
  Inherited Create(AOwner);

  FAbstractText := TEpiTranslatedText.Create(Self, rsAbstract);
  FCreator      := TEpiTranslatedText.Create(Self, rsCreator);
  FDescription  := TEpiTranslatedText.Create(Self, rsDescription);
  FStudyOwner   := TEpiTranslatedText.Create(Self, rsStudy);
  FProtocol     := TEpiTranslatedText.Create(Self, rsProtocol);
  FProvenance   := TEpiTranslatedText.Create(Self, rsProvenance);
  FReferences   := TEpiTranslatedText.Create(Self, rsReferences);
  FRightsHolder := TEpiTranslatedText.Create(Self, rsRightsholder);
  FSource       := TEpiTranslatedText.Create(Self, rsSource);
  FSpatial      := TEpiTranslatedText.Create(Self, rsSpatial);
  FSubject      := TEpiTranslatedText.Create(Self, rsSubject);
  FTemporal     := TEpiTranslatedText.Create(Self, rsTemporal);
  FTitle        := TEpiTranslatedText.Create(Self, rsTitle);

  FCreated := Now;
  FModifiedDate := FCreated;

  RegisterClasses([FAbstractText, FCreator, FDescription,
    FStudyOwner, FProtocol, FProvenance, FReferences, FRightsHolder,
    FSource, FSpatial, FSubject, FTemporal, FTitle]);
end;

destructor TEpiStudy.Destroy;
begin
  FAbstractText.Free;
  FCreator.Free;
  FDescription.Free;
  FStudyOwner.Free;
  FProtocol.Free;
  FProvenance.Free;
  FReferences.Free;
  FRightsHolder.Free;
  FSource.Free;
  FSpatial.Free;
  FSubject.Free;
  FTemporal.Free;
  FTitle.Free;
  inherited Destroy;
end;

function TEpiStudy.XMLName: string;
begin
  Result := rsStudy;
end;

function TEpiStudy.SaveToXml(Content: String; Lvl: integer): string;
begin
  Content :=
    SaveNode(Lvl + 1, rsAuthor, Author) +
    SaveNode(Lvl + 1, rsCreated, Created) +
    SaveNode(Lvl + 1, rsLanguage, Language) +
    SaveNode(Lvl + 1, rsModified, ModifiedDate);
  Result := inherited SaveToXml(Content, Lvl);
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

