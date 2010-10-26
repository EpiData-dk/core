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
    procedure SetAuthor(const AValue: string);
    procedure SetLanguage(const AValue: string);
    procedure SetModifiedDate(const AValue: TDateTime);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    function   SaveToXml(Content: String; Lvl: integer): string; override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    Property   Protocol: TEpiTranslatedText read FProtocol;
    Property   AbstractText: TEpiTranslatedText read FAbstractText;
    Property   Author: string read FAuthor write SetAuthor;
    Property   StudyOwner: TEpiTranslatedText read FStudyOwner;
    Property   Created: TDateTime read FCreated;
    Property   Creator: TEpiTranslatedText read FCreator;
    Property   Description: TEpiTranslatedText read FDescription;
    Property   Language: string read FLanguage write SetLanguage;
    Property   ModifiedDate: TDateTime read FModifiedDate write SetModifiedDate;
    Property   Provenance: TEpiTranslatedText read FProvenance;
    Property   References: TEpiTranslatedText read FReferences;
    Property   RightsHolder: TEpiTranslatedText read FRightsHolder;
    Property   Source: TEpiTranslatedText read FSource;
    Property   Spatial: TEpiTranslatedText read FSpatial;
    Property   Subject: TEpiTranslatedText read FSubject;
    Property   Temporal: TEpiTranslatedText read FTemporal;
    Property   Title: TEpiTranslatedText read FTitle;
  end;

implementation

uses
  epistringutils;

{ TEpiStudy }

procedure TEpiStudy.SetAuthor(const AValue: string);
var
  Val: String;
begin
  if FAuthor = AValue then exit;
  Val := FAuthor;
  FAuthor := AValue;
  DoChange(eegStudy, Word(esceSetAuthor), @Val);
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

constructor TEpiStudy.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);

  FAbstractText := TEpiTranslatedText.Create(Self, rsAbstract);
  FCreator      := TEpiTranslatedText.Create(Self, rsCreator);
  FDescription  := TEpiTranslatedText.Create(Self, rsDescription);
  FStudyOwner   := TEpiTranslatedText.Create(Self, rsOwner);
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
  Protocol.LoadFromXml(Root);
  AbstractText.LoadFromXml(Root);
  Author       := LoadNodeString(Root, rsAuthor);
  StudyOwner.LoadFromXml(Root);
  // Access FCreated directly.
  FCreated     := LoadNodeDateTime(Root, rsCreated);
  Creator.LoadFromXml(Root);
  Description.LoadFromXml(Root);
  Language     := LoadNodeString(Root, rsLanguage);
  ModifiedDate := LoadNodeDateTime(Root, rsModified);
  Provenance.LoadFromXml(Root);
  References.LoadFromXml(Root);
  Rightsholder.LoadFromXml(Root);
  Source.LoadFromXml(Root);
  Subject.LoadFromXml(Root);
  Temporal.LoadFromXml(Root);
  Title.LoadFromXml(Root);
end;

end.

