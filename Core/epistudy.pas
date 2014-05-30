unit epistudy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, Laz2_DOM;

type


  TEpiStudyChangeEvent = (esceAgency, esceAuthor, esceIdentifier,
    esceKeywords, esceModifiedDate, esceNotes, esceVersion);

  { TEpiStudy }

  TEpiStudy = class(TEpiCustomBase)
  private
    FAbstractText: TEpiTranslatedTextWrapper;
    FAgency: string;
    FAuthor: string;
    FCitations: TEpiTranslatedTextWrapper;
    FCreated: TDateTime;
    FDataCollectionEnd: TDateTime;
    FDataCollectionStart: TDateTime;
    FDesign: TEpiTranslatedTextWrapper;
    FFunding: TEpiTranslatedTextWrapper;
    FGeographicalCoverage: TEpiTranslatedTextWrapper;
    FIdentifier: string;
    FKeywords: string;
    FModifiedDate: TDateTime;
    FNotes: string;
    FPopulation: TEpiTranslatedTextWrapper;
    FPublisher: TEpiTranslatedTextWrapper;
    FPurpose: TEpiTranslatedTextWrapper;
    FRights: TEpiTranslatedTextWrapper;
//    FTimeCoverage: TEpiTranslatedTextWrapper;
    FTitle: TEpiTranslatedTextWrapper;
    FUnitOfObservation: TEpiTranslatedTextWrapper;
    FVersion: string;
    procedure SetAgency(AValue: string);
    procedure SetAuthor(const AValue: string);
    procedure SetIdentifier(const AValue: string);
    procedure SetKeywords(AValue: string);
    procedure SetModifiedDate(const AValue: TDateTime);
    procedure SetNotes(AValue: string);
    procedure SetVersion(const AValue: string);
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    function   SaveToXml(Content: String; Lvl: integer): string; override;
    procedure  LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap); override;

  protected
    function   SaveToDom(RootDoc: TDOMDocument): TDOMElement; override;
  public
    Property   AbstractText: TEpiTranslatedTextWrapper read FAbstractText;
    Property   Author: string read FAuthor write SetAuthor;
    property   Agency: string read FAgency write SetAgency;
    property   Citations: TEpiTranslatedTextWrapper read FCitations;
    Property   Created: TDateTime read FCreated;
    property   Funding: TEpiTranslatedTextWrapper read FFunding;
    property   GeographicalCoverage: TEpiTranslatedTextWrapper read FGeographicalCoverage;
    property   Identifier: string read FIdentifier write SetIdentifier;
    Property   ModifiedDate: TDateTime read FModifiedDate write SetModifiedDate;
    property   Notes: string read FNotes write SetNotes;
    property   Publisher: TEpiTranslatedTextWrapper read FPublisher;
    property   Purpose: TEpiTranslatedTextWrapper read FPurpose;
    property   Population: TEpiTranslatedTextWrapper read FPopulation;
    property   Rights: TEpiTranslatedTextWrapper read FRights;
//    property   TimeCoverage: TEpiTranslatedTextWrapper read FTimeCoverage;
    Property   Title: TEpiTranslatedTextWrapper read FTitle;
    Property   Keywords: string read FKeywords write SetKeywords;
    property   Version: string read FVersion write SetVersion;

    // version 2 properties:
    property   DataCollectionStart: TDateTime read FDataCollectionStart write FDataCollectionStart;
    property   DataCollectionEnd: TDateTime read FDataCollectionEnd write FDataCollectionEnd;
    property   Design: TEpiTranslatedTextWrapper read FDesign;
    property   UnitOfObservation: TEpiTranslatedTextWrapper read FUnitOfObservation;


  { Cloning }
  protected
    function DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
      ReferenceMap: TEpiReferenceMap): TEpiCustomBase; override;
  end;

implementation

uses
  epistringutils;

{ TEpiStudy }

procedure TEpiStudy.SetAgency(AValue: string);
var
  Val: String;
begin
  Val := FAgency;
  if FAgency = AValue then Exit;
  FAgency := AValue;
  DoChange(eegStudy, Word(esceAgency), @Val);
end;

procedure TEpiStudy.SetAuthor(const AValue: string);
var
  Val: String;
begin
  Val := FAuthor;
  if FAuthor = AValue then exit;
  FAuthor := AValue;
  DoChange(eegStudy, Word(esceAuthor), @Val);
end;

procedure TEpiStudy.SetIdentifier(const AValue: string);
var
  Val: String;
begin
  Val := FIdentifier;
  if FIdentifier = AValue then exit;
  FIdentifier := AValue;
  DoChange(eegStudy, Word(esceIdentifier), @Val);
end;

procedure TEpiStudy.SetKeywords(AValue: string);
var
  Val: String;
begin
  Val := FKeywords;
  if FKeywords = AValue then Exit;
  FKeywords := AValue;
  DoChange(eegStudy, Word(esceKeywords), @Val);
end;

procedure TEpiStudy.SetModifiedDate(const AValue: TDateTime);
var
  Val: TDateTime;
begin
  Val := ModifiedDate;
  if FModifiedDate = AValue then exit;
  FModifiedDate := AValue;
  DoChange(eegStudy, Word(esceModifiedDate), @Val);
end;

procedure TEpiStudy.SetNotes(AValue: string);
var
  Val: String;
begin
  Val := Notes;
  if FNotes = AValue then Exit;
  FNotes := AValue;
  DoChange(eegStudy, Word(esceNotes), @Val);
end;

procedure TEpiStudy.SetVersion(const AValue: string);
var
  Val: String;
begin
  Val := Version;
  if FVersion = AValue then exit;
  FVersion := AValue;
  DoChange(eegStudy, Word(esceVersion), @Val);
end;

constructor TEpiStudy.Create(AOwner: TEpiCustomBase);
begin
  Inherited Create(AOwner);

  FAuthor               := '';
  FAbstractText         := TEpiTranslatedTextWrapper.Create(Self, rsAbstract, rsText);
  FCitations            := TEpiTranslatedTextWrapper.Create(Self, rsCitations, rsText);
  FFunding              := TEpiTranslatedTextWrapper.Create(Self, rsFunding, rsText);
  FGeographicalCoverage := TEpiTranslatedTextWrapper.Create(Self, rsGeoCoverage, rsText);
  FIdentifier           := '';
  FKeyWords             := '';
  FNotes                := '';
  FPublisher            := TEpiTranslatedTextWrapper.Create(Self, rsPublisher, rsText);
  FPurpose              := TEpiTranslatedTextWrapper.Create(Self, rsPurpose, rsText);
  FPopulation           := TEpiTranslatedTextWrapper.Create(Self, rsPopulation, rsText);
  FRights               := TEpiTranslatedTextWrapper.Create(Self, rsRights, rsText);
//  FTimeCoverage         := TEpiTranslatedTextWrapper.Create(Self, rsTimeCoverage, rsText);
  FTitle                := TEpiTranslatedTextWrapper.Create(Self, rsTitle, rsText);
  FVersion              := '';
  FCreated              := Now;
  FModifiedDate         := FCreated;

  // version 2 properties:
  FDataCollectionStart  := MaxDateTime;
  FDataCollectionEnd    := MaxDateTime;
  FDesign               := TEpiTranslatedTextWrapper.Create(Self, rsDesign, rsText);
  FUnitOfObservation    := TEpiTranslatedTextWrapper.Create(Self, rsUnitOfObservation, rsText);


  RegisterClasses([FAbstractText, FCitations, FDesign, FFunding,
    FGeographicalCoverage, FPublisher, FPurpose, FPopulation, FRights, //FTimeCoverage,
    FTitle, FUnitOfObservation]);
end;

destructor TEpiStudy.Destroy;
begin
  FAbstractText.Free;
  FCitations.Free;
  FFunding.Free;
  FGeographicalCoverage.Free;
  FPublisher.Free;
  FPurpose.Free;
  FPopulation.Free;
  FRights.Free;
//  FTimeCoverage.Free;
  FTitle.Free;
  inherited Destroy;
end;

function TEpiStudy.XMLName: string;
begin
  Result := rsStudy;
end;

function TEpiStudy.SaveToXml(Content: String; Lvl: integer): string;
begin
  if Modified then
    ModifiedDate := Now;

  Content :=
    SaveNode(Lvl + 1, rsAuthor, Author) +
    SaveNode(Lvl + 1, rsAgency, Agency) +
    SaveNode(Lvl + 1, rsCreated, Created) +
    SaveNode(Lvl + 1, rsIdentifier, Identifier) +
    SaveNode(Lvl + 1, rsKeywords, Keywords) +
    SaveNode(Lvl + 1, rsModified, ModifiedDate) +
    SaveNode(Lvl + 1, rsNotes, Notes) +
    SaveNode(Lvl + 1, rsVersion, Version);
  if (DataCollectionStart <> MaxDateTime) then
    Content += SaveNode(Lvl + 1, rsDataColectionStart, DataCollectionStart);
  if (DataCollectionEnd <> MaxDateTime) then
    Content += SaveNode(Lvl + 1, rsDataColectionEnd,   DataCollectionEnd);
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiStudy.LoadFromXml(Root: TDOMNode; ReferenceMap: TEpiReferenceMap);
var
  Node: TDOMNode;
begin
  FAuthor          := LoadNodeString(Root, rsAuthor);
  FAgency          := LoadNodeString(Root, rsAgency, FAgency, false);
  FCreated         := LoadNodeDateTime(Root, rsCreated);
  FIdentifier      := LoadNodeString(Root, rsIdentifier, FIdentifier, false);
  FKeywords        := LoadNodeString(Root, rsKeywords, FKeywords, false);
  FModifiedDate    := LoadNodeDateTime(Root, rsModified);
  FNotes           := LoadNodeString(Root, rsNotes, FNotes, false);
  FVersion         := LoadNodeString(Root, rsVersion, FVersion, false);

  // Root = <Study>
  FAbstractText.LoadFromXml(Root, ReferenceMap);
  FCitations.LoadFromXml(Root, ReferenceMap);
  FFunding.LoadFromXml(Root, ReferenceMap);
  FGeographicalCoverage.LoadFromXml(Root, ReferenceMap);
  FPublisher.LoadFromXml(Root, ReferenceMap);
  FPurpose.LoadFromXml(Root, ReferenceMap);
  FPopulation.LoadFromXml(Root, ReferenceMap);
  FRights.LoadFromXml(Root, ReferenceMap);
//  FTimeCoverage.LoadFromXml(Root, ReferenceMap);
  FTitle.LoadFromXml(Root, ReferenceMap);

  // Version 2:
  // -- all loads defaults to fatal=false, since none of them are required.
  FDataCollectionStart := LoadNodeDateTime(Root, rsDataColectionStart, FDataCollectionStart, false);
  FDataCollectionEnd   := LoadNodeDateTime(Root, rsDataColectionEnd,   FDataCollectionEnd,   false);
  FDesign.LoadFromXml(Root, ReferenceMap);
  FUnitOfObservation.LoadFromXml(Root, ReferenceMap);
end;

function TEpiStudy.SaveToDom(RootDoc: TDOMDocument): TDOMElement;
begin
  Result := inherited SaveToDom(RootDoc);

  SaveTextContent(Result, rsAuthor, Author);
  SaveTextContent(Result, rsAgency, Agency);
  SaveTextContent(Result, rsCreated, Created);
  SaveTextContent(Result, rsIdentifier, Identifier);
  SaveTextContent(Result, rsKeywords, Keywords);
  SaveTextContent(Result, rsModified, ModifiedDate);
  SaveTextContent(Result, rsNotes, Notes);
  SaveTextContent(Result, rsVersion, Version);
end;

function TEpiStudy.DoClone(AOwner: TEpiCustomBase; Dest: TEpiCustomBase;
  ReferenceMap: TEpiReferenceMap): TEpiCustomBase;
begin
  Result := inherited DoClone(AOwner, Dest, ReferenceMap);
  with TEpiStudy(Result) do
  begin
    FAuthor          := Self.FAuthor;
    FAgency          := Self.FAgency;
    FCreated         := Self.Created;
    FIdentifier      := Self.FIdentifier;
    FKeywords        := Self.FKeywords;
    FModifiedDate    := Self.FModifiedDate;
    FNotes           := Self.FNotes;
    FVersion         := Self.FVersion;
  end;
end;

end.

