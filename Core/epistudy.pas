unit epistudy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, DOM;

type

  { TEpiStudy }

  TEpiStudy = class(TEpiCustomBase)
  private
    FAbstractText: TEpiTranslatedTextWrapper;
    FAuthor: string;
    FCitations: TEpiTranslatedTextWrapper;
    FCreated: TDateTime;
    FFunding: TEpiTranslatedTextWrapper;
    FGeographicalCoverage: TEpiTranslatedTextWrapper;
    FIdentifier: string;
    FLanguage: string;
    FModifiedDate: TDateTime;
    FNotes: string;
    FOtherLanguages: string;
    FPublisher: TEpiTranslatedTextWrapper;
    FPurpose: TEpiTranslatedTextWrapper;
    FRights: TEpiTranslatedTextWrapper;
    FTimeCoverage: TEpiTranslatedTextWrapper;
    FTitle: TEpiTranslatedTextWrapper;
    FVersion: string;
    procedure SetAuthor(const AValue: string);
    procedure SetIdentifier(const AValue: string);
    procedure SetLanguage(const AValue: string);
    procedure SetModifiedDate(const AValue: TDateTime);
    procedure SetNotes(AValue: string);
    procedure SetOtherLanguages(const AValue: string);
    procedure SetVersion(const AValue: string);
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    function   SaveToXml(Content: String; Lvl: integer): string; override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    Property   AbstractText: TEpiTranslatedTextWrapper read FAbstractText;
    Property   Author: string read FAuthor write SetAuthor;
    property   Citations: TEpiTranslatedTextWrapper read FCitations;
    Property   Created: TDateTime read FCreated;
    property   Funding: TEpiTranslatedTextWrapper read FFunding;
    property   GeographicalCoverage: TEpiTranslatedTextWrapper read FGeographicalCoverage;
    property   Identifier: string read FIdentifier write SetIdentifier;
    Property   Language: string read FLanguage write SetLanguage;
    Property   ModifiedDate: TDateTime read FModifiedDate write SetModifiedDate;
    property   Notes: string read FNotes write SetNotes;
    property   OtherLanguages: string read FOtherLanguages write SetOtherLanguages;
    property   Publisher: TEpiTranslatedTextWrapper read FPublisher;
    property   Purpose: TEpiTranslatedTextWrapper read FPurpose;
    property   Rights: TEpiTranslatedTextWrapper read FRights;
    property   TimeCoverage: TEpiTranslatedTextWrapper read FTimeCoverage;
    Property   Title: TEpiTranslatedTextWrapper read FTitle;
    property   Version: string read FVersion write SetVersion;
  end;

implementation

uses
  epistringutils;

{ TEpiStudy }

procedure TEpiStudy.SetAuthor(const AValue: string);
begin
  if FAuthor = AValue then exit;
  FAuthor := AValue;
end;

procedure TEpiStudy.SetIdentifier(const AValue: string);
begin
  if FIdentifier = AValue then exit;
  FIdentifier := AValue;
end;

procedure TEpiStudy.SetLanguage(const AValue: string);
begin
  if FLanguage = AValue then exit;
  FLanguage := AValue;
end;

procedure TEpiStudy.SetModifiedDate(const AValue: TDateTime);
begin
  if FModifiedDate = AValue then exit;
  FModifiedDate := AValue;
end;

procedure TEpiStudy.SetNotes(AValue: string);
begin
  if FNotes = AValue then Exit;
  FNotes := AValue;
end;

procedure TEpiStudy.SetOtherLanguages(const AValue: string);
begin
  if FOtherLanguages = AValue then exit;
  FOtherLanguages := AValue;
end;

procedure TEpiStudy.SetVersion(const AValue: string);
begin
  if FVersion = AValue then exit;
  FVersion := AValue;
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
  FLanguage             := '';
  FNotes                := '';
  FOtherLanguages       := '';
  FPublisher            := TEpiTranslatedTextWrapper.Create(Self, rsPublisher, rsText);
  FPurpose              := TEpiTranslatedTextWrapper.Create(Self, rsPurpose, rsText);
  FRights               := TEpiTranslatedTextWrapper.Create(Self, rsRights, rsText);
  FTimeCoverage         := TEpiTranslatedTextWrapper.Create(Self, rsTimeCoverage, rsText);
  FTitle                := TEpiTranslatedTextWrapper.Create(Self, rsTitle, rsText);
  FVersion              := '';

  FCreated := Now;
  FModifiedDate := FCreated;

  RegisterClasses([FAbstractText, FCitations, FFunding,
    FGeographicalCoverage, FPublisher, FPurpose, FRights, FTimeCoverage,
    FTitle]);
end;

destructor TEpiStudy.Destroy;
begin
  FAbstractText.Free;
  FCitations.Free;
  FFunding.Free;
  FGeographicalCoverage.Free;
  FPublisher.Free;
  FPurpose.Free;
  FRights.Free;
  FTimeCoverage.Free;
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
    SaveNode(Lvl + 1, rsIdentifier, Identifier) +
    SaveNode(Lvl + 1, rsLanguage, Language) +
    SaveNode(Lvl + 1, rsModified, ModifiedDate) +
    SaveNode(Lvl + 1, rsNotes, Notes) +
    SaveNode(Lvl + 1, rsOtherLanguages, OtherLanguages) +
    SaveNode(Lvl + 1, rsVersion, Version);
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiStudy.LoadFromXml(Root: TDOMNode);
var
  Node: TDOMNode;
begin
  // Root = <Study>
  FAbstractText.LoadFromXml(Root);
  FAuthor       := LoadNodeString(Root, rsAuthor);
  FCitations.LoadFromXml(Root);
  FCreated      := LoadNodeDateTime(Root, rsCreated);
  FFunding.LoadFromXml(Root);
  FGeographicalCoverage.LoadFromXml(Root);
  if LoadNode(Node, Root, rsNotes, false) then
    FNotes := LoadNodeString(Root, rsNotes);
  FIdentifier   := LoadNodeString(Root, rsIdentifier);
  FLanguage     := LoadNodeString(Root, rsLanguage);
  RootOwner.SetLanguage(FLanguage, true);

  FModifiedDate := LoadNodeDateTime(Root, rsModified);
  FOtherLanguages := LoadNodeString(Root, rsOtherLanguages);
  FPublisher.LoadFromXml(Root);
  FPurpose.LoadFromXml(Root);
  FRights.LoadFromXml(Root);
  FTimeCoverage.LoadFromXml(Root);
  FTitle.LoadFromXml(Root);
  FVersion := LoadNodeString(Root, rsVersion);
end;

end.

