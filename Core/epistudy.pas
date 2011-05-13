unit epistudy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, DOM;

type

  { TEpiStudy }

//  TEpiStudyChangeEventType = ();
  TEpiStudy = class(TEpiCustomBase)
  private
    FAbstractText: TEpiTranslatedText;
    FAuthor: string;
    FCitations: TEpiTranslatedText;
    FCreated: TDateTime;
    FFunding: TEpiTranslatedText;
    FGeographicalCoverage: TEpiTranslatedText;
    FIdentifier: string;
    FLanguage: string;
    FModifiedDate: TDateTime;
    FOtherLanguages: string;
    FPublisher: TEpiTranslatedText;
    FPurpose: TEpiTranslatedText;
    FRights: TEpiTranslatedText;
    FTimeCoverage: TEpiTranslatedText;
    FTitle: TEpiTranslatedText;
    FVersion: string;
    procedure SetAuthor(const AValue: string);
    procedure SetIdentifier(const AValue: string);
    procedure SetLanguage(const AValue: string);
    procedure SetModifiedDate(const AValue: TDateTime);
    procedure SetOtherLanguages(const AValue: string);
    procedure SetVersion(const AValue: string);
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor Destroy; override;
    function   XMLName: string; override;
    function   SaveToXml(Content: String; Lvl: integer): string; override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    Property   AbstractText: TEpiTranslatedText read FAbstractText;
    Property   Author: string read FAuthor write SetAuthor;
    property   Citations: TEpiTranslatedText read FCitations;
    Property   Created: TDateTime read FCreated;
    property   Funding: TEpiTranslatedText read FFunding;
    property   GeographicalCoverage: TEpiTranslatedText read FGeographicalCoverage;
    property   Identifier: string read FIdentifier write SetIdentifier;
    Property   Language: string read FLanguage write SetLanguage;
    Property   ModifiedDate: TDateTime read FModifiedDate write SetModifiedDate;
    property   OtherLanguages: string read FOtherLanguages write SetOtherLanguages;
    property   Publisher: TEpiTranslatedText read FPublisher;
    property   Purpose: TEpiTranslatedText read FPurpose;
    property   Rights: TEpiTranslatedText read FRights;
    property   TimeCoverage: TEpiTranslatedText read FTimeCoverage;
    Property   Title: TEpiTranslatedText read FTitle;
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
  FAbstractText         := TEpiTranslatedText.Create(Self, rsAbstract);
  FCitations            := TEpiTranslatedText.Create(Self, rsCitations);
  FFunding              := TEpiTranslatedText.Create(Self, rsFunding);
  FGeographicalCoverage := TEpiTranslatedText.Create(Self, rsGeoCoverage);
  FIdentifier           := '';
  FLanguage             := '';
  FOtherLanguages       := '';
  FPublisher            := TEpiTranslatedText.Create(Self, rsPublisher);
  FPurpose              := TEpiTranslatedText.Create(Self, rsPurpose);
  FRights               := TEpiTranslatedText.Create(Self, rsRights);
  FTimeCoverage         := TEpiTranslatedText.Create(Self, rsTimeCoverage);
  FTitle                := TEpiTranslatedText.Create(Self, rsTitle);
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
    SaveNode(Lvl + 1, rsOtherLanguages, OtherLanguages) +
    SaveNode(Lvl + 1, rsVersion, Version);
  Result := inherited SaveToXml(Content, Lvl);
end;

procedure TEpiStudy.LoadFromXml(Root: TDOMNode);
begin
  // Root = <Study>
  FAbstractText.LoadFromXml(Root);
  FAuthor       := LoadNodeString(Root, rsAuthor);
  FCitations.LoadFromXml(Root);
  FCreated      := LoadNodeDateTime(Root, rsCreated);
  FFunding.LoadFromXml(Root);
  FGeographicalCoverage.LoadFromXml(Root);
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

