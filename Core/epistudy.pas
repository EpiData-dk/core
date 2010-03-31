unit epistudy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustomclass, DOM;

type

  { TEpiStudy }

  TEpiStudy = class(TEpiCustomClass)
  private
    FAbstractText: string;
    FAuthor: string;
    FCreated: TDateTime;
    FCreator: string;
    FDescription: string;
    FId: string;
    FLanguage: string;
    FModifiedDate: TDateTime;
    FOwner: string;
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
    procedure SetId(const AValue: string);
    procedure SetLanguage(const AValue: string);
    procedure SetModifiedDate(const AValue: TDateTime);
    procedure SetOwner(const AValue: string);
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
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
    procedure  SaveToStream(St: TStream; Lvl: Integer); override;
    procedure  LoadFromXml(Root: TDOMNode); override;
    Property   Id: string read FId write SetId;
    Property   Protocol: string read FProtocol write SetProtocol;
    Property   AbstractText: string read FAbstractText write SetAbstractText;
    Property   Author: string read FAuthor write SetAuthor;
    Property   Owner: string read FOwner write SetOwner;
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

{ TEpiStudy }

procedure TEpiStudy.SetAbstractText(const AValue: string);
begin
  if FAbstractText = AValue then exit;
  FAbstractText := AValue;
end;

procedure TEpiStudy.SetAuthor(const AValue: string);
begin
  if FAuthor = AValue then exit;
  FAuthor := AValue;
end;

procedure TEpiStudy.SetCreator(const AValue: string);
begin
  if FCreator = AValue then exit;
  FCreator := AValue;
end;

procedure TEpiStudy.SetDescription(const AValue: string);
begin
  if FDescription = AValue then exit;
  FDescription := AValue;
end;

procedure TEpiStudy.SetId(const AValue: string);
begin
  if FId = AValue then exit;
  FId := AValue;
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

procedure TEpiStudy.SetOwner(const AValue: string);
begin
  if FOwner = AValue then exit;
  FOwner := AValue;
end;

procedure TEpiStudy.SetProtocol(const AValue: string);
begin
  if FProtocol = AValue then exit;
  FProtocol := AValue;
end;

procedure TEpiStudy.SetProvenance(const AValue: string);
begin
  if FProvenance = AValue then exit;
  FProvenance := AValue;
end;

procedure TEpiStudy.SetReferences(const AValue: string);
begin
  if FReferences = AValue then exit;
  FReferences := AValue;
end;

procedure TEpiStudy.SetRightsHolder(const AValue: string);
begin
  if FRightsHolder = AValue then exit;
  FRightsHolder := AValue;
end;

procedure TEpiStudy.SetSource(const AValue: string);
begin
  if FSource = AValue then exit;
  FSource := AValue;
end;

procedure TEpiStudy.SetSpatial(const AValue: string);
begin
  if FSpatial = AValue then exit;
  FSpatial := AValue;
end;

procedure TEpiStudy.SetSubject(const AValue: string);
begin
  if FSubject = AValue then exit;
  FSubject := AValue;
end;

procedure TEpiStudy.SetTemporal(const AValue: string);
begin
  if FTemporal = AValue then exit;
  FTemporal := AValue;
end;

procedure TEpiStudy.SetTitle(const AValue: string);
begin
  if FTitle = AValue then exit;
  FTitle := AValue;
end;

constructor TEpiStudy.Create(AOwner: TObject);
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
  S :=
    Ins(Lvl)     + '<Study>' + LineEnding +
    Ins(Lvl + 1) + '<Protocol>' + StringToXml(Protocol) + '</Protocol>' + LineEnding +
    Ins(Lvl + 1) + '<Abstract>' + StringToXml(AbstractText) + '</Abstract>' + LineEnding +
    Ins(Lvl + 1) + '<Author>' + StringToXml(Author) + '</Author>' + LineEnding +
    Ins(Lvl + 1) + '<Owner>' + StringToXml(Owner) + '</Owner>' + LineEnding +
    Ins(Lvl + 1) + '<Created>' + DateTimeToStr(Created) + '</Created>' + LineEnding +
    Ins(Lvl + 1) + '<Creator>' + StringToXml(Creator) + '</Creator>' + LineEnding +
    Ins(Lvl + 1) + '<Description>' + StringToXml(Description) + '</Description>' + LineEnding +
    Ins(Lvl + 1) + '<Language>' + StringToXml(Language) + '</Language>' + LineEnding +
    Ins(Lvl + 1) + '<Modified>' + DateTimeToStr(ModifiedDate) + '</Modified>' + LineEnding +
    Ins(Lvl + 1) + '<Provenance>' + StringToXml(Provenance) + '</Provenance>' + LineEnding +
    Ins(Lvl + 1) + '<References>' + StringToXml(References) + '</References>' + LineEnding +
    Ins(Lvl + 1) + '<Rightsholder>' + StringToXml(RightsHolder) + '</Rightsholder>' + LineEnding +
    Ins(Lvl + 1) + '<Source>' + StringToXml(Source) + '</Source>' + LineEnding +
    Ins(Lvl + 1) + '<Subject>' + StringToXml(Subject) + '</Subject>' + LineEnding +
    Ins(Lvl + 1) + '<Temporal>' + StringToXml(Temporal) + '</Temporal>' + LineEnding +
    Ins(Lvl + 1) + '<Title>' + StringToXml(Title) + '</Title>' + LineEnding +
    Ins(Lvl)     + '</Study>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiStudy.LoadFromXml(Root: TDOMNode);
begin
  // Root = <Study>
  Protocol     := UTF8Encode(Root.FindNode('Protocol').TextContent);
  AbstractText := UTF8Encode(Root.FindNode('Abstract').TextContent);
  Author       := UTF8Encode(Root.FindNode('Author').TextContent);
  Owner        := UTF8Encode(Root.FindNode('Owner').TextContent);
  // Access FCreated directly.
  FCreated      := StrToDateTime(Root.FindNode('Created').TextContent);
  Creator      := UTF8Encode(Root.FindNode('Creator').TextContent);
  Description  := UTF8Encode(Root.FindNode('Description').TextContent);
  Language     := UTF8Encode(Root.FindNode('Language').TextContent);
  ModifiedDate := StrToDateTime(Root.FindNode('Modified').TextContent);
  Provenance   := UTF8Encode(Root.FindNode('Provenance').TextContent);
  References   := UTF8Encode(Root.FindNode('References').TextContent);
  Rightsholder := UTF8Encode(Root.FindNode('Rightsholder').TextContent);
  Source       := UTF8Encode(Root.FindNode('Source').TextContent);
  Subject      := UTF8Encode(Root.FindNode('Subject').TextContent);
  Temporal     := UTF8Encode(Root.FindNode('Temporal').TextContent);
  Title        := UTF8Encode(Root.FindNode('Title').TextContent);
end;

end.

