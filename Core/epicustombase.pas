unit epicustombase;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM;

type

  { TEpiCustomBase }

  TEpiChangeEvent = procedure(Sender: TObject; EventGroup: Word; EventType: Word; Data: Pointer) of object;

  TEpiCustomBase = class
  private
    FOwner: TEpiCustomBase;
  protected
    constructor Create(AOwner: TEpiCustomBase); virtual;
    Function    Ins(Level: integer): string;
  public
    procedure  SaveToStream(St: TStream; Lvl: integer); virtual;
    procedure  LoadFromXml(Root: TDOMNode); virtual;
  private
    // OnChange-hook privates
    FOnChangeList: ^TEpiChangeEvent;
    FOnChangeListCount: Integer;
    FUpdateCount: Integer;
  public
    // OnChange-hook methods
    procedure  BeginUpdate; virtual;
    procedure  EndUpdate; virtual;
    procedure  RegisterOnChangeHook(Event: TEpiChangeEvent); virtual;
    procedure  UnRegisterOnChangeHook(Event: TEpiChangeEvent); virtual;
  end;

  { TEpiCustomItem }

  TEpiCustomItem = class(TEpiCustomBase)
  private

  protected
    constructor Create(AOwner: TEpiCustomBase); override;
  protected
    FId: string;
    FName: string;
    function GetId: string; virtual;
    function GetName: string;
    procedure SetId(const AValue: string); virtual;
    procedure SetName(const AValue: string);
  public
    property Id: string read GetId write SetId;
    property Name: string read GetName write SetName;
  end;

  { TEpiCustomList }

  TEpiCustomList = class(TEpiCustomBase)
  private
    FList: TFPList;
  protected
     constructor Create(AOwner: TEpiCustomBase); override;
  public

  end;

implementation

{ TEpiCustomBase }

constructor TEpiCustomBase.Create(AOwner: TEpiCustomBase);
begin
  FOwner := AOwner;
end;

function TEpiCustomBase.Ins(Level: integer): string;
begin

end;

procedure TEpiCustomBase.SaveToStream(St: TStream; Lvl: integer);
var
  S: String;
begin
  S := Ins(LvL) + '<' + ClassName + '>Not Implemented Yet</' + ClassName + '>' + LineEnding;
  St.Write(S[1], Length(S));
end;

procedure TEpiCustomBase.LoadFromXml(Root: TDOMNode);
begin
  // Do nothing - should be overridden in descendants.
end;

procedure TEpiCustomBase.BeginUpdate;
begin

end;

procedure TEpiCustomBase.EndUpdate;
begin

end;

procedure TEpiCustomBase.RegisterOnChangeHook(Event: TEpiChangeEvent);
begin

end;

procedure TEpiCustomBase.UnRegisterOnChangeHook(Event: TEpiChangeEvent);
begin

end;

{ TEpiCustomList }

constructor TEpiCustomList.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
  FList := TFPList.Create;
end;

{ TEpiCustomItem }

constructor TEpiCustomItem.Create(AOwner: TEpiCustomBase);
begin
  inherited Create(AOwner);
end;

function TEpiCustomItem.GetId: string;
begin
  result := FId;
end;

function TEpiCustomItem.GetName: string;
begin

end;

procedure TEpiCustomItem.SetId(const AValue: string);
begin

end;

procedure TEpiCustomItem.SetName(const AValue: string);
begin
  if FName = AValue then exit;
  FName := AValue;
end;


end.

