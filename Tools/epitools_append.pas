unit epitools_append;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  epidocument,
  epidatafiles;

type

  { TEpiToolAppend }

  TEpiToolAppend = class
  private
    FOnError: TNotifyEvent;
    procedure SetOnError(AValue: TNotifyEvent);
  public
    constructor Create;
    destructor  Destroy; override;
    function    Append(MainDocument, AppendDocument: TEpiDocument): boolean; overload;
    function    Append(MainDataFile, AppendDataFile: TEpiDataFile): boolean; overload;
  public
    // Events
    property    OnError: TNotifyEvent read FOnError write SetOnError;
  end;

implementation

{ TEpiToolAppend }

procedure TEpiToolAppend.SetOnError(AValue: TNotifyEvent);
begin
  if FOnError = AValue then Exit;
  FOnError := AValue;
end;

constructor TEpiToolAppend.Create;
begin

end;

destructor TEpiToolAppend.Destroy;
begin
  inherited Destroy;
end;

function TEpiToolAppend.Append(MainDocument, AppendDocument: TEpiDocument
  ): boolean;
begin

end;

function TEpiToolAppend.Append(MainDataFile, AppendDataFile: TEpiDataFile
  ): boolean;
begin

end;

end.

