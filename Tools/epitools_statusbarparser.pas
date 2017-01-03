unit epitools_statusbarparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TEpiSBSIdentType = (
    esiData,       // found a %d()
    esiField,      // found a %f()
    esiCaption     // found a %c()
  );

  TEpiSBSParserTextFound = procedure(Sender: TObject; Const S: string) of object;
  TEpiSBSParserIdentifierFound = procedure(Sender: TObject; IdentType: TEpiSBSIdentType; Const IdentName: string) of object;
  TEpiSBSParserError = procedure(Sender: TObject; CaretPos: Integer; Const Msg: String) of object;

  { TEpiStatusbarStringParser }

  TEpiStatusbarStringParser = class
  private
    FParseString: string;
  public
    constructor Create;
    destructor Destroy; override;
    function ParseString(Const S: string): boolean;

  { Parse Events }
  private
    FOnTextFound: TEpiSBSParserTextFound;
    FOnIdentifierFound: TEpiSBSParserIdentifierFound;
    FOnParseError: TEpiSBSParserError;
    procedure DoTextFound(Const S: string);
    procedure DoIdentifierFound(IdentType: TEpiSBSIdentType; Const IdentName: string);
    procedure DoParseError(CaretPos: Integer; Const Msg: String);
  public
    property OnTextFound: TEpiSBSParserTextFound read FOnTextFound write FOnTextFound;
    property OnIdentifierFound: TEpiSBSParserIdentifierFound read FOnIdentifierFound write FOnIdentifierFound;
    property OnParseError: TEpiSBSParserError read FOnParseError write FOnParseError;
  end;

implementation

{ TEpiStatusbarStringParser }

constructor TEpiStatusbarStringParser.Create;
begin
  FParseString := '';
end;

destructor TEpiStatusbarStringParser.Destroy;
begin
  inherited Destroy;
end;

function TEpiStatusbarStringParser.ParseString(const S: string): boolean;
var
  IdentName, Text: String;
  I, P, L: Integer;
  IdentType: TEpiSBSIdentType;
begin
  Result := true;
  FParseString := S;

  I := 1;
  L := Length(S);

  while I <= L do
  begin
    if FParseString[i] = '%' then
    begin
      Inc(i);
      case FParseString[i] of
        'c': IdentType := esiCaption;
        'd': IdentType := esiData;
        'f': IdentType := esiField;
      else
        DoParseError(I, 'Unknown specifier: ' + FParseString[i]);
        Exit(false);
      end;

      Inc(i);
      if (FParseString[i] <> '(') then
      begin
        DoParseError(I, '"(" expected after specifier, but "' + FParseString[i] + '" found');
        Exit(false);
      end;
      Inc(i);

      P := I;
      while I <= L do
      begin
        if FParseString[I] = ')' then
          Break;
        Inc(I);
      end;

      if (I > L) then
      begin
        DoParseError(I, '")" expected after specifier, but was not found');
        Exit(false);
      end;

      IdentName := Copy(FParseString, P, I - P);
      DoIdentifierFound(IdentType, IdentName);

      Inc(I);
      Continue;
    end;

    P := I;
    while (I <= L) and
          (FParseString[I] <> '%')
    do
      Inc(I);

    Text := Copy(FParseString, P, I - P);
    DoTextFound(Text);
  end;
end;

procedure TEpiStatusbarStringParser.DoTextFound(const S: string);
begin
  if Assigned(FOnTextFound) then
    FOnTextFound(Self, S);
end;

procedure TEpiStatusbarStringParser.DoIdentifierFound(
  IdentType: TEpiSBSIdentType; const IdentName: string);
begin
  if Assigned(FOnIdentifierFound) then
    FOnIdentifierFound(Self, IdentType, IdentName);
end;

procedure TEpiStatusbarStringParser.DoParseError(CaretPos: Integer;
  const Msg: String);
begin
  if Assigned(FOnParseError) then
    FOnParseError(Self, CaretPos, Msg);
end;

end.

