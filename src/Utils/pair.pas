unit pair;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TPair }

  generic TPair<T, S> = class
  private
    FFirst: T;
    FSecond: S;
  protected
    property First: T read FFirst write FFirst;
    property Second: S read FSecond write FSecond;
  public
    constructor Create();
    constructor Create(AFirst: T; ASecond: S);
  end;

implementation

{ TPair }

constructor TPair.Create();
begin
  Create(nil, nil);
end;

constructor TPair.Create(AFirst: T; ASecond: S);
begin
  FFirst := AFirst;
  FSecond := ASecond;
end;

end.

