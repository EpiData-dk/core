unit epi_iso639;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Epi_ISO639_AddDesciption(Const List: TStrings);
procedure Epi_ISO639_AddLangCodes(Const List: TStrings);
procedure Epi_ISO639_AddLangAndDesciption(Const List: TStrings);

implementation

uses
  epistringutils;

const
{$I lang.inc}

{$I desc.inc}


procedure Epi_ISO639_AddDesciption(const List: TStrings);
begin
  List.AddStrings(DescArray);
end;

procedure Epi_ISO639_AddLangCodes(const List: TStrings);
begin
  List.AddStrings(LangArray);
end;

procedure Epi_ISO639_AddLangAndDesciption(const List: TStrings);
var
  i: Integer;
begin
  List.BeginUpdate;
  List.Clear;

  for i := Low(LangArray) to High(LangArray) do
    List.AddObject(DescArray[i], TString.Create(LangArray[i]));

  List.EndUpdate;
end;

end.

