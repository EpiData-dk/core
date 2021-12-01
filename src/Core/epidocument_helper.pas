unit epidocument_helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument;

type

  { TEpiDocumentHelper }

  TEpiDocumentHelper = class helper for TEpiDocument
  public
    function IsEncrypted(): boolean;
    function IsPasswordProtected(): boolean;
    function IsExtendedAccessProtected(): boolean;
  end;

implementation

{ TEpiDocumentHelper }

function TEpiDocumentHelper.IsEncrypted(): boolean;
begin
  result := IsPasswordProtected() or IsExtendedAccessProtected();
end;

function TEpiDocumentHelper.IsPasswordProtected(): boolean;
begin
  result := (self.PassWord <> '');
end;

function TEpiDocumentHelper.IsExtendedAccessProtected(): boolean;
begin
  result := (self.Admin.Initialized);
end;

end.

