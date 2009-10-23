unit UWarning;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  LResources, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TWarningForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    Label4: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WarningForm: TWarningForm;

implementation

initialization
  {$i uwarning.lrs}

end.
