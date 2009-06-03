unit UWarning;

{$IFDEF FPC}
  {$mode DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC} LResources, {$ELSE} Windows, {$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
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

{$IFNDEF FPC}
  {$R *.dfm}
{$ENDIF}

{$IFDEF FPC}
initialization
  {$i uwarning.lrs}
{$ENDIF}


end.
