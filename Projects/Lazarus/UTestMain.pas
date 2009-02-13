unit UTestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls, Grids;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    edInputFilename: TLabeledEdit;
    OpenDialog1: TOpenDialog;
    SpeedButton1: TSpeedButton;
    Button1: TButton;
    edOutputFilename: TLabeledEdit;
    Button2: TButton;
    SpeedButton2: TSpeedButton;
    SaveDialog1: TSaveDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    sg: TStringGrid;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ErrorMsg(s:string);
    procedure out(s:string);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

Uses
  UEpiDataFile;

var
  epd: TEpiDataFile;

procedure TForm1.ErrorMsg(s:string);
begin
  MessageDlg(s,mtError,[mbOK],0);
end;

procedure TForm1.out(s:string);
begin
  memo1.Lines.append(s);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if sender=SpeedButton1 then if OpenDialog1.Execute then edInputFilename.Text:=OpenDialog1.FileName;
  if sender=SpeedButton2 then if SaveDialog1.Execute then edOutputFilename.Text:=SaveDialog1.FileName;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
  epd:=NIL;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if assigned(epd) then FreeAndNil(epd);
  if trim(edInputFilename.Text)='' then
    begin
      ErrorMsg('No filename for input file entered');
      exit;
    end;
  if (not fileexists(edInputFilename.text)) then
    begin
      ErrorMsg('Input file does not exist');
      exit;
    end;
  epd:=TEpiDataFile.Create;
  out('Datafile: '+edInputFilename.text);
  if epd.Open(edInputFilename.Text,[eoInMemory, eoIgnoreChecks,oeIgnoreIndex]) then
    begin
      out('Data file opened with succes');
      out('Num fields = '+inttostr(epd.NumFields));
      out('Num data fields = '+inttostr(epd.NumDataFields));
      out('Num records = '+inttostr(epd.NumRecords));
    end
  else
    begin
      out('Could not open data file');
      out('Errorcode: '+inttostr(epd.ErrorCode));
      out('Errortext: '+epd.ErrorText);
    end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if assigned(epd) then FreeAndNil(epd);
end;

end.
