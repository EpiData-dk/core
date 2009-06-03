unit UCheckFileCmds;

interface

uses
  Classes, UValueLabels, UDataFileTypes, UCheckFileTypes, Dialogs, Graphics;

type

  TChkCommands = class;

{  TChkCommand = Class(TObject)
  protected
    function    CommandType(): TChkCmdType; virtual; abstract;
  public
    Constructor Create(); virtual;
    Destructor  Destroy(); override;
    property    CmdType: TChkCmdType read CommandType;
  end;

  TChkIf = class(TChkCommand)
  private
    FIfCmds:    TChkCommands;
    FElseCmds:  TChkCommands;
    FExpr:      String;
    FShowExpr:  String;
  protected
    function    CommandType(): TChkCmdType; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    Property    Expr: string read FExpr write FExpr;
    Property    ShowExpr:  string read FShowExpr write FShowExpr;
    Property    IfCmds: TChkCommands read FIfCmds write FIfCmds;
    Property    ElseCmds: TChkCommands read FElseCmds write FElseCmds;
  end;         }
  
  PCmd = ^TCmd;
  TCmd = record
    Next: PCmd;
    case Command: TChkCmdType of
      cmdIF:
         (IfExpr:          String[200];
          IfShowExpr:      String[200];
          IfCmds:          TChkCommands;
          ElseCmds:        TChkCommands);
      cmdHelp:
         (HelpString:      String[250];
          HelpType:        TMsgDlgType;
          HelpKeys:        String[10]);
      cmdHide, cmdUnHide,
      cmdClear, cmdGoto:
         (HideVarNumber:   Integer;
          HideVarName:     String[10]);
      cmdComLegal:
         (clVarNumber:     Integer;
          ValueLabelName:  String[40];
          ValueLabel:      TValueLabelSet;
          ValueLabelIsFieldRef: Boolean;
          ValueLabelUse:   string[50];
          ShowList:        Boolean);
      cmdTypeString:
         (tsVarNumber:     Integer;
          TypeText:        String[40];
          Typecolor:       TColor);
      cmdRelate:
         (RelField:        String[10];
          RelFileNo:       Integer;
          RelFileStr:      String[200];
          One2One:         Boolean);
      cmdLet:
         (VarName:         String[20];
          VarNumber:       Integer;
          VarIsField:      Boolean;
          CodedWithLET:    Boolean;
          LetExpr:         String[200]);
      cmdComment:
         (Comment:         String[200]);
      cmdDefine:
         (FName:           String[20];
          FeltType:        TFieldType;
          FLength:         Integer;
          FNumDecimals:    Byte;
          FScope:          TFieldScope);
      cmdWriteNote:
         (FNote:           String[200];
          ShowNotes:       Boolean);
      cmdCopyToClipboard:
         (CopyStr:         String[200]);
      cmdBackup:
         (DestLib:         String[200];
          zipit:           Boolean;
          encryptit:       Boolean;
          filename:        String[200];
          pw:              string[30];
          dateit:          Boolean);
      cmdBeep:
         (BeepType:        TBeepTypes);
      cmdLoad:
         (DLLName:         String[200]);
      cmdExecute:
         (ExecCmdLine:     String[255];
          ExecParams:      String[255];
          ExecHide:        Boolean;
          ExecWait:        Boolean);
      cmdLeaveField:
         (cLeaveStyle:     TLeaveStyles;
          IsLastField:     Boolean);
      cmdColor:
         (ColorCmd:        Byte;      //1=color question, 2=color data,  3=color background, 4=color fieldname
          TxtColor:        TColor;
          BgColor:         TColor;
          IsEpiInfoNo:     Boolean;
          CFieldno:        Byte);
    end;

  TChkCommands = class(TObject)
  private
    FList: TList;
    function GetCount:integer;
    function GetItem(index:integer):PCmd;
    Procedure DisposeCommandList(aList: TList);
  public
    constructor Create();
    destructor  Destroy(); override;
    procedure   AddCommand(aCmd: PCmd);
    procedure   Clone(var Dest: TChkCommands);
    function    NewCmd: PCmd;
    property    Count: integer read GetCount;
    property    items[index:integer]:PCmd read GetItem; default;
    property    List:TList read FList;
  end;

implementation

uses
  SysUtils;

 { TChkCommands }

constructor TChkCommands.Create();
begin
  inherited;
  FList:=TList.Create;
end;

destructor TChkCommands.Destroy();
begin
  DisposeCommandList(FList);
  inherited;
end;

Procedure TChkCommands.DisposeCommandList(aList: TList);
VAR
  n:Integer;
  tmpCmdRec: PCmd;
BEGIN
  FOR n:=0 TO AList.Count-1 DO
    BEGIN
      TmpCmdRec := PCmd(AList.Items[n]);
      CASE tmpCmdRec^.Command OF
        cmdIF:
          BEGIN
            IF tmpCmdRec^.IfCmds <> NIL THEN
              DisposeCommandList(tmpCmdRec^.IfCmds.List);
            IF tmpCmdRec^.ElseCmds <> NIL THEN
               DisposeCommandList(tmpCmdRec^.ElseCmds.List);
          END;
      END;  //case
      Dispose(PCmd(Alist.Items[n]));
    END;  //for
  FreeAndNil(Alist);
END;

procedure TChkCommands.AddCommand(aCmd: PCmd);
begin
  FList.Add(aCmd);
end;

function TChkCommands.GetCount:integer;
begin
  result := FList.count;
end;

function TChkCommands.GetItem(index:integer):PCmd;
begin
  result := NIL;
  if (index >= 0) and (index < FList.Count) then
    result := PCmd(FList.Items[index])
end;

procedure TChkCommands.Clone(var Dest: TChkCommands);
var
  n: integer;
  aCmd, new: PCmd;
begin
  if (not assigned(dest)) then
    dest := TChkCommands.create;
  for n := 0 TO FList.Count-1 do
  begin
    aCmd := PCmd(FList.Items[n]);
    new := NewCmd;
    new^ := aCmd^;
    new^.IfCmds :=NIL;
    new^.ElseCmds :=NIL;
    if aCmd^.IfCmds <> NIL then
    begin
      new^.IfCmds := TChkCommands.create;
      aCmd^.IfCmds.Clone(new^.IfCmds);
    end;
    if aCmd^.ElseCmds <> NIL then
    begin
      new^.ElseCmds := TChkCommands.create;
      aCmd^.ElseCmds.Clone(new^.ElseCmds);
    end;
    dest.AddCommand(new);
  end;
end;

function TChkCommands.NewCmd: PCmd;
begin
  new(result);
  result^.IfCmds := NIL;
  result^.ElseCmds := NIL;
end;

(*
{ TChkCommand }

constructor TChkCommand.Create;
begin

end;

destructor TChkCommand.Destroy;
begin

  inherited;
end;

{ TChkIf }

function TChkIf.CommandType: TChkCmdType;
begin
  Result := cmdIF;
end;

constructor TChkIf.Create;
begin
  inherited;

end;

destructor TChkIf.Destroy;
begin

  inherited;
end;
    *)
end.
