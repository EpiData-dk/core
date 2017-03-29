unit epiopenfile_cache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiopenfile, epiadmin,
  epidocument;

type

  { TEpiDocumentFileCache }

  TEpiDocumentFileCache = class
  private
    FDocumentFileClass: TEpiDocumentFileClass;
    FFileList: TStringList;
    FOnError: TOpenEpiErrorEvent;
    FOnPassword: TRequestPasswordEvent;
    FOnProgress: TEpiProgressEvent;
    FOnWarning: TOpenEpiWarningEvent;
    function DoLoadFile(Const FileName: string; ReadOnly: boolean): integer;
  public
    constructor Create;
    destructor Destroy; override;
    function OpenFile(Const FileName: string; ReadOnly: boolean = false): TEpiDocumentFile;
    property OnPassword: TRequestPasswordEvent read FOnPassword write FOnPassword;
    property OnWarning: TOpenEpiWarningEvent read FOnWarning write FOnWarning;
    property OnError: TOpenEpiErrorEvent read FOnError write FOnError;
    property OnProgress: TEpiProgressEvent read FOnProgress write FOnProgress;
    property DocumentFileClass: TEpiDocumentFileClass read FDocumentFileClass write FDocumentFileClass;
  end;

implementation

{ TEpiDocumentFileCache }

function TEpiDocumentFileCache.DoLoadFile(const FileName: string;
  ReadOnly: boolean): integer;
var
  DocFile: TEpiDocumentFile;
begin
  Result := -1;
  DocFile := FDocumentFileClass.Create;
  DocFile.OnPassword := OnPassWord;
  Docfile.OnWarning  := OnWarning;
  Docfile.OnError    := OnError;
//  Docfile.OnProgress := OnProgress;

  if DocFile.OpenFile(FileName, ReadOnly) then
    Result := FFileList.AddObject(FileName, Docfile)
  else
    DocFile.Free;
end;

constructor TEpiDocumentFileCache.Create;
begin
  FFileList := TStringList.Create;
  FDocumentFileClass := TEpiDocumentFile;
end;

destructor TEpiDocumentFileCache.Destroy;
var
  i: Integer;
begin
  for i := 0 to FFileList.Count - 1 do
    FFileList.Objects[i].Free;

  FFileList.Free;

  inherited Destroy;
end;

function TEpiDocumentFileCache.OpenFile(const FileName: string;
  ReadOnly: boolean): TEpiDocumentFile;
var
  Idx: Integer;
begin
  Idx := FFileList.IndexOf(FileName);

  if Idx < 0 then
    Idx := DoLoadFile(FileName, ReadOnly);

  if Idx = -1 then
    Result := nil
  else
    Result := TEpiDocumentFile(FFileList.Objects[Idx]);
end;

end.

