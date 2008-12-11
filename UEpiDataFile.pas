unit UEpiDataFile;

interface

uses
  UEpiDataConstants,UCustomFileHandler,URecFileHandler,SysUtils;

TYPE

  TErrorEvent = procedure(errorcode: Integer) of object;
  TTranslateEvent = function(langcode:Integer; text:widestring): widestring of object;

  TEpiDataFile = class(TObject)
    private
      FonError:     TErrorEvent;
      FonTranslate: TTranslateEvent;
      FcurrentRec:  Integer;   //Current record
      FNumRecords:  Integer;   //Number of records in file
      FFileHandler: TcustomFileHandler;
      procedure Error(errorcode:integer);
      function Translate(langcode: Integer; Text: WideString): widestring;
      procedure SetCurrentRec(value:integer);
    public
      constructor create;
      function   load(filename:string=''; aOptions:TEpiDataFileOptions=[]):boolean;  //Loads file in internal structure
      function   Read(RecordNum:integer):boolean;
      function   Write(RecordNum:Integer):boolean;   //Saves one record on disk, updates indices
      function   Commit:boolean;                     //Saves whole file on disk

      property onError:TErrorEvent read FonError write FonError;
      property onTranslate:TTranslateEvent read FonTranslate write FonTranslate;

      property CurrentRec:integer read FCurrentRec write SetCurrentRec;

  end;



implementation

{************ TEpiDataFile *******************}
constructor TEpiDataFile.create;
begin
  inherited create;
  FonError:=NIL;
  FonTranslate:=NIL;
  FcurrentRec:=-1;
  FFileHandler:=NIL;
end;

procedure TEpiDataFile.Error(errorcode:integer);
begin
  if assigned(FonError) then FonError(errorcode);
end;

function TEpiDataFile.Translate(langcode: Integer; Text: WideString):widestring;
begin
  if Assigned(FonTranslate) then
    result := FonTranslate(langcode, text)
  else
    result := text;
end;

function TEpiDataFile.load(filename:string=''; aOptions:TEpiDataFileOptions=[]):boolean;
var
  ext: string;
begin
  result:=false;
  if filename<>'' then ext:=AnsiLowerCase(ExtractFileExt(filename));
  if (ext='.rec') or (ext='') then FFileHandler:=TrecFileHandler.create
{  else if ext='dta' then FFileHandler:=TStataFileHandler.create
  else if ext='txt' then FFileHandler:=TTxtFileHandler.create
  else if ext='sas' then FFileHandler:=TSASFileHandler.create
  else if ext='sps' then FFileHandler:=TSPSSFileHandler.create
  else if ext='xls' then FFileHandler:=TXLSFileHandler.create}
  else
    begin
      //error('Filetype `'+ext+'` not supported');
      exit;
    end;
  if assigned(FFileHandler) then
    begin
      FFileHandler.load(filename,aOptions);
      result:=true;
    end
  else result:=false;
end;

function TepiDataFile.Read(RecordNum:integer):boolean;
begin
  if RecordNum>FNumRecords then
    begin
      //Error('Recordnumber exceeds total number of records');
      result:=false;
    end
  else FcurrentRec:=RecordNum;
end;

function TEpiDataFile.Write(RecordNum:Integer):boolean;
begin
  if (Not assigned(FFileHandler)) then
    begin
      //Error('No Filehandler');
      result:=false;
    end
  else
    begin
      FFileHandler.Write(RecordNum);
      result:=true;
    end;

end;

function TEpiDataFile.Commit:boolean;
begin
end;

procedure TEpiDataFile.SetCurrentRec(value:integer);
begin
  if value>FNumRecords then
    begin
      //Error('Recordnumber exceeds total number of records');
    end
  else FcurrentRec:=value;
end;

end.

