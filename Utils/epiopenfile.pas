unit epiopenfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiadmin, epidocument, epicustombase;

type
  TOpenEpiWarningType =
    (wtLockFile,       // Trying to open a file with a .lock file present.
     wtDatePattern,    // A date pattern has been detected in the file, could be an auto backup
     wtTimeBackup,     // A file with .bak found, which indicated that the program could have shutdown unexpectedly.
     wtTimeBackup2nd
    );
  TOpenEpiWarningResult = (wrYes, wrNo, wrCancel);
  TOpenEpiWarningEvent = function (WarningType: TOpenEpiWarningType;
    Const Msg: string): TOpenEpiWarningResult of object;

  TOpenEpiErrorEvent = procedure(Const Msg: string) of object;

  { TEpiDocumentFile }

  TEpiDocumentFile = class
  private
    // The GUID is unique for the entire instance of the program. This ensures
    // that when checking a .lock file we know if the current program own the
    // lock or not.
    FGuid: TGuid; static;

    type
      TLockFile = record
        GUID: TGuid;
        TimeStamp: TTimeStamp;
      end;
      PLockFile = ^TLockFile;

  private
    // Private event holders
    FOnError: TOpenEpiErrorEvent;
    FOnPassword: TRequestPasswordEvent;
    FOnWarning: TOpenEpiWarningEvent;
  private
    // Internal housekeeping of current open EpiDocument.
    FFileName: string;
    FTimeStamp: TTimeStamp;
    FEpiDoc: TEpiDocument;
    procedure DocumentChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    function ReadLockFile(Const Fn: string): PLockFile;
    procedure WriteLockFile(Const Fn: string; LF: PLockFile);
    procedure CreateLockFile;
    procedure DeleteLockFile;
  protected
    function LockFileExists(Const FileName: string;
      out Msg: string): boolean;
    function DatePatternExists(Const FileName: string;
      out AltFileName: string; out Msg: string): boolean;
    function BackupFileExists(Const FileName: string;
      out Msg: string): boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function OpenFile(Const FileName: string): TEpiDocument;
    function SaveFile(Const FileName: string): Boolean;
  public
    // Event properties
    property OnPassword: TRequestPasswordEvent read FOnPassword write FOnPassword;
    property OnWarning: TOpenEpiWarningEvent read FOnWarning write FOnWarning;
    property OnError: TOpenEpiErrorEvent read FOnError write FOnError;
  public
    // Other properties
    property FileName: string read FFileName;
  end;

implementation

uses
  epimiscutils, LazUTF8, RegExpr, FileUtil;

var
  OpenEpiDocumentInstance: TEpiDocumentFile = nil;

{ TEpiDocumentFile }

constructor TEpiDocumentFile.Create;
begin
  if IsEqualGUID(FGuid, GUID_NULL) then
    CreateGUID(FGuid);
end;

destructor TEpiDocumentFile.Destroy;
begin
  DeleteLockFile;
  inherited Destroy;
end;

procedure TEpiDocumentFile.DocumentChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  // Housekeeping to know if document is being destroyed.

  if not (EventGroup = eegCustomBase) then exit;
  if TEpiCustomChangeEventType(EventType) <> ecceDestroy then exit;

  DeleteLockFile;
end;

function TEpiDocumentFile.ReadLockFile(const Fn: string): PLockFile;
var
  St: TFileStream;
begin
  Result := nil;
  if not FileExistsUTF8(Fn) then exit;

  Result := New(PLockFile);
  St := TFileStream.Create(Fn, fmOpenRead);
  St.Read(Result^, SizeOf(TLockFile));
  St.Free;
end;

procedure TEpiDocumentFile.WriteLockFile(const Fn: string; LF: PLockFile);
var
  St: TFileStream;
begin
  if FileExistsUTF8(Fn) and
     FileIsReadOnlyUTF8(Fn) then exit;

  St := TFileStream.Create(Fn, fmCreate);
  St.Write(LF^, SizeOf(TLockFile));
  St.Free;
end;

function TEpiDocumentFile.LockFileExists(const FileName: string; out Msg: string
  ): boolean;
var
  LockFileName: String;
  LF: PLockFile;
begin
  result := false;
  LockFileName := FFileName + '.lock';

  if FileExistsUTF8(LockFileName) then
  begin
    LF := ReadLockFile(LockFileName);

    if IsEqualGUID(LF^.GUID, FGuid) then
      begin
        // TODO: Handle that this file is already opened by this program.
      end
    else
      begin
        Msg := 'This file is locked by another program!' + LineEnding +
               'The file was locked at: ' + DateTimeToStr(TimeStampToDateTime(LF^.TimeStamp)) + LineEnding
               + LineEnding +
               'Continue opening this file?';
        result := true;
      end;
  end;
end;

function TEpiDocumentFile.DatePatternExists(const FileName: string; out
  AltFileName: string; out Msg: string): boolean;
var
  R: TRegExpr;
  S: RegExprString;
  S2: String;
  B: Boolean;
begin
  Result := false;

  // Check for date in the file being opened.
  R := TRegExpr.Create;
  R.Expression := '([0-9]{4}.[0-9]{2}.[0-9]{2}\.)';
  B := R.Exec(FileName);

  AltFileName := R.Replace(FileName, '', false);
  S := ChangeFileExt(Filename, BoolToStr(ExtractFileExt(Filename) = '.epz', '.epx', '.epz'));
  if B and
     (FileExistsUTF8(S) or FileExistsUTF8(S2))
  then
  begin
    if FileExistsUTF8(S) then
      AltFileName := S;

    Result := true;

    Msg :=   'This file seem to be an automated backup file (on closing the program):' + LineEnding + LineEnding +
             'File: ' + SysToUTF8(ExtractFileName(UTF8ToSys(FileName)))          +
               ' (' + FormatDateTime('YYYY/MM/DD HH:NN:SS', FileDateToDateTime(FileAgeUTF8(FileName))) + ')' + LineEnding +
             'Original: ' + SysToUTF8(ExtractFileName(UTF8ToSys(S))) +
               ' (' + FormatDateTime('YYYY/MM/DD HH:NN:SS', FileDateToDateTime(FileAgeUTF8(S))) + ')' + LineEnding +
             LineEnding +
             'Load the original instead?';
  end;
  R.Free;
end;

function TEpiDocumentFile.BackupFileExists(const FileName: string; out
  Msg: string): boolean;
begin
  if FileExistsUTF8(FileName + '.bak') then
  begin
    Msg :=   'A timed backup file exists. (loading of this overwrites previous project file)' + LineEnding + LineEnding +
             'File: ' + SysToUTF8(ExtractFileName(UTF8ToSys(FileName)))          +
               ' (' + FormatDateTime('YYYY/MM/DD HH:NN:SS', FileDateToDateTime(FileAgeUTF8(FileName))) + ')' + LineEnding +
             'Recovery: ' + #9 + SysToUTF8(ExtractFileName(UTF8ToSys(FileName + '.bak'))) +
               ' (' + FormatDateTime('YYYY/MM/DD HH:NN:SS', FileDateToDateTime(FileAgeUTF8(FileName + '.bak'))) + ')' + LineEnding +
             LineEnding +
             'Load the backup instead?';
  end;
end;

procedure TEpiDocumentFile.CreateLockFile;
var
  LF: PLockFile;
  LockFileName: String;
begin
  LockFileName := FFileName + '.lock';

  LF := New(PLockFile);
  with LF^ do
  begin
    GUID := FGuid;
    TimeStamp := DateTimeToTimeStamp(Now);
  end;
  WriteLockFile(LockFileName, LF);
  Dispose(LF);
end;

procedure TEpiDocumentFile.DeleteLockFile;
var
  LockFileName: String;
begin
  LockFileName := FFileName + '.lock';

  if FileExistsUTF8(LockFileName) then
    DeleteFileUTF8(LockFileName);
end;

function TEpiDocumentFile.OpenFile(const FileName: string): TEpiDocument;
var
  St: TMemoryStream;
  Msg: String;
  Res: TOpenEpiWarningEvent;
  AltFn: string;
  Fn: String;
begin
  Result := nil;
  FFileName := FileName;

  if LockFileExists(FFileName, Msg) then
    case OnWarning(wtLockFile, Msg) of
      wrYes:
        ;
      wrNo,
      wrCancel:
        Exit;
    end;
{
  if DatePatternExists(FFileName, AltFn, Msg) then
    case OnWarning(wtDatePattern, Msg) of
      wrYes:
        FFileName := AltFn;
      wrNo:
        ;
      wrCancel:
        Exit;
    end;

  if BackupFileExists(FFileName, Msg) then
    case OnWarning(wtTimeBackup, Msg) of
      wrYes:    FFileName := FFileName + '.bak';
      wrNo:     begin
                  Msg := 'Loading ' + SysToUTF8(ExtractFileName(UTF8ToSys(FFileName))) + ' will delete recovery file.' + LineEnding +
                         'Continue?';
                  case OnWarning(wtTimeBackup2nd, Msg) of
                    wrNo:  Exit;
                  end;
                end;
    end;               }

  try
    try
      Msg := '';

      Result := TEpiDocument.Create('en');
      St := TMemoryStream.Create;

      if ExtractFileExt(UTF8ToSys(FFileName)) = '.epz' then
        ZipFileToStream(St, FFileName)
      else
        St.LoadFromFile(UTF8ToSys(FFileName));
      St.Position := 0;

      result.OnPassword := FOnPassword;
      result.LoadFromStream(St);
    except
      on E: TEpiCoreException do
        Msg := 'Unable to open the file: ' + FFileName + LineEnding + E.Message;

      on E: EFOpenError do
        Msg := 'Unable to open the file: ' + FFileName + LineEnding +
               'File is corrupt or does not exist.' + LineEnding +
               E.Message;

      on EEpiBadPassword do
        Msg := 'Unable to open the file: ' + FFileName + LineEnding +
               LineEnding +
               'Invalid Password!';

      on E: Exception do
        Msg := 'Unable to open the file: ' + FFileName + LineEnding +
               'An error occured:' + LineEnding +
               E.Message;
    end;

    if (Msg <> '') and
       Assigned(FOnError)
    then
    begin
      FOnError(Msg);
      FreeAndNil(Result);
      Exit;
    end;

    CreateLockFile;
    FEpiDoc := Result;
    Result.RegisterOnChangeHook(@DocumentChange, true);
  finally
    St.Free;
  end;
end;

function TEpiDocumentFile.SaveFile(const FileName: string): Boolean;
var
  LockFileName: String;
  LF: PLockFile;
  Msg: String;
begin
  FFileName := FileName;
  LockFileName := FFileName + '.lock';

  LF := ReadLockFile(LockFileName);
  if not IsEqualGUID(LF^.GUID, FGuid) then
  begin
    // This file is locked by another program -> most likely because the "stole"
    // our .lock file.
    Msg := 'You are trying to save the file: ' + FFileName + LineEnding +
           'But this file is locked by another program' + LineEnding +
           'The file was locked at: ' + DateTimeToStr(TimeStampToDateTime(LF^.TimeStamp)) + LineEnding
           + LineEnding +
           'Continuing may overwrite data! Are you sure?';

    if OnWarning(wtLockFile, Msg) <> wrYes then exit;
  end;


  FEpiDoc.SaveToFile(FFileName);

  if not IsEqualGUID(LF^.GUID, FGuid) then
    CreateLockFile;
end;

end.

