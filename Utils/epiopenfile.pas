unit epiopenfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiadmin, epidocument, epicustombase,
  epieventtypes;

type

  TOpenEpiWarningType =
    (wtLockFile,       // Trying to open a file with a .lock file present.
     wtDatePattern,    // A date pattern has been detected in the file, could be an auto backup
     wtTimeBackup,     // A file with .bak found, which indicated that the program could have shutdown unexpectedly.
     wtTimeBackup2nd,
     wtSysReadOnly     // The file is readonly on OS level.
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
        UserName: string[64];
        ComputerName: string[64];
      end;
      PLockFile = ^TLockFile;

  private
    // Private event holders
    FOnError: TOpenEpiErrorEvent;
    FOnPassword: TRequestPasswordEvent;
    FOnWarning: TOpenEpiWarningEvent;
    FOnProgress: TEpiProgressEvent;
  private
    // Aux. functions
    function GetHostNameWrapper: string;
    function GetUserNameWrapper: string;
  private
    // Internal housekeeping of current open EpiDocument.
    FReadOnly: boolean;
    FFileName: string;
    FTimeStamp: TTimeStamp;
    FEpiDoc: TEpiDocument;
    procedure DocumentChange(Const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    function GetIsSaved: boolean;
    function ReadLockFile(Const Fn: string): PLockFile;
    procedure WriteLockFile(Const Fn: string; LF: PLockFile);
    procedure CreateLockFile;
    procedure DeleteLockFile;
    procedure DeleteBackupFile;
  protected
    function GetFileName: string; virtual;
    function LockFileExists(Const FileName: string;
      out Msg: string): boolean;
    function DatePatternExists(Const FileName: string;
      out AltFileName: string; out Msg: string): boolean;
    function BackupFileExists(Const FileName: string;
      out Msg: string): boolean;
    function IsOSReadOnly(Const FileName: string;
      out Msg: string): boolean;
    procedure DoSaveFile(Const AFileName: string);
    procedure DoOpenFile(Const AFileName: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function CreateNewDocument(Const Lang: string): TEpiDocument;
    function CreateClonedDocument(Const SourceDoc: TEpiDocument): TEpiDocument;
    function OpenFile(Const AFileName: string;
      Const AReadOnly: boolean = false): boolean;
    function SaveFile(Const AFileName: string): Boolean;
    function SaveBackupFile: boolean;
  public
    // Event properties
    property OnPassword: TRequestPasswordEvent read FOnPassword write FOnPassword;
    property OnWarning: TOpenEpiWarningEvent read FOnWarning write FOnWarning;
    property OnError: TOpenEpiErrorEvent read FOnError write FOnError;
    property OnProgress: TEpiProgressEvent read FOnProgress write FOnProgress;
  public
    // Other properties
    property FileName: string read GetFileName;
    property Document: TEpiDocument read FEpiDoc;
    property ReadOnly: Boolean read FReadOnly;
    property IsSaved: boolean read GetIsSaved;
  end;

implementation

uses
  {$IFDEF Windows}
  windows,
  {$ENDIF}
  {$IFDEF unix}
  Unix,
  {$ENDIF}
  epimiscutils, LazFileUtils, LazUTF8, RegExpr;

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
  if Assigned(FEpiDoc)
  then
  begin
    FEpiDoc.UnRegisterOnChangeHook(@DocumentChange);
    FreeAndNil(FEpiDoc);

    if not ReadOnly then
    begin
      DeleteLockFile;
      DeleteBackupFile;
    end;
  end;

  inherited Destroy;
end;

function TEpiDocumentFile.CreateNewDocument(const Lang: string): TEpiDocument;
begin
  if not Assigned(FEpiDoc) then
  begin
    FEpiDoc := TEpiDocument.Create(Lang);
    Result := FEpiDoc;
  end;
end;

function TEpiDocumentFile.CreateClonedDocument(const SourceDoc: TEpiDocument
  ): TEpiDocument;
begin
  if Assigned(Document) then exit;
  if not Assigned(SourceDoc) then exit;

  FEpiDoc := TEpiDocument(SourceDoc.Clone);
  Result := FEpiDoc;
end;

function TEpiDocumentFile.GetHostNameWrapper: string;
{$IFDEF WINDOWS}
var
  Buffer: Array[0..127] of WideChar;
  Sz: DWORD;
{$ENDIF}
begin
  Result := '';

  {$IFDEF Windows}
  Sz := SizeOf(Buffer);
  GetComputerNameW(Buffer, Sz);
  Result := WideCharToString(Buffer);
  {$ENDIF}
  {$IFDEF unix}
  Result := GetHostName;
  {$ENDIF}
end;

function TEpiDocumentFile.GetUserNameWrapper: string;
var
  Buffer: array[0..127] of WideChar;
  Sz: DWORD;
begin
  {$IFDEF MSWINDOWS}
  Sz := SizeOf(Buffer);
  GetUserNameW(Buffer, Sz);
  Result := WideCharToString(Buffer);
  {$ENDIF}
  {$IFDEF UNIX}
  Result := GetEnvironmentVariableUTF8('USER');
  {$ENDIF}
  if Result = '' then
    Result := 'Unknown';
end;

procedure TEpiDocumentFile.DocumentChange(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  // Housekeeping to know if document is being destroyed.

  if not (EventGroup = eegCustomBase) then exit;
  if TEpiCustomChangeEventType(EventType) <> ecceDestroy then exit;
  if (Initiator <> FEpiDoc) then exit;

  DeleteLockFile;
end;

function TEpiDocumentFile.GetIsSaved: boolean;
begin
  result := FFileName <> '';
end;

function TEpiDocumentFile.GetFileName: string;
begin
  if FFilename = '' then
    Result := '(Not Saved)'
  else
    Result := FFilename;
end;

function TEpiDocumentFile.ReadLockFile(const Fn: string): PLockFile;
var
  St: TFileStream;
begin
  Result := nil;
  if not FileExistsUTF8(Fn) then exit;

  Result := New(PLockFile);
  St := TFileStream.Create(UTF8ToSys(Fn), fmOpenRead);
  St.Read(Result^, SizeOf(TLockFile));
  St.Free;
end;

procedure TEpiDocumentFile.WriteLockFile(const Fn: string; LF: PLockFile);
var
  St: TFileStream;
begin
  if FileExistsUTF8(Fn) and
     FileIsReadOnlyUTF8(Fn) then exit;

  St := TFileStream.Create(UTF8ToSys(Fn), fmCreate);
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
        Msg := 'The file: ' + FileName + LineEnding +
               'is locked by another program!' + LineEnding +
               'The file was locked at: ' + DateTimeToStr(TimeStampToDateTime(LF^.TimeStamp)) + LineEnding +
               'By user: ' + LF^.UserName + LineEnding +
               'On computer: ' + LF^.ComputerName + LineEnding +
               LineEnding +
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
  S := ChangeFileExt(AltFileName, BoolToStr(ExtractFileExt(AltFileName) = '.epz', '.epx', '.epz'));
  if B and
     (FileExistsUTF8(AltFileName) or FileExistsUTF8(S))
  then
  begin
    if FileExistsUTF8(S) then
      AltFileName := S;

    Result := true;

    Msg :=   'This file seem to be an automated backup file (on closing the program):' + LineEnding + LineEnding +
             'File: ' + SysToUTF8(ExtractFileName(UTF8ToSys(FileName)))          +
               ' (' + FormatDateTime('YYYY/MM/DD HH:NN:SS', FileDateToDateTime(FileAgeUTF8(FileName))) + ')' + LineEnding +
             'Original: ' + SysToUTF8(ExtractFileName(UTF8ToSys(S))) +
               ' (' + FormatDateTime('YYYY/MM/DD HH:NN:SS', FileDateToDateTime(FileAgeUTF8(AltFileName))) + ')' + LineEnding +
             LineEnding +
             'Load the original instead?';
  end;
  R.Free;
end;

function TEpiDocumentFile.BackupFileExists(const FileName: string; out
  Msg: string): boolean;
begin
  Result := false;
  if FileExistsUTF8(FileName + '.bak') then
  begin
    Msg :=   'A timed backup file exists. (loading of this overwrites previous project file)' + LineEnding + LineEnding +
             'File: ' + SysToUTF8(ExtractFileName(UTF8ToSys(FileName)))          +
               ' (' + FormatDateTime('YYYY/MM/DD HH:NN:SS', FileDateToDateTime(FileAgeUTF8(FileName))) + ')' + LineEnding +
             'Recovery: ' + #9 + SysToUTF8(ExtractFileName(UTF8ToSys(FileName + '.bak'))) +
               ' (' + FormatDateTime('YYYY/MM/DD HH:NN:SS', FileDateToDateTime(FileAgeUTF8(FileName + '.bak'))) + ')' + LineEnding +
             LineEnding +
             'Load the backup instead?';
    result := true;
  end;
end;

function TEpiDocumentFile.IsOSReadOnly(const FileName: string; out Msg: string
  ): boolean;
begin
  Result := false;
  if FileIsReadOnlyUTF8(FileName) then
  begin
    Msg := 'The project is marked "read only" by the operating system.' + LineEnding +
           'If you choose to open this file, you cannot save it again with the same file name.' + LineEnding +
           LineEnding +
           'File: ' + SysToUTF8(ExtractFileName(UTF8ToSys(FileName))) + LineEnding +
           LineEnding +
           'Do you wish to continue loading the file?';
    Result := true;
  end;
end;

procedure TEpiDocumentFile.CreateLockFile;
var
  LF: PLockFile;
  LockFileName: String;
begin
  if not IsSaved then exit;

  LockFileName := FileName + '.lock';

  LF := New(PLockFile);
  with LF^ do
  begin
    GUID         := FGuid;
    TimeStamp    := DateTimeToTimeStamp(Now);
    ComputerName := GetHostNameWrapper;
    UserName     := GetUserNameWrapper;
  end;
  WriteLockFile(LockFileName, LF);
  Dispose(LF);
end;

procedure TEpiDocumentFile.DeleteLockFile;
var
  LockFileName: String;
begin
  if not IsSaved then exit;

  LockFileName := FileName + '.lock';

  if FileExistsUTF8(LockFileName) then
    DeleteFileUTF8(LockFileName);
end;

procedure TEpiDocumentFile.DeleteBackupFile;
var
  BackupFileName: String;
begin
  if not IsSaved then exit;

  BackupFileName := FileName + '.bak';

  if FileExistsUTF8(BackupFileName) then
    DeleteFileUTF8(BackupFileName);
end;

procedure TEpiDocumentFile.DoSaveFile(const AFileName: string);
var
  Ms: TMemoryStream;
  Fs: TFileStream;
begin
  Ms := TMemoryStream.Create;

  FEpiDoc.OnProgress := OnProgress;
  FEpiDoc.SaveToStream(Ms);
  Ms.Position := 0;

  if UTF8Pos('.epz', UTF8LowerCase(AFileName)) > 0 then
    StreamToZipFile(Ms, UTF8ToSys(AFileName))
  else
    begin
      Fs := TFileStream.Create(UTF8ToSys(AFileName), fmCreate);
      Fs.CopyFrom(Ms, Ms.Size);
      Fs.Free;
    end;
end;

procedure TEpiDocumentFile.DoOpenFile(const AFileName: string);
var
  St: TMemoryStream;
  CurrentDir: String;
begin
  FEpiDoc := TEpiDocument.Create('en');
  St := TMemoryStream.Create;

  CurrentDir := GetCurrentDirUTF8;
  SetCurrentDirUTF8(ExtractFileDir(AFileName));

  try
    if ExtractFileExt(UTF8ToSys(AFileName)) = '.epz' then
      ZipFileToStream(St, AFileName)
    else
      St.LoadFromFile(UTF8ToSys(AFileName));
    St.Position := 0;

    FEpiDoc.OnPassword := OnPassword;
    FEpiDoc.OnProgress := OnProgress;
    FEpiDoc.LoadFromStream(St);
  finally
    St.Free;
    SetCurrentDirUTF8(CurrentDir);
  end;
end;

function TEpiDocumentFile.OpenFile(const AFileName: string;
  const AReadOnly: boolean): boolean;
var
  St: TMemoryStream;
  Msg: String;
  Res: TOpenEpiWarningEvent;
  AltFn: string;
  Fn: String;
  LoadBackupFile: Boolean;
begin
  Result         := false;
  FFileName      := AFileName;
  FReadOnly      := AReadOnly;
  LoadBackupFile := false;

  if not ReadOnly then
  begin
    if IsOSReadOnly(FileName, Msg) then
      case OnWarning(wtSysReadOnly, Msg) of
        wrYes:
          FReadOnly := true;
        wrNo:
          Exit;
      end;


    if LockFileExists(FileName, Msg) then
      case OnWarning(wtLockFile, Msg) of
        wrYes:
          ;
        wrNo,
        wrCancel:
          Exit;
      end;

    if DatePatternExists(FileName, AltFn, Msg) then
      case OnWarning(wtDatePattern, Msg) of
        wrYes:
          begin
            // User wanted to open the alternate file.
            // Call OpenFile again to do the same checks on this file!
            Result := OpenFile(AltFn);
            Exit;
          end;
        wrNo:
          ;
        wrCancel:
          Exit;
      end;

    if BackupFileExists(FileName, Msg) then
      case OnWarning(wtTimeBackup, Msg) of
        wrYes:    LoadBackupFile := true;
        wrNo:     begin
                    Msg := 'Loading ' + SysToUTF8(ExtractFileName(UTF8ToSys(FileName))) + ' will delete recovery file.' + LineEnding +
                           'Continue?';
                    case OnWarning(wtTimeBackup2nd, Msg) of
                      wrNo:  Exit;
                    end;
                  end;
        wrCancel: Exit;
      end;
  end;  // ReadOnly

  if LoadBackupFile then
    Fn := FileName + '.bak'
  else
    Fn := FileName;

  try
    try
      Msg := '';
      DoOpenFile(Fn);
    except
      on E: TEpiCoreException do
        Msg := 'Unable to open the file: ' + Fn + LineEnding + E.Message;

      on E: EFOpenError do
        Msg := 'Unable to open the file: ' + Fn + LineEnding +
               'File is corrupt or does not exist.' + LineEnding +
               E.Message;

      on EEpiBadPassword do
        Msg := 'Unable to open the file: ' + Fn + LineEnding +
               LineEnding +
               'Invalid Password!';

      on E: EEpiBadVersion do
        Msg := E.Message;

      on E: Exception do
        Msg := 'Unable to open the file: ' + Fn + LineEnding +
               'An error occured:' + LineEnding +
               E.Message;
    end;

    if (Msg <> '') then
    begin
      if Assigned(FOnError) then
        FOnError(Msg);
      FreeAndNil(FEpiDoc);
      Exit;
    end;

    if not ReadOnly then
      CreateLockFile;
    FEpiDoc.RegisterOnChangeHook(@DocumentChange, true);
    Result := true;
  finally
//    St.Free;
  end;
end;

function TEpiDocumentFile.SaveFile(const AFileName: string): Boolean;
var
  LockFileName: String;
  LF: PLockFile;
  Msg: String;
  FirstSave: Boolean;
begin
  result := false;
  if not Assigned(Document) then exit;

  FirstSave := false;

    // Document haven't been saved before
  if (not IsSaved) or
    // Document is being saved under a new name.
     (FileName <> AFileName)
  then
    FirstSave := true;

  // if the file was loaded as readonly
  // (by OS or explicit), proceed only if
  // saving under a new name (FirstSave = true)
  if ReadOnly and
     (not FirstSave)
  then
    Exit;

  if (IsSaved) and
     (FileName <> AFileName)
  then
    DeleteLockFile;

  FFileName := AFileName;
  LockFileName := FileName + '.lock';

  if not FileExistsUTF8(LockFileName) then
    if FirstSave then
      CreateLockFile
    else
      begin
        // No .lock file exists, but this document have been saved before
        // (eg. because it was loaded).
        // This is either because the user manually deleted the .lock file
        // or because another program delete it (eg. by stealing the .lock file).
        //
        //    A (lock)     B (steal lock)  B (close+unlock)  A (close... no .lock file)
        //    |            |               |                 |
        //  --\------------\---------------\-----------------\-------------->  (time)
        Msg := 'You are trying to save the file: ' + FileName + LineEnding +
               'But the lock file is missing' + LineEnding +
               'The project file may have been edited by another EntryClient/Manager program!' + LineEnding +
               LineEnding +
               'Continuing may overwrite data! Are you sure?';
        if OnWarning(wtLockFile, Msg) <> wrYes then exit;
      end;

  LF := ReadLockFile(LockFileName);
  if Assigned(LF) and
     (not IsEqualGUID(LF^.GUID, FGuid))
  then
    begin
      // This file is locked by another program -> most likely because the "stole"
      // our .lock file.
      Msg := 'You are trying to save the file: ' + FileName + LineEnding +
             'But this file is locked by another program' + LineEnding +
             'The file was locked at: ' + DateTimeToStr(TimeStampToDateTime(LF^.TimeStamp)) + LineEnding +
             'By user: ' + LF^.UserName + LineEnding +
             'On computer: ' + LF^.ComputerName + LineEnding +
             LineEnding +
             'Continuing may overwrite data! Are you sure?';

      if OnWarning(wtLockFile, Msg) <> wrYes then exit;
    end;

  try
    DoSaveFile(FileName);

    if (Not Assigned(LF)) or
       (not IsEqualGUID(LF^.GUID, FGuid))
    then
      CreateLockFile;

    FReadOnly := false;
    Result := true;
  finally
    Dispose(LF);
  end;
end;

function TEpiDocumentFile.SaveBackupFile: boolean;
var
  BackupFileName: String;
begin
  if not IsSaved then exit;

  BackupFileName := FileName + '.bak';
  DoSaveFile(BackupFileName);
end;

end.

