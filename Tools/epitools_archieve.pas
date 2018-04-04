unit epitools_archieve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zipper;

type

  TEpiToolArchiveProgressEvent = procedure (Sender: TObject;
    FileNo, FileProgress: Integer;
    Const Filename: String; out Cancel: boolean) of object;

  { TEpiToolCompressor }

  TEpiToolCompressor = class
  private
    FFiles: TStrings;
    FOnCompressError: TNotifyEvent;
    FOnEncryptionError: TNotifyEvent;
    FOnProgress: TEpiToolArchiveProgressEvent;
    FPassword: UTF8String;
    FRootDir: UTF8String;
    FZip: TZipper;
  private
    // Progress handling
    FFilecounter: integer;
    FCurrentProgress: Integer;
    FCurrentFilename: string;
    procedure InternalEndFile(Sender: TObject; const Ratio: Double);
    procedure InternalProgress(Sender: TObject; const Pct: Double);
    procedure InternalStartFile(Sender: TObject; const AFileName: String);
  protected
    procedure DoProgress(OverallProgress, FileProgress: Integer; Const Filename: string); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function CompressToFile(const Filename: UTF8String): boolean;
    function CompressToStream(ST: TStream): boolean; virtual;
    property RootDir: UTF8String read FRootDir write FRootDir;
    // Input files to compress (and encrypt)
    property Files: TStrings read FFiles;
    // Password used for encrypting file (if empty just zip)
    property Password: UTF8String read FPassword write FPassword;
    property OnCompressError: TNotifyEvent read FOnCompressError write FOnCompressError;
    property OnEncryptionError: TNotifyEvent read FOnEncryptionError write FOnEncryptionError;
    property OnProgress: TEpiToolArchiveProgressEvent read FOnProgress write FOnProgress;
  end;

  { TEpiToolDeCompressor }

  TEpiToolDeCompressor = class
  private
    FDestinationDir: UTF8String;
    FOnDecompressionError: TNotifyEvent;
    FOnDecryptionError: TNotifyEvent;
    FPassword: UTF8String;
    FDecompressStream: TStream;
    procedure UnzipCloseStream(Sender: TObject; var AStream: TStream);
    procedure UnzipOpenStream(Sender: TObject; var AStream: TStream);
  protected
    function InternalDecompress(ST: TStream): boolean;
    procedure DoDecryptionError();
  public
    constructor Create;
    destructor Destroy; override;
    function DecompressFromFile(Const Filename: UTF8String): boolean;
    function DecompressFromStream(ST: TStream): boolean;
    function DecryptFromFile(Const Filename: UTF8String): boolean;
    function DecryptFromStream(ST: TStream): boolean;
    // Directory where all files are unzipped. Make sure the destination exists!
    property DestinationDir: UTF8String read FDestinationDir write FDestinationDir;
    property Password: UTF8String read FPassword write FPassword;
    property OnDecompressionError: TNotifyEvent read FOnDecompressionError write FOnDecompressionError;
    property OnDecryptionError: TNotifyEvent read FOnDecryptionError write FOnDecryptionError;
  end;

implementation

uses
  LazUTF8Classes, DCPrijndael, DCPsha512, LazFileUtils;

const
  EPITOOL_ARCHIVE_MAGIC = 'EPI1';

type

  { TEpiZipper }

  TEpiZipper = class(TZipper)
  private
    FCanceled: boolean;
  protected
    procedure ZipOneFile(Item: TZipFileEntry); override;
  public
    constructor Create;
    procedure Cancel;
    property Canceled: boolean read FCanceled;
  end;

{ TEpiZipper }

procedure TEpiZipper.ZipOneFile(Item: TZipFileEntry);
begin
  if FCanceled then exit;
  inherited ZipOneFile(Item);
end;

constructor TEpiZipper.Create;
begin
  inherited Create;
  FCanceled := false;
end;

procedure TEpiZipper.Cancel;
begin
  FCanceled := true;
end;

{ TEpiToolCompressor }

procedure TEpiToolCompressor.InternalStartFile(Sender: TObject;
  const AFileName: String);
begin
  Inc(FFilecounter);
  FCurrentFilename := AFileName;
end;

procedure TEpiToolCompressor.InternalProgress(Sender: TObject; const Pct: Double
  );
var
  Val, Current: Int64;
  Overall: Integer;
begin
  //Overall := (100 * FFilecounter) div Files.Count;
  Current := Trunc(Pct);
  DoProgress(FFilecounter, Current, FCurrentFilename);
end;

procedure TEpiToolCompressor.InternalEndFile(Sender: TObject;
  const Ratio: Double);
begin
  //
end;

procedure TEpiToolCompressor.DoProgress(OverallProgress, FileProgress: Integer;
  const Filename: string);
var
  Cancel: boolean;
begin
  Cancel := false;

  if Assigned(OnProgress) then
    OnProgress(Self, OverallProgress, FileProgress, Filename, Cancel);

  if Cancel then
    TEpiZipper(FZip).Cancel;
end;

constructor TEpiToolCompressor.Create;
begin
  FFiles := TStringListUTF8.Create;
  FFilecounter := 0;
end;

destructor TEpiToolCompressor.Destroy;
begin
  FFiles.Clear;
  FFiles.Free;
  inherited Destroy;
end;

function TEpiToolCompressor.CompressToFile(const Filename: UTF8String): boolean;
var
  FS: TFileStreamUTF8;
begin
  FS := TFileStreamUTF8.Create(Filename, fmCreate);
  result := CompressToStream(FS);
  FS.Free;

  if (not result) and
     (FileExistsUTF8(Filename))
  then
    DeleteFileUTF8(Filename);
end;

function TEpiToolCompressor.CompressToStream(ST: TStream): boolean;
var
  InternalStream: TFileStreamUTF8;
  Encrypter: TDCP_rijndael;
  S, FN: String;
  Canceled: Boolean;
begin
  InternalStream := TFileStreamUTF8.Create(GetTempFileNameUTF8('',''), fmCreate);

  FZip := TEpiZipper.Create;
  FZip.InMemSize := 64 * 1024 * 1024;  // This allows for 64MB files to be compressed in memory;
  FZip.OnStartFile := @InternalStartFile;
  FZip.OnEndFile   := @InternalEndFile;
  FZip.OnProgress  := @InternalProgress;

  for S in Files do
    begin
      if FileIsInPath(S, RootDir) then
        FZip.Entries.AddFileEntry(S, ExtractRelativepath(RootDir, S))
      else
        FZip.Entries.AddFileEntry(S);
    end;

  FFilecounter := 0;
  FCurrentProgress := 0;
  FZip.SaveToStream(InternalStream);
  Canceled := TEpiZipper(FZip).Canceled;
  FZip.Free;

  if Canceled then
    begin
      InternalStream.Free;
      Result := false;
      Exit;
    end;

  InternalStream.Position := 0;

  if (Password <> '') then
    begin
      ST.Write(EPITOOL_ARCHIVE_MAGIC[1], Length(EPITOOL_ARCHIVE_MAGIC));

      Encrypter := TDCP_rijndael.Create(nil);
      Encrypter.InitStr(Password, TDCP_sha512);
      Encrypter.EncryptStream(InternalStream, ST, InternalStream.Size);

      Encrypter.Free;
    end
  else
    ST.CopyFrom(InternalStream, InternalStream.Size);

  FN := InternalStream.FileName;
  InternalStream.Free;
  DeleteFileUTF8(Fn);

  Result := true;
end;

{ TEpiToolDeCompressor }

procedure TEpiToolDeCompressor.UnzipOpenStream(Sender: TObject;
  var AStream: TStream);
begin
  AStream := FDecompressStream;
end;

procedure TEpiToolDeCompressor.UnzipCloseStream(Sender: TObject;
  var AStream: TStream);
begin
  // Set AStream to nil, otherwise it free's the stream in the unzipper;
  AStream := nil;
end;

function TEpiToolDeCompressor.InternalDecompress(ST: TStream): boolean;
var
  UnZip: TUnZipper;
begin
  FDecompressStream := ST;

  UnZip := TUnZipper.Create;
  UnZip.OutputPath := DestinationDir;
  UnZip.OnOpenInputStream := @UnzipOpenStream;
  UnZip.OnCloseInputStream := @UnzipCloseStream;
  UnZip.UnZipAllFiles;

  Result := true;
end;

procedure TEpiToolDeCompressor.DoDecryptionError();
begin
  if Assigned(OnDecryptionError) then
    OnDecryptionError(Self);
end;

constructor TEpiToolDeCompressor.Create;
begin

end;

destructor TEpiToolDeCompressor.Destroy;
begin
  inherited Destroy;
end;

function TEpiToolDeCompressor.DecompressFromFile(const Filename: UTF8String
  ): boolean;
var
  FS: TFileStreamUTF8;
begin
  FS := TFileStreamUTF8.Create(Filename, fmOpenRead);
  Result := DecompressFromStream(FS);
  FS.Free;
end;

function TEpiToolDeCompressor.DecompressFromStream(ST: TStream): boolean;
var
  Buffer: Array[0..3] of Char;
  S: String;
  Decrypter: TDCP_rijndael;
  InternalStream: TMemoryStream;
begin
  ST.ReadBuffer(Buffer, SizeOf(Buffer));

  S := String(Buffer);
  if (S = 'PK'#03#04) then
    begin
      // Regular zip file
      if (Password <> '') then
        begin
          DoDecryptionError();
          Exit(false);
        end;

      Result := InternalDecompress(ST);
    end;

  if (S = EPITOOL_ARCHIVE_MAGIC) then
    begin
      // Encrypted EpiData (new format)
      if (Password = '') then
        begin
          DoDecryptionError();
          Exit(false);
        end;

//      DecryptFromStream(ST);

      InternalStream := TMemoryStream.Create;

      Decrypter := TDCP_rijndael.Create(nil);
      Decrypter.InitStr(Password, TDCP_sha512);
      Decrypter.DecryptStream(ST, InternalStream, ST.Size - ST.Position);
      Decrypter.Free;

      Result := InternalDecompress(InternalStream);
      InternalStream.Free;
    end;

  // Should we implement a feature to import old .ZKY files?
end;

function TEpiToolDeCompressor.DecryptFromFile(const Filename: UTF8String
  ): boolean;
begin

end;

function TEpiToolDeCompressor.DecryptFromStream(ST: TStream): boolean;
begin

end;

end.

