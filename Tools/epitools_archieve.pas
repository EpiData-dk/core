unit epitools_archieve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zipper;

type

  TEpiToolArchiveProgressEvent = procedure (Sender: TObject;
    FileNo, FileProgress: Integer;
    Const Filename: String; out Cancel: boolean) of object;

  TEpiToolArchiveError = procedure (Sender: TObject;
    Const Msg: String) of object;

  TEpiToolArchiveTotalFileCount = procedure (Sender: TObject; TotalCount: Integer) of object;

  { TEpiToolCompressor }

  TEpiToolCompressor = class
  private
    FFiles: TStrings;
    FOnCompressError: TEpiToolArchiveError;
    FOnEncryptionError: TEpiToolArchiveError;
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
    procedure DoProgress(FileCount, FileProgress: Integer; Const Filename: string); virtual;
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
    property OnCompressError: TEpiToolArchiveError read FOnCompressError write FOnCompressError;
    property OnEncryptionError: TEpiToolArchiveError read FOnEncryptionError write FOnEncryptionError;
    property OnProgress: TEpiToolArchiveProgressEvent read FOnProgress write FOnProgress;
  end;

  { TEpiToolDeCompressor }

  TEpiToolDeCompressor = class
  private
    FDestinationDir: UTF8String;
    FOnDecompressionError: TEpiToolArchiveError;
    FOnDecryptionError: TEpiToolArchiveError;
    FOnProgress: TEpiToolArchiveProgressEvent;
    FOnTotalFileCount: TEpiToolArchiveTotalFileCount;
    FPassword: UTF8String;
    FDecompressStream: TStream;
    FFileCount: Integer;
    FFileName: UTF8String;
    FUnZip: TUnZipper;
    procedure UnzipCloseStream(Sender: TObject; var AStream: TStream);
    procedure UnzipFileEnd(Sender: TObject; const Ratio: Double);
    procedure UnzipFileStart(Sender: TObject; const AFileName: String);
    procedure UnzipOpenStream(Sender: TObject; var AStream: TStream);
    procedure UnzipProgress(Sender: TObject; const Pct: Double);
  protected
    procedure DoProgress(FileCount, FileProgress: Integer; Const Filename: string); virtual;
    function  InternalDecompress(ST: TStream): boolean; virtual;
    procedure DoDecryptionError(Const Msg: String); virtual;
    procedure DoDecompressionError(Const Msg: String); virtual;
    procedure DoTotalFileCount(TotalCount: Integer); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function DecompressFromFile(Const Filename: UTF8String): boolean;
    function DecompressFromStream(ST: TStream): boolean; virtual;
    function DecryptFromFile(Const InputFilename, OutputFileName: UTF8String): boolean;
    function DecryptFromStream(InputStream, OutputStream: TStream): boolean; virtual;
    // Directory where all files are unzipped. Make sure the destination exists!
    property DestinationDir: UTF8String read FDestinationDir write FDestinationDir;
    property OnDecompressionError: TEpiToolArchiveError read FOnDecompressionError write FOnDecompressionError;
    property OnDecryptionError: TEpiToolArchiveError read FOnDecryptionError write FOnDecryptionError;
    property OnProgress: TEpiToolArchiveProgressEvent read FOnProgress write FOnProgress;
    property OnTotalFileCount: TEpiToolArchiveTotalFileCount read FOnTotalFileCount write FOnTotalFileCount;
    property Password: UTF8String read FPassword write FPassword;
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
  if Canceled then exit;
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

type

  { TEpiUnZipper }

  TEpiUnZipper = class(TUnZipper)
  private
    FCanceled: boolean;
  protected
    procedure UnZipOneFile(Item: TFullZipFileEntry); override;
  public
    constructor Create;
    procedure Cancel;
    property Canceled: boolean read FCanceled;
  end;

{ TEpiUnZipper }

procedure TEpiUnZipper.UnZipOneFile(Item: TFullZipFileEntry);
begin
  if Canceled then exit;
  inherited UnZipOneFile(Item);
end;

constructor TEpiUnZipper.Create;
begin
  inherited Create;
  FCanceled := false;
end;

procedure TEpiUnZipper.Cancel;
begin
  FCanceled := true;
end;

{ TEpiToolCompressor }

procedure TEpiToolCompressor.InternalStartFile(Sender: TObject;
  const AFileName: String);
begin
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
  Inc(FFilecounter);
end;

procedure TEpiToolCompressor.DoProgress(FileCount, FileProgress: Integer;
  const Filename: string);
var
  Cancel: boolean;
begin
  Cancel := false;

  if Assigned(OnProgress) then
    OnProgress(Self, FileCount, FileProgress, Filename, Cancel);

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
  DCPHash: TDCP_sha512;
  Digest: pointer;
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
      // Write the Epidata Archive Magic no first in the file
      ST.Write(EPITOOL_ARCHIVE_MAGIC[1], Length(EPITOOL_ARCHIVE_MAGIC));

      // Write a hashed version of the password
      GetMem(Digest, TDCP_sha512.GetHashSize div 8);
      DCPHash := TDCP_sha512.Create(nil);
      DCPHash.Init;
      DCPHash.UpdateStr(Password);
      DCPHash.Final(Digest^);
      DCPHash.Free;
      ST.Write(Digest^, TDCP_sha512.GetHashSize div 8);

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

procedure TEpiToolDeCompressor.UnzipProgress(Sender: TObject; const Pct: Double
  );
begin
  DoProgress(FFileCount, Trunc(Pct), FFileName);
end;

procedure TEpiToolDeCompressor.DoProgress(FileCount, FileProgress: Integer;
  const Filename: string);
var
  Cancel: Boolean;
begin
  Cancel := false;

  if Assigned(OnProgress) then
    OnProgress(Self, FileCount, FileProgress, Filename, Cancel);

  if Cancel then
    TEpiUnZipper(FUnZip).Cancel;
end;

procedure TEpiToolDeCompressor.UnzipCloseStream(Sender: TObject;
  var AStream: TStream);
begin
  // Set AStream to nil, otherwise it free's the stream in the unzipper;
  AStream := nil;
end;

procedure TEpiToolDeCompressor.UnzipFileEnd(Sender: TObject; const Ratio: Double
  );
begin
  Inc(FFileCount);
end;

procedure TEpiToolDeCompressor.UnzipFileStart(Sender: TObject;
  const AFileName: String);
begin
  FFileName := AFileName;
end;

function TEpiToolDeCompressor.InternalDecompress(ST: TStream): boolean;
var
  UnZip: TUnZipper;
begin
  FDecompressStream := ST;

  UnZip := TEpiUnZipper.Create;
  UnZip.OutputPath := DestinationDir;
  UnZip.OnOpenInputStream := @UnzipOpenStream;
  UnZip.OnCloseInputStream := @UnzipCloseStream;
  UnZip.OnProgress := @UnzipProgress;
  UnZip.OnStartFile := @UnzipFileStart;
  UnZip.OnEndFile := @UnzipFileEnd;

  try
    UnZip.Examine;

    DoTotalFileCount(UnZip.Entries.Count);

    UnZip.UnZipAllFiles;
  except
  end;

  Result := true;
end;

procedure TEpiToolDeCompressor.DoDecryptionError(const Msg: String);
begin
  if Assigned(OnDecryptionError) then
    OnDecryptionError(Self, Msg);
end;

procedure TEpiToolDeCompressor.DoDecompressionError(const Msg: String);
begin
  if Assigned(OnDecompressionError) then
    OnDecompressionError(Self, Msg);
end;

procedure TEpiToolDeCompressor.DoTotalFileCount(TotalCount: Integer);
begin
  if Assigned(OnTotalFileCount) then
    OnTotalFileCount(Self, TotalCount);
end;

constructor TEpiToolDeCompressor.Create;
begin
  FFileCount := 0;
  FFileName := '';
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
  S, FN: String;
  Decrypter: TDCP_rijndael;
  InternalStream: TMemoryStream;
  TmpStream: TFileStreamUTF8;
begin
  result := false;

  ST.ReadBuffer(Buffer, SizeOf(Buffer));

  S := String(Buffer);
  if (S = 'PK'#03#04) then
    begin
      // Regular zip file
      if (Password <> '') then
        begin
          DoDecryptionError('This is a regular zip-file. No password is required.');
          Exit(false);
        end;

      Result := InternalDecompress(ST);
    end;

  if (S = EPITOOL_ARCHIVE_MAGIC) then
    begin
      // Encrypted EpiData (new format)
      if (Password = '') then
        begin
          DoDecryptionError('This is an encrypted EpiData file. Please provide a password!');
          Exit(false);
        end;

      ST.Position := 0;
      TmpStream := TFileStreamUTF8.Create(GetTempFileNameUTF8('', ''), fmCreate);

      Result := DecryptFromStream(ST, TmpStream) and
                InternalDecompress(TmpStream);

      TmpStream.Free;
    end;

  // Should we implement a feature to import old .ZKY files?
end;

function TEpiToolDeCompressor.DecryptFromFile(const InputFilename,
  OutputFileName: UTF8String): boolean;
var
  InputStream, OutputStream: TFileStreamUTF8;
begin
  InputStream := TFileStreamUTF8.Create(InputFilename, fmOpenRead);
  OutputStream := TFileStreamUTF8.Create(OutputFileName, fmCreate);
  Result := DecryptFromStream(InputStream, OutputStream);
  InputStream.Free;
  OutputStream.Free;
end;

function TEpiToolDeCompressor.DecryptFromStream(InputStream,
  OutputStream: TStream): boolean;
var
  Buffer: array[0..3] of char;
  Decrypter: TDCP_rijndael;
  DCPHash: TDCP_sha512;
  Digest, ReadDigest: Pointer;
  HashByteSize: Integer;
  S: String;
begin
  result := false;

  InputStream.ReadBuffer(Buffer, SizeOf(Buffer));
  S := String(buffer);

  if (S <> EPITOOL_ARCHIVE_MAGIC) then
    begin
      DoDecryptionError('File is not a valid EpiData encrypted archive!');
      Exit;
    end;

  HashByteSize := TDCP_sha512.GetHashSize div 8;

  // Write a hashed version of the password
  GetMem(Digest, HashByteSize);
  DCPHash := TDCP_sha512.Create(nil);
  DCPHash.Init;
  DCPHash.UpdateStr(Password);
  DCPHash.Final(Digest^);
  DCPHash.Free;

  GetMem(ReadDigest, HashByteSize);
  InputStream.Read(ReadDigest^, HashByteSize);

  if (not CompareMem(ReadDigest, Digest, HashByteSize)) then
    begin
      DoDecryptionError('Incorrect password!');
      Exit;
    end;

  Decrypter := TDCP_rijndael.Create(nil);
  Decrypter.InitStr(Password, TDCP_sha512);
  Decrypter.DecryptStream(InputStream, OutputStream, InputStream.Size - InputStream.Position);
  Decrypter.Free;

  result := true;
end;

end.

