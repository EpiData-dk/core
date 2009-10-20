unit validationunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, UEpiDataFile;

type

  { TDatafileValidator }

  TDatafileValidator = class
  private
    procedure ValidateOriginalDatafile(Df: TEpiDataFile);
  public
    constructor Create;
    destructor Destroy; override;
    Procedure DirectoryHandler(FileIterator: TFileIterator);
    Procedure FileHandler(FileIterator: TFileIterator);
  end;

implementation

uses
  UImportExport;

{ TDatafileValidator }

procedure TDatafileValidator.ValidateOriginalDatafile(Df: TEpiDataFile);
var
  Fn: String;
begin
  // Validating the original file is done using a <df-filename>.import file, organised in this manner:

  {
   Line          1: Number [n] of fields (includes question lines in EpiData files).
   Line          2: Number [r] of records (includes records marked for deletion).
   Line      3 -> : n lines containing in comma seperated format:
                   a: Field type (as a number according to TFieldType;
                   b: Field length
                   c: Field decimals
                   d: Does this field have a value label (0 = no, 1 = yes)
   Line n+3 -> EOF: any number of lines in comma seperated format:
                   a: Field no. (index into the list above - 0 indexed)
                   b: Record no. (index into the number of records - 1 indexed)
                   c: Data that must be present in this field. For strings encapsulate the data in quotation marks.
  }
  Fn := Df.FileName + ExtensionSeparator + 'import';


end;

constructor TDatafileValidator.Create;
begin
  //
end;

destructor TDatafileValidator.Destroy;
begin
  inherited Destroy;
end;

procedure TDatafileValidator.DirectoryHandler(FileIterator: TFileIterator);
begin
  // So far do nothing (it recurses automatically into the directory).
end;

procedure TDatafileValidator.FileHandler(FileIterator: TFileIterator);
var
  Ext: String;
  Importer: TEpiImportExport;
  Fn: String;
  Df: TEpiDataFile;
begin
  Ext := ExtractFileExt(Utf8ToAnsi(FileIterator.FileName));
  Fn := FileIterator.FileName;

  try
    Df := TEpiDataFile.Create;
    Importer := TEpiImportExport.Create;

    Case Ext of
      'recxml', 'rec':
        begin
          DF.Open(Fn);
        end;
      'dta':
        Importer.ImportStata(Fn, Df);
      'ods':
        Importer.ImportSpreadSheet(Fn, Df);
      'csv', 'txt':
        Importer.ImportTXT(Fn, Df, nil);
      'dbf':
        Importer.ImportDBase(Fn, Df);
    end;

  ValidateOriginalDatafile(Df);

  finally
    if Assigned(Importer) then FreeAndNil(Importer);
  end;
end;

end.

