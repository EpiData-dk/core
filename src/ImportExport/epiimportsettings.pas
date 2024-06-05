unit epiimportsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles;

type

  { TEpiImportSettings }

  TEpiImportSettings = class
  private
    FImportData: boolean;
    FSteam: TStream;
    FOutputDatafile: TEpiDataFile;
  public
    constructor Create; virtual;
    property ImportData: boolean read FImportData write FImportData;
    property ImportSteam: TStream read FSteam write FSteam;
    property OutputDatafile: TEpiDataFile read FOutputDatafile write FOutputDatafile;
  end;


  { TEpiTextImportSetting }
  TEpiTextImportFirstLineIsHeader = (eflGuess, eflTrue, eflFalse);

  TEpiTextImportSetting = class(TEpiImportSettings)
  private
    FDelimiter: Char;
    FFirstLineIsHeader: TEpiTextImportFirstLineIsHeader;
    FQuoteCharacter: Char;
    FStringFieldsCanBeUppercase: boolean;
  public
    constructor Create; override;
    // If delimiter is set to #0 then it will be guessed (#0 is default)
    property Delimiter: Char read FDelimiter write FDelimiter;

    // If quote is set to #0  no quotes are expected to seperate strings. Hence if a string
    // contains the delimiter that is just too bad. (" is default)
    property QuoteCharacter: Char read FQuoteCharacter write FQuoteCharacter;

    // Can be se to True, False which forces importing to treat first line respectively as header or not,
    // or Guess (guess) if letting the algorithm try to figure it out
    property FirstLineIsHeader: TEpiTextImportFirstLineIsHeader read FFirstLineIsHeader write FFirstLineIsHeader;

    // If set to true, detecting a field will consider uppercase as a possible field type. This is the default.
    // If set to false, a string field can never be uppercase.
    property StringFieldsCanBeUppercase: boolean read FStringFieldsCanBeUppercase write FStringFieldsCanBeUppercase;
  end;



implementation

{ TEpiImportSettings }

constructor TEpiImportSettings.Create;
begin
  FImportData := true;
end;

{ TEpiTextImportSetting }

constructor TEpiTextImportSetting.Create;
begin
  inherited Create;
  FFirstLineIsHeader := eflGuess;
  FQuoteCharacter := '"';
  FDelimiter := #0;
  FStringFieldsCanBeUppercase := true;
end;

end.

