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

  TEpiTextImportSetting = class(TEpiImportSettings)
  private
    FGuess: boolean;
    FDelimiter: UTF8String;
    FFirstLineIsHeader: Boolean;
    FQuoteCharacter: UTF8String;
  public
    constructor Create; override;
    // If GUESS is true, all other options below are not used during import and everything is guessed.
    property Guess: boolean read FGuess write FGuess;
    property Delimiter: UTF8String read FDelimiter write FDelimiter;
    property QuoteCharacter: UTF8String read FQuoteCharacter write FQuoteCharacter;
    property FirstLineIsHeader: Boolean read FFirstLineIsHeader write FFirstLineIsHeader;
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
  FFirstLineIsHeader := true;
  FGuess := true;
  FQuoteCharacter := '"';
  FDelimiter := ',';
end;

end.

