unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TValidatorSetting = record
    ErrorsAreFatal: boolean;
    WarningsAreFatal: boolean;
    IncludeSubDirs: boolean;
    LogFile: string;
    NoOutput: boolean;
    ExportFilter: TStrings;
  end;

var
  VSettings: TValidatorSetting;

implementation

end.
