{%MainUnit epidatafiles.pas}

type

  TEpiFieldsChangeEventType = (
    efceName, efceSetDecimal, efceSetLeft, efceSetLength,
    efceSetTop, efceSetSize, efceData
  );

  { TEpiFields }

  TEpiFields = class(TEpiCustomList)
  private
    function GetDataFile: TEpiDataFile;
    function GetField(index: integer): TEpiField;
  public
    constructor Create(AOwner: TEpiCustomBase); override;
    destructor  Destroy; override;
    function    XMLName: string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;
    function    NewField(FieldType: TEpiFieldType): TEpiField;
    function    FieldExists(AField: TEpiField): boolean;
    function    FieldExistsByName(Const AName: string): boolean;
    property    DataFile: TEpiDataFile read GetDataFile;
    property    Field[index: integer]: TEpiField read GetField; default;
  end;

  { TEpiField }

  TEpiField = class(TEpiCustomControlItem)
  private
    FCapacity: Integer;
    FDataFile: TEpiDataFile;
    FDecimals: Cardinal;
    FLength: Cardinal;
    FFieldType: TEpiFieldType;
    FName: string;
    FRanges: TEpiRanges;
    FSize: Integer;
    FValueLabelSet: TEpiValueLabelSet;
    FQuestion:  TEpiHeading;
    LocalFmtSettings: TFormatSettings;
    function    GetSettings: TEpiXMLSettings;
    procedure   SetDecimals(const AValue: Cardinal);
    procedure   SetLength(const AValue: Cardinal);
    procedure   LocalEventHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure   SetName(const AValue: string);
    procedure   SetValueLabelSet(const AValue: TEpiValueLabelSet);
    function    GetIsMissingValue(const index: Integer): boolean;
    procedure   SetIsMissingValue(const index: Integer; const AValue: boolean);
  protected
    procedure   CheckIndex(Const index: integer); virtual;
    function    GetAsBoolean(const index: Integer): EpiBool; virtual; abstract;
    function    GetAsDate(const index: Integer): EpiDate; virtual; abstract;
    function    GetAsDateTime(const index: Integer): EpiDateTime; virtual; abstract;
    function    GetAsFloat(const index: Integer): EpiFloat; virtual; abstract;
    function    GetAsInteger(const index: Integer): EpiInteger; virtual; abstract;
    function    GetAsString(const index: Integer): EpiString; virtual; abstract;
    function    GetAsTime(const index: Integer): EpiTime; virtual; abstract;
    function    GetAsValue(const index: Integer): EpiVariant; virtual; abstract;
    function    GetCapacity: Integer; virtual; abstract;
    function    GetIsMissing(const index: Integer): boolean; virtual; abstract;
    function    GetSize: Integer; virtual;
    procedure   SetAsBoolean(const index: Integer; const AValue: EpiBool); virtual; abstract;
    procedure   SetAsDate(const index: Integer; const AValue: EpiDate); virtual; abstract;
    procedure   SetAsDateTime(const index: Integer; const AValue: EpiDateTime); virtual; abstract;
    procedure   SetAsFloat(const index: Integer; const AValue: EpiFloat); virtual; abstract;
    procedure   SetAsInteger(const index: Integer; const AValue: EpiInteger); virtual; abstract;
    procedure   SetAsString(const index: Integer; const AValue: EpiString); virtual; abstract;
    procedure   SetAsTime(const index: Integer; const AValue: EpiTime); virtual; abstract;
    procedure   SetAsValue(const index: Integer; const AValue: EpiVariant); virtual; abstract;
    procedure   SetCapacity(AValue: Integer); virtual; abstract;
    procedure   SetIsMissing(const index: Integer; const AValue: boolean); virtual; abstract;
    procedure   SetSize(const AValue: Integer); virtual;

    property    Capacity: Integer read GetCapacity write SetCapacity;
    property    Settings: TEpiXMLSettings read GetSettings;

  { Data access properties }
  public
    property    AsBoolean[const index: Integer]: EpiBool read GetAsBoolean write SetAsBoolean;
    property    AsInteger[const index: Integer]: EpiInteger read GetAsInteger write SetAsInteger; default;
    property    AsFloat[const index: Integer]: EpiFloat read GetAsFloat write SetAsFloat;
    property    AsDate[const index: Integer]: EpiDate read GetAsDate write SetAsDate;
    property    AsTime[const index: Integer]: EpiTime read GetAsTime write SetAsTime;
    property    AsDateTime[const index: Integer]: EpiDateTime read GetAsDateTime write SetAsDateTime;
    property    AsString[const index: Integer]: EpiString read GetAsString write SetAsString;
    property    AsValue[const index: Integer]: EpiVariant read GetAsValue write SetAsValue;
    property    IsMissing[const index: Integer]: boolean read GetIsMissing write SetIsMissing;
    property    IsMissingValue[const index: Integer]: boolean read GetIsMissingValue write SetIsMissingValue;

  { Field Properties }
  public
    property    Name: string read FName write SetName;
    property    FieldType: TEpiFieldType read FFieldType;
    property    Length:    Cardinal read FLength write SetLength;
    property    Decimals:  Cardinal read FDecimals write SetDecimals;
    property    Size:      Integer read GetSize write SetSize;
    property    Question:  TEpiHeading read FQuestion;
    property    ValueLabelSet: TEpiValueLabelSet read FValueLabelSet write SetValueLabelSet;
    property    Ranges:    TEpiRanges read FRanges write FRanges;

  { Helper methods }
  public
    procedure   Exchange(i,j: integer); virtual; abstract;
    function    Compare(i,j: integer): integer; virtual; abstract;
    procedure   Assign(const AEpiCustomBase: TEpiCustomBase); override;

  { Class inheritance, load/save, etc. }
  protected
    // Never ever create a field directly!
    constructor Create(AOwner: TEpiCustomBase; AFieldType: TEpiFieldType);
    class function IdString: string; override;
  public
    class function CreateField(AOwner: TEpiCustomBase;
      AFieldType: TEpiFieldType): TEpiField;
    destructor  Destroy; override;
    function    XMLName: string; override;
    function    SaveToXml(Content: String; Lvl: integer): string; override;
    procedure   LoadFromXml(Root: TDOMNode); override;
    property    DataFile: TEpiDataFile read FDataFile;
  end;

  { TEpiIntField }
  // Handles following field types:
  // ftInteger, ftAutoInc
  TEpiIntField = class(TEpiField)
  private
    FData: array of EpiInteger;
  protected
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsDateTime(const index: Integer): EpiDateTime; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsTime(const index: Integer): EpiTime; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsDateTime(const index: Integer; const AValue: EpiDateTime); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsTime(const index: Integer; const AValue: EpiTime); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetCapacity(AValue: Integer); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
  public
    class function CheckMissing(AValue: EpiInteger): boolean;
    class function DefaultMissing: EpiInteger;
    function Compare(i, j: integer): integer; override;
    procedure Exchange(i, j: integer); override;
  end;

  { TEpiFloatField }
  // Handles following field types:
  // ftFloat
  TEpiFloatField = class(TEpiField)
  private
     FData: array of EpiFloat;
  protected
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsDateTime(const index: Integer): EpiDateTime; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsTime(const index: Integer): EpiTime; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsDateTime(const index: Integer; const AValue: EpiDateTime); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsTime(const index: Integer; const AValue: EpiTime); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetCapacity(AValue: Integer); override;
  public
    class function CheckMissing(AValue: EpiFloat): boolean;
    class function DefaultMissing: EpiFloat;
    function Compare(i, j: integer): integer; override;
    procedure Exchange(i, j: integer); override;
    function  FormatString: string;
  end;

  { TEpiBoolField }
  // Handles following field types:
  // ftBoolean
  TEpiBoolField = class(TEpiField)
  private
     FData: array of EpiBool;
  protected
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsDateTime(const index: Integer): EpiDateTime; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsTime(const index: Integer): EpiTime; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsDateTime(const index: Integer; const AValue: EpiDateTime); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsTime(const index: Integer; const AValue: EpiTime); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetCapacity(AValue: Integer); override;
  public
    class function CheckMissing(AValue: EpiBool): boolean;
    class function DefaultMissing: EpiBool;
    function Compare(i, j: integer): integer; override;
    procedure Exchange(i, j: integer); override;
  end;

  { TEpiStringField }
  // Handles following field types:
  // ftString, ftUpperString
  TEpiStringField = class(TEpiField)
  private
    FData: array of EpiString;
  protected
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsDateTime(const index: Integer): EpiDateTime; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsTime(const index: Integer): EpiTime; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsDateTime(const index: Integer; const AValue: EpiDateTime); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsTime(const index: Integer; const AValue: EpiTime); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetCapacity(AValue: Integer); override;
  public
    class function CheckMissing(AValue: EpiString): boolean;
    class function DefaultMissing: EpiString;
    function Compare(i, j: integer): integer; override;
    procedure Exchange(i, j: integer); override;
  end;

  { TEpiDateField }
  // Handles following field types:
  // ftDMYDate, ftDMYToday, ftMDYDate, ftMDYToday, ftYMDDate, ftYMDToday
  // - note: this is very much subject to change as we introduce time/timedate fields.
  TEpiDateField = class(TEpiField)
  private
    FData: array of EpiDate;
  protected
    function GetAsBoolean(const index: Integer): EpiBool; override;
    function GetAsDate(const index: Integer): EpiDate; override;
    function GetAsDateTime(const index: Integer): EpiDateTime; override;
    function GetAsFloat(const index: Integer): EpiFloat; override;
    function GetAsInteger(const index: Integer): EpiInteger; override;
    function GetAsString(const index: Integer): EpiString; override;
    function GetAsTime(const index: Integer): EpiTime; override;
    function GetAsValue(const index: Integer): EpiVariant; override;
    function GetCapacity: Integer; override;
    function GetIsMissing(const index: Integer): boolean; override;
    procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
    procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
    procedure SetAsDateTime(const index: Integer; const AValue: EpiDateTime); override;
    procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
    procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
    procedure SetAsString(const index: Integer; const AValue: EpiString); override;
    procedure SetAsTime(const index: Integer; const AValue: EpiTime); override;
    procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
    procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
    procedure SetCapacity(AValue: Integer); override;
  public
    class function CheckMissing(AValue: EpiDate): boolean;
    class function DefaultMissing: EpiDate;
    function Compare(i, j: integer): integer; override;
    procedure Exchange(i, j: integer); override;
  end;

  { TEpiDateTimeField }
  // Handles following field types:
  // ftTime, ftTimeNow
  TEpiDateTimeField = class(TEpiField)
  private
     FData: array of EpiDateTime;
  protected
     function GetAsBoolean(const index: Integer): EpiBool; override;
     function GetAsDate(const index: Integer): EpiDate; override;
     function GetAsDateTime(const index: Integer): EpiDateTime; override;
     function GetAsFloat(const index: Integer): EpiFloat; override;
     function GetAsInteger(const index: Integer): EpiInteger; override;
     function GetAsString(const index: Integer): EpiString; override;
     function GetAsTime(const index: Integer): EpiTime; override;
     function GetAsValue(const index: Integer): EpiVariant; override;
     function GetCapacity: Integer; override;
     function GetIsMissing(const index: Integer): boolean; override;
     procedure SetAsBoolean(const index: Integer; const AValue: EpiBool); override;
     procedure SetAsDate(const index: Integer; const AValue: EpiDate); override;
     procedure SetAsDateTime(const index: Integer; const AValue: EpiDateTime); override;
     procedure SetAsFloat(const index: Integer; const AValue: EpiFloat); override;
     procedure SetAsInteger(const index: Integer; const AValue: EpiInteger); override;
     procedure SetAsString(const index: Integer; const AValue: EpiString); override;
     procedure SetAsTime(const index: Integer; const AValue: EpiTime); override;
     procedure SetAsValue(const index: Integer; const AValue: EpiVariant); override;
     procedure SetIsMissing(const index: Integer; const AValue: boolean); override;
     procedure SetCapacity(AValue: Integer); override;
  public
    class function CheckMissing(AValue: EpiDateTime): boolean;
    class function DefaultMissing: EpiDateTime;
    function Compare(i, j: integer): integer; override;
    procedure Exchange(i, j: integer); override;
  end;

