unit epi_parser_types;

{$mode objfpc}{$H+}

interface

uses
  epidatafiles;

type
  IdString = String[64];

  TParserResultType = (
    rtAny,          // Special result type that is always compatible/covertable with/to other result types (except rtUndefined). (Eg. missing literal)
    rtBoolean,
    rtInteger,
    rtFloat,
    rtString,
    rtObject,
    rtUndefined     // Default result type which is ALWAYS incompatible with all other types. Classes should always override the default value.
  );
  TParserResultTypes = set of TParserResultType;

const
  SParserResultType: array[TParserResultType] of string = (
    'any',
    'boolean',
    'integer',
    'float',
    'string',
    'object',
    'undefined'
  );

type
  TParserOperationType = (
    // Literals
    otTrue,
    otFalse,
    otMissingLiteral,
    otBoolLiteral,
    otIntegerLiteral,
    otFloatLiteral,
    otStringLiteral,
    otIdentifier,

    // Binary ops
    otAnd,
    otOr,
    otMod,
    otDiv,
    otMult,
    otPlus,
    otMinus,
    otDivide,
    otExponential,
{    otShl,
    otShr,
    otXor,}

    // Unary (excluding minus which is defined above)
    otNot,

    // Relational
    otEQ,
    otNEQ,
    otLT,
    otLTE,
    otGT,
    otGTE,

    // Built in statements
    otIf,
    otThen,
    otElse,

    // typecast
    otStringCast,
    otIntegerCast,
    otFloatCast,
    otBoolCast,

    // Symbols
    otOpenParan,
    otCloseParan,
    otOpenBracket,
    otCloseBracket,
    otSemicolon,
    otComma,
    otPeriod,
    otAssign,

    // Term
    otVariable,

    // Function
    otFunction,

    // -date functions
    otFuncToday,          // Current date
    otFuncDay,            // Day part of a date
    otFuncMonth,          // Month part of a date
    otFuncYear,           // Year part of a date
    otFuncDayOfWeek,      // Day of the week, result is a number between 1-7
    otFuncWeek,           // Week number

    // -time functions
    otFuncNow,            // Current time
    otFuncHour,           // Hour part of a time
    otFuncMinut,          // Minut part of a time
    otFuncSecond,         // Seconds part of a time

    // -string functions
    otFuncSubString,      // Takes substring out of string
    otFuncLength,         // Length in representable chars
    otFuncPos,            // Returns position of character in string
    otFuncTrim,           // Trims string for whitespace
    otFuncLower,          // lowercases a string
    otFuncUpper,           // UPPERCASES a string

    // -math functions
    otFuncAbs,            // Abs(x)
    otFuncExp,            // Exp(x), e^x...
    otFuncFraction,       // Fraction(x), fractional part of a float
    otFuncLn,             // Ln(x), Natural logarithm
    otFuncLog,            // Log(x), 10-based logarithm
    otFuncRound,          // Round(x,x), round float to number of digits
    otFuncSqrt,           // Srqt(x), Square root of X.
    otFuncRandom          // Random(x), Generata random integer number between 0 - X.
  );

  TGotoOption = (
    goClear,
    goMissing,
    goNoOpt
  );

  TExecutorGetRecordIndex = function(Sender: TObject): integer;
  TExecutorError = procedure(const Msg: string; const LineNo,
    ColNo: integer; const TextFound: string) of object;
  TExecutorSetFieldValue = procedure(Const Sender: TObject;
    Const F: TEpiField; Const Value: Variant) of object;
  TExecutorGetFieldValue = function(Const Sender: TObject;
    Const F: TEpiField): Variant of object;

implementation

end.

