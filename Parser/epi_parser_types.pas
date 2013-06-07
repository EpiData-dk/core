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
    otVariable
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

