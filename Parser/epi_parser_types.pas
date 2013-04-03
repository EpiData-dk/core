unit epi_parser_types;

{$mode objfpc}{$H+}

interface

type
  IdString = String[64];

  TParserResultType = (
    rtBoolean,
    rtInteger,
    rtFloat,
    rtString,
    rtObject,
    rtUndefined
  );


  TParserOperationType = (
    // Literals
    otTrue,
    otFalse,
    otFloat,
    otNumber,
    otHexNumber,
    otString,
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

  TExecutorGetRecordIndex = function(Sender: TObject): integer;
  TExecutorError = procedure(const Msg: string; const LineNo,
      ColNo: integer; const TextFound: string);

implementation

end.

