unit parser_types;

{$mode objfpc}{$H+}

interface

type
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

implementation

end.

