L		[A-Za-z]
D		[0-9]
H		[0-9A-Fa-f]

 (* Unicode recognition *)
 (* 2 bytes unicodes *)
U2		([\302-\337][\200-\277])

 (* 3 bytes unicodes *)
U3_1		([\340][\240-\277][\200-\277])
U3_2		([\341-\354][\200-\277][\200-\277])
U3_3		([\355][\200-\237][\200-\277])
U3_4		([\356-\357][\200-\277][\200-\277])
U3		({U3_1}|{U3_2}|{U3_3}|{U3_4})

 (* 4 bytes unicodes  *)
U4_1		([\360][\220-\277][\200-\277][\200-\277])
U4_2		([\361-\363][\200-\277][\200-\277][\200-\277])
U4_3		([\364][\200-\217][\200-\277][\200-\277])
U4		({U4_1}|{U4_2}|{U4_3})

 (* compound *)
U		({L}|{U2}|{U3}|{U4})

%start normal comment mcomment text
 
  var
    commentlvl: integer = 0;

%%

  var
    result : integer;
  
 (* Command tokens - add here for new commands *)
<normal>"begin"		return(OPBegin);
<normal>"end"		return(OPEnd);
<normal>"define"	return(OPDefine);
<normal>"info"		return(OPInfo);
<normal>"note"		return(OPNote);
<normal>"warning"	return(OPWarning);
<normal>"goto"		return(OPGoto);
<normal>"write"		return(OPWrite);
<normal>"clear"		return(OPClear);
<normal>"missing"	return(OPMissing);


 (* General tokens *)
 (* Do not edit anything below this line unless you know   *)
 (* what you are doing!                                    *)
 (* ====================================================== *)

 (* Begin comment states *)
<normal>"//"		start(comment);  (* Comments using "//" (single line comments) *)
<normal>"(*"		start(mcomment); (* Comments using "(* *)" *)

 (* Begin text reading state *)
<normal>\"              start(text);

 (* Constants tokens *)
<normal>"true"          return(OPTrue);
<normal>"false"         return(OPFalse);

 (* Binary OPerator tokens *)
<normal>"or"            return(OPOr);
<normal>"and"           return(OPAnd);
<normal>"mod"           return(OPMod);
<normal>"div"           return(OPDiv);
<normal>"*"             return(OPMult);
<normal>"+"             return(OPPlus);
<normal>"-"             return(OPMinus);
<normal>"/"             return(OPDivide);
 (* <normal>"shl"           return(OPShl);
<normal>"shr"           return(OPShr);
<normal>"xor"           return(OPXor); *)

 (* Unary OPerators *)
<normal>"not"           return(OPNot);
 (* unary minus would be here too, but is identified in the binary OPerator section *)
   
 (* Comparison tokens *)
<normal>"="             return(OPEQ);
<normal>"<>"            return(OPNEQ);
<normal>"<"             return(OPLT);
<normal>"<="            return(OPLTE);
<normal>">"             return(OPGT);
<normal>">="            return(OPGTE);

 (* Keyword tokens *)
<normal>"if"            return(OPIf);
<normal>"then"          return(OPThen);
<normal>"else"          return(OPElse);

 (* Typeident/Typecast tokens *)
<normal>"string"        return(OPStringType);
<normal>"integer"       return(OPIntegerType);
<normal>"float"         return(OPFloatType);
<normal>"boolean"       return(OPBooleanType);
<normal>"date"		return(OPDateType);

 (* Misc. tokens *)
<normal>":="            return(OPAssign);      
<normal>"("             return(OPOpenParan);      
<normal>")"             return(OPCloseParan);
<normal>";"             return(OPSemicolon);      (* Command seperator *)
<normal>"."             return(OPPeriod);         (* Missing value identifier *)
<normal>","             return(OPComma);
<normal>":"             return(OPColon);
 // <normal>"["             return(OPOpenBracket);    (* L. bracket for variable indexing *)
 // <normal>"]"             return(OPCloseBracket);   (* R. bracket for variable indexing *)

 (* Special case tokens *)
<normal>[ \t]+        ;                           (* ignore whitespace *)
<normal>\n              yyaccept;

 (* Identifiers *)
<normal>{U}+({U}|{D})*
                        begin
                          yylval.yyIdString := yytext;
                          return(OPIdentifier);
                        end;

 (* Recnumber - special case identifier *)
<normal>\_[nN]          begin
                          yylval.yyIdString := 'RECNUMBER';
                          return(OPIdentifier);
                        end;

 (* Integer *)
<normal>{D}+            begin
                          val(yytext, yylval.yyEpiInteger, result);
                          if result=0 then
                            return(OPIntegerLiteral)
                          else
                            return(OPIllegal);
                        end;

 (* Floating point *)
<normal>{D}+([\.\,]{D}+)?([Ee][+-]?{D}+)?
                        begin
                          val(yytext, yylval.yyEpiFloat, result);
                          if (Result = 0) or
                             TryStrToFloat(yytext, yylval.yyEpiFloat)
                          then
                            return(OPFloatLiteral)
                          else
                            return(OPIllegal);
                        end;

 (* Hex numbers (starts with $) *)
<normal>\${H}+          begin
                          val(yytext, yylval.yyEpiInteger, result);
                          if result=0 then
                            return(OPIntegerLiteral)
                          else
                            return(OPIllegal);
                        end;

<normal>.		yyerror('NOT ACCEPTED: ' + yytext );

 (* Comment states *)
<comment>\n             start(normal);  (* back to normal state after newline*)
<comment>.              ;               (* ignore rest *)
<mcomment>"(*"		begin
 	                  { Increase comment lvl, such that nesten comments are allowed }
			  inc(commentlvl);
			end;
<mcomment>"*)"          begin
			  if commentlvl = 0 then
			    (* back to normal state *)
			    start(normal)  
			  else
			    (* decrease nested comment level *)
			    dec(commentlvl);
			end;
<mcomment>.             ;               (* ignore rest *)


 (* String reading (for filenames, OPtions, etc.) *)
<text>\"                start(normal);
<text>\n		return(OPIllegal);  (* Unterminated string *)
<text>[^\"\n]*
                        begin
			  yylval.yyPString := NewStr(yytext);
                          return(OPStringLiteral);
                        end;  

%%
