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
 (* U4_3		([\364][\200-\217][\200-\277][\200-\277]) *)
U4		({U4_1}|{U4_2})

 (* compound *)
U		({L}|{U2}|{U3}|{U4})

%start normal comment mcomment text

%%

  var result : integer;
  
 (* Command tokens - add here for new commands *)
<normal>"begin"		return(OPBegin);
<normal>"end"		return(OPEnd);
<normal>"clear"		return(OPClear);
<normal>"define"	return(OPDefine);
<normal>"execute"	return(OPExecute);

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
<normal>"and"           return(OPAnd);
<normal>"mod"           return(OPMod);
<normal>"div"           return(OPDiv);
<normal>"shl"           return(OPShl);
<normal>"shr"           return(OPShr);
<normal>"or"            return(OPOr);
<normal>"xor"           return(OPXor);
<normal>"*"             return(OPMult);
<normal>"+"             return(OPPlus);
<normal>"-"             return(OPMinus);
<normal>"/"             return(OPDivide);

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

 (* Typecast tokens *)
<normal>"string"        return(OPStringCast);
<normal>"integer"       return(OPIntegerCast);
<normal>"float"         return(OPFloatCast);

 (* Misc. tokens *)
<normal>":="            return(OPAssign);      
<normal>"("             return(OPOpenParan);      
<normal>")"             return(OPCloseParan);
<normal>"["             return(OPOpenBracket);    (* L. bracket for variable indexing *)
<normal>"]"             return(OPCloseBracket);   (* R. bracket for variable indexing *)
<normal>";"             return(OPSemicolon);      (* Command seperator *)
<normal>","             return(OPComma);
<normal>"."             return(OPPeriod);         (* Missing value identifier *)

 (* Special case tokens *)
<normal>[ \t]+        ;                           (* ignore whitespace *)
<normal>\n              yyaccept;

 (* Identifiers *)
<normal>{U}+({U}{D})*
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
                          val(yytext, yylval.yyInteger, result);
                          if result=0 then
                            return(OPNumber)
                          else
                            return(OPIllegal);
                        end;

 (* Floating point *)
<normal>{D}+(\.{D}+)?([Ee][+-]?{D}+)?
                        begin
                          val(yytext, yylval.yyExtended, result);
                          if result=0 then
                            return(OPFloat)
                          else
                            return(OPIllegal);
                        end;

 (* Hex numbers (starts with $) *)
<normal>\${H}+          begin
                          val(yytext, yylval.yyInteger, result);
                          if result=0 then
                            return(OPHexNumber)
                          else
                            return(OPIllegal);
                          end;

<normal>.		yyerror('NOT ACCEPTED: ' + yytext );

 (* Comment states *)
<comment>\n             start(normal);  (* back to normal state after newline*)
<comment>.              ;               (* ignore rest *)
<mcomment>"*)"          start(normal);  (* back to normal state *)
<mcomment>.             ;               (* ignore rest *)


 (* String reading (for filenames, OPtions, etc.) *)
<text>\"                start(normal);
<text>\n		return(OPIllegal);  (* Unterminated string *)
<text>{U}*
                        begin
			  yylval.yyIdString := yytext;
                          return(OPString);
                        end;  

%%
