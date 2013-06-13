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
  
 (* Statement tokens - add here for new commands *)
<normal>[Dd][Ee][Ff][Ii][Nn][Ee]	return(OPDefine);
<normal>[Ii][Nn][Ff][Oo]		return(OPInfo);
<normal>[Nn][Oo][Tt][Ee]		return(OPNote);
<normal>[Ww][Aa][Rr][Nn][Ii][Nn][Gg]	return(OPWarning);
<normal>[Gg][Oo][Tt][Oo]		return(OPGoto);
<normal>[Ww][Rr][Ii][Tt][Ee]		return(OPWrite);
<normal>[Cc][Ll][Ee][Aa][Rr]		return(OPClear);
<normal>[Mm][Ii][Ss][Ss][Ii][Nn][Gg]	return(OPMissing);


 (* General tokens *)
 (* Do not edit anything below this line unless you know   *)
 (* what you are doing!                                    *)
 (* ====================================================== *)

 (* Begin comment states *)
<normal>"//"		start(comment);  (* Comments using "//" (single line comments) *)
<normal>"(*"		start(mcomment); (* Comments using "(* *)" *)

 (* Begin text reading state *)
<normal>\"              begin
			  yylval.yyPString := nil;
			  start(text);
			end;

 (* Constants tokens *)
<normal>[Tt][Rr][Uu][Ee]          return(OPTrue);
<normal>[Ff][Aa][Ll][Ss][Ee]         return(OPFalse);

 (* Binary OPerator tokens *)
<normal>[Oo][Rr]        return(OPOr);
<normal>[Aa][Nn][Dd]    return(OPAnd);
<normal>[Mm][Oo][Dd]    return(OPMod);
<normal>[Dd][Ii][Vv]    return(OPDiv);
<normal>"*"             return(OPMult);
<normal>"+"             return(OPPlus);
<normal>"-"             return(OPMinus);
<normal>"/"             return(OPDivide);
<normal>"^"             return(OPExponential);

 (* Unary OPerators *)
<normal>[Nn][Oo][Tt]           return(OPNot);
 (* unary minus would be here too, but is identified in the binary Operator section *)
   
 (* Comparison tokens *)
<normal>"="             return(OPEQ);
<normal>"<>"            return(OPNEQ);
<normal>"<"             return(OPLT);
<normal>"<="            return(OPLTE);
<normal>">"             return(OPGT);
<normal>">="            return(OPGTE);

 (* Internal Keyword tokens *)
<normal>[Bb][Ee][Gg][Ii][Nn]		return(OPBegin);
<normal>[Ee][Nn][Dd]		return(OPEnd);
<normal>[Ii][Ff]            return(OPIf);
<normal>[Tt][Hh][Ee][Nn]         return(OPThen);
<normal>[Ee][Ll][Ss][Ee]          return(OPElse);

 (* Typeident/Typecast tokens *)
<normal>[Ss][Tt][Rr][Ii][Nn][Gg]        return(OPStringType);
<normal>[Ii][Nn][Tt][Ee][Gg][Ee][Rr]    return(OPIntegerType);
<normal>[Ff][Ll][Oo][Aa][Tt]            return(OPFloatType);
<normal>[Bb][Oo][Oo][Ll][Ee][Aa][Nn]    return(OPBooleanType);
<normal>[Dd][Aa][Tt][Ee]		return(OPDateType);
<normal>[Tt][Ii][Mm][Ee]		return(OPTimeType);

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
<normal>\_?{U}+({U}|{D})*
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
<normal>{D}+(\.{D}+)?([Ee][+-]?{D}+)?
                        begin
                          val(yytext, yylval.yyEpiFloat, result);
                          if (Result = 0)
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
<text>\"                begin
                          return(OPStringLiteral);
			  start(normal);
                        end;  
<text>\n		return(OPIllegal);  (* Unterminated string *)
<text>[^\"\n]*		yylval.yyPString := NewStr(yytext);


%%
