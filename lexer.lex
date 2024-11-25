structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn ()=> Tokens.EOF(!pos,!pos)

%%
%header (functor MyLexFun(structure Tokens: My_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%

\n       => ( pos:= (!pos) + 1; lex()); 
{ws}+    => (lex());
".+." => (Tokens.ADDRAT(!pos , !pos )) ;
".-." => (Tokens.SUBRAT(!pos , !pos )) ;
".*." => (Tokens.MULRAT(!pos , !pos )) ;
"./." => (Tokens.DIVRAT(!pos , !pos )) ;
"inverse"   => (Tokens.INVERSERAT(!pos , !pos )) ;
"procedure" => (Tokens.PROCEDURE(!pos , !pos )) ;
"tt" => ( Tokens.TRUE(!pos , !pos ) ) ;
"ff" => (Tokens.FALSE(!pos , !pos )) ;  
"("      => (Tokens.LPAREN(!pos , !pos ));
")"      => (Tokens.RPAREN(!pos , !pos )) ; 
"}"      => (Tokens.RBRACE(!pos , !pos )) ; 
"{"      => (Tokens.LBRACE(!pos , !pos )) ; 
"!"      => (Tokens.NOT(!pos , !pos )) ; 
"&&"       => ( Tokens.AND(!pos , !pos )) ; 
"||"       => (Tokens.OR(!pos , !pos )) ;
";"       => (Tokens.SEMICOLON(!pos , !pos )) ;
","       => (Tokens.COMMA(!pos , !pos )) ;
":="       => (Tokens.ASSIGN(!pos , !pos )) ;
"+"       => (Tokens.PLUS(!pos , !pos )) ;
"-"       => (Tokens.MINUS(!pos , !pos )) ;
"*"       => (Tokens.TIMES(!pos , !pos )) ;
"/"       => (Tokens.DIV(!pos , !pos )) ;
"%"       => (Tokens.MOD(!pos , !pos )) ;
"make_rat"  => (Tokens.MAKERAT(!pos , !pos )) ;
"rat"   => (Tokens.RAT(!pos , !pos )) ;
"showRat"   => (Tokens.SHOWRAT(!pos , !pos )) ;
"showDecimal"   => (Tokens.SHOWDECIMAL(!pos , !pos )) ;
"fromDecimal"   => (Tokens.FROMDECIMAL(!pos , !pos )) ;
"toDecimal"  => (Tokens.TODECIMAL(!pos , !pos )) ;
"call"      => (Tokens.CALL(!pos , !pos )) ;
"read"       => (Tokens.READ(!pos , !pos )) ;
"print"     => (Tokens.PRINT(!pos , !pos )) ;
"if"       => (Tokens.IF(!pos , !pos )) ;
"then"      => (Tokens.THEN(!pos , !pos )) ;
"else" => (Tokens.ELSE(!pos , !pos )) ; 
"fi"       => (Tokens.FI(!pos , !pos )) ;
"while"       => (Tokens.WHILE(!pos , !pos )) ;
"od"       => (Tokens.OD(!pos , !pos )) ;
"do"       => (Tokens.DO(!pos , !pos )) ;
"integer"      => (Tokens.INTEGER(!pos , !pos )) ;
"boolean"      => (Tokens.BOOLEAN(!pos , !pos )) ;
"rational"     => (Tokens.RATIONAL(!pos , !pos )) ;
"<="       => (Tokens.LEQ(!pos , !pos )) ;
">="       => (Tokens.GEQ(!pos , !pos )) ;
">"       => (Tokens.GT(!pos , !pos )) ;
"="       => (Tokens.EQ(!pos , !pos )) ;
"<>"       => (Tokens.NEQ(!pos , !pos )) ;
"<"       => (Tokens.LT(!pos , !pos )) ;
"~" => (Tokens.NEGINT(!pos , !pos)) ;
"(*"(.|\n)"*)"    => (lex());
{alpha}({alpha} | {digit})* => ( Tokens.IDENTIFIER( yytext ,!pos , !pos )) ; 
["+"|"~"]?({digit}){digit}* => ( Tokens.BIGINT( yytext , !pos , !pos ));
["+"|"~"]?{digit}*"."{digit}*"("{digit}+")" => ( Tokens.DECIMAL( yytext , !pos , !pos )) ;

. => (print ("Unknown token found at " ^ (Int.toString (!pos)) ^ ": <" ^ yytext ^ ">. Continuing.\n"); continue());