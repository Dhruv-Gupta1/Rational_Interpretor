structure DataTypes = 
struct
datatype AST = PROG of BLK
and BLK = BLK of DECSEQ * CMDLST
and DECSEQ = DECSEQ of VARDEC option * PROCDEC option
and VARDEC = VARDEC of RatVarDec option * IntVarDec option * BoolVarDec option
and RatVarDec = RatVarDec of identifier list
and IntVarDec = IntVarDec of identifier list
and BoolVarDec = BoolVarDec of identifier list
and PROCDEC = PROCDEC of PROCDEF list 
and PROCDEF = PROCDEF of identifier * BLK  
and CMDLST = CMDLST of CMD list



and Exp = ADDRAT of Exp * Exp | SUBRAT of Exp * Exp | MULRAT of Exp * Exp | DIVRAT of Exp * Exp | INVERSERAT of Exp | IDVAR of identifier | NEGINT of Exp | PLUS of Exp * Exp | MINUS of Exp * Exp | TIMES of Exp * Exp | DIV of Exp * Exp | MOD of Exp * Exp | AND of Exp * Exp | OR of Exp * Exp | NOT of Exp | LT of Exp * Exp | LEQ of Exp * Exp | NEQ of Exp * Exp | EQ of Exp * Exp | GT of Exp * Exp | GEQ of Exp * Exp | TRUE | FALSE | bigint of string | MAKERAT of Exp * Exp | decimal of string




and CMD = CMDASSIGN of assigncmd | CMDWH of whilecmd | CMDITE of ifthenelse | CMDREAD of readcmd | CMDPRINT of printcmd | CMDCALL of callcmd

and assigncmd = ASSIGNCMD of identifier * Exp
and whilecmd = WHCMD of Exp * CMDLST
and ifthenelse = ITECMD of Exp * CMDLST * CMDLST
and readcmd = READCMD of identifier
and printcmd = PRINTCMD of Exp
and callcmd = CALLCMD of identifier

and identifier = IDEN of string
end;