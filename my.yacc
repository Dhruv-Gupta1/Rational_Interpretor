open DataTypes ;

%%
%name My 

%term LBRACE | RBRACE | LPAREN | RPAREN | EOF | IF | ELSE | THEN | FI | WHILE | DO | OD | SEMICOLON | COMMA | BOOLEAN | RATIONAL | LT | LEQ | NEQ | EQ | GT | GEQ | PLUS | MINUS | TIMES | DIV | MOD | ADDRAT | SUBRAT | MULRAT | DIVRAT | INVERSERAT | NOT | AND | OR | TRUE | FALSE | PROCEDURE | NEGINT |  MAKERAT | RAT | SHOWRAT | SHOWDECIMAL | FROMDECIMAL | TODECIMAL | ASSIGN | CALL | READ | PRINT | IDENTIFIER of string | DECIMAL of string | BIGINT of string | INTEGER


%nonterm start of AST
        | blockstructure of BLK
        | declarationsequence of DECSEQ
        | variabledeclaration of VARDEC
        | rationalvariabledeclaration of RatVarDec
        | integervariabledeclaration of IntVarDec
        | booleanvariabledeclaration of BoolVarDec
        | proceduredeclarations of PROCDEC
        | proceduredefination of PROCDEF
        | proceduredefinations of PROCDEF list
        | commandlist of CMDLST
        | command of CMD
        | commands of CMD list
        | expression of Exp
        | Identlist of identifier list
        | Ident of identifier
        | assignmentcommand of assigncmd
        | conditionalcommand of ifthenelse
        | whilecommand of whilecmd
        | callcommand of callcmd
        | readcommand of readcmd
        | printcommand of printcmd




%eop EOF
%noshift EOF
%pos int 
%verbose 
%nodefault


%right ASSIGN
%left OR
%left AND
%left GT GEQ LT LEQ EQ
%left PLUS MINUS ADDRAT SUBRAT
%left TIMES DIV MULRAT DIVRAT 
%left MOD INVERSERAT
%left NOT NEGINT 
%left LPAREN RPAREN LBRACE RBRACE

%%


start: blockstructure (PROG( blockstructure ))
blockstructure: declarationsequence commandlist  (BLK( declarationsequence , commandlist ))
declarationsequence: variabledeclaration proceduredeclarations (DECSEQ(SOME variabledeclaration , SOME proceduredeclarations )) | proceduredeclarations (DECSEQ(NONE , SOME proceduredeclarations )) | variabledeclaration (DECSEQ(SOME variabledeclaration , NONE )) | (DECSEQ(NONE,NONE))
variabledeclaration: rationalvariabledeclaration integervariabledeclaration booleanvariabledeclaration  (VARDEC( SOME rationalvariabledeclaration , SOME integervariabledeclaration , SOME booleanvariabledeclaration )) | rationalvariabledeclaration integervariabledeclaration (VARDEC( SOME rationalvariabledeclaration , SOME integervariabledeclaration , NONE )) | rationalvariabledeclaration booleanvariabledeclaration (VARDEC( SOME rationalvariabledeclaration , NONE , SOME booleanvariabledeclaration )) | integervariabledeclaration booleanvariabledeclaration (VARDEC( NONE , SOME integervariabledeclaration , SOME booleanvariabledeclaration )) | rationalvariabledeclaration (VARDEC( SOME rationalvariabledeclaration , NONE , NONE )) | integervariabledeclaration (VARDEC( NONE , SOME integervariabledeclaration , NONE )) | booleanvariabledeclaration (VARDEC( NONE , NONE , SOME booleanvariabledeclaration )) | (VARDEC( NONE , NONE , NONE ))


rationalvariabledeclaration: RATIONAL Identlist SEMICOLON (RatVarDec( Identlist )) 
integervariabledeclaration: INTEGER Identlist SEMICOLON (IntVarDec( Identlist ))
booleanvariabledeclaration: BOOLEAN Identlist SEMICOLON (BoolVarDec( Identlist ))






Identlist: Ident (([Ident])) | Ident COMMA Identlist ((Ident::Identlist))

 
proceduredeclarations: proceduredefinations (PROCDEC( proceduredefinations )) | (PROCDEC( [] ))

proceduredefinations: (([])) | proceduredefination SEMICOLON proceduredefinations  ((proceduredefination::proceduredefinations))

proceduredefination: PROCEDURE Ident blockstructure (PROCDEF( Ident ,blockstructure))

commands: (([])) 
        | command SEMICOLON commands ((command::commands))

commandlist: LBRACE commands RBRACE (CMDLST(commands)) 



expression: LPAREN expression RPAREN (( expression ))
        | expression PLUS expression ( PLUS( expression1 , expression2 ) )
        | expression MINUS expression ( MINUS( expression1 , expression2 ) )
        | expression TIMES expression ( TIMES( expression1 , expression2 ) )
        | expression DIV expression ( DIV( expression1 , expression2 ) )
        | expression MOD expression ( MOD( expression1 , expression2 ) )
        | NEGINT expression ( NEGINT( expression ) )
        | expression ADDRAT expression ( ADDRAT( expression1 , expression2 ) )
        | expression SUBRAT expression ( SUBRAT( expression1 , expression2 ) )
        | expression MULRAT expression ( MULRAT( expression1 , expression2 ) )
        | expression DIVRAT expression ( DIVRAT( expression1 , expression2 ) )
        | INVERSERAT expression ( INVERSERAT( expression ) )
        | expression AND expression ( AND( expression1 , expression2 ) )
        | expression OR expression ( OR( expression1 , expression2 ) )
        | expression EQ expression ( EQ( expression1 , expression2 ) )
        | expression NEQ expression ( NEQ( expression1 , expression2 ) )
        | expression LT expression ( LT( expression1 , expression2 ) )
        | expression LEQ expression ( LEQ( expression1 , expression2 ) )
        | expression GT expression ( GT( expression1 , expression2 ) )
        | MAKERAT LPAREN expression COMMA expression RPAREN ( MAKERAT( expression1 , expression2 ) )
        | NOT expression ( NOT( expression ) )
        | Ident (IDVAR( Ident ))
        | BIGINT ( bigint( BIGINT ) )
        | DECIMAL ( decimal( DECIMAL ) )
        | TRUE (( TRUE ))
        | FALSE ((FALSE ))


command: assignmentcommand      (CMDASSIGN( assignmentcommand ))
        | callcommand           (CMDCALL( callcommand ))
        | readcommand           (CMDREAD( readcommand ))
        | printcommand          (CMDPRINT( printcommand ))
        | conditionalcommand    (CMDITE( conditionalcommand ))
        | whilecommand          (CMDWH( whilecommand ))


assignmentcommand: Ident ASSIGN expression (ASSIGNCMD( Ident , expression))
callcommand: CALL Ident (CALLCMD( Ident ))
readcommand: READ LPAREN Ident RPAREN   (READCMD( Ident ))
printcommand: PRINT LPAREN expression RPAREN   (PRINTCMD( expression ))
conditionalcommand: IF expression THEN commandlist ELSE commandlist FI   (ITECMD( expression , commandlist1 , commandlist2 ))
whilecommand: WHILE expression DO commandlist OD   (WHCMD( expression , commandlist ))

Ident: IDENTIFIER (IDEN(IDENTIFIER))