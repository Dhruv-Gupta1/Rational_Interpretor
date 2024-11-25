# Assignment 4 COL226 


## Documenting grammer

### EBNF rules for the grammer:

    Program ::= Block .
    Block ::= DeclarationSeq CommandSeq .
    DeclarationSeq ::= [VarDecls] [ProcDecls] .
    VarDecls ::= [RatVarDecls] [IntVarDecls] [BoolVarDecls] .
    RatVarDecls ::= rational Ident {; Ident }; .
    IntVarDecls ::= integer Ident {; Ident}; .
    BoolVarDecls ::= boolean Ident {; Ident}; .
    ProcDecls ::= [ProcDef {;ProcDecls};] .
    ProcDef ::= procedure Ident Block .
    CommandSeq ::= {{Command;}} .
    Command ::= AssignmentCmd | CallCmd | ReadCmd | PrintCmd |
    ConditionalCmd | WhileCmd .
    AssignmentCmd ::= Ident := Expression .
    CallCmd ::= call Ident .
    ReadCmd ::= read(Ident).
    PrintCmd ::= print( Expression ).
    ConditionalCmd ::= if BoolExpression then CommandSeq else CommandSeq fi .
    Expression ::= ( Expression ) | Expression + Expression | Expression - Expression | Expression * Expression | Expression / Expression | Expression % Expression | ~ Expression | - Expression | Expression < Expression | Expression > Expression | Expression <= Expression | Expression >= Expression | Expression = Expression | Expression <> Expression | Expression and Expression | Expression or Expression | not Expression | Expression .+. Expression | Expression .-. Expression | Expression .*. Expression | Expression ./. Expression | Identifier | Integer | Rational | Boolean .
    Identifier ::= IDENTIFIER .
    ConditionalCmd ::= if Expression then CommandSeq else CommandSeq fi .
    WhileCmd ::= while Expression do CommandSeq od .

### Terminal States
    
    LBRACE | RBRACE | LPAREN | RPAREN | EOF | IF | ELSE | THEN | FI | WHILE | DO | OD | SEMICOLON | COMMA | BOOLEAN | RATIONAL | LT | LEQ | NEQ | EQ | GT | GEQ | PLUS | MINUS | TIMES | DIV | MOD | ADDRAT | SUBRAT | MULRAT | DIVRAT | INVERSERAT | NOT | AND | OR | TRUE | FALSE | PROCEDURE | NEGINT |  MAKERAT | RAT | SHOWRAT | SHOWDECIMAL | FROMDECIMAL | TODECIMAL | ASSIGN | CALL | READ | PRINT | IDENTIFIER of string | DECIMAL of string | BIGINT of string | INTEGER

### Non Terminal States

    start of AST | blockstructure of BLK | declarationsequence of DECSEQ | variabledeclaration of VARDEC | rationalvariabledeclaration of RatVarDec | integervariabledeclaration of IntVarDec | booleanvariabledeclaration of BoolVarDec | proceduredeclarations of PROCDEC | proceduredefination of PROCDEF | proceduredefinations of PROCDEF list | commandlist of CMDLST | command of CMD | commands of CMD list | expression of Exp | Identlist of identifier list | Ident of identifier | assignmentcommand of assigncmd | conditionalcommand of ifthenelse | whilecommand of whilecmd | callcommand of callcmd | readcommand of readcmd | printcommand of printcmd

# How to run the project

In terminal run the following commands:

    make work

It compiles the file `prog.rat` and generates the output in `output.txt` file.
Use the following command to clean the directory from all the generated files:

    make clean