<div align="left" style="position: relative;">
<h1>Interpretor for Rationals in Standard ML</h1>
<!-- <p align="left">
	<em><code>❯ REPLACE-ME</code></em>
</p> -->
<p align="left">
	<img src="https://img.shields.io/github/languages/top/Dhruv-Gupta1/Rational_Interpretor?style=flat&color=ff1200" alt="repo-top-language">
	<img src="https://img.shields.io/github/languages/count/Dhruv-Gupta1/Rational_Interpretor?style=flat&color=ff1200" alt="repo-language-count">
<p align="left">
	</p>
</div>
<br clear="right">

##  Overview

This project is a part of the course COL226 at IIT Delhi. It is an interpreter for a simple language that supports rational numbers. The interpreter is implemented in SML and uses the tools like lex and yacc for parsing and interpreting the input program.

---

##  Features

<code>❯ REPLACE-ME</code>

---



###  Project Index
<details open>
	<summary><b><code>RATIONAL_INTERPRETOR/</code></b></summary>
	<details> <!-- __root__ Submodule -->
		<summary><b>__root__</b></summary>
		<blockquote>
			<table>
			<tr>
				<td><b><a href='https://github.com/Dhruv-Gupta1/Rational_Interpretor/blob/master/lexer.lex'>lexer.lex</a></b></td>
				<td><code>❯ REPLACE-ME</code></td>
			</tr>
			<tr>
				<td><b><a href='https://github.com/Dhruv-Gupta1/Rational_Interpretor/blob/master/glue.sml'>glue.sml</a></b></td>
				<td><code>❯ REPLACE-ME</code></td>
			</tr>
			<tr>
				<td><b><a href='https://github.com/Dhruv-Gupta1/Rational_Interpretor/blob/master/a.sml'>a.sml</a></b></td>
				<td><code>❯ REPLACE-ME</code></td>
			</tr>
			<tr>
				<td><b><a href='https://github.com/Dhruv-Gupta1/Rational_Interpretor/blob/master/Rat.cm'>Rat.cm</a></b></td>
				<td><code>❯ REPLACE-ME</code></td>
			</tr>
			<tr>
				<td><b><a href='https://github.com/Dhruv-Gupta1/Rational_Interpretor/blob/master/evaluator.sml'>evaluator.sml</a></b></td>
				<td><code>❯ REPLACE-ME</code></td>
			</tr>
			<tr>
				<td><b><a href='https://github.com/Dhruv-Gupta1/Rational_Interpretor/blob/master/while_ast.sml'>while_ast.sml</a></b></td>
				<td><code>❯ REPLACE-ME</code></td>
			</tr>
			<tr>
				<td><b><a href='https://github.com/Dhruv-Gupta1/Rational_Interpretor/blob/master/prog.rat'>prog.rat</a></b></td>
				<td><code>❯ REPLACE-ME</code></td>
			</tr>
			<tr>
				<td><b><a href='https://github.com/Dhruv-Gupta1/Rational_Interpretor/blob/master/rational.sml'>rational.sml</a></b></td>
				<td><code>❯ REPLACE-ME</code></td>
			</tr>
			<tr>
				<td><b><a href='https://github.com/Dhruv-Gupta1/Rational_Interpretor/blob/master/my.yacc'>my.yacc</a></b></td>
				<td><code>❯ REPLACE-ME</code></td>
			</tr>
			<tr>
				<td><b><a href='https://github.com/Dhruv-Gupta1/Rational_Interpretor/blob/master/Makefile'>Makefile</a></b></td>
				<td><code>❯ REPLACE-ME</code></td>
			</tr>
			<tr>
				<td><b><a href='https://github.com/Dhruv-Gupta1/Rational_Interpretor/blob/master/datatypes.sml'>datatypes.sml</a></b></td>
				<td><code>❯ REPLACE-ME</code></td>
			</tr>
			<tr>
				<td><b><a href='https://github.com/Dhruv-Gupta1/Rational_Interpretor/blob/master/output.txt'>output.txt</a></b></td>
				<td><code>❯ REPLACE-ME</code></td>
			</tr>
			</table>
		</blockquote>
	</details>
</details>

---


## Documentation

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