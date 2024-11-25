clean:
	@rm -rf .cm
	@rm -rf lexer.lex.sml
	@rm -rf my.yacc.sig
	@rm -rf my.yacc.sml
	@rm -rf my.yacc.desc
	@rm -rf my.yacc.output
	@rm -rf CM.make
	@rm -rf Rat.compile

work:
	sml < a.sml 
	@make clean