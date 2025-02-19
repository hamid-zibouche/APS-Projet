LEX_ML = ocamllex
YACC_ML = /usr/bin/ocamlyacc
#YACC_ML = /usr/local/bin/ocamlyacc #chemin de prof
OCAMLC = ocamlc

#parser: ast.ml lexer.mll parser.mly
	

#printer: printer.ml
	

print_ast: 
	$(OCAMLC) -c ast.ml
	$(LEX_ML) -o lexer.ml lexer.mll
	$(YACC_ML) -b parser parser.mly
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -c parser.ml

	$(OCAMLC) -c printer.ml

	$(OCAMLC) -o prologTerm ast.cmo lexer.cmo parser.cmo prologTerm.ml
	$(OCAMLC) -o evaluateur ast.cmo lexer.cmo parser.cmo evaluateur.ml

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f print_ast
	rm -f prologTerm
	rm -f evaluateur
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml
