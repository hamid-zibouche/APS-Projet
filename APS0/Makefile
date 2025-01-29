LEX_ML = ocamllex
YACC_ML = /usr/local/.opam/4.14.0/bin/ocamlyacc
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

	$(OCAMLC) -o print_ast ast.cmo lexer.cmo parser.cmo printer.cmo main.ml

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f print_ast
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml
