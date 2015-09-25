all: gui

gui:
	ocamlbuild -use-ocamlfind -I plugins/puz gui.native

puz:
	ocamlbuild -use-ocamlfind plugins/puz/puz.native

clean:
	ocamlbuild -clean
