all: gtk

tk:
	ocamlbuild -use-ocamlfind -I plugins/puz tkgui.native

gtk:
	ocamlbuild -use-ocamlfind -I plugins/puz gtkgui.native

puz:
	ocamlbuild -use-ocamlfind plugins/puz/puz.native

clean:
	ocamlbuild -clean
