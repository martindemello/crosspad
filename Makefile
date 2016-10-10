INCLUDES=-I src -I src/plugins/aclite

all: gtk

tk:
	ocamlbuild -use-ocamlfind $(INCLUDES) src/frontend/tkgui.native

gtk:
	ocamlbuild -use-ocamlfind $(INCLUDES) src/frontend/gtkgui.native

cli:
	ocamlbuild -use-ocamlfind $(INCLUDES) src/frontend/cli.native

clean:
	ocamlbuild -clean
