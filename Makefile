INCLUDES=-I src -I src/plugins/aclite -I src/plugins/json

all: gtk

tk:
	ocamlbuild -use-ocamlfind $(INCLUDES) src/frontend/tkgui.native

gtk:
	ocamlbuild -use-ocamlfind $(INCLUDES) src/frontend/gtkgui.native

cli:
	ocamlbuild -use-ocamlfind $(INCLUDES) src/frontend/cli.native

web:
	ocamlbuild -use-ocamlfind $(INCLUDES) src/frontend/opium_web.native

a:
	ocamlbuild -use-ocamlfind $(INCLUDES) src/plugins/aclite/puz_bit.native

b:
	ocamlbuild -use-ocamlfind $(INCLUDES) src/plugins/aclite/test_bit.native

clean:
	ocamlbuild -clean
