all: gtk

tk:
	ocamlbuild -use-ocamlfind -I plugins/aclite frontend/tkgui.native

gtk:
	ocamlbuild -use-ocamlfind -I plugins/aclite frontend/gtkgui.native

cli:
	ocamlbuild -use-ocamlfind -I plugins/aclite frontend/cli.native

clean:
	ocamlbuild -clean
