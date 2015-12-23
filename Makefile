all: gtk

tk:
	ocamlbuild -use-ocamlfind -I plugins/aclite tkgui.native

gtk:
	ocamlbuild -use-ocamlfind -I plugins/aclite gtkgui.native

cli:
	ocamlbuild -use-ocamlfind -I plugins/aclite cli.native

clean:
	ocamlbuild -clean
