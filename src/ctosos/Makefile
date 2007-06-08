OCAMLMAKEFILE = OCamlMakefile

SOURCES	= ctosos.ml
RESULT	= ctosos

LIBS = unix str cil
INCDIRS = /home/rshea/mote/cil-1.3.5/obj/x86_LINUX
#INCDIRS = /home/rshea/research/cil-1.3.5/obj/x86_LINUX

include $(OCAMLMAKEFILE)

dot: ._d
	rm -f ctosos.dot
	cat ._d/*.d | ocamldot > ctosos.dot
	dot -Tps ctosos.dot > ctosos.ps
