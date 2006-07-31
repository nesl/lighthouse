OCAMLMAKEFILE = OCamlMakefile

SOURCES	= memUtil.ml isEquivalent.ml deadFlow.ml fillFlow.ml ownFlow.ml returnFlow.ml addAnnotations.ml makeOneCFG.ml memory.ml
RESULT	= memory

DOC_FILES = addAnnotations.mli isEquivalent.mli
OCAMLDOCFLAGS = -stars

LIBS = unix str cil
INCDIRS = /home/rshea/mote/cil-1.3.5/obj/x86_LINUX

include $(OCAMLMAKEFILE)

dot:
	rm -f lighthouse.dot
	ocamldep *.ml | ocamldot > lighthouse.dot
