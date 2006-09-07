OCAMLMAKEFILE = OCamlMakefile

SOURCES	= memUtil.ml isEquivalent.ml mayAliasWrapper.ml addAnnotations.ml callerAllocates.ml isStored.ml isDead.ml makeOneCFG.ml memory.ml
RESULT	= memory

DOC_FILES = memUtil.mli isEquivalent.mli mayAliasWrapper.mli addAnnotations.mli callerAllocates.mli isStored.mli isDead.mli makeOneCFG.mli memory.ml
OCAMLDOCFLAGS = -stars

LIBS = unix str cil
INCDIRS = /home/rshea/mote/cil-1.3.5/obj/x86_LINUX

include $(OCAMLMAKEFILE)

dot:
	rm -f lighthouse.dot
	ocamldep *.ml | ocamldot > lighthouse.dot
