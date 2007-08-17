OCAMLMAKEFILE = OCamlMakefile

# SOURCES	= memUtil.ml isEquivalent.ml mayAliasWrapper.ml addAnnotations.ml callerAllocates.ml isStored.ml isDead.ml makeOneCFG.ml memory.ml
# RESULT	= memory

# DOC_FILES = memUtil.mli isEquivalent.mli mayAliasWrapper.mli addAnnotations.mli callerAllocates.mli isStored.mli isDead.mli makeOneCFG.mli memory.ml
# OCAMLDOCFLAGS = -stars


SOURCES	= memUtil.ml makeOneCFG.ml\
		  state.ml specParse.ml apollo.ml dot2transCheck.ml rocket.ml
RESULT	= rocket

LIBS = unix str odot cil
#INCDIRS = /home/rshea/mote/cil-1.3.5/obj/x86_LINUX
INCDIRS = /home/rshea/research/cil-1.3.5/obj/x86_LINUX

include $(OCAMLMAKEFILE)

dot: ._d
	rm -f lighthouse.dot
	cat ._d/*.d | ocamldot > lighthouse.dot
	dot -Tps lighthouse.dot > lighthouse.ps
