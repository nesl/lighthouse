
# Path to CIL installation
CIL_DIR=-I /home/rshea/mote/cil-1.3.5/obj/x86_LINUX


########################################

# Compilers / generators
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep

# all relevant -I options here
INCLUDES=$(CIL_DIR)

# all relevant -L options here
LIB_DIRS=-ccopt -L/home/rshea/mote/cil/obj/x86_LINUX

# LIBS
LIBS=unix.cma str.cma cil.cma
OPTLIBS=unix.cmxa str.cmxa cil.cmxa

# add other options for ocamlc here
OCAMLFLAGS=-g $(LIB_DIRS) $(INCLUDES)    

# add other options for ocamlopt here
OCAMLOPTFLAGS=$(LIB_DIRS) $(INCLUDES) 

# The list of object files for the memory checker
MEMORY_OBJS=\
	memUtil.cmo mustFlow.cmo \
	aliasWrapper.cmo \
	deadFlow.cmo fillFlow.cmo ownFlow.cmo returnFlow.cmo \
	addAnnotations.cmo makeOneCFG.cmo
	
OPT_MEMORY_OBJS=\
	memUtil.cmx mustFlow.cmx \
	aliasWrapper.cmx \
	deadFlow.cmx fillFlow.cmx ownFlow.cmx returnFlow.cmx \
	addAnnotations.cmx makeOneCFG.cmx
	

memory: $(OPT_MEMORY_OBJS) memory.cmx
	$(OCAMLOPT) -o memory $(OCAMLOPTFLAGS) $(OPTLIBS) $(OPT_MEMORY_OBJS) memory.cmx


memoryByte: $(MEMORY_OBJS) memory.cmo
	$(OCAMLC) -o memoryByte $(OCAMLFLAGS) $(LIBS) $(MEMORY_OBJS) memory.cmo

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f *.o
	rm -f *.cm[iox]
	#rm -f memory memoryByte

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

