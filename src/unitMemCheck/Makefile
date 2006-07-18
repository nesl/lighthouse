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

# The list of object files (.cmo / .cmx) for the memory checker
OBJS=
	
OPT_OBJS=

all: $(OPT_OBJS) cilDriver.cmx
	$(OCAMLOPT) -o cilDriver $(OCAMLOPTFLAGS) $(OPTLIBS) $(OPT_OBJS) cilDriver.cmx


memoryByte: $(OBJS) cilDriver.cmo
	$(OCAMLC) -o cilDriverByte $(OCAMLFLAGS) $(LIBS) $(OBJS) cilDriver.cmo

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

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

