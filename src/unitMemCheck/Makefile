# General define of the base Makefile
OCAMLMAKEFILE = OCamlMakefile

####
# Global build options
####
export LIBS = unix str cil oUnit
export INCDIRS = /home/rshea/mote/cil-1.3.5/obj/x86_LINUX +oUnit


####
# Add projects here
#### 
define PROJ_cd
  SOURCES = cilDriver.ml unitCilDriver.ml
  RESULT = unitCilDriver
endef
export PROJ_cd


define PROJ_aa
  SOURCES = ../memCheck/addAnnotations.ml cilDriver.ml unitAddAnnotations.ml
  RESULT = unitAddAnnotations
endef
export PROJ_aa


define PROJ_pt
  SOURCES = cilDriver.ml unitPtranal.ml
  RESULT = unitPtranal
endef
export PROJ_pt


define PROJ_ma
  SOURCES = ../memCheck/makeOneCFG.ml ../memCheck/memUtil.ml ../memCheck/mustFlow.ml cilDriver.ml unitMustAlias.ml
  RESULT = unitMustAlias
endef
export PROJ_ma


####
# Default project to bulid
####
ifndef SUBPROJS
  export SUBPROJS = cd aa pt ma
endif

####
# General build rulse
####
all:	bc

# Catch-all target will be applied to all subprojects automatically
%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@


