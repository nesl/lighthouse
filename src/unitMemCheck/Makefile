# General define of the base Makefile
OCAMLMAKEFILE = OCamlMakefile
export OCAMLFLAGS="-g"

####
# Global build options
####
export LIBS = unix str cil oUnit
export INCDIRS = /home/rshea/mote/cil-1.3.5/obj/x86_LINUX +oUnit
#export INCDIRS = /home/rshea/research/cil-1.3.5/obj/x86_LINUX +oUnit


####
# Add projects here
#### 
define PROJ_cd
  SOURCES = cilDriver.ml cilDriverUnit.ml
  RESULT = cilDriverUnit
endef
export PROJ_cd


define PROJ_aa
  SOURCES = ../memCheck/addAnnotations.ml cilDriver.ml addAnnotationsUnit.ml
  RESULT = addAnnotationsUnit
endef
export PROJ_aa


define PROJ_pt
  SOURCES = cilDriver.ml ptranalTest.ml
  RESULT = ptranalTest
endef
export PROJ_pt


define PROJ_ie
  SOURCES = ../memCheck/makeOneCFG.ml ../memCheck/memUtil.ml ../memCheck/isEquivalent.ml cilDriver.ml isEquivalent00Unit.ml
  RESULT = isEquivalent00Unit
endef
export PROJ_ie


define PROJ_ch
  SOURCES = ../memCheck/makeOneCFG.ml ../memCheck/memUtil.ml ../memCheck/isEquivalent.ml cilDriver.ml isEquivalent01Unit.ml
  RESULT = isEquivalent01Unit
endef
export PROJ_ch

define PROJ_mem
  SOURCES = ../memCheck/makeOneCFG.ml ../memCheck/addAnnotations.ml ../memCheck/memUtil.ml cilDriver.ml memUtilUnit.ml
  RESULT = memUtilUnit
endef
export PROJ_mem

define PROJ_may
  SOURCES = ../memCheck/makeOneCFG.ml ../memCheck/mayAliasWrapper.ml cilDriver.ml mayAliasWrapperUnit.ml
  RESULT = mayAliasWrapperUnit
endef
export PROJ_may

define PROJ_cal
  SOURCES = ../memCheck/makeOneCFG.ml ../memCheck/addAnnotations.ml ../memCheck/memUtil.ml ../memCheck/isEquivalent.ml ../memCheck/callerAllocates.ml cilDriver.ml callerAllocatesUnit.ml
  RESULT = callerAllocatesUnit
endef
export PROJ_cal

define PROJ_id
  SOURCES = ../memCheck/makeOneCFG.ml ../memCheck/addAnnotations.ml ../memCheck/memUtil.ml ../memCheck/isEquivalent.ml ../memCheck/mayAliasWrapper.ml ../memCheck/isDead.ml cilDriver.ml isDeadUnit.ml
  RESULT = isDeadUnit
endef
export PROJ_id

define PROJ_is
  SOURCES = ../memCheck/makeOneCFG.ml ../memCheck/addAnnotations.ml ../memCheck/memUtil.ml ../memCheck/isEquivalent.ml ../memCheck/isStored.ml cilDriver.ml isStoredUnit.ml
  RESULT = isStoredUnit
endef
export PROJ_is

####
# Default project to bulid
####
ifndef SUBPROJS
  export SUBPROJS = cd aa pt ie ch mem may cal id is
endif

####
# General build rule
####
all:	bc

# Catch-all target will be applied to all subprojects automatically
%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@


