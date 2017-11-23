# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
# 
# analytical solutions for an explosion source in full space
#
# this library provides the same API as libgrrefsub and can replace
# libgrrefsub like a plugin in syg or gremlin
# 
# REVISIONS and CHANGES 
#    04/07/2002   V1.0   Thomas Forbriger
#    17/01/2011   V1.1   distinguish library creation and installation
#    12/01/2014   V1.2   use FFLAGS instead of FCFLAGS conforming to GNU make
#                        default rules
# 
# ============================================================================
#

LIBRARIES=libanafsexphyd.a

.PHONY: all
all: install

.PHONY: doc
doc: 

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

.PHONY: install-include
install-include:

.PHONY: reinstall
reinstall:
	$(MAKE) clean
	$(MAKE) install
	 
# check mandatory environment variable settings
# ---------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)

#----------------------------------------------------------------------

flist: Makefile $(wildcard *.f *.inc)
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o *.a *.d

#----------------------------------------------------------------------

FLAGS += $(MYFLAGS) -fPIC
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += $(FLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS+=-L$(LOCLIBDIR)
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)

#======================================================================
# Fortran dependencies
# --------------------
%.d: %.f
	echo $<: $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
include $(patsubst %.f,%.d,$(wildcard *.f))

%.o: %.f
	$(FC) $(FFLAGS) $(FCOPTS) -o $@ -c $<

#----------------------------------------------------------------------
# libraries
libanafsexphyd.a: interface.o fsexphyd.o

$(LIBRARIES): %.a: ; ar rcv $@ $^; ranlib $@

# ----- END OF Makefile ----- 
