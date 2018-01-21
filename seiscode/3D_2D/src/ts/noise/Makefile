# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
# 
# noise conversion programs
# 
# REVISIONS and CHANGES 
#    06/06/2005   V1.0   Thomas Forbriger
#    01/12/2010   V1.1   migrated to new SVN scheme
#    09/02/2015   V1.2   only nlnmtab remains in seitosh
# 
# ============================================================================
#

PROGRAMS=nlnmtab

.PHONY: all
all: install

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

# check mandatory environment variable settings
# ---------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)

FLAGS += $(MYFLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += $(FLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS+=-L$(LOCLIBDIR)
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)

#----------------------------------------------------------------------
# standard edit targets
 
flist: Makefile $(wildcard *.f *.c) README COPYING
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o

#----------------------------------------------------------------------

nlnmtab: %: %.o
	$(FC) -o $@ $< -L$(LOCLIBDIR) -ltf

# ----- END OF Makefile ----- 
