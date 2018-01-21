# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id: $
# 
# Copyright (c) 2014 by Thomas Forbriger (BFO Schiltach) 
# 
# calculate synthetic travel times for seismic body waves
# 
# REVISIONS and CHANGES 
#    10/11/2014   V1.0   Thomas Forbriger
# 
# ============================================================================
#
 
PROGRAMS=lazek ttplot

.PHONY: all
all: install

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)

FLAGS += $(MYFLAGS) -fPIC
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += $(FLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

%.o: %.f
	$(FC) -c -o $@ $< $(FFLAGS)

flist: Makefile *.f README COPYING
	echo $^ | tr ' ' '\n' | sort > $@
edit: flist; vim $<

clean:
	-/bin/rm *.o *.bak

lazek ttplot: %: %.o
	$(FC) -o $@ $< -L$(LOCLIBDIR) -lttsynt -ltf -lrefpar $(TF_LINK_PGPLOT)

# ----- END OF Makefile ----- 
