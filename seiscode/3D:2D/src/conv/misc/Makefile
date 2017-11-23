# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
# 
# some other formats
# 
# REVISIONS and CHANGES 
#    13/11/2006   V1.0   Thomas Forbriger
# 
# ============================================================================
#
PROGRAMS=mgm2sff 

.PHONY: all
all: install

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

flist: Makefile $(wildcard *.cc *.sh) README COPYING
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist

FLAGS=
CXXFLAGS=-fhonor-std -Wall $(FLAGS)
CXXFLAGS=-Wall $(FLAGS)
LDFLAGS=-L$(LOCLIBDIR)
CPPFLAGS=-I$(LOCINCLUDEDIR) $(FLAGS)

mgm2sff: %: %.o
	$(CXX) -o $@ $^ -I$(LOCINCLUDEDIR) -ltsioxx \
                    -lsffxx -ltime++ -lgsexx -laff -ltsxx -ldatrwxx \
                    -ltfxx -L$(LOCLIBDIR) $(CXXFLAGS) $(FLAGS)

# ----- END OF Makefile ----- 
