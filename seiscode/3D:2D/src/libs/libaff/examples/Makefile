# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
# 
# compile usage examples
# 
# REVISIONS and CHANGES 
#    21/10/2016   V1.0   Thomas Forbriger
# 
# ============================================================================
#

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_LINK_FORTRAN)

# ----------------------------------------------------------------------
 
CPPFLAGS=-I$(LOCINCLUDEDIR) 
LDFLAGS=-L$(LOCLIBDIR) 

CXXFLAGS=-fhonor-std -Wall $(FLAGS)
CXXFLAGS=-Wall -pedantic $(FLAGS)
CXXFLAGS=-Wall $(FLAGS)

ARG=

all:

flist: Makefile
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.P *.code *.o *.bak
	-find . -type f -perm +0111 | xargs --no-run-if-empty /bin/rm -v

STANDARDEXAMPLES=arrayexample
EXECUTABLES=$(STANDARDEXAMPLES) 

$(addsuffix .o,$(EXECUTABLES)): %.o: %.cc
	$(CXX) -c -o $@ $< $(CXXFLAGS) $(CPPFLAGS) $(FLAGS)

$(STANDARDEXAMPLES): %: %.o; $(CXX) -o $@ $< $(LDFLAGS) -laff

$(addsuffix .run,$(EXECUTABLES)): %.run: %; ./$< $(ARG); rm -fv $< $<.o

# ----- END OF Makefile ----- 
