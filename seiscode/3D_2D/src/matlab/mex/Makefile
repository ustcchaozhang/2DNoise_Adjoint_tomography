# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
# 
# compile mex files
# 
# REVISIONS and CHANGES 
#    27/04/2010   V1.0   Thomas Forbriger
# 
# ============================================================================
#

all:

flist: Makefile $(wildcard *.cc) README COPYING
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist

mextest.mexa64: mextest.cc
	mex $< -cxx -output $@ -L$(LOCLIBDIR) -I$(LOCINCLUDEDIR) -ltime++

# ----- END OF Makefile ----- 
