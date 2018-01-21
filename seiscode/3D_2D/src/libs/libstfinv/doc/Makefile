# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2013 by Thomas Forbriger (BFO Schiltach) 
# 
# LaTeX documentation for libstfinv
# 
# REVISIONS and CHANGES 
#    13/02/2013   V1.0   Thomas Forbriger
# 
# ============================================================================
#

all:

flist: Makefile $(wildcard *.tex)
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist

%.dvi: %.tex; latex $(patsubst %.tex,%,$<)
%.ps: %.dvi; dvips -ta4 $(patsubst %.dvi,%,$<)
%.pdf: %.dvi; pdflatex $(patsubst %.dvi,%,$<)
%.psp: %.ps; gv -infoSilent $<; /bin/rm -fv $<
%.x: %.dvi; xdvi -s 4 $(patsubst %.dvi,%,$<)

# ----- END OF Makefile ----- 
