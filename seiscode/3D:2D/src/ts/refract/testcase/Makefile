# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2015 by Thomas Forbriger (BFO Schiltach) 
# 
# refract test case
# 
# REVISIONS and CHANGES 
#    07/12/2015   V1.0   Thomas Forbriger
# 
# ============================================================================
#

all:

flist: Makefile README $(wildcard *.cfg)
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist

# ----------------------------------------------------------------------
#  synthetic test data
synt.TZ synt.TR: $(wildcard *.cfg)
	refmet -o synt -v -4 -c 2lay.main.cfg

# contains trace at 0m offset
set0.sff: synt.TZ
	printf "fac  2.\nend  \n" | tidofi -v -o -cs \
	  $@ $< t:1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49

# contains only traces at offsets larger than 0m
set1.sff: synt.TZ
	printf "fac  2.\nend  \n" | tidofi -v -o -cs \
	  $@ $< t:4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49

set2.sff: synt.TZ
	printf "fac  2.5\nend  \n" | tidofi -v -o -cs \
	  $@ $< t:2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50

set3.sff: synt.TZ
	printf "fac  7.\nend  \n" | tidofi -v -o -cs \
	  $@ $< t:3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51-76

plotset: set0.sff set1.sff set2.sff set3.sff

plot: set0.sff set1.sff set2.sff set3.sff
	refract -p -C -Sr .577 -Sx -20.,150. -St -0.1,0.3 \
	  set3.sff set1.sff set2.sff set0.sff s:5,1,5

# ----- END OF Makefile ----- 
