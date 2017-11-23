# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
# 
# how to compile libaff tests
#
# ----
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version. 
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
# ----
#
#
# arguments to test programs can be passed through variable ARG
#
# REVISIONS and CHANGES 
#    06/12/2002   V1.0   Thomas Forbriger (copied from libcontxx)
#    17/12/2002   V1.1   introduced seriestest
#    22/12/2002   V1.2   started interface test to Fortran 77
#    12/01/2014   V1.3   use FFLAGS instead of FCFLAGS conforming to GNU make
#                        default rules
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

STANDARDTEST=arraytest subscriptortest shapetest simplearraytest reprtest \
  seriestest helpertest operatortest functionstest
BINARYTEST=binarraytest
F77TEST=f77test
EXECUTABLES=$(STANDARDTEST) $(BINARYTEST) $(F77TEST)

$(addsuffix .o,$(STANDARDTEST) $(BINARYTEST) $(F77TEST)): %.o: %.cc
	$(CXX) -c -o $@ $< $(CXXFLAGS) $(CPPFLAGS) $(FLAGS)

$(STANDARDTEST): %: %.o; $(CXX) -o $@ $< $(LDFLAGS) -laff
$(BINARYTEST): %: %.o; $(CXX) -o $@ $< $(LDFLAGS) -laff

$(addsuffix .run,$(EXECUTABLES)): %.run: %; $< $(ARG); rm -fv $< $<.o

#======================================================================
#
# Fortran 77 interface test
# -------------------------
F2C=f2c

%.f2c.o: %.f f77common.inc
	$(F2C) -C++ -f -u $<
	$(CXX) -c -o $@ $(<:.f=.c) $(CXXFLAGS) $(CPPFLAGS) $(FLAGS)
	/bin/rm -fv $(<:.f=.c)

%.o: %.f f77common.inc
	$(FC) -o $@ -c $< $(FFLAGS) $(FLAGS)

%.code: %.f f77common.inc
	$(F2C) -C++ -f -u $<
	-/bin/mv -v $(patsubst %.f,%.c,$<) $(patsubst %.f,%.code,$<)

%.P: %.f f77common.inc
	$(F2C) -C++ -f -u -P -\!c $<

f77common_com.P: f77procs.f f77common.inc
	$(F2C) -C++ -f -u -ec $<
	/bin/rm -fv $(patsubst %.f,%.c,$<)
	sed -e 's/^struct/extern struct/'  $(patsubst %.P,%.c,$@) > $@

f77interface.o: f77interface.cc f77proto.h f77procs.P f77common_com.P
	$(CXX) -c -o $@ $< $(CXXFLAGS) $(CPPFLAGS) $(FLAGS)

f2ctest: f77test.o f77procs.f2c.o f77interface.o
	$(CXX) -o $@ $^ $(LDFLAGS) -laff -lf2c -lm

f77test: f77test.o f77procs.o f77interface.o
	$(CXX) -o $@ $^ $(LDFLAGS) -laff $(TF_LINK_FORTRAN) -lm

#======================================================================
# test illegal conditions
illegal:
	-$(MAKE) reprtest FLAGS=-DILLEGAL1
	-$(MAKE) seriestest FLAGS=-DILLEGAL1
	-$(MAKE) shapetest FLAGS=-DILLEGAL1
	-$(MAKE) simplearraytest FLAGS=-DILLEGAL1
	-$(MAKE) simplearraytest FLAGS=-DILLEGAL2
	-$(MAKE) arraytest FLAGS=-DILLEGAL1
	-$(MAKE) arraytest FLAGS=-DILLEGAL2
	-$(MAKE) arraytest FLAGS=-DILLEGAL3
	-$(MAKE) arraytest FLAGS=-DILLEGAL4
	-$(MAKE) arraytest FLAGS=-DILLEGAL5
	-$(MAKE) arraytest FLAGS=-DILLEGAL6
	-$(MAKE) arraytest FLAGS=-DILLEGAL7
	-$(MAKE) f77test FLAGS=-DILLEGAL1
	-$(MAKE) f77test FLAGS=-DILLEGAL2
	-$(MAKE) f77test FLAGS=-DILLEGAL3
	-$(MAKE) f77test FLAGS=-DILLEGAL4
	-$(MAKE) f77test FLAGS=-DILLEGAL5

# ----- END OF Makefile ----- 
