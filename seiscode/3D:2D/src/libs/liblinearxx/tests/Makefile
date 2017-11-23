# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
# 
# compile test programs for liblinearxx
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
# along with this program. If not, see <http://www.gnu.org/licenses/>.
# ----
#
# REVISIONS and CHANGES 
#    27/10/2016   V1.0   Thomas Forbriger
# 
# ============================================================================
#

STANDARDTEST=lapacktest matrixtest
EXECUTABLES=$(STANDARDTEST)

all:

flist: Makefile $(wildcard *.cc)
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist $(EXECUTABLES)

# ----------------------------------------------------------------------
# check for required environment variables
#
CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))
#

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR)

# ----------------------------------------------------------------------

# set preprocessor options
CPPFLAGS=-I$(LOCINCLUDEDIR)
# set linker options
LDFLAGS=-L$(LOCLIBDIR)
# set compiler options
CXXFLAGS=-Wall $(FLAGS)

# runtime parameters can be passed by setting the ARG variable
ARG=

$(STANDARDTEST): %: %.o; $(CXX) -o $@ $< $(LDFLAGS) \
  -ltfxx -llinearxx -laff -llapack -lblas -lgsl -lgslcblas

$(addsuffix .run,$(EXECUTABLES)): %.run: %; ./$< $(ARG); rm -fv $< $<.o

# ----- END OF Makefile ----- 
