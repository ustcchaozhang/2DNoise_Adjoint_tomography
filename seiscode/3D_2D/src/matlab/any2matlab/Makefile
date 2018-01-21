# this is <Makefile>
# ----------------------------------------------------------------------------
#
# Copyright (c) 2010 by Daniel Armbruster 
#
# ----
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# ----
#
# Daniel Armbruster 30/09/2010
# 
# REVISIONS and CHANGES 
#    15/07/2010   V1.0   	Thomas Forbriger & Daniel Armbruster
#    30/09/2010   V1.1   	make code functional in new repository
#    13/10/2010   V1.2   	- check environment variables
#                        	- deploy doxygen output as a copy
#                        	- use configurable path in doxygen output -
#                        	changed doxygen path name to any2matlab, since I
#                        	expect more mex programs to contribute
#                        	documentation in the future
#    05/05/2011	V1.3 		provides now the build of an any2matlab package 
#  					with src/pakete/Makefile.packages
#    21/07/2011	V1.4		add mechanism to find MATLAB mex compiler binary
#
# ----------------------------------------------------------------------------
# Usage:
#
# make all				creates the documentation and the
# any2matlab binary
#
# make install			builds the any2matlab binary
#
# make package			builds a full package of any2matlab with all
# 					library dependencies and including documentation
# 					to distribute
#  
# ============================================================================
#

# check operating system
OS := $(shell uname -m)

ifeq ($(OS),x86_64)
TYPE:=mexa64
else
TYPE:=mexglx
endif
 
PROGRAMS=any2matlab.$(TYPE)

# ----------------------------------------------------------------------
#
CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_BROWSER TF_WWWBASEDIR)
#
# ----------------------------------------------------------------------

.PHONY: all
all: install doxydoc

.PHONY: fullpackage
package: fullpackage

INSTDIR=$(LOCBINDIR)/matlab

# install target is defined inside conditional (see below)
##.PHONY: install
##install: $(addprefix $(INSTDIR)/,$(PROGRAMS))

$(INSTDIR)/%: %
	mkdir -pv $(dir $@)
	/bin/mv -fv $< $@

# ----------------------------------------------------------------------
#
README=$(filter-out %.bak,$(wildcard README* */README* doxy*txt))
SRC=$(wildcard *.cc)
DOCSRC=$(addprefix doc/,Makefile any2matlab.tex)

flist: Makefile $(README) $(SRC) COPYING \
	$(wildcard doxy*cfg) $(DOCSRC) $(TF_EDIT)
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean:  
	-/bin/rm *.o *.bak *.o77 *.exe *.ps *.xxx *.d

#======================================================================
# installation part
# ------------------

# check for matlab installation
# only try installation if matlab binary executable is available
ifeq ($(strip $(shell /bin/bash -c "type -t matlab")),)
.PHONY: install any2matlab.$(TYPE)
install any2matlab.$(TYPE):
	@echo
	@echo ERROR matlab binary executable not found
	@echo skipping installation of any2matlab
	@echo
else

# find the binary of MATLAB mex compiler
# If an error occurs add the path to your MATLAB mex compiler binary e.g.
# MEX=/usr/local/MATLAB/R2011a/bin/mex
MEX=

ifndef $(MEX)
MEX:=$(shell tmp=`matlab -n | grep -e "->  r   MATLAB"`; \
echo $${tmp\#*=})/bin/mex	
endif

dump:
	echo $(MEX)

MEXFLAGS =-cxx

# depends on operating system $(OS)
.PHONY: install
install: $(addprefix $(INSTDIR)/,$(PROGRAMS))
any2matlab.$(TYPE): any2matlab.cc
	$(MEX) $< $(MEXFLAGS) -output $@ \
        -L$(LOCLIBDIR) -I$(LOCINCLUDEDIR) -ldatrwxx\
        -lsffxx -lgsexx -laff -ltime++

endif # ($(shell type matlab; echo $$?),1)

#======================================================================
# documentation part
# ------------------
#
# targets commonly used:
# ----------------------
#
# make doxyclean        removes all documentation
# make doxydoc          creates doxygen documentation in the DOXYWWWPATH
# make doxyview         creates doxygen documentation and launches netscape to
#                       browse in the documentation
#
# If you launch "make doxydoc" the documentation will be written to
# DOXYWWWPATH (see below). This is meant to export the documentation through
# your homepage. The doxyfull directory is just a symbolic link to this
# directory.
#

$(call CHECKVARS,TF_WWWBASEDIR TF_BROWSER)

DOXYWWWPATH=$(TF_WWWBASEDIR)/any2matlab

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH)

DOXYSRC=$(README) $(SRC) 

# create doxygen intermediate configuration
PWD=$(shell env pwd)
doxydoc.xxx: doxyfull.cfg
	sed 's,<OUTPUTDIRECTORY>,$(DOXYWWWPATH),g;s,<STRIPFROMPATH>,$(PWD),g' \
	  $< > $@

$(DOXYWWWPATH)/html/index.html: doxydoc.xxx $(DOXYSRC)
	mkdir -vp $(DOXYWWWPATH)
	doxygen $<

doxydoc: $(DOXYWWWPATH)/html/index.html

doxyfullview doxyview: $(DOXYWWWPATH)/html/index.html
	$(TF_BROWSER) file:$< &

# ----- END OF Makefile ----- 
