# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
# 
# rheology library
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
# Please have a look at the README file coming along with the source code in
# this directory for further notes regarding provided functionality,
# prerequisites and installation.
#
# 05/01/2003   V1.1   added C++ sections
# 
# REVISIONS and CHANGES 
#    18/01/2010   V1.0   Thomas Forbriger (thof)
#    08/10/2010   V1.1   migrate to SVN environment
#                        check environment
#                        discard f2c fallback option
#    17/11/2010   V1.2   migrated to SVN scheme
#                        removed incdep dependency
#    17/01/2011   V1.3   distinguish library creation and installation
#    03/02/2014 thof:	 do not strip comments from header files
# 
# ============================================================================
#
# moved to gfortran
#

.PHONY: all
all: install doc rhetest

.PHONY: doc
doc: doxydoc

LIBRARIES=librheology.a librheologyxx.a

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

#----------------------------------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)

# compiler and preprocessor flags
FLAGS=
CXXFLAGS=-Wall -pedantic $(FLAGS) -O3
CXXFLAGS=-Wall $(FLAGS) -O3
CC=gcc
FLAGS += $(MYFLAGS)
CFLAGS += -O2 $(FLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-none $(FLAGS)
LDFLAGS+=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS+=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

#----------------------------------------------------------------------
# standard edit targets

# Target clean removes everything except the source code, headers and
# documentation. (See also doxyclean)
.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.o | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o install-include librheologyxx.a *.xxx junk* *.a

flist: $(wildcard *.f) Makefile $(wildcard cxx/*.h cxx/*.cc) \
  README COPYING
	echo $^ | tr ' ' '\n' | sort > $@
edit: flist; vim $<
#----------------------------------------------------------------------

%.d: %.f
	echo $<: $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
include $(patsubst %.f,%.d,$(wildcard *.f))

%.o: %.f; $(FC) -c -o $@ $< $(FFLAGS)

LIBOBS=rl_sls.o rl_qtrans.o
librheology.a: $(LIBOBS)
	ar rcv $@ $^
	ranlib $@

rhetest: rhetest.o librheology.a
	$(FC) -o $(LOCBINDIR)/rhetest rhetest.o -lrheology \
        -ltf -L$(LOCLIBDIR)

#======================================================================
# C++ section

CXXDIR=cxx

# files
# -----

# all header files used within this project
HEADERS=$(wildcard $(CXXDIR)/*.h)
# all source code to be compiled to object files and to be included
# in the binary version of the library
# (see below for the configuration of a preinstantiated version of template
# code)
SRC=$(wildcard $(CXXDIR)/*.cc)

# place where we will copy header files
INCINSTALLPATH=$(LOCINCLUDEDIR)

# name of installed (exported) header files (these are the names in your
# include directory)
INSTHEADER=$(addprefix $(INCINSTALLPATH)/,$(patsubst $(CXXDIR)/%,%,$(HEADERS)))

#======================================================================
# dependencies
# ------------
#
# The compiler is used to create dependency files, which are included below.

%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

include $(patsubst %.cc,%.d,$(SRC)) 

#======================================================================
# header files
# ------------
#
      
$(INCINSTALLPATH)/%.h: $(CXXDIR)/%.h
	mkdir -vp $(dir $@)
	-rm -fv $@
	/bin/cp -vpd $< $@

# install header files
.PHONY: install-include
install-include: $(INSTHEADER)

#======================================================================
# reinstall target
# is necessary in case of header file problems (e.g. remcmmnt not installed)
.PHONY: clean-include
clean-include: 
	/bin/rm -fv $(INSTHEADER)

.PHONY: reinstall
reinstall:
	$(MAKE) clean
	$(MAKE) clean-include
	$(MAKE) install

#----------------------------------------------------------------------
# create the binary library
# -------------------------
LIBOBS=$(patsubst %.cc,%.o,$(SRC))

librheologyxx.a: install-include $(LIBOBS)
	ar rcv $@ $(LIBOBS)
	ranlib $@

#======================================================================
# documentation part
# ------------------
#

$(call CHECKVARS,TF_WWWBASEDIR TF_BROWSER)

DOXYWWWPATH=$(TF_WWWBASEDIR)/librheology

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH) doxydoc.xxx

DOXYSRC=$(README) $(HEADERS) $(SRC) \

# create doxygen intermediate configuration
PWD=$(shell env pwd)
doxydoc.xxx: doxydoc.cfg
	sed 's,<OUTPUTDIRECTORY>,$(DOXYWWWPATH),g;s,<STRIPFROMPATH>,$(PWD),g' \
	  $< > $@

$(DOXYWWWPATH)/html/index.html: doxydoc.xxx $(DOXYSRC)
	mkdir -vp $(DOXYWWWPATH)
	doxygen $<

doxydoc: $(DOXYWWWPATH)/html/index.html

doxyview: $(DOXYWWWPATH)/html/index.html
	$(TF_BROWSER) file:$< &
#======================================================================
# create package
# --------------
ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	PACKAGE=librheology \
	PACKAGEEXPORT="trunk/src/libs/librheology:src" \
	PACKAGETARGETS="src:install: src:doc:" \
	PACKAGELIBS="-"
endif
# ----- END OF Makefile ----- 
