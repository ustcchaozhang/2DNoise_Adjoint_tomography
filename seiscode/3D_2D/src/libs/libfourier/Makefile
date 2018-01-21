# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
# 
# Fourier coefficients library
#
# ----
# libfourier is free software; you can redistribute it and/or modify
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
# Fourier library: Operations in the Fourier domain
# =================================================
#
# Please have a look at the README file coming along with the source code in
# this directory for further notes regarding provided functionality,
# prerequisites and installation.
# 
# REVISIONS and CHANGES 
#    07/11/2002   V1.0   Thomas Forbriger (thof)
#    05/01/2003   V1.1   uuuhhhhhh
#                        had to resolve a specific problem: my library
#                        procuded object files for Fortran code with the same
#                        name of the object files of C++ code
#    17/12/2007   V1.2   move to gfortran and 64bit compilation
#    18/09/2010   V1.3   start migration to SVN scheme
#                        - discard f2c (fallback) solution
#                        - check variables
#                        - copy headers (no symbolic links)
#                        - new doxygen definitions
#                        - package creation not yet implemented
#                        - set TF_REMCMMNT to cat if not defined
#    07/10/2010   V1.4   - migrate to FFTW3
#    15/11/2010   V1.5   - avoid makefdoc.pl
#    18.12.2010   V1.6   - shared library does not work satisfactory
#    11.01.2011   V1.7   - renamed Fortran source code such that
#                          Fotran object files to not conflict with
#                          C++ object files
#    17/01/2011   V1.8   distinguish library creation and installation
#    29/01/2013   V1.9   added tarball rules
#    26/01/2014          (thof): no longer strip comments from header files
# 
# ============================================================================
#

.PHONY: all
all: install doc

.PHONY: doc
doc: doxydoc libfourier.doc

LIBRARIES=libfourier.a libfourierxx.a
#shared: libfourierxx.so

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

#----------------------------------------------------------------------

DOXYTXT=$(wildcard doxygen*txt)

flist: Makefile $(wildcard *.f *.inc *.h *.cc *.c README *.cfg) COPYING \
	$(DOXYTXT)
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean:
	-/bin/rm -fv *.o *.bak *.o77 flist *.of2c
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.o | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o install-include *.xxx junk* *.a *.so

# ============================================================================
#
CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))
#

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR)
$(call CHECKVARS,TF_BROWSER TF_WWWBASEDIR)

AR=ar
AS=as
RANLIB=ranlib

#----------------------------------------------------------------------
# uncomment this to activate the FFTW fallback solution
# i.e. to use FFTW instead of FFTW3
# this option will disappear in the near future! Better not use it...
#FFTWFALLBACK=-DFFTWFALLBACK
#FFTWLIB=-ldrfftw -ldfftw
FFTWLIB=-lfftw3
#----------------------------------------------------------------------

FLAGS+=-fPIC
FFLAGS+=-ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS+=$(FLAGS)
CXXFLAGS+=-Wall -fPIC $(FLAGS) 
LDFLAGS+=-L$(LOCLIBDIR) -fPIC
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)

LIBSRC=$(wildcard *.f)
LIBOBS=$(patsubst %.f,%.o,$(LIBSRC))
DOCS=libfourier.doc

docs: $(DOCS)

%.o: %.f
	$(FC) $(FFLAGS) -c -o $@ $<

libfourier.doc: $(LIBSRC) $(wildcard *.inc)
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^cS/ { hot=1; \
	    print FILENAME; \
          print "  c\n  c"; \
          next; } \
        /^cE/ { hot=0; } \
        { if (hot==1) { print "  " $$0; } }' $^ > $@

libfourier.a: $(LIBOBS)

%.a: 
	$(AR) rcv $@ $^
	$(RANLIB) $@

# ============================================================================
# files
# -----

# all header files used within this project
HEADERS=$(shell find . -name \*.h)
# all source code to be compiled to object files and to be included
# in the binary version of the library
# (see below for the configuration of a preinstantiated version of template
# code)
SRC=fcommand.cc filters.cc polesnzeroes.cc fftwaff.cc error.cc fftwaffar.cc
# test programs are placed in a subdirectory
TESTS=$(wildcard tests/*.cc)
# whereever we find a README, we will use it
README=$(shell find . -name README) 
# the frame of doxygen documentation is palced in text files 
DOXYTXT=$(shell find . -name doxygen\*.txt) 

# place where we will copy header files
INCINSTALLPATH=$(LOCINCLUDEDIR)/fourier
# place where we will copy the binary library
LIBINSTALLPATH=$(LOCLIBDIR)

# name of installed (exported) header files (these are the names in your
# include directory)
INSTHEADER=$(addprefix $(INCINSTALLPATH)/,$(filter-out ./tests/%,$(HEADERS)))

# compiler and preprocessor flags
FLAGS=
#CXXFLAGS += -Wall -pedantic $(FLAGS) -O3
CXXFLAGS += -Wall $(FLAGS) -O3
LDFLAGS += -L$(LOCLIBDIR)
CPPFLAGS += -I$(LOCINCLUDEDIR) $(FLAGS)

LIBOBSXX=$(patsubst %.cc,%.o,$(SRC))

libfourierxx.a: install-include $(LIBOBSXX)
	ar rcv $@ $(LIBOBSXX)
	ranlib $@

libfourierxx.so: install-include libfourier.a $(LIBOBSXX) 
	$(CXX) $(CXXFLAGS) -shared -o $@ $(LIBOBSXX) -lfourier \
        -L$(LOCLIBDIR)

#======================================================================
# dependencies
# ------------
#
# The compiler is used to create dependency files, which are included below.

%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $(TMPPARDEF) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

include $(patsubst %.cc,%.d,$(SRC)) 

#======================================================================
# header files
# ------------
      
$(INCINSTALLPATH)/%.h: %.h
	@mkdir -vp $(dir $@)
	-@rm -fv $@
	@/bin/cp -vpd $< $@

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

#======================================================================
# tests
# -----

cfftwtest: cfftwtest.c
	$(CC) -o $@ $< -ldrfftw -ldfftw -lm

cxxfftwtest: cxxfftwtest.cc libfourierxx.a
	$(CXX) -o $@ $^ -ltfxx -laff $(FFTWLIB) -lm \
        $(CPPFLAGS) $(LDFLAGS)

cxxfftwartest: cxxfftwartest.cc libfourierxx.a
	$(CXX) -o $@ $^ -ldatrwxx -lsffxx -lgsexx -ltime++ \
	  -ltfxx -laff $(FFTWLIB) -lm \
        $(CPPFLAGS) $(LDFLAGS)

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
# make doxyconf         edit the doxygen configuration file
#
# If you launch "make doxydoc" the documentation will be written to
# DOXYWWWPATH (see below). This is meant to export the documentation through
# your homepage. The doxyfull directory is just a symbolic link to this
# directory.
#

$(call CHECKVARS,TF_WWWBASEDIR TF_BROWSER)

DOXYWWWPATH=$(TF_WWWBASEDIR)/libfourier

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH) doxydoc.xxx

DOXYSRC=$(DOXYTXT) $(HEADERS) $(SRC) 

# create doxygen intermediate configuration
PWD=$(shell env pwd)
doxydoc.xxx: doxydoc.cfg
	sed 's,<OUTPUTDIRECTORY>,$(DOXYWWWPATH),g;s,<STRIPFROMPATH>,$(PWD),g' \
	  $< > $@
# create commented version of doxygen configuration
doxycomm.xxx: doxydoc.cfg
	/bin/cp -vf $< $@; doxygen -u $@

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
        PACKAGE=libfourier \
        PACKAGEEXPORT="trunk/src/libs/libfourier:src" \
        PACKAGETARGETS="src:install: src:doc:" \
        PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
        PACKAGE=libfourierwithlibs \
        PACKAGEEXPORT="trunk/src/libs/libfourier:src" \
        PACKAGETARGETS="src:install: src:doc:" \
        PACKAGELIBS="libaff"
endif

# ----- END OF Makefile ----- 
