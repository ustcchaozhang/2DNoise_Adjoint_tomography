# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
# 
# provide access to LAPACK for AFF modules
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
# Please have a look at the README file coming along with the source code in
# this directory for further notes regarding provided functionality,
# prerequisites and installation.
#
# REVISIONS and CHANGES 
#    07/02/2004   V1.0   Thomas Forbriger (thof)
#    18/09/2010   V1.1   start migration to SVN scheme
#                        - check variables
#                        - copy headers (no symbolic links)
#                        - set TF_REMCMMNT to cat if not defined
#                        - new doxygen definitions
#                        - package creation not yet implemented
#    17/01/2011   V1.2   distinguish library creation and installation
#    26/01/2014          (thof) no longer strip comments from header files
# 
# ============================================================================
#

.PHONY: all
all: install doc

.PHONY: doc
doc: doxydoc

LIBRARIES=liblinearxx.a
# shared: liblinearxx.so

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

# ============================================================================
#
CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))
#

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR)
$(call CHECKVARS,TF_BROWSER TF_WWWBASEDIR)

#----------------------------------------------------------------------

HEADERS=$(wildcard *.h lapack/*.h)
#SRC=misc/fortranio.cc misc/commandline.cc misc/ioswap.cc misc/error.cc \
#  misc/blitzfortranio.cc hacks/hack_blitztogrepg.cc
LIBSRC=error.cc lapack/dposv_if.cc operators/dot.cc operators/transpose.cc \
  operators/checknxn.cc

TESTSRC=$(wildcard tests/*.cc)
EXAMPLESSRC=$(wildcard examples/*.cc)

CONTRIBFILES=$(wildcard contrib/*.h) contrib/README.lapack++

INCINSTALLPATH=$(LOCINCLUDEDIR)/linearxx
LIBINSTALLPATH=$(LOCLIBDIR)

# name of installed (exported) header files
INSTHEADER=$(addprefix $(INCINSTALLPATH)/,$(notdir $(HEADERS)))

#
# general part
# ------------
#

FLAGS=
CXXFLAGS+=-Wall -fPIC $(FLAGS)
LDFLAGS+=-L$(LOCLIBDIR) $(TF_LINK_FORTRAN)
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)

EDITFILES=Makefile $(wildcard *.cfg) COPYING README

flist: $(EDITFILES) $(wildcard */Makefile) $(CONTRIBFILES) \
       $(HEADERS) $(LIBSRC) $(TESTSRC) $(EXAMPLESSRC)
	echo $(EDITFILES) | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(HEADERS) $(LIBSRC) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(filter examples/%,$^) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(filter tests/%,$^) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(filter-out tests/% examples/% $(EDITFILES) $(HEADERS) $(LIBSRC),$^) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.o | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.xxx | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.a *.so 

#======================================================================
# pattern rules
# -------------

%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

#======================================================================
# library part
# ------------

include $(patsubst %.cc,%.d,$(LIBSRC))

liblinearxx.a: $(patsubst %.cc,%.o,$(LIBSRC))
	ar rcv $@ $^
	ranlib $@

liblinearxx.so: $(patsubst %.cc,%.o,$(LIBSRC))
	$(CXX) $(CXXFLAGS) -shared -o $@ $^ -llapack -lblas $(LDFLAGS)

#----------------------------------------------------------------------
# header files
# ------------

$(INCINSTALLPATH)/%.h: %.h
	@mkdir -p $(INCINSTALLPATH)
	-@rm -fv $@
	@/bin/cp -vpd $< $@

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

DOXYWWWPATH=$(TF_WWWBASEDIR)/liblinearxx

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH) doxydoc.xxx

DOXYSRC=$(LIBSRC) $(HEADERS)

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
# is delegated to Makefile.packages
# which still has to be coded
PACKAGE=liblinearxx
#ifdef TF_MAKEPKG
#include $(TF_MAKEPKG)
#endif

#======================================================================
# create package
# --------------

ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	PACKAGE=liblinearxx \
	PACKAGEEXPORT="trunk/src/libs/liblinearxx:src" \
	PACKAGETARGETS="src:install: src:doc:" \
	PACKAGELIBS="-"

.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	PACKAGE=liblinearxxwithlibs \
	PACKAGEEXPORT="trunk/src/libs/liblinearxx:src" \
	PACKAGETARGETS="src:install: src:doc:" \
	PACKAGELIBS="libaff libtsxx libsffxx libtime libgsexx libseife libdatrwxx libtfxx"

endif
# ----- END OF Makefile ----- 
