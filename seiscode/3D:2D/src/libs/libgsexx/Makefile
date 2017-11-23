# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
# 
# GSE++ library to read and write GSE waveforms
# 
# ----
# libgsexx is free software; you can redistribute it and/or modify
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
#    16/03/2002   V1.0   Thomas Forbriger
#    14/11/2002   V1.1   place doxygen docu on WWW
#    25/01/2006   V1.2   - added target gsexx_write_example
#                        - reorganized installation procedure and
#                          installation path definitions
#    18/09/2010   V1.3   start migration to SVN scheme
#                        - check variables
#                        - copy headers (no symbolic links)
#                        - new doxygen definitions
#                        - package creation not yet implemented
#    17/01/2011   V1.4   distinguish library creation and installation
# 
# ============================================================================
# general part
# ------------
#

.PHONY: all
all: install doc

.PHONY: doc
doc: doxydoc

LIBRARIES=libgsexx.a libgsexx.so

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

.PHONY: reinstall
reinstall:
	$(MAKE) clean
	$(MAKE) clean-include
	$(MAKE) install

# ============================================================================
# a variable definition to check variable settings
# ------------------------------------------------
#
#
CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))
#

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_BROWSER TF_WWWBASEDIR)


#----------------------------------------------------------------------

# whereever we find a README, we will use it
README=$(shell find . -name README)
# the frame of doxygen documentation is palced in text files
DOXYTXT=$(shell find . -name doxygen\*.txt)


#----------------------------------------------------------------------

FLAGS=
FLAGS+=-fPIC $(MYFLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS+=-L$(LOCLIBDIR)
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)

flist: Makefile $(README) $(wildcard *.h *.cc *.cfg) $(TF_EDIT) $(DOXYTXT) \
  COPYING
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.d *.o gsexx_tests *.a *.so *.xxx

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

LIBSRC=gsexx.cc gsexx_TCHK2.cc gsexx_TWID2.cc gsexx_TSTA2.cc gsexx_TDAT2.cc \
       gsexx_cm6.cc
INCSRC=gsexx.h gsexx_diff.h gsexx_TDAT2.h

include $(patsubst %.cc,%.d,$(LIBSRC))

$(LOCLIBDIR) $(LOCINCLUDEDIR): ; mkdir -pv $@

libgsexx.a: $(patsubst %.cc,%.o,$(LIBSRC)) $(LOCLIBDIR)
	ar rcv $@ $(patsubst %.cc,%.o,$(LIBSRC))
	ranlib $@

libgsexx.so: $(patsubst %.cc,%.o,$(LIBSRC)) $(LOCLIBDIR)
	$(CXX) $(CXXFLAGS) -shared -o $@ $(patsubst %.cc,%.o,$(LIBSRC))

.PHONY: install-include
install-include: $(LOCINCLUDEDIR) $(LOCINCLUDEDIR)/gsexx.h
$(LOCINCLUDEDIR)/gsexx.h: gsexx.h
	/bin/cp -fv $< $@

.PHONY: clean-include
clean-include: 
	-/bin/rm -fv $(LOCINCLUDEDIR)/gsexx.h

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

DOXYWWWPATH=$(TF_WWWBASEDIR)/libgsexx

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH) doxydoc.xxx

DOXYSRC=$(DOXYTXT) $(LIBSRC) $(INCSRC) gsexx_tests.cc gsexx_write_example.cc

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
# test code
# ---------

gsexx_write_example gsexx_tests: %: %.cc
	$(CXX) -o $@ $< -I$(LOCINCLUDEDIR) -lgsexx -L$(LOCLIBDIR)
tests: gsexx_tests; $<; rm -fv $<

# Target examples requires SFF programs gse2sff and stuplo to be installed
# to plot the data. Use gsexx_write_example separately to check GSE
# data by other means.
examples: gsexx_write_example
	$<; gse2sff example.gse example.sff -o; stuplo example.sff
	rm -fv $< example.gse example.sff
#======================================================================
# create package
# --------------
# is delegated to Makefile.packages
# which still has to be coded
ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	PACKAGE=libgsexx \
	PACKAGEEXPORT="trunk/src/libs/libgsexx:src" \
	PACKAGETARGETS="src:install: src:doc:" \
	PACKAGELIBS="-"
endif

# ----- END OF Makefile ----- 
