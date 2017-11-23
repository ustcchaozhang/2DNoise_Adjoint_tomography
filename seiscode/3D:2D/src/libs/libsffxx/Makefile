# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2003 by Thomas Forbriger (BFO Schiltach) 
# 
# SFF++ library: reading and writing SFF from C++
#
# ----
# libsffxx is free software; you can redistribute it and/or modify
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
#    18/12/2003   V1.0   Thomas Forbriger
#    28/03/2006   V1.1   provide a package
#    13/04/2010   V1.2   start implementing WIDX
#    18/09/2010   V1.3   start migration to SVN scheme
#                        - check variables
#                        - copy headers (no symbolic links)
#                        - new doxygen definitions
#                        - package creation not yet implemented
#    17/01/2011   V1.4   distinguish library creation and installation
# 
# ============================================================================
#

.PHONY: all
all: install doc

.PHONY: doc
doc: doxydoc

LIBRARIES=libsffxx.a libsffxx.so

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

# ============================================================================
# a variable definition to check variable settings
# ------------------------------------------------
#
CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))
#

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_BROWSER TF_WWWBASEDIR)

# whereever we find a README, we will use it
README=$(shell find . -name README)
# the frame of doxygen documentation is palced in text files
DOXYTXT=$(shell find . -name doxygen\*.txt)


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
	-/bin/rm -vf flist *.d *.o sffxx_tests *.a *.so *.xxx

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

LIBSRC=sffxx.cc wid2compare.cc wid2inrange.cc wid2isample.cc \
  wid2lastsample.cc sffostream.cc srcesynref.cc offset.cc sffverbose.cc \
  widXio.cc 
INCSRC=sffxx.h sffostream.h 

include $(patsubst %.cc,%.d,$(LIBSRC))

libsffxx.a: $(patsubst %.cc,%.o,$(LIBSRC))
	ar rcv $@ $^
	ranlib $@

libsffxx.so: $(patsubst %.cc,%.o,$(LIBSRC))
	$(CXX) $(CXXFLAGS) -shared -o $@ $^

#----------------------------------------------------------------------
# header files
# ------------
INCINSTALLPATH=$(LOCINCLUDEDIR)
INSTHEADER=$(addprefix $(INCINSTALLPATH)/,$(notdir $(INCSRC)))

$(INCINSTALLPATH)/%.h: %.h
	mkdir -p $(INCINSTALLPATH)
	-rm -fv $@
	/bin/cp -fv $< $@

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

DOXYWWWPATH=$(TF_WWWBASEDIR)/libsffxx

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH) doxydoc.xxx

DOXYSRC=$(DOXYTXT) $(LIBSRC) $(INCSRC) sfftest.cc

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
ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=libsffxx \
	  PACKAGEEXPORT="trunk/src/libs/libsffxx:src" \
	  PACKAGETARGETS="src:all:" \
	  PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=libsffxxwithlibs \
	  PACKAGEEXPORT="trunk/src/libs/libsffxx:src" \
	  PACKAGETARGETS="src:all:" \
	  PACKAGELIBS="libtime:libaff:libgsexx"
endif

#======================================================================
# test code
# ---------

sfftest: sfftest.cc 
	$(CXX) -o $@ $< -I$(LOCINCLUDEDIR) -lsffxx -ltime++ -lgsexx -laff \
        -ltfxx \
      -L$(LOCLIBDIR) $(CXXFLAGS) $(FLAGS)
tests: sfftest; $<; rm -fv $<

# ----- END OF Makefile ----- 
