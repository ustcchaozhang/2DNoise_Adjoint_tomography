# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
# 
# Makefile for libdatrwxx
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
# DATRW++ library: seismic data I/O with multiple formats
# =======================================================
#
# Please have a look at the README file coming along with the source code in
# this directory for further notes regarding provided functionality,
# prerequisites and installation
#
# REVISIONS and CHANGES 
#    30/03/2004   V1.0   Thomas Forbriger (thof)
#    29/06/2007   V1.1   included SAC binary reading
#    19/09/2007   V1.2   included raw GSE reading
#    08/07/2008   V1.3   started tracereader
#    18/09/2010   V1.4   start migration to SVN scheme
#                        - check variables
#                        - copy headers (no symbolic links)
#                        - new doxygen definitions
#                        - package creation not yet implemented
#    14/10/2010   V1.5   included tfascii reading
#    17/01/2011   V1.6   distinguish library creation and installation
#    04/02/2011   V1.7   added seife format module
#    07/09/2011   V1.8   added support for libdatrwxxinfo binary executable
#    07/09/2011   V1.9   correction: first mv libraries, then compile
#                        libdatrwxxinfo - Daniel Armbruster
#    13/09/2011   V1.10  prepare development for Thies DL1 data
#    03/11/2011   V1.11  generally add source code of not yet finished formats
#                        ASCII, binary, and Thies DL1
#    24/11/2011   V1.12  distinguish between executables and libaries
#                        necessary for package creation
#    13/01/2012   V1.13  libdatrwxxinfo does not require other libraries
#    26/01/2014          (thof) no longer strip comments from header files
#    19/02/2014 thof:	 provide mseedtest libdatrwxxtests binary executables
#                        rework test program target
# 
# ============================================================================
#

LIBRARIES=libdatrwxx.a libdatrwxx.so
PROGRAMS=libdatrwxxinfo mseedtest libdatrwxxtests
TESTPROGRAMS=writetest sactest hpmotest mseedtest pdastest tfasciitest \
    seifetest asciitest binarytest libdatrwxxtests \
      tsofttest sfftest sutest thiesdl1test

.PHONY: all
all: install doxydoc

.PHONY: doc
doc: doxydoc

.PHONY: install
install:  installlibs installbin

.PHONY: installbin
installbin: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(dir $@)
	/bin/mv -fv $< $@

.PHONY: installlibs
installlibs: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(dir $@)
	/bin/mv -fv $(word 2,$^) $@

.PHONY: installtests
installtests: $(addprefix $(LOCBINDIR)/,$(TESTPROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(dir $@)
	/bin/mv -fv $< $@

# ============================================================================
#
CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))
#

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_BROWSER TF_WWWBASEDIR)

SUBDIRS=$(addsuffix /,sff tsoft tfascii su bonjer pdas mseed hpmo sac gse)
#SUBDIRS+=$(addsuffix /,binary ascii)
SUBDIRS+=$(addsuffix /,seife)
SUBDIRS+=$(addsuffix /,thiesdl1 ascii binary)
ifdef DEVEL
SUBDIRS+=$(addsuffix /,thiesdl1 ascii binary)
endif

LIBHEADERS=$(filter-out none,$(wildcard *.h))
LIBHEADERS+=$(wildcard $(addsuffix *.h,$(SUBDIRS)))

LIBSRC=$(filter-out $(MAINSRC),$(wildcard *.cc))
LIBSRC+=$(wildcard $(addsuffix *.cc,$(SUBDIRS)))

TESTHEADERS=$(wildcard tests/*.h)
TESTSRC=$(wildcard tests/*.cc)

# whereever we find a README, we will use it
README=$(shell find . -name README)
# the frame of doxygen documentation is palced in text files
DOXYTXT=$(shell find . -name doxygen\*.txt) 

INCINSTALLPATH=$(LOCINCLUDEDIR)/datrwxx
LIBINSTALLPATH=$(LOCLIBDIR)

# name of installed (exported) header files
INSTHEADER=$(addprefix $(INCINSTALLPATH)/,$(notdir $(LIBHEADERS)))

#
# general part
# ------------
#

FLAGS=
FLAGS+=-fPIC $(MYFLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS+=-L$(LOCLIBDIR)
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)

MAINSRC=$(addsuffix .cc,libdatrwxxinfo)

DOXYTXT=$(wildcard doxygen*txt */doxygen*txt)
EDITFILES=$(MAINSRC) Makefile README $(wildcard *.cfg contrib/*.h) COPYING $(DOXYTXT)
EDITALL=$(EDITFILES) $(LIBHEADERS) $(LIBSRC) $(TESTSRC) $(EDITFILES)
EDITFORMATHEADERS=$(filter $(patsubst %/,%.h,$(SUBDIRS)),$(LIBHEADERS))
EDITROOT=$(filter-out $(EDITFORMATHEADERS),$(wildcard *.cc *.h))
EDITFIRST=$(filter-out $(LIBHEADERS) $(LIBSRC) $(TESTSRC),$(EDITALL))
EDITSECOND=$(filter-out $(TESTSRC) $(EDITROOT) $(EDITFIRST),$(EDITALL))

flist: $(LIBHEADERS) $(LIBSRC) $(TESTSRC) $(EDITFILES) 
	echo $(filter $(EDITFIRST),$^) | tr ' ' '\n' | sort > $@
	echo "----" >> $@
	echo $(filter $(EDITROOT),$^) | tr ' ' '\n' | sort >> $@
	echo "----" >> $@
	echo $(sort $(filter $(EDITSECOND),$^)) | tr ' ' '\n' >> $@
	echo "----" >> $@
	echo $(filter-out $(EDITSECOND) $(EDITROOT) $(EDITFIRST),$^) \
	  | tr ' ' '\n' | sort >> $@
	echo "----" >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

testvar:
	echo $(MAINSRC)
	echo $(LIBSRC)

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.o | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.hd | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.a *.so *.xxx $(PROGRAMS)

#======================================================================
# library part
# ------------

include $(patsubst %.cc,%.d,$(LIBSRC) $(TESTSRC))

libdatrwxx.a: $(patsubst %.cc,%.o,$(LIBSRC))
	ar rcv $@ $^
	ranlib $@

libdatrwxx.so: $(patsubst %.cc,%.o,$(LIBSRC))
	$(CXX) $(CXXFLAGS) -shared -o $@ $^

#----------------------------------------------------------------------
# header files
# ------------

%.hd: %.h; echo $(INCINSTALLPATH)/$(notdir $<): $< > $@

include $(patsubst %.h,%.hd,$(LIBHEADERS) $(TESTHEADERS))

$(INCINSTALLPATH)/%.h: 
	@mkdir -p $(INCINSTALLPATH)
	-@rm -fv $@
	@/bin/cp -vpd $< $@

.PHONY: install-include
install-include: $(INSTHEADER)

#======================================================================
# pattern rules
# -------------

%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

#======================================================================
# binary executables
# ------------------

libdatrwxxinfo: %: %.o 
	$(CXX) $(FLAGS) -o $@ $^ -I. -I$(LOCINCLUDEDIR) \
        -ldatrwxx -lsffxx -lgsexx -ltime++ -laff \
        -L$(LOCLIBDIR)

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
# test code
# ---------

#STATIC=-static
STATIC=
$(TESTPROGRAMS): %: tests/%.o $(LOCLIBDIR)/libdatrwxx.a
	$(CXX) $(FLAGS) -o $@ $^ -I. -I$(LOCINCLUDEDIR) $(STATIC) \
        -lsffxx -lgsexx -ltfxx -laff -ltime++ \
        -L$(LOCLIBDIR)

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

DOXYWWWPATH=$(TF_WWWBASEDIR)/libdatrwxx

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH) doxydoc.xxx

DOXYSRC=$(LIBSRC) $(LIBHEADERS) $(DOXYTXT) $(TESTSRC) $(MAINSRC)

PWD=$(shell env pwd)
# create doxygen intermediate configuration
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

# ----- END OF Makefile ----- 
