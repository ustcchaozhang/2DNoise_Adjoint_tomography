# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2003 by Thomas Forbriger (BFO Schiltach) 
# 
# convert from and to various seismic formats
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
# conv/many suite: convert (m)any seismic data format(s)
# ======================================================
#
# A set of programs to read, analyze, and convert seismic data files
# in many different formats (all formats supported by libdatrwxx).
#
# See README for further details and installation hints.
# 
# REVISIONS and CHANGES 
#    16/09/2003   V1.0   Thomas Forbriger
#    29/03/2006   V1.1   provide package
#    05/12/2007   V1.2   use new regex module after migration to fenoglio
#                        removed DOS exe-targets
#    17/12/2007   V1.3   moved to gfortran and 64bit
#    06/10/2010   V1.4   start migration to SVN scheme
#                        - remove fallback
#                        - check variables
#                        - package creation is not yet configured
#                        - libboost_regex is hardwired now
#    19/03/2011   V1.5   added libseife to snapshot package and reordered
#                        libraries in package
#    13/02/2012   V1.6   any-type-tools now live in subdirectory many
#    25/03/2013   V1.7   detailed installation instructions
# 
# ============================================================================
#
PROGRAMS=any2sff anyindex anyextract any2ascii any2any

.PHONY: all
all: install doc

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

.PHONY: doc
doc: doxydoc

.PHONY: allstatic
allstatic: 
	$(MAKE) install \
	  PROGRAMS="any2anystat anyindexstat anyextractstat any2asciistat"

flist: README Makefile $(wildcard *.f *.cc *.txt) doxydoc.cfg \
  $(wildcard sub/*.cc sub/*.h) COPYING
	echo $^ | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean:
	-/bin/rm *.o *.bak *.o77 *.exe flist *.ps $(PROGRAMS) *.xxx *.d
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v

# ============================================================================
#
CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))
#
$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR)

AR=ar
AS=as
RANLIB=ranlib

FLAGS += $(MYFLAGS) $(STATIC)
CFLAGS += -O2
FFLAGS += -ff2c -ffixed-line-length-none

CXXFLAGS += -Wall $(FLAGS)
LDFLAGS+=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS+=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS) -I sub

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

SRCFILES=$(wildcard *.cc sub/*.cc)
include $(patsubst %.cc,%.d,$(SRCFILES)) 

#----------------------------------------------------------------------

%.o: %.f
	$(FC) $(FFLAGS) -c $< -o $@

GAPANALYSISOBS=sub/completenessbins.o \
  sub/fncompleteness.o \
  sub/fngaps.o \
  sub/fnprintgaps.o \
  sub/fngnuplotplot.o \
  sub/fnseriesofmissingsamples.o \
  sub/structgapanalysis.o \
  sub/structgapseries.o 

anyextract: %: %.o $(GAPANALYSISOBS)
	$(CXX) -o $@ $^ -I$(LOCINCLUDEDIR) -ldatrwxx \
            -lsffxx -ltime++ -lgsexx -laff -ltfxx -lboost_regex \
            -L$(LOCLIBDIR) $(CXXFLAGS) $(FLAGS) $(STATIC) $(LDFLAGS)
anyextractstat: %stat: %.o $(GAPANALYSISOBS)
	$(CXX) -o $@ $^ -I$(LOCINCLUDEDIR) -ldatrwxx -static \
            -lsffxx -ltime++ -lgsexx -laff -ltfxx -lboost_regex \
            -L$(LOCLIBDIR) $(CXXFLAGS) $(FLAGS) $(STATIC) $(LDFLAGS)

any2any: %: %.o
	$(CXX) -o $@ $^ -I$(LOCINCLUDEDIR) -ldatrwxx \
            -lsffxx -ltime++ -lgsexx -laff -ltfxx \
            -L$(LOCLIBDIR) $(CXXFLAGS) $(FLAGS) $(STATIC) $(LDFLAGS)
any2anystat: %stat: %.o
	$(CXX) -o $@ $^ -I$(LOCINCLUDEDIR) -ldatrwxx \
            -lsffxx -ltime++ -lgsexx -laff -ltfxx -static \
            -L$(LOCLIBDIR) $(CXXFLAGS) $(FLAGS) $(STATIC) $(LDFLAGS)

anyindex any2ascii any2sff: %: %.o
	$(CXX) -o $@ $^ -I$(LOCINCLUDEDIR) -ldatrwxx \
            -lsffxx -ltime++ -lgsexx -laff \
            -ltfxx -L$(LOCLIBDIR) $(CXXFLAGS) $(FLAGS) $(LDFLAGS)

any2asciistat any2sffstat anyindexstat: %stat: %.o
	$(CXX) -o $@ $^ -I$(LOCINCLUDEDIR) -static -ldatrwxx \
            -lsffxx -ltime++ -lgsexx -laff -ltfxx -lboost_regex \
            -L$(LOCLIBDIR) $(CXXFLAGS) $(FLAGS) $(LDFLAGS)

%.ps: %.cc
	a2ps -1 -o $@ \
      --center-title="$(shell ident $< | grep $< | tail -1 | tr '$$' ' ')" \
      $<
%.psp: %.ps; gv $<; /bin/rm -fv $<

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

DOXYWWWPATH=$(TF_WWWBASEDIR)/convmany

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH)

DOXYSRC=Makefile README $(wildcard *.cc *.c *.h *.f sub/*.cc sub/*.h) \
	  doxygen.txt

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
ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=convmany \
	  PACKAGEEXPORT="trunk/src/conv/many:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=convmanywithlibs \
	  PACKAGEEXPORT="trunk/src/conv/many:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="libtime:libaff:libgsexx:libsffxx:libdatrwxx:libtfxx"
endif

# ----- END OF Makefile ----- 
