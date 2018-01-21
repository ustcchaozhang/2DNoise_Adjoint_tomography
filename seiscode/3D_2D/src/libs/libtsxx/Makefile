# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2003 by Thomas Forbriger (BFO Schiltach) 
# 
# basic code to handle time series analysis
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
#    18/12/2003   V1.0   Thomas Forbriger
#    29/03/2006   V1.1   provide package creation target
#    18/09/2010   V1.2   start migration to SVN scheme
#                        - check variables
#                        - copy headers (no symbolic links)
#                        - set TF_REMCMMNT to cat if not defined
#                        - new doxygen definitions
#                        - package creation not yet implemented
#    01/12/2010   V1.3   disabled newly invented shared library
#                        linking to libtfxx.so also requires 
#                        libseife even in cases where the main program
#                        doesn't need them; this is a linker issue
#    17/01/2011   V1.4   distinguish library creation and installation
#    25/01/2014   V1.5   comment stripping is removed
#    20/05/2015   V1.6   provide usage information in text files
#
# 18.12.2010
# there is no problem with the shared library, when other libraries are linked
# at build time; shared library has same size but contains linker information;
# introduced target shared; commented out this shared, since not all libraries
# to be linked are shared libraries
# 
# ============================================================================

.PHONY: all
all: install doc

.PHONY: doc
doc: doxydoc

LIBRARIES=libtsxx.a
#shared: libtsxx.so

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

INCINSTALLPATH=$(LOCINCLUDEDIR)/tsxx
LIBINSTALLPATH=$(LOCLIBDIR)
 
# name of installed (exported) header files
INSTHEADER=$(addprefix $(INCINSTALLPATH)/,$(notdir $(HEADERS)))

# whereever we find a README, we will use it
README=README
# the frame of doxygen documentation is palced in text files
DOXYTXT=$(wildcard doxygen*.txt)
# usage information and online help
DESCRIPTIONTXT=$(filter-out $(DOXYTXT),$(wildcard *.txt))

HEADERS=$(shell find . -name \*.h)
LIBSRC=tsxx.cc filter.cc seifexx.cc seifeclass.cc \
  anyfilter.cc filterbase.cc ipo.cc ipolin.cc random.cc \
  dropcontainer.cc firfilters.cc tapers.cc ovtaper.cc \
  error.cc \
  $(patsubst %.txt,%.cc,$(DESCRIPTIONTXT))

#
# general part
# ------------
#

FLAGS=
CXXFLAGS+=-fPIC -Wall $(FLAGS)
LDFLAGS+=-L$(LOCLIBDIR)
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)

CODEFILES=$(filter-out $(EDITFILES) \
	    $(patsubst %.txt,%.h,$(DESCRIPTIONTXT)) \
	    $(patsubst %.txt,%.cc,$(DESCRIPTIONTXT)),\
	    $(wildcard *.h *.cc))
OTHERFILES=Makefile $(wildcard *.cfg) $(README) COPYING $(DOXYTXT) \
	     $(DESCRIPTIONTXT)
flist: $(CODEFILES) $(OTHERFILES) $(TF_EDIT)
	echo $(filter-out $(DESCRIPTIONTXT),$(OTHERFILES)) \
	  | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(CODEFILES) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(DESCRIPTIONTXT) | tr ' ' '\n' | sort >> $@
	echo "----" >> $@
	echo $(filter-out $(CODEFILES) $(OTHERFILES),$^) \
	  | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.d *.o tsxx_tests *.s *.so *.xxx
	-/bin/rm -fv $(patsubst %.txt,%.h,$(DESCRIPTIONTXT))
	-/bin/rm -fv $(patsubst %.txt,%.cc,$(DESCRIPTIONTXT))

# ======================================================================
# description and online texts
# ----------------------------
%.cc %.h: %.txt
	echo "// DO NOT EDIT: this file is automatically derived from $<" \
	  > $(patsubst %.txt,%.h,$<)
	echo "extern char $(patsubst %.txt,%,$<)[];" >> $(patsubst %.txt,%.h,$<)
	echo "// DO NOT EDIT: this file is automatically derived from $<" \
	  > $(patsubst %.txt,%.cc,$<)
	echo "#include \"$(patsubst %.txt,%.h,$<)\"" >> $(patsubst %.txt,%.cc,$<)
	echo "char $(patsubst %.txt,%,$<)[]=" >> $(patsubst %.txt,%.cc,$<)
	echo "{" >> $(patsubst %.txt,%.cc,$<)
	cat $< | egrep -v '^#' | sed -e 's/\\/\\\\/g' | sed -e 's/"/\\"/g' \
	  | sed -e 's/$$/\\n"/' | sed -e 's/^/  "/'\
	  >> $(patsubst %.txt,%.cc,$<)
	echo "};" >> $(patsubst %.txt,%.cc,$<)


#======================================================================
# pattern rules
# -------------

# dependencies
%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

include $(patsubst %.cc,%.d,$(LIBSRC))
include $(patsubst %.txt,%.d,$(DESCRIPTIONTXT))

#======================================================================
# library part
# ------------

libtsxx.a: $(patsubst %.cc,%.o,$(LIBSRC))
	ar rcv $@ $^
	ranlib $@

libtsxx.so: $(patsubst %.cc,%.o,$(LIBSRC))
	$(CXX) $(CXXFLAGS) -shared -o $@ $^ -lseife -lgsl \
        -lgslcblas -L$(LOCLIBDIR)

#----------------------------------------------------------------------

$(INCINSTALLPATH)/%.h: %.h
	mkdir -p $(INCINSTALLPATH)
	-rm -fv $@
	/bin/cp -vpd $< $@

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

DOXYWWWPATH=$(TF_WWWBASEDIR)/libtsxx

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH) doxydoc.xxx

DOXYSRC=$(LIBSRC) $(HEADERS) $(DOXYTXT)

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

tstest: tstest.cc
	$(CXX) $(FLAGS) -o $@ $< -I$(LOCINCLUDEDIR) -ltsxx -laff -L$(LOCLIBDIR) \
        -ltfxx -lgsl -lgslcblas -lcseife
tests: tstest; $<; rm -fv $<

# ----- END OF Makefile ----- 
