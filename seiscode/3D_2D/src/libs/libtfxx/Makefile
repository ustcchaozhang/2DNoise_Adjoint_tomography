# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
# 
# tf class library
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
# TF++, Miscellaneous classes and modules in C++
# ==============================================
#
# Please have a look at the README file coming along with the source code in
# this directory for further notes regarding provided functionality,
# prerequisites and installation.
#
# REVISIONS and CHANGES 
#    15/11/2002   V1.0   Thomas Forbriger (thof)
#    24/06/2003   V1.1   disabled blitz part of the code
#    28/03/2006   V1.2   provide a package
#    29/03/2006   V1.3   temporary header install path for package creation
#    05/12/2007   V1.4   migration:
#                        - provide regexx wrapper class
#                        - use new environment variables
#    18/09/2010   V1.5   start migration to SVN scheme
#                        - check variables
#                        - copy headers (no symbolic links)
#                        - new doxygen definitions
#                        - package creation not yet implemented
#                        - set TF_REMCMMNT to cat if not defined
#                        - remove REGEXX options
#    01/12/2010   V1.6   disabled newly invented shared library
#                        linking to libtfxx.so also requires 
#                        libgsl and libboost in cases where the main program
#                        doesn't need them; this is a linker issue
#    17/01/2011   V1.7   distinguish library creation and installation
#    25/03/2013   V1.8   added comments and package creation
#    26/03/2013   V1.9   fullpackage source code tar-ball is tested
#    26/01/2014		 (thof) no longer strip comments from header files
#    27/01/2014          (thof) re-installed header dependency files
#    03/02/2014 thof:	 remove TF_LINK_REGEX
# 
# ============================================================================
#

.PHONY: all
all: install doc

.PHONY: doc
doc: doxydoc

LIBRARIES=libtfxx.a
#shared: libtfxx.so

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

#HEADERS=$(shell find . -name \*.h)
DIRSTOINCLUDE= cmdline misc affio containers \
  strings fileio num fs
#./hacks
#./blitzio
HEADERS=$(wildcard $(addsuffix /*.h,$(DIRSTOINCLUDE)))
SRC=$(wildcard $(addsuffix /*.cc,$(DIRSTOINCLUDE)))
#TESTS=$(filter-out tests/blitztest,$(wildcard tests/*.cc))
TESTS=tests/commandlinetest.cc tests/fortraniotest.cc \
  tests/stringtest.cc tests/handletest.cc tests/regextest.cc \
  tests/filetest.cc tests/rangetest.cc tests/fstest.cc \
  tests/exceptiontest.cc tests/misctest.cc
#tests/blitztest.cc
FTESTS=$(wildcard tests/*.f)
DOXYTXT=$(wildcard doxygen*.txt)

INCINSTALLPATH=$(LOCINCLUDEDIR)/tfxx

# name of installed (exported) header files
INSTHEADER=$(addprefix $(INCINSTALLPATH)/,$(notdir $(HEADERS)))

# set switch flag for regexx
ifeq ($(TF_LIBREGEXX),yes) 
  REGEXXFLAG=-DOLDLIBREGEXX
else
  REGEXXFLAG=
endif

FLAGS=-fPIC $(MYFLAGS)
CXXFLAGS=-fhonor-std -Wall $(FLAGS) 
CXXFLAGS=$(FLAGS) 
LDFLAGS=-L$(LOCLIBDIR) 
CPPFLAGS=-I$(LOCINCLUDEDIR) $(FLAGS) $(REGEXXFLAG)
#CPPFLAGS=-I$(LOCINCLUDEDIR) $(FLAGS) -I$(BLITZINCLUDE)

CODEFILES=$(SRC) $(HEADERS)
TESTFILES=$(TESTS) $(FTESTS)
OTHERFILES=Makefile tests/Makefile doxydoc.cfg COPYING README $(DOXYTXT)
flist: $(CODEFILES) $(OTHERFILES) $(TF_EDIT) $(TESTFILES)
	echo $(OTHERFILES) | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(CODEFILES) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(TESTFILES) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(filter-out $(CODEFILES) $(TESTFILES) $(OTHERFILES),$^) \
	  | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.o | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.hd | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o install-include libtfxx.a *.a *.so *.xxx
	cd tests; $(MAKE) clean

#======================================================================
# dependencies
# ------------

%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

include $(patsubst %.cc,%.d,$(SRC) $(TESTS))


#======================================================================
# library part
# ------------

libtfxx.a: $(SRC:.cc=.o)
	ar rcv $@ $^
	ranlib $@

libtfxx.so: $(SRC:.cc=.o)
	$(CXX) $(CXXFLAGS) -shared -o $@ $^ -lgsl -lboost_regex -lgslcblas \
	   -L$(LOCLIBDIR)

#----------------------------------------------------------------------
# header files
# ------------

%.hd: %.h; echo $(INCINSTALLPATH)/$(notdir $<): $< > $@

include $(patsubst %.h,%.hd,$(HEADERS))
 
$(INCINSTALLPATH)/%.h: 
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

DOXYWWWPATH=$(TF_WWWBASEDIR)/libtfxx

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
# is delegated to Makefile.packages
ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=libtfxx \
	  PACKAGEEXPORT="trunk/src/libs/libtfxx:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="-"
endif

#======================================================================
# delegate test targets
# ---------------------

tests/%: tests/%.cc install-include libtfxx.a
	cd tests; echo "#############################"; $(MAKE) $(notdir $@)

tests/%: tests/%.f 
	cd tests; echo "#############################"; $(MAKE) $(notdir $@)

tests/%.run: tests/%
	cd tests; echo "#############################"; $(MAKE) $(notdir $@)

tests/%.remake: 
	/bin/rm -fv $(patsubst %.remake,%,$@)
	/bin/rm -fv $(patsubst %.remake,%.o,$@)
	$(MAKE) $(patsubst %.remake,%,$@)

# ----- END OF Makefile ----- 
