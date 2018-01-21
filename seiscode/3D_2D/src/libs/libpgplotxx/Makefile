# this is <Makefile>
# ----------------------------------------------------------------------------
#   ($Id$)
# 
# 05/06/2001 by Thomas Forbriger (IMGF Frankfurt)
# 
# C++ interface for PGPLOT
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
#    05/06/2001   V1.0   Thomas Forbriger (thof)
#    14/11/2002   V1.1   place docu on the WWW
#    28/12/2008   v1.2   reorganized Makefile and provide separate
#                        subdirectory for header files
#    18/09/2010   V1.3   start migration to SVN scheme
#                        - check variables
#                        - copy headers (no symbolic links)
#                        - new doxygen definitions
#                        - package creation not yet implemented
#                        - set TF_REMCMMNT to cat if not defined
#    17/01/2011   V1.4   distinguish library creation and installation
#    26/01/2014          (thof) no longer strip comments from header files
#    16/03/2015          provide rules to add descriptive text from text files
# 
# ============================================================================
# 

.PHONY: all
all: install doc example

.PHONY: doc
doc: doxydoc

LIBRARIES=libpgplotxx.a libpgplotxx.so

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
$(call CHECKVARS,TF_BROWSER TF_WWWBASEDIR TF_LINK_PGPLOT TF_LINK_FORTRAN)

PGPLOTLIB=-lpgplotxx -lcpgplot \
          $(TF_LINK_PGPLOT)  \
          -L$(LOCLIBDIR)

# root headers are those that must be installed in the root of the include
# directory hierarchy for historical reasons
HEADERS=pgplotxx.h xpgplotxx.h affpgplot.h pghandle.h structs.h \
	  device.h basicdevice.h
# only library source code
SRC=xpgplotxx.cc affpgplot.cc pgplotxx.cc structs.cc 
# example and test source code
EXSRC=pgtestxx.cc pgplotxxdemo.cc
# descriptive texts
DESCTXT=pgplot_escape_sequences_help_text.txt

INCINSTALLPATH=$(LOCINCLUDEDIR)/pgplotxx

# name of installed (exported) header files
INSTHEADER=$(addprefix $(INCINSTALLPATH)/,$(notdir $(HEADERS)))

FLAGS+=-fPIC $(MYFLAGS)
CXXFLAGS+=$(FLAGS) 
LDFLAGS+=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS+=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

flist: Makefile $(DESCTXT) $(HEADERS) $(SRC) \
  doxydoc.cfg $(EXSRC) COPYING README
	echo $(HEADERS) $(SRC) | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(DESCTXT) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(EXSRC) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(filter-out $(DESCTXT) $(HEADERS) $(SRC) $(EXSRC),$^) \
	  | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.o | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o install-include libtpgplotxx.a
	-/bin/rm -fv pgplotxxdemo *.so *.a *.xxx
	-/bin/rm -fv *_text.cc *_text.h

#======================================================================
# source code generators
# ----------------------

# description and online texts
# ----------------------------
%.cc %.h: %.txt
	echo "// DO NOT EDIT: this file is automatically derived from $<" \
	  > $(patsubst %.txt,%.h,$<)
	echo "extern const char* const $(patsubst %.txt,%,$<);" \
	  >> $(patsubst %.txt,%.h,$<)
	echo "// DO NOT EDIT: this file is automatically derived from $<" \
	  > $(patsubst %.txt,%.cc,$<)
	echo "#include \"$(patsubst %.txt,%.h,$<)\"" >> $(patsubst %.txt,%.cc,$<)
	echo "const char* const $(patsubst %.txt,%,$<)=" \
	  >> $(patsubst %.txt,%.cc,$<)
	echo "{" >> $(patsubst %.txt,%.cc,$<)
	cat $< | egrep -v '^#' | sed -e 's/\\/\\\\/g' | sed -e 's/"/\\"/g' \
	  | sed -e 's/$$/\\n"/' | sed -e 's/^/  "/'\
	  >> $(patsubst %.txt,%.cc,$<)
	echo "};" >> $(patsubst %.txt,%.cc,$<)

include $(patsubst %.txt,%.d,$(DESCTXT))

#======================================================================
# dependencies
# ------------

%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

include $(patsubst %.cc,%.d,$(SRC) $(EXSRC))


#======================================================================
# library part
# ------------
OBJ=$(patsubst %.cc,%.o,$(SRC) $(patsubst %.txt,%.cc,$(DESCTXT)))

libpgplotxx.a: $(OBJ)
	ar rcv $@ $^
	ranlib $@

libpgplotxx.so: $(OBJ)
	$(CXX) $(CXXFLAGS) -shared -o $@ $^

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

#----------------------------------------------------------------------
# eaxmple and test code

pgplotxxdemo: pgplotxxdemo.o install; $(CXX) $< -o $@ $(PGPLOTLIB) -laff \
  $(TF_LINK_FORTRAN)
pgtestxx: pgtestxx.o install; $(CXX) $< -o $@ $(PGPLOTLIB) -laff -ltfxx \
  $(TF_LINK_FORTRAN)

pgtestxx.run: pgtestxx; pgtestxx -all; /bin/rm -fv pgtestxx

example: pgplotxxdemo
	./pgplotxxdemo
	rm -fv pgplotxxdemo pgplotxxdemo.o

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

DOXYWWWPATH=$(TF_WWWBASEDIR)/libpgplotxx

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH) doxydoc.xxx

DOXYSRC=$(CODE) $(HEADERS)

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
# export PACKAGELINKSFORTRAN=yes
export PACKAGENEEDSPGPLOT=yes
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	PACKAGE=libpgplotxx \
	PACKAGEEXPORT="trunk/src/libs/libpgplotxx:src" \
	PACKAGETARGETS="src:install: src:doc:" \
	PACKAGELIBS="-"

.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	PACKAGE=libpgplotxxwithlibs \
	PACKAGEEXPORT="trunk/src/libs/libpgplotxx:src" \
	PACKAGETARGETS="src:install: src:doc:" \
	PACKAGELIBS="libaff"
endif
# ----- END OF Makefile ----- 
