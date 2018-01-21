# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2014 by Thomas Forbriger (BFO Schiltach) 
# 
# Time series input/output functions
# 
# REVISIONS and CHANGES 
#    27/01/2014   Thomas Forbriger (thof)
# 
# ============================================================================
#

.PHONY: all
all: install doc

.PHONY: doc
doc: doxydoc

LIBRARIES=libtsioxx.a

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

HEADERS=$(wildcard *.h)
SRC=$(wildcard *.cc)

DOXYTXT=$(wildcard doxygen*.txt)

INCINSTALLPATH=$(LOCINCLUDEDIR)/tsioxx

# name of installed (exported) header files
INSTHEADER=$(addprefix $(INCINSTALLPATH)/,$(notdir $(HEADERS)))

FLAGS+=-fPIC $(MYFLAGS)
CXXFLAGS+= -Wall $(FLAGS) 
CXXFLAGS+=$(FLAGS) 
LDFLAGS+=-L$(LOCLIBDIR) 
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS) 

CODEFILES=$(HEADERS) $(SRC)
OTHERFILES=Makefile doxydoc.cfg COPYING README $(DOXYTXT)
flist: $(CODEFILES) $(OTHERFILES) $(TF_EDIT)
	echo $(OTHERFILES) | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(CODEFILES) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(filter-out $(CODEFILES) $(OTHERFILES),$^) \
	  | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.o | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o install-include libtfxx.a *.a *.so *.xxx

#======================================================================
# dependencies
# ------------

%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

include $(patsubst %.cc,%.d,$(SRC))

#======================================================================
# library part
# ------------

libtsioxx.a: $(SRC:.cc=.o)
	ar rcv $@ $^
	ranlib $@

libtsioxx.so: $(SRC:.cc=.o)
	$(CXX) $(CXXFLAGS) -shared -o $@ $^ -L$(LOCLIBDIR)

#----------------------------------------------------------------------

$(call CHECKVARS,INCINSTALLPATH)
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

DOXYWWWPATH=$(TF_WWWBASEDIR)/libtsioxx

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
	  PACKAGE=libtsioxx \
	  PACKAGEEXPORT="trunk/src/libs/libtsioxx:src" \
	  PACKAGETARGETS="src:install:-i src:doc" \
	  PACKAGELIBS="-"

# due to the undesired circular dependency between libtfxx and libtsxx, we
# have to install libtsxx as a primary source in the fullpacke
# see ticket:215 and ticket:10
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=libtsioxxwithlibs \
	  PACKAGEEXPORT="trunk/src/libs/libtsioxx:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="libgsexx:libtime:libsffxx:libaff:libseife:libdatrwxx:libtsxx:libtfxx"
endif

# ----- END OF Makefile ----- 
