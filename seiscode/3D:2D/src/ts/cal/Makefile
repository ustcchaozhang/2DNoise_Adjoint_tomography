# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
# 
# calibration programs
# 
# REVISIONS and CHANGES 
#    12/06/2007   V1.0   Thomas Forbriger (thof)
#    20/11/2009   V1.1   use gfortran
#    22/01/2011   V1.2   migration to new scheme
#    01/02/2014 thof:	 revision of linker rule
#                        removed obsolete lines
# 
# ============================================================================
#
PROGRAMS=dccal

.PHONY: all
all: install

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

.PHONY: doc
doc: doxydoc

#----------------------------------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_BROWSER TF_WWWBASEDIR)

FLAGS += $(MYFLAGS)
CFLAGS += -O2 $(FLAGS)

STATIC=
CXXFLAGS += -Wall $(FLAGS)
LDFLAGS += -L$(LOCLIBDIR) $(STATIC)
CPPFLAGS += -I$(LOCINCLUDEDIR) $(FLAGS)
FCLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)

#----------------------------------------------------------------------

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o *.xxx.* *.ps *~ $(PROGRAMS)

flist: $(wildcard *.f *.inc Makefile *.cc) doxydoc.cfg COPYING README
	echo $^ | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

#----------------------------------------------------------------------

dccal: \
  %: %.o
	$(LINK.cc)  $^ $(LOADLIBES) $(LDLIBS) -o $@ \
	  -ltsioxx -ltsxx -ldatrwxx -lsffxx -ltime++ -lgsexx -laff -ltfxx

#======================================================================

$(call CHECKVARS,TF_WWWBASEDIR TF_BROWSER)

DOXYWWWPATH=$(TF_WWWBASEDIR)/cal

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH)

DOXYSRC=$(wildcard *.cc *.h README.*)

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
#
#======================================================================
# create package
# --------------
# is delegated to Makefile.packages
##ifdef TF_MAKEPKG
##.PHONY: package
### PACKAGELINKSFORTRAN=yes
### PACKAGENEEDSPGPLOT=yes
##package: $(TF_MAKEPKG)
##	$(MAKE) -f $< \
##	  PACKAGE=convsff \
##	  PACKAGEEXPORT="trunk/src/conv/sff:src" \
##	  PACKAGETARGETS="src:all" \
##	  PACKAGELIBS="-"
##.PHONY: fullpackage
##fullpackage: $(TF_MAKEPKG)
##	$(MAKE) -f $< \
##	  PACKAGE=convsffwithlibs \
##	  PACKAGEEXPORT="trunk/src/conv/sff:src" \
##	  PACKAGETARGETS="src:all" \
##	  PACKAGELIBS="libsff libtf libsffu libts libaff libtime libgsexx libsffxx libdatrwxx libtfxx libtsxx"
##endif

# ----- END OF Makefile ----- 
