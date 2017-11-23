# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
# 
# some code to control the Thies datalogger DL1/N
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
# 
# REVISIONS and CHANGES 
#    24/11/2008   V1.0   Thomas Forbriger
# 
# ============================================================================
#
 
PROGRAMS=DL1logger DL1direct

.PHONY: all
all: install

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

#----------------------------------------------------------------------

# do not link against shared libraries
# since DL1logger is called by cron, do not rely on LD_LIBRARY_PATH or
# LD_RUN_PATH being set appropriately
STATIC=-static

HEADERS=dataline.h  error.h  functions.h  logger.h  memory.h  record.h \
  thiesdl1.h version.h
SUBSRC=error.cc dataline.cc record.cc thiesdl1.cc functions.cc memory.cc \
  logger.cc  cvsids.cc func_pathname.cc func_writedata.cc 
MAINSRC=DL1direct.cc DL1logger.cc tester.cc
AUTOSRC=DL1logger_longusage_text.cc DL1logger_notes_text.cc \
	  DL1logger_operation_text.cc DL1logger_usage_text.cc

SUBOBS=$(patsubst %.cc,%.o,$(AUTOSRC) $(SUBSRC))
ALLSRC=$(AUTOSRC) $(SUBSRC) $(MAINSRC)
AUXFILES=Makefile README COPYING

#----------------------------------------------------------------------

flist: $(wildcard  *.cfg *.sh *.tex *.conf *.txt) $(AUXFILES)  \
  $(HEADERS) $(SUBSRC) $(MAINSRC)
	echo $(AUXFILES) | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(MAINSRC) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(SUBSRC) $(HEADERS) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(filter-out $(MAINSRC) $(SUBSRC) $(HEADERS) $(AUXFILES),$^) \
	  | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o *.xxx *.d
	-/bin/rm -vf *_text.cc *_text.h

#----------------------------------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_BROWSER TF_WWWBASEDIR)

FLAGS += $(MYFLAGS) -fPIC
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += $(FLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

#======================================================================
# automatically generated source code
# ===================================
#
# source code version identification
# ----------------------------------

idstrings.xxx: $(filter-out cvsids.cc,$(MAINSRC) $(SUBSRC) $(patsubst %.cc,%.txt,$(AUTOSRC)))
	/bin/bash -c 'if $$(git status  >/dev/null 2>&1); \
	  then \
	    for d in $^; do \
	      echo "   " $$d $$(git log -n 1 --pretty="%h %ci %cn" -- $$d); \
	    done;\
	  else \
	    ident $^ | egrep "^ +\\\$$Id:" | sort | uniq; \
	  fi' > $@

cvsids.xxx: idstrings.xxx
	awk 'BEGIN{ print "const char* const CVSIDS[]={"; } \
          { print "\"" $$0 "\","; } \
          END{ print "0};"; }' $< > $@

cvsids.cc: cvsids.xxx

#----------------------------------------------------------------------
# description and online texts
# ----------------------------
#
%.cc %.h: %.txt
	echo "// DO NOT EDIT: this file is automatically derived from $<" \
	  > $(patsubst %.txt,%.h,$<)
	echo "extern char $(patsubst %.txt,%,$<)[];" >> $(patsubst %.txt,%.h,$<)
	echo "// DO NOT EDIT: this file is automatically derived from $<" \
	  > $(patsubst %.txt,%.cc,$<)
	echo "#include \"$(patsubst %.txt,%.h,$<)\"" >> $(patsubst %.txt,%.cc,$<)
	echo "char $(patsubst %.txt,%,$<)[]=" >> $(patsubst %.txt,%.cc,$<)
	echo "{" >> $(patsubst %.txt,%.cc,$<)
	cat $< | egrep -v '^#' | sed -e 's/"/\\"/g' \
	  | sed -e 's/$$/\\n"/' | sed -e 's/^/  "/'\
	  >> $(patsubst %.txt,%.cc,$<)
	echo "};" >> $(patsubst %.txt,%.cc,$<)
     
#======================================================================
# pattern rules
# -------------

%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

-include $(patsubst %.cc,%.d,$(ALLSRC))

#======================================================================

tester DL1direct DL1logger: %: %.o $(SUBOBS)
	$(CXX) $(FLAGS) -o $@ $^ $(STATIC) -I$(LOCINCLUDEDIR) \
        -ldatrwxx -lsffxx -lgsexx -lserialxx -ltime++ -ltfxx -laff \
        -L$(LOCLIBDIR)

#======================================================================
# documentation part
# ------------------
DOXYWWWPATH=$(TF_WWWBASEDIR)/ThiesDL1
.PHONY: doxyclean doxyview doxybriefview doxyfullconf doxybriefconf

doxyclean: ;/bin/rm -rfv doc/* docbrief/* doc docbrief

DOXYSRC=$(wildcard *.cc doxy*.txt) $(HEADERS) cvsids.xxx

doc/%: doxygen.cfg $(DOXYSRC)
	mkdir -vp $(DOXYWWWPATH)
	/bin/rm -rfv doc/* doc
	ln -sfv $(DOXYWWWPATH) doc
	doxygen $<

doxyfull: doc/html/index.html
doxydoc: doxyfull

doxyfullview: doxyfull; $(TF_BROWSER) file:$(DOXYWWWPATH)/html/index.html &
doxyview: doxyfullview 

#======================================================================
# documentation
# -------------
# extract version string
version.xxx: version.h
	grep "define DL1_LOGGER_SOFTWARE_VERSION" $< \
	  | cut -f 2 -d '"' > $@
DL1logger.xxx: ; /bin/bash -c "DL1logger -xhelp 2>&1" > $@
DL1direct.xxx: ; /bin/bash -c "DL1direct -help 2>&1" > $@
DL1work=$(HOME)/git/Seitosh/src/conv/ThiesDL1
DL1logscript=$(DL1work)/DL1logger.sh
DL1logscript.xxx: $(DL1logscript); /bin/cp -vpd $< $@
DL1launchscript=$(DL1work)/launchDL1logger.sh
DL1launchscript.xxx: $(DL1launchscript); /bin/cp -vpd $< $@
DL1ntpscript=$(DL1work)/ntpreport.sh
DL1ntpscript.xxx: $(DL1ntpscript); /bin/cp -vpd $< $@
DL1recording.tex: DL1logger.xxx DL1direct.xxx DL1logscript.xxx \
  DL1launchscript.xxx DL1ntpscript.xxx version.xxx

$(patsubst %.tex,%,$(wildcard *.tex)): %: %.dvi

%.eps: %.ps; ps2epsi $< $@
%.pdf: %.eps; epstopdf $<


%.dvi: %.tex; latex $(patsubst %.tex,%,$<)

%.ps: %.dvi; dvips -ta4 $(patsubst %.dvi,%,$<)
%.pdf: %.tex; pdflatex $(patsubst %.tex,%,$<)

%.x: %.dvi; xdvi $(patsubst %.dvi,%,$<) &
%.psp: %.ps; gv $<
%.pdp: %.pdf; acroread $<

#======================================================================
# create package
# --------------
# is delegated to Makefile.packages
ifdef TF_MAKEPKG
.PHONY: package
# export PACKAGELINKSFORTRAN=yes
# export PACKAGENEEDSPGPLOT=yes
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=ThiesDL1 \
	  PACKAGEEXPORT="trunk/src/conv/ThiesDL1:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=ThiesDL1withlibs \
	  PACKAGEEXPORT="trunk/src/conv/ThiesDL1:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="libaff libtime libtfxx libgsexx libsffxx libdatrwxx libserialxx"
endif

# ----- END OF Makefile ----- 
