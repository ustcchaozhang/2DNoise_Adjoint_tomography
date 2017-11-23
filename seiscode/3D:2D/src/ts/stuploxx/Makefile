# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
# 
# stuploxx extended plot project
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
# REVISIONS and CHANGES 
#    25/01/2008   V1.0   Thomas Forbriger (thof)
#    12/10/2010   V1.1   migrate to SVN environment
#                        - remove f2c fallback option
#                        - check environment
#                        - doxygen reworked
#    18/01/2011   V1.2   new install target
#    01/02/2014 thof:	 link against libtsioxx.a
#    17/03/2015 thof:	 adjust libpgplotxx interface
# 
# ============================================================================
#

PROGRAMS=stuploxx

.PHONY: all
all: install doxydoc

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_BROWSER TF_WWWBASEDIR)
$(call CHECKVARS,TF_LINK_PGPLOT TF_LINK_FORTRAN)

FLAGS += $(MYFLAGS)
CFLAGS += -O2 $(FLAGS)

STATIC=
CXXFLAGS += -Wall $(FLAGS)
FCLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)

LDFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR))) $(STATIC)
CPPFLAGS=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

DESCRIPTIONTXT= \
  description_text.txt \
  examples_text.txt \
  help_text.txt \
  interactive_text.txt \
  usage_text.txt
SRC= \
  constants.cc \
  constantskeys.cc \
  constantsoptions.cc \
  datafile.cc \
  datatrace.cc \
  functions.cc \
  globalsettings.cc \
  label.cc \
  panel.cc \
  scaling.cc \
  screen.cc \
  stuploxx.cc \
  utilitystructures.cc \
  windowpanel.cc 
HEADER= \
  constants.h \
  datafile.h \
  datatrace.h \
  functions.h \
  globalsettings.h \
  label.h \
  panel.h \
  scaling.h \
  screen.h \
  utilitystructures.h \
  windowpanel.h
EXTRAFILES= \
  stuploxx.sh \
  COPYING \
  doxydoc.cfg \
  Makefile \
  README \
  README.doxydoc \
  README.flow \
  README.notes \
  README.struct
OBJ=$(patsubst %.cc,%.o,$(SRC) $(patsubst %.txt,%.cc,$(DESCRIPTIONTXT)))

#----------------------------------------------------------------------
# standard edit targets
flist: $(DESCRIPTIONTXT) $(SRC) $(HEADER) $(EXTRAFILES)
	echo $(SRC) $(HEADER) | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(DESCRIPTIONTXT) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(EXTRAFILES) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist
	-/bin/rm *.o *.bak *.o77 example.* *.d

#======================================================================
# dependencies
# ------------

%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

include $(patsubst %.txt,%.d,$(DESCRIPTIONTXT))
include $(patsubst %.cc,%.d,$(SRC))

#======================================================================
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
	cat $< | egrep -v '^#' | sed -e 's/"/\\"/g' \
	  | sed -e 's/$$/\\n"/' | sed -e 's/^/  "/'\
	  >> $(patsubst %.txt,%.cc,$<)
	echo "};" >> $(patsubst %.txt,%.cc,$<)

#======================================================================

stuploxx: $(OBJ)
	$(CXX) -o $@ $^ -lpgplotxx -lcpgplot \
         $(TF_LINK_PGPLOT) $(TF_LINK_FORTRAN) -ltsioxx \
        -ltfxx -ltsxx -ldatrwxx -lsffxx -lgsexx -ltime++ -laff \
        $(LDFLAGS) $(CXXFLAGS) $(FLAGS)

#======================================================================

example.sff: ; tesiff $@
exampleshift.sff: ; tesiff -s $@ 
%.sh.sff: %.sff; sehefi $< $@ -v -c BHX -s BFX -a AUX -i STSX

example.ps: example.sff exampleshift.sh.sff stuploxx
	stuploxx -device $@/cps -labu -labe -labc -labr \
        $< u:"\\gmm/s\\u2" sf:1.5 a:lulatsch \
          n:"file %F %NF(%NT)" \
        $(word 2,$^) t:4 p:3 cs:1.0 s:9.5,12 ch:0.5 ci:4 lw:10 \
          u:"\\gmm/s\\u2" a:lulatsch \
          n:"%%%NF/%NT%%" \
        $(word 2,$^) t:6  cs:0.8 s:9.5,12 ch:0.2 u:"\\gmm/s\\u2" \
	  h:0.,0.5,0.8 ci:5 lw:3 ls:4 \
          n:"ST:%S CH:%C %UT" a:kratsch

example2.ps: example.sff stuploxx
	stuploxx -D -v -labh=0. -npanels 10 -device $@/cps -labe \
         $< t:2 p:3 junkfile $< t:2 p:6 $< t:3 p:6

example3.ps: example.sff stuploxx
	stuploxx -device $@/cps -chts 3. \
	  example.sff t:4 example.sff t:6 u:"my units"

%.psp: %.ps; gv $<; /bin/rm -fv $<

interactive: example.sff stuploxx
	stuploxx $< -interactive -hdevice pgp.ps/cps -hcommand="gv pgp.ps"

repeat: example.sff stuploxx
	stuploxx $< -repeat=2 -repcmd=date

help: stuploxx; $< -help 2>&1 | less

#======================================================================

$(call CHECKVARS,TF_WWWBASEDIR TF_BROWSER)

DOXYWWWPATH=$(TF_WWWBASEDIR)/stuploxx

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

#======================================================================
# create package
# --------------
# is delegated to Makefile.packages
ifdef TF_MAKEPKG
export PACKAGELINKSFORTRAN=yes
export PACKAGENEEDSPGPLOT=yes
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=stuploxx \
	  PACKAGEEXPORT="trunk/src/ts/stuploxx:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=stuploxxwithlibs \
	  PACKAGEEXPORT="trunk/src/ts/stuploxx:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="libtsioxx libpgplotxx libtfxx libtsxx\
	  libaff libtime libgsexx libsffxx libdatrwxx"
endif

# ----- END OF Makefile ----- 
