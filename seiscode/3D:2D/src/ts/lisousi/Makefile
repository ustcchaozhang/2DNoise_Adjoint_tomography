# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2013 by Thomas Forbriger (BFO Schiltach) 
# 
# lisousi: Line Source Simulation
#
# ----
# lisousi is free software; you can redistribute it and/or modify
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
#    17/04/2013   V1.0   Thomas Forbriger
# 
# ============================================================================
#

PROGRAMS=lisousi

.PHONY: all
all: install doc

.PHONY: doc
doc: doxydoc

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)

FLAGS += $(MYFLAGS)
CFLAGS += -O2 
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)

# use STATIC=-static to produce statically linked binaries
STATIC=
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS+=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS+=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
     $(FLAGS)

#----------------------------------------------------------------------
# standard edit targets

EDITFILES=examples/README Makefile README $(wildcard *.cfg) COPYING doxygen.txt
DESCRIPTIONTXT=$(filter-out $(EDITFILES),$(wildcard *.txt))
EDITSRC=$(filter-out $(EDITFILES) \
	  $(patsubst %.txt,%.h,$(DESCRIPTIONTXT)) \
	  $(patsubst %.txt,%.cc,$(DESCRIPTIONTXT)),\
	  $(wildcard *.cc *.h *.c *.f *.gpt *.inc))

flist: $(EDITSRC) doxydoc.cfg README COPYING $(EDITFILES) $(TF_EDIT) \
  $(DESCRIPTIONTXT)
	echo $(filter $(EDITFILES),$^) | tr ' ' '\n' | sort > $@
	echo "----" >> $@
	echo $(filter $(EDITSRC),$^) | tr ' ' '\n' | sort >> $@
	echo "----" >> $@
	echo $(filter $(DESCRIPTIONTXT),$^) | tr ' ' '\n' | sort >> $@
	echo "----" >> $@
	echo $(filter-out $(EDITFILES) $(DESCRIPTIONTXT) $(EDITSRC),$^) \
	  | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o *.xxx.* *.ps *~ $(PROGRAMS) *.d
	-/bin/rm -fv $(patsubst %.txt,%.h,$(DESCRIPTIONTXT))
	-/bin/rm -fv $(patsubst %.txt,%.cc,$(DESCRIPTIONTXT))

# ======================================================================
# pattern rules
# -------------

#----------------------------------------------------------------------
# description and online texts
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

#----------------------------------------------------------------------
# dependencies
%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

include $(patsubst %.txt,%.d,$(DESCRIPTIONTXT)) 
include $(patsubst %.cc,%.d,$(wildcard *.cc)) 

# ======================================================================
# compile and link lisousi
# ------------------------
#

lisousi: \
  %: $(patsubst %.txt,%.o,$(DESCRIPTIONTXT)) \
     $(patsubst %.cc,%.o,$(wildcard *.cc))
	$(CXX) -o $@ $^ -I$(LOCINCLUDEDIR) -lfourierxx -lfftw3 -lm \
                  -lsffxx -ldatrwxx -llinearxx -lgsl -lgslcblas \
                  -ltsxx -ltfxx -lsffxx -lgsexx -ltime++ -laff \
                  -lsffxx -lm\
                  -L$(LOCLIBDIR) $(CXXFLAGS) $(FLAGS) $(LDFLAGS)

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

DOXYWWWPATH=$(TF_WWWBASEDIR)/lisousi

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH)

DOXYSRC=README $(wildcard *.h *.cc *.f *.txt) 

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
# pass PACKAGESRCONLY=yes for a source code package
ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
        PACKAGE=lisousi \
        PACKAGEEXPORT="trunk/src/ts/lisousi:src" \
        PACKAGETARGETS="src:all" \
        PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
        PACKAGE=lisousiwithlibs \
        PACKAGEEXPORT="trunk/src/ts/lisousi:src" \
        PACKAGETARGETS="src:install" \
	PACKAGELIBS="libaff:libtime:libseife:libfourier:liblinearxx:libgsexx:libdatrwxx:libsffxx:libtsxx:libtfxx"
endif

# ----- END OF Makefile ----- 
