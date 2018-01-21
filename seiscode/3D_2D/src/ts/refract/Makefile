# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2007, 2010 by Thomas Forbriger (BFO Schiltach) 
# 
# refract Makefile
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
#    21/06/2007   V1.0   Thomas Forbriger
#    13/12/2007   V1.1   g77 is now the default Fortran compiler
#    17/12/2007   V1.2   moved to gfortran
#    11/10/2010   V1.3   migrate to SVN environment
#                        - discard f2c option (fallback)
#                        - check environment
#    13/11/2010   V1.4   replaces shell script incdep
#                        added documentation
#    18/11/2010   V1.5   addad target refractx as a libfapidxx test case
#    21/11/2012   V1.6   include object file in traget list in .d files
# 
# ============================================================================
#
.PHONY: all
all: install doc

PROGRAMS=refract refractx

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

.PHONY: doc
doc: functions.doc

#----------------------------------------------------------------------
# standard edit targets
flist: $(wildcard *.f sub/*.f include/*.inc) Makefile COPYING README
	echo $^ | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

edit: flist
	vim $<

clean:
	-/bin/rm flist 
	-find . \( -name \*.o -o -name \*.bak -o -name \*.o77 \) \
	  | xargs --no-run-if-empty rm -v
	-find . \( -name \*.d \) \
	  | xargs --no-run-if-empty rm -v

#----------------------------------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_LINK_PGPLOT)

FLAGS += $(MYFLAGS)
CFLAGS +=  $(FLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
LDFLAGS+=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
    $(FLAGS)

REFSUB=refract_readdata.o refract_skipdata.o refract_setdefaults.o \
  refract_mpc.o refract_setfullrange.o refract_pgframe.o refract_settracevp.o \
  refract_pgtrace.o refract_setscale.o refract_doplot.o refract_loopaction.o \
  refract_pgframeact.o refract_setflags.o refract_setpara.o \
  refract_selstyle.o refract_pgnamscal.o refract_pgfilenames.o \
  refract_varplot.o refract_pgparameters.o refract_ttreduce.o \
  refract_message.o refract_dopicks.o refract_domodel.o refract_usage.o \
  refract_cmdopt.o refract_selfilestyle.o refract_preread.o \
  refract_pgdefaults.o refract_vpframe.o refract_plotoffset.o \
  refract_setrefrange.o refract_avgamp.o
REFOBS=refract.o $(addprefix sub/, $(REFSUB))

# Fortran dependencies
# --------------------
%.d: %.f
	echo $< $(patsubst %.f,%.o,$<): $(addprefix include/,$(patsubst include/%,%,$(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq))) > $@
include $(patsubst %.o,%.d,$(REFOBS))

#======================================================================
# function documentation
functions.doc: $(patsubst %.o,%.f,$(REFOBS))
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^cS/ { hot=1; \
	    print FILENAME; \
          print "  c\n  c -----------------------------------------\n  c"; \
          next; } \
        /^cE/ { hot=0; } \
        { if (hot==1) { print "  " $$0; } }' $^ > $@

#======================================================================
# compile and link
%.o: %.f
	$(FC) -o $@ -c $< -O2 $(FFLAGS) -Iinclude

refract: $(REFOBS)
	$(FC) -o refract $(REFOBS) \
        -ltf -lsffu -lts -ltime -lsff \
        $(TF_LINK_PGPLOT) $(LDFLAGS) \
        -L$(LOCLIBDIR) 

refractx: $(REFOBS)
	$(FC) -o $@ $(REFOBS) \
        -ltf -lsffu -lts -ltime \
	  -lfapidxx -ldatrwxx -lsffxx -lgsexx -ltime++ -laff -lstdc++ \
        $(TF_LINK_PGPLOT) $(LDFLAGS) \
        -L$(LOCLIBDIR) 

#======================================================================
# create package
# --------------
# is delegated to Makefile.packages
ifdef TF_MAKEPKG
# export PACKAGELINKSFORTRAN=yes
export PACKAGENEEDSPGPLOT=yes
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=refract \
	  PACKAGEEXPORT="trunk/src/ts/refract:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=refractwithlibs \
	  PACKAGEEXPORT="trunk/src/ts/refract:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="libseife:libsff:libtime:libaff:libsffu:libts:libgsexx:libsffxx:libdatrwxx:libtsxx:libtfxx:libtf:libfapidxx"
endif


# ----- END OF Makefile ----- 
