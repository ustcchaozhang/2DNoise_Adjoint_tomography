# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2009 by Thomas Forbriger (BFO Schiltach) 
# 
# fidase: prepare a homogeneous data set from individual shot files
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
#    08/04/2009   V1.0   Thomas Forbriger
#    15/11/2010   V1.1   reworked makefile; avoid makedoc.tcl and incdep
#    14/01/2011   V1.2   implemented fidasex with libfapidxx interface
# 
# ============================================================================
#
 
PROGRAMS=fidase fidasex

.PHONY: all
all: install fidase.doc

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_LINK_PGPLOT)

LINLIB=-lblas -llapack
PGPLOTLIB=$(TF_LINK_PGPLOT)
CFLAGS= -I${LOCINCLUDEDIR} 
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
LDFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
#----------------------------------------------------------------------

.PHONY: clean edit

EDITFILES=Makefile COPYING README
flist: $(EDITFILES) $(FIDAOBS:.o=.f) *.inc
	echo $(EDITFILES) | tr ' ' '\n' | sort > $@
	echo $(FIDAOBS:.o=.f) | tr ' ' '\n' | sort >> $@
	echo *.inc | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

edit: flist; vim $<

clean:
	-/bin/rm -vf *.o *.bak *.d $(PROGRAMS)

#----------------------------------------------------------------------

FIDAOBS=fidase.o fidase_readdata.o fidase_skipdata.o fidase_writedata.o \
  fidase_readtaper.o fidase_evaltaper.o fidase_enfit.o fidase_resamp.o \
  fidase_destack.o

#----------------------------------------------------------------------

%.d: %.f
	echo $(patsubst %.f,%.o,$<): $< $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
include $(patsubst %.o,%.d,$(FIDAOBS))

#----------------------------------------------------------------------

fidase.doc: $(sort $(patsubst %.o,%.f,$(FIDAOBS)))
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^cS/ { hot=1; \
	    print FILENAME; \
          print "  c\n  c"; \
          next; } \
        /^cE/ { hot=0; } \
        { if (hot==1) { print "  " $$0; } }' $^ > $@

#----------------------------------------------------------------------

%.o: %.f
	$(FC) -o $@ -c $< $(FFLAGS)

fidase: $(FIDAOBS)
	$(FC) -o $@ $^ \
        -ltf -lts -lsffu -ltime_trad -lsff \
        $(PGPLOTLIB) $(LINLIB) \
        $(LDFLAGS) -L$(LOCLIBDIR) 

fidasex: $(FIDAOBS)
	$(FC) -o $@ $^ \
        -ltf -lts -lsffu -ltime_trad  \
	  -lfapidxx -ldatrwxx -lsffxx -lgsexx -ltime++ -laff \
        $(PGPLOTLIB) $(LINLIB) -lstdc++\
        $(LDFLAGS) -L$(LOCLIBDIR) 

.PHONY: list-libraries
list-libraries:
	grep ' -l' Makefile | tr ' ' '\n' | egrep '^-l' |sort | uniq
	grep ' -l' Makefile | tr ' ' '\n' | egrep 'PGP' |sort | uniq

# 
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
          PACKAGE=fidase \
          PACKAGEEXPORT="trunk/src/ts/fidase:src" \
          PACKAGETARGETS="src:all" \
          PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
          PACKAGE=fidasewithlibs \
          PACKAGEEXPORT="trunk/src/ts/fidase:src" \
          PACKAGETARGETS="src:all" \
          PACKAGELIBS="libsff libts libtf libsffu libaff libtime libgsexx libsffxx libdatrwxx libfapidxx"
endif
#


# ----- END OF Makefile ----- 
