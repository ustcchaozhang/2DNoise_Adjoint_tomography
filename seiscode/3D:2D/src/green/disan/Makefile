# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
# 
# some code for dispersion analysis
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
#    05/06/2007   V1.0   Thomas Forbriger
#    08/10/2009   V1.1   moved to gfortran
#    16/07/2010   V1.2   use correct libpgplot
#    08/10/2010   V1.3   migrate to SVN environment
#                        discard f2c option (fallback solution)
#                        check environment
#                        discard package creation
#    10/11/2010   V1.4   only greda requires libnumrec
#    13/11/2010   V1.5   replace libnumrec by GSL
# 
# ============================================================================

PROGRAMS=greda gabor phadi hivexpanco hivgabor grereso gaborx phadix gredax

.PHONY: all
all: install

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(dir $@)
	/bin/mv -fv $< $@

#----------------------------------------------------------------------
# standard edit targets
.PHONY: clean edit

EDITFILES=Makefile $(wildcard *.f *.inc) COPYING README
flist: Makefile $(wildcard *.f *.inc) COPYING README $(TF_EDIT)
	echo $(EDITFILES) | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(filter-out $(EDITFILES),$^) | tr ' ' '\n' | sort >> $@

edit: flist
	vim $<

clean:
	-/bin/rm -vf *.o *.bak *.d flist $(PROGRAMS)

#----------------------------------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_LINK_PGPLOT)

LINLIB=-llapack -lblas
BUGGY=
PGPLOTLIB=$(TF_LINK_PGPLOT)
CFLAGS +=-O2 
GFLAG=
#FFLAGS += -ff2c -Wall -ffixed-line-length-none $(FLAGS) -frecord-marker=4
# setting binary markers to 4 for gfortran 4.1 prevents reading the PGPLOT
# FONT file
FFLAGS += -ff2c -Wall -ffixed-line-length-none $(FLAGS) 
LDFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

%.o: %.f; $(FC) -c -o $@ $< $(FFLAGS)

# Fortran dependencies
# --------------------
%.d: %.f
	echo $<: $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
-include $(patsubst %.f,%.d,$(wildcard *.f))

greda: greda.o greda_phasor.o
	$(FC) -o $@ $^ -lsffu -lts -ltf $(LINLIB) \
        -lgsl -lgslcblas -ltime -lsff $(LDFLAGS)

grereso: grereso.o
	$(FC) -o $@ $^ -lsffu -lts -ltf $(LINLIB) \
        -lgsl -lgslcblas -ltime -lsff $(LDFLAGS)

hivexpanco hivgabor gabor phadi: %: %.o
	$(FC) -o $@ $<  -lsffu -lts -ltf $(LINLIB) \
        -ltime -lsff $(PGPLOTLIB) $(LDFLAGS)

# binaries linked against libfapidxx and colleagues, being able to read SU
# data and other formats:
gredax: greda.o greda_phasor.o
	$(FC) -o $@ $^ -lsffu -lts -ltf $(LINLIB) \
        -lgsl -lgslcblas -ltime -lfapidxx -ldatrwxx -lsffxx -lgsexx\
	  -ltime++ -laff $(LDFLAGS)
gaborx phadix: %x: %.o
	$(FC) -o $@ $<  -lsffu -lts -ltf $(LINLIB) \
        -ltime -lfapidxx -ldatrwxx -lsffxx -lgsexx -laff -ltime++ \
	  $(PGPLOTLIB) $(LDFLAGS)


.PHONY: list-libraries
list-libraries:
	grep ' -l' Makefile | tr ' ' '\n' | egrep '^-l' |sort | uniq
	grep ' -l' Makefile | tr ' ' '\n' | egrep 'PGP' |sort | uniq



#======================================================================
# create package
# --------------
# is delegated to Makefile.packages
ifdef TF_MAKEPKG
.PHONY: package
# export PACKAGELINKSFORTRAN=yes
export PACKAGENEEDSPGPLOT=yes
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=disan \
	  PACKAGEEXPORT="trunk/src/green/disan:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=disanwithlibs \
	  PACKAGEEXPORT="trunk/src/green/disan:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="libsff libfapidxx libtf libsffu libts libaff libtime libgsexx libsffxx libdatrwxx"
endif

# ----- END OF Makefile ----- 
