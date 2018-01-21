# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
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
#    15/09/2008   V1.1   moved to gfortran
#    08/10/2010   V1.2   migrate to new SVN environment
#                        discard f2c fallback option
#                        check environment
#                        discard package creation
#    15/11/2010   V1.3   replaced incdep by explicite rule
# 
# ============================================================================
#

PROGRAMS=grepg

.PHONY: all
all: install

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_LINK_PGPLOT)

FLAGS += $(MYFLAGS)
CFLAGS += -O2 $(FLAGS)
#FFLAGS += -ff2c -Wall -ffixed-line-length-none $(FLAGS) -frecord-marker=4
# setting binary markers to 4 for gfortran 4.1 prevents reading the PGPLOT
# FONT file
FFLAGS += -ff2c -Wall -ffixed-line-length-none $(FLAGS) 

# use STATIC=-static to produce statically linked binaries
STATIC=
LDFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR))) $(STATIC)
CPPFLAGS=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

#----------------------------------------------------------------------
# standard edit targets
.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist
	-/bin/rm -vf *.o *.bak *.o77 flist *.f2.o *.d

.PHONY: edit
edit: flist ; vim flist
flist: *.f *.inc Makefile README COPYING\
  README.PGPLOT.copyright.notice
	echo $^ | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@
#----------------------------------------------------------------------

GREBOBS=grepg.o grepg_message.o grepg_dopicks.o grepg_selstyle.o \
  grepg_phase.o grepg_phasewedg.o grepg_poly.o grepg_remavg.o \
  grepg_contr.o grepg_prepcol.o grepg_readfourier.o grepg_phasor.o

%.d: %.f
	echo $<: $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
-include $(patsubst %.o,%.d,$(GREBOBS))

# only the original is in SVN
# so we can change the current dimensions without affecting the SVN
grepg_ardim.xxx.inc: grepg_ardim.inc; /bin/cp -v $< $@

%.o: %.f; $(FC) -c -o $@ $< $(FFLAGS)
grepg: $(GREBOBS)
	$(FC) $(FFLAGS) $^ -o $@ \
        -ltf $(TF_LINK_PGPLOT) $(LDFLAGS)

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
	  PACKAGE=grepg \
	  PACKAGEEXPORT="trunk/src/green/grepg:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=grepgwithlibs \
	  PACKAGEEXPORT="trunk/src/green/grepg:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="libtf"
endif

# ----- END OF Makefile ----- 
