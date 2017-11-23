# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
# 
# waveform plot programs
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
# targets:
#   stuplo:		plot seismogram waveforms
#   splot:		simple spectral analysis (plot amplitude spectrum)
#   susplo:		plot stacked amplitude spectra
#   pamo:		plot particle motion 
#   damplo:		plot spatial decay of wave energy
#
# targets available with libfapidxx interface:
#   stuplox, damplox, pamox
# 
# REVISIONS and CHANGES 
#    30/01/2007   V1.0   Thomas Forbriger
#    07/12/2007   V1.1   default Fortran compiler now is g77
#    17/12/2007   V1.2   moved to gfortran
#    08/10/2010   V1.3   migrate to SVN environment
#                        - discard f2c option
#    13/01/2011   V1.4   - remove polplot from list of standard targets
#                          polplot has never been realized
#                        - add target stuplox
#                        - damplox was ready to be linked, since it uses
#                          libsffu simple read functions
#    04/11/2015   V1.5   - provide pamox
# 
# ============================================================================
#

PROGRAMS=damplo pamo splot stuplo susplo stuplox damplox pamox

.PHONY: all
all: install

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

#----------------------------------------------------------------------
# check environment
CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_LINK_PGPLOT)

PGPLOTLIB=$(TF_LINK_PGPLOT)

FLAGS += $(MYFLAGS)
CFLAGS += -O2 $(FLAGS)

# use STATIC=-static to produce statically linked binaries
STATIC=
#CXXFLAGS += -fhonor-std -Wall $(FLAGS)
CXXFLAGS += -Wall $(FLAGS)
LDFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR))) $(STATIC)
CPPFLAGS=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)

#----------------------------------------------------------------------
# standard edit targets
flist: Makefile $(wildcard *.f *.cc) COPYING README
	echo $^ | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist $(PROGRAMS)
	-/bin/rm *.o *.bak *.o77 
#----------------------------------------------------------------------
# pattern rules

%.o: %.f
	$(FC) -o $@ -c $< $(FFLAGS)

# targets
stuplo splot susplo: %: %.o
	$(FC) $< -o $@ -ltf -lsff -lsffu -ltime $(PGPLOTLIB) $(LDFLAGS)
pamo damplo: %: %.o
	$(FC) $< -o $@ -ltf -lsffu -ltime -lsff $(PGPLOTLIB) $(LDFLAGS)
stuplox: %x: %.o
	$(FC) $< -o $@ -ltf $(PGPLOTLIB) $(LDFLAGS) \
	  -lfapidxx -ldatrwxx -lsffxx -lgsexx -laff -lsffu -ltime++
pamox damplox: %x: %.o
	$(FC) $< -o $@ -ltf -lsffu -ltime \
	  -lfapidxx -ldatrwxx -lsffxx -lgsexx -ltime++ -laff \
	  $(PGPLOTLIB) $(LDFLAGS)

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
	  PACKAGE=plotprograms \
	  PACKAGEEXPORT="trunk/src/ts/plot:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=plotprogramswithlibs \
	  PACKAGEEXPORT="trunk/src/ts/plot:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="libsff libtf libsffu libts libaff libtime libfapidxx libgsexx libsffxx libdatrwxx libtfxx libtsxx"
endif

# ----- END OF Makefile ----- 
