# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
# 
# calculate Green's function expansion coefficients
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
#    15/09/2008   V1.0   Thomas Forbriger
#                        use gfortran
#    17/11/2010   V1.1   migrated to new SVN scheme
#    18/01/2011   V1.2   implememnted gresyx
# 
# ============================================================================
#
#PROGRAMS=dig gresy gresynoise syg rhesyg wsyg gresyx
PROGRAMS=dig gresy gresynoise syg rhesyg gresyx

.PHONY: all
all: install

$(LOCBINDIR): ; mkdir -pv $@
.PHONY: install
install: $(LOCBINDIR)
	$(MAKE) $(PROGRAMS)
	/bin/mv -fv $(PROGRAMS) $(LOCBINDIR)
	 
# check mandatory environment variable settings
# ---------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)

FLAGS += $(MYFLAGS)
CFLAGS += -O2 $(FLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-none $(FLAGS)

# use STATIC=-static to produce statically linked binaries
STATIC=
CXXFLAGS=-fhonor-std -Wall $(FLAGS)
CXXFLAGS=-Wall $(FLAGS)
LDFLAGS=-L$(LOCLIBDIR) $(STATIC)
CPPFLAGS=-I$(LOCINCLUDEDIR) $(FLAGS)

#----------------------------------------------------------------------

clean:
	-/bin/rm *.o *.bak *.o77 *.odbg *.f2c.o *.d $(PROGRAMS)
	echo $(patsubst %.f,%.c,$(wildcard *.f)) | \
        xargs --no-run-if-empty rm -fv

flist: $(wildcard *.f *.inc) Makefile README COPYING
	echo $^ | tr ' ' '\n' | sort > $@
edit: flist; vim $<

#----------------------------------------------------------------------

%.d: %.f
	echo $<: $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
-include $(patsubst %.f,%.d,$(wildcard *.f))

#----------------------------------------------------------------------

%.o: %.f; $(FC) -c -o $@ $< $(FFLAGS)

dig: dig.o
	$(FC) -o $@ $< \
        -ltf -L$(LOCLIBDIR) 

wsyg: syg.o
	$(FC) -o $@ $< -lwrefsub -lrefpar \
        -ltf -L$(LOCLIBDIR) 

# gevas version
# currently not supported
gsyg: syg.o
	$(FC) -o $@ $< -lgrgevsub -lflgevas -lwo -lrefpar \
        -ltf -L$(LOCLIBDIR) 

syg: syg.o
	$(FC) -o $@ $< -lgrrefsub -lrefpar \
        -ltf -L$(LOCLIBDIR)

gresynoise: gresynoise.o nyquist_check.o
	$(FC) -o $@ $^ -lgrrefsub -lrefpar \
        -ltf -lgsl -lgslcblas -lsff -L$(LOCLIBDIR)

gresy: gresy.o nyquist_check.o
	$(FC) -o $@ $^ -lgrrefsub -lrefpar -lsffu \
        -ltf -lsff -ltime -L$(LOCLIBDIR)

rhesyg: rhesyg.o
	$(FC) -o rhesyg rhesyg.o -lgrrefsub -lrheology -lrefpar \
        -ltf -L$(LOCLIBDIR)

# analytical section
# currently not supported
afehsyg: syg.o
	$(FC) -o $@ $< -lanafsexphyd -lrefpar \
        -ltf -L$(LOCLIBDIR) 

gresyx: gresy.o nyquist_check.o
	$(FC) -o $@ $^ -lgrrefsub -lrefpar -lsffu  -ltf \
        -lfapidxx -ldatrwxx -lsffxx -lgsexx -ltime++ -laff -L$(LOCLIBDIR)

#======================================================================
# create package
# --------------
# is delegated to Makefile.packages
ifdef TF_MAKEPKG
.PHONY: package
# export PACKAGELINKSFORTRAN=yes
export PACKAGENEEDSPGPLOT=NO
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=greensynt \
	  PACKAGEEXPORT="trunk/src/green/synt:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=greensyntwithlibs \
	  PACKAGEEXPORT="trunk/src/green/synt:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="libaff libtime libgsexx libsffxx libtfxx \
	    libdatrwxx libfapidxx \
	    libgrrefsub librefpar librheology libsff libsffu libtf \
	    libwrefsub"
endif

# ----- END OF Makefile ----- 
