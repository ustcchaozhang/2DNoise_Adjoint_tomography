# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
# 
# gremlin tools
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
# available programs:
# -------------------
#   ires.f
#     calculate source time function from Fourier expansion coefficients
#   resi.f
#     calculate Fourier expansion coefficients from a time series provided as
#     source time function (output has the format produced by gremlin)
#   grestf.cc
#     calculate least optimal source time function by a least-squares
#     regression using Fourier-Bessel coefficients of observed data
#     (calculated by grade) and synthetic coefficients (calculated by syg)
#     the output is written in the format produced by gremlin
#
# model file format conversion 
#   mocon.f
#     refmet discrete to gremlin polynomial and vice versa
#   mocox.cc
#     gremlin polynomial to GEMINI polynomial or nodal format
#   motab.f
#     produce an ASCII table of model parameters from a polynomial model
#   moval.f
#     evaluate polynomial model at a given depth
#   pmotra.f
#     plot polynomial model together with synthetic and read travel times
#   polex.f
#     find appropriate polynomial coefficients for given parameter values
#   splimo.f
#     add additional section to polynomial model file
#
# polynomial model file manipulation (gremlin format)
#   clemo.f
#     create new template for polynomial model file or clean existing file
#   mop.f
#     plot polynomial model
#   mops.f
#     create V_S model from V_P model
#   moshift.f
#     modify model incrementally
#
# discrete model file manipulation (refmet format)
#   moche.f
#     check model parameters for physical feasibility
#
# Required libraries:
# -------------------
#    polex requires:
#      libtf
#    moche requires:
#      libtf, librefpar
#    moshift, moval, motab, mops, clemo, and splimo require:
#      libgin, libtf
#    mocon requires: 
#      libtf, librefpar, libgin
#    mop and pmotra require:
#      libgin, libtf, libpgplot
#    mocoxx and grestf require:
#	 libgremlin1xx, libfourierxx, libtfxx, libaff 
#
#   libgin, libgremlin1xx.a: 
#     is available from directory src/green/gremlin1
#     just call 'make libraries' in directory src/green/gremlin1 of your
#     working copy
#   libtf, libtfxx, libaff, libfourierxx, librefpar:
#     are available from directory src/libs
#     just call 'make install' in directory src/libs of your working copy
#   libpgplot:
#     is expected to be installed from external
# 
# REVISIONS and CHANGES 
#    09/02/2010   V1.0   Thomas Forbriger
#    10/11/2010   V1.1   only grereso depends on libnumrec
#    13/11/2010   V1.2   moved grereso to disan
#    10/11/2014   V1.3   librefpar replaces librefread
# 
# ============================================================================
#
PROGRAMS=polex moche moval motab clemo splimo mocon mop pmotra mocox grestf \
	   ires mops moshift resi resix iresx

.PHONY: all
all: install

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)
	
# check mandatory environment variable settings
# ---------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)

LINLIB=-llapack -lblas

FLAGS += $(MYFLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += $(FLAGS)
LDFLAGS+=-L$(LOCLIBDIR)
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)

AR=ar
RANLIB=ranlib

LIBGIN=-lgin

CXXSRC=mocox.cc grestf.cc

#----------------------------------------------------------------------

flist: Makefile $(wildcard *.f *.inc *.cc)
	echo $^ | tr ' ' '\n' | sort > $@
.PHONY: edit
edit: flist; vim $<

clean:
	-/bin/rm *.o *.bak *.o77 *.d

#----------------------------------------------------------------------
# Fortran dependencies
# --------------------
%.d: %.f
	echo $<: $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
include $(patsubst %.f,%.d,$(wildcard *.f))

%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $(TMPPARDEF) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

include $(patsubst %.cc,%.d,$(CXXSRC)) 

../libs/%.inc:
	cd $(dir $@); $(MAKE) $(notdir $@)

#----------------------------------------------------------------------

%.o: %.f
	$(FC) -o $@ -c $< $(FFLAGS)

resi ires: %: %.o
	$(FC) -o $@ $^ $(FFLAGS) -ltf -lsffu -ltime -lsff $(LDFLAGS)

resix iresx: %x: %.o
	$(FC) -o $@ $^ $(FFLAGS) -ltf -lsffu -ltime \
	  -lfapidxx -ldatrwxx -lsffxx -lgsexx -ltime++ -laff $(LDFLAGS)

polex: %: %.o
	$(FC) -o $@ $^ $(FFLAGS) -ltf  $(LDFLAGS)

moche: %: %.o
	$(FC) -o $@ $^ $(FFLAGS) -lrefpar -ltf  $(LDFLAGS)

moshift moval motab mops clemo splimo: %: %.o
	$(FC) -o $@ $^ $(FFLAGS) -lgin -ltf  $(LDFLAGS)

mocon: %: %.o
	$(FC) -o $@ $^ $(FFLAGS) -lgin -lrefpar -ltf  $(LDFLAGS)

mop pmotra: %: %.o
	$(FC) -o $@ $^ $(FFLAGS) -lgin -ltf \
	  $(LDFLAGS) $(TF_LINK_PGPLOT)

$(CXXSRC:.cc=): %: %.o
	$(CXX) -o $@ $< $(LDFLAGS) -lgremlin1xx -lfourierxx -ltfxx -laff 

# ----- END OF Makefile ----- 
