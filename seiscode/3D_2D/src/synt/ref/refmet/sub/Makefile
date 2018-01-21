# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
# 
# subroutines and functions for reflectivity programs
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
#    24/09/2014  thof:   move part of the source code to librefpar
# 
# ============================================================================
#

LIBRARIES=librefread.a

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

#----------------------------------------------------------------------
# standard edit targets
flist: Makefile $(wildcard *.f *.c) COPYING README
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

clean:
	-/bin/rm *.o *.bak *.o77 *.f2c.o

#----------------------------------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)

#----------------------------------------------------------------------

FLAGS += $(MYFLAGS)
CFLAGS += -O2 $(FLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-none $(FLAGS) \
  -fimplicit-none -pedantic -std=legacy

# use STATIC=-static to produce statically linked binaries
STATIC=
CXXFLAGS=-Wall $(FLAGS)
LDFLAGS=-L$(LOCLIBDIR) $(STATIC)
CPPFLAGS=-I$(LOCINCLUDEDIR) $(FLAGS)

#----------------------------------------------------------------------
 
# install-include is provided for compatibility with other Makefile
# but is not used here
.PHONY: install-include
install-include:

#----------------------------------------------------------------------

LIBREFMETOBS=refmet_basinf.o refmet_rmain.o

librefread.a: $(LIBREFMETOBS)
	ar rcv librefread.a $(LIBREFMETOBS)
	ranlib librefread.a

# ----- END OF Makefile ----- 
