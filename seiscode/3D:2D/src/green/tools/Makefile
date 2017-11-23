# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
# 
# some tools to manipulate green files
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
#    27/03/2002   V1.0   Thomas Forbriger
#    09/02/2010   V1.1   move to gfortran
#    17/11/2010   V1.2   migrated to SVN scheme
# 
# ============================================================================
#

PROGRAMS=gredi grenorm gretap greto magres

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

FLAGS += $(MYFLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += $(FLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS+=-L$(LOCLIBDIR)
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)

#----------------------------------------------------------------------

flist: Makefile README COPYING $(wildcard *.f *.inc)
	echo $^ | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o *.d

#----------------------------------------------------------------------
# Fortran dependencies
# --------------------
%.d: %.f
	echo $<: $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
-include $(patsubst %.f,%.d,$(wildcard *.f))

$(PROGRAMS): %: %.f
	$(FC) $(FFLAGS) -o $@ $^ -ltf -lsff $(LDFLAGS)

#======================================================================
# create package
# --------------
# is delegated to Makefile.packages
ifdef TF_MAKEPKG
# PACKAGELINKSFORTRAN=yes
# PACKAGENEEDSPGPLOT=yes
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
          PACKAGE=greentools \
          PACKAGEEXPORT="trunk/src/green/tools:src" \
          PACKAGETARGETS="src:all" \
          PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
          PACKAGE=greentoolswithlibs \
          PACKAGEEXPORT="trunk/src/green/tools:src" \
          PACKAGETARGETS="src:all" \
          PACKAGELIBS="libaff libtf libsff"
endif


# ----- END OF Makefile ----- 
