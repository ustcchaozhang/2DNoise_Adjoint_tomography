# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
# 
# tools related to reflectivity
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
#    03/02/2011   V1.0   Thomas Forbriger
# 
# ============================================================================
#
PROGRAMS= cmt2refmet momte queko rcvgen modrecipes flamops flamop gemmodpg \
	    chopmod

.PHONY: all
all: install

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

#----------------------------------------------------------------------
# check mandatory environment variable settings
# ---------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_LINK_PGPLOT)

FLAGS += $(MYFLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += $(FLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS+=-L$(LOCLIBDIR)
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)

#----------------------------------------------------------------------

flist: Makefile $(wildcard *.f) README COPYING
	echo $^ | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist $(PROGRAMS) *.o

#----------------------------------------------------------------------
# Fortran dependencies
%.d: %.f
	echo $< $(patsubst %.f,%.o,$<): \
        $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
include $(patsubst %.f,%.d,$(wildcard *.f))

#----------------------------------------------------------------------

cmt2refmet momte queko rcvgen: %: %.o
	$(FC) $< -o $@ -ltf -L$(LOCLIBDIR)

modrecipes: %: %.o
	$(FC) $< -o $@ -lrefpar -ltf -L$(LOCLIBDIR) 

flamops flamop gemmodpg: %: %.o
	$(FC) $< -o $@ -lemod -lrefpar -ltf $(TF_LINK_PGPLOT) -L$(LOCLIBDIR) 

# amppg: %: %.o
# 	$(FC) $< -o $@ -ltf $(LIBPGPLOT) \
#         $(F2CLIB) -L$(LOCLIBDIR)
# 	newprog $@
# 
# makesrc: makesrc.o
# 	$(FC) makesrc.o -o makesrc -ltf -ltfio -lsff \
#         $(F2CLIB) -L$(LOCLIBDIR)
# 	newprog makesrc
# 
# rcvgen cmt2refmet queko momte: %: %.o
# 	$(CC) $< -o $@ -ltf $(F2CLIB) -L$(LOCLIBDIR)
# 	newprog $@
# 
# modrecipes: %: %.o
# 	$(CC) $< -o $@ -ltf -lrefread $(F2CLIB) -L$(LOCLIBDIR)
# 	newprog $@
# 
chopmod: %: %.o
	$(FC) $< -o $@ -ltf -lemod $(F2CLIB) -L$(LOCLIBDIR)
# 
# resi ires: %: %.o
# 	$(CC) $< -o $@ -lsffu -lsff -ltf -ltime $(F2CLIB) -L$(LOCLIBDIR)
# 	newprog $@
# 
# gem2asc flamop flamops gemmodpg: %: %.o
# 	$(CC) -o $@ $< $(LIBEMOD) $(LIBTF) $(LIBPGPLOT) $(LIBF2C)
# 	newprog $@
#
#======================================================================
# create package
# --------------
### is delegated to Makefile.packages
##ifdef TF_MAKEPKG
##.PHONY: package
### PACKAGELINKSFORTRAN=yes
### PACKAGENEEDSPGPLOT=yes
##package: $(TF_MAKEPKG)
##	$(MAKE) -f $< \
##	  PACKAGE=convsff \
##	  PACKAGEEXPORT="trunk/src/conv/sff:src" \
##	  PACKAGETARGETS="src:all" \
##	  PACKAGELIBS="-"
##.PHONY: fullpackage
##fullpackage: $(TF_MAKEPKG)
##	$(MAKE) -f $< \
##	  PACKAGE=convsffwithlibs \
##	  PACKAGEEXPORT="trunk/src/conv/sff:src" \
##	  PACKAGETARGETS="src:all" \
##	  PACKAGELIBS="libsff libtf libsffu libts libaff libtime libgsexx libsffxx libdatrwxx libtfxx libtsxx"
##endif
##
# ----- END OF Makefile ----- 
