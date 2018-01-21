# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
# 
# time series data header manipulation
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
#    14/03/2002   V1.0   Thomas Forbriger
#    05/07/2007   V1.1   prepared for package creation
#    13/12/2007   V1.2   g77 is the standard Fortran compiler now
#    17/01/2011   V1.3   migrated to new SVN scheme
#                        implemented offlix and chacox
#    03/03/2012   V1.4   added tijerasxx (damb)
#    07/01/2013   V1.5   added sehefixx
#    03/01/2014   V1.6   added package rules
# 
# ============================================================================
#

PROGRAMS=sehefi coma epi chaco sesoc offli sesot merse cooset \
	   offlix chacox tijerasxx sehefixx

.PHONY: all
all: install

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

.PHONY: list-programs
list-programs:
	echo $(PROGRAMS) | tr ' ' '\n' | sort

#----------------------------------------------------------------------

TESTCASEMAKE=$(filter-out %.bak,$(wildcard testcases/Makefile*))

flist: Makefile COPYING README $(wildcard *.f *.cc) $(TESTCASEMAKE) \
  $(TF_EDIT)
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o
	-/bin/rm -vf $(PROGRAMS)

#----------------------------------------------------------------------

# check mandatory environment variable settings
# ---------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)

FLAGS += $(MYFLAGS) -fPIC
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += $(FLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

#----------------------------------------------------------------------

%.o: %.f; $(FC) -o $@ -c $(FFLAGS) $<

sehefi coma epi chaco sesoc: %: %.o
	$(FC) -o $@ $< -ltf -lsff $(LDFLAGS)

offli sesot merse: %: %.o
	$(FC) -o $@ $< -ltf -lsffu -ltime_trad -lsff \
	  $(LDFLAGS) -L$(LOCLIBDIR)

chacox: %x: %.o
	$(FC) -o $@ $< -ltf \
	  -lfapidxx -ldatrwxx -lsffxx -lgsexx -ltime++ -laff \
	  $(LDFLAGS) -L$(LOCLIBDIR)

offlix: %x: %.o
	$(FC) -o $@ $< -ltf -lsffu -ltime_trad \
	  -lfapidxx -ldatrwxx -lsffxx -lgsexx -ltime++ -laff \
	  $(LDFLAGS) -L$(LOCLIBDIR)

cooset sehefixx tijerasxx: %: %.o
	$(CXX) -o $@ $^ -I$(LOCINCLUDEDIR) -ldatrwxx \
            -lsffxx -ltime++ -lgsexx -laff -ltfxx \
            $(LDFLAGS) $(CXXFLAGS) $(FLAGS) $(LDFLAGS) -L$(LOCLIBDIR)


.PHONY: list-libraries
list-libraries:
	grep ' -l' Makefile | tr ' ' '\n' | egrep '^-l' | sort | uniq

#======================================================================
# create package
# --------------
# is delegated to Makefile.packages
ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=tshd \
	  PACKAGEEXPORT="trunk/src/ts/hd:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=tshdwithlibs \
	  PACKAGEEXPORT="trunk/src/ts/hd:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="libsff libtf libsffu libaff libtime libgsexx libsffxx libdatrwxx libfapidxx libtfxx libtsxx libseife"
endif

# ----- END OF Makefile ----- 
