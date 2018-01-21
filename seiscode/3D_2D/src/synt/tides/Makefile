# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Makefile fuer tools /src/synt/tides
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
#    31/07/2015   V1.0   Thomas Forbriger
# 
# ============================================================================

PROGRAMS=bfotide tidesff tidesffx
# gez does not compile correctly

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
# standard edit targets
flist: Makefile COPYING README $(sort $(wildcard *.cc *.f *.inc)) 
	echo $^ | tr ' ' '\n' > $@
	echo '----' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

clean:
	-/bin/rm -vf *.o *.bak *.o77 *.f2c.o $(PROGRAMS)
#----------------------------------------------------------------------
# check mandatory environment variable settings
# ---------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_LINK_PGPLOT)

# check Fortran code FLAGS=-fcheck=all
FLAGS += $(MYFLAGS) -fPIC
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += $(FLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)

#----------------------------------------------------------------------

%.o: %.f
	$(FC) -o $@ -c $< -O2 $(FFLAGS)

tidesff: %: %.o
	$(FC) -Wall -o$@ -fno-automatic $< \
	  -lsffu -ltf -ltime -lsff $(LDFLAGS) -L$(LOCLIBDIR)

tidesffx: %x: %.o
	$(FC) -Wall -o$@ -fno-automatic $< \
	  -lsffu -ltf -ltime -lfapidxx -ldatrwxx -lsffxx -lgsexx -laff -ltime++ \
	  $(LDFLAGS) -L$(LOCLIBDIR)

bfotide gez: %: %.o
	$(FC) -O3 -Wall -o$@ -fno-automatic $< $(LDFLAGS) -L$(LOCLIBDIR)


#======================================================================
# test cases
# ----------

#----------------------------------------------------------------------

%.psp: %.ps; gv $<; /bin/rm -fv $<
# 
#
# ----- END OF Makefile ----- 
