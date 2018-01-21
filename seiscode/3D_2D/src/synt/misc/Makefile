# this is <Makefile>
# ----------------------------------------------------------------------------
# 16/11/2001 by Thomas Forbriger (IMGF Frankfurt)
# 
# Makefile fuer tools /src/synt/misc
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
# 
# REVISIONS and CHANGES
#    16/11/2001   V1.0   Thomas Forbriger
#    03/11/2008   V1.1   use new compiler gfortran
#                        code compiles but is not tested
#    27.12.2010   V1.2   migrate to new SVN scheme
#    20.01.2011   V1.3   distinguish between compilation and installation
#    05.01.2014   V1.4   preparing to be published
#    31.07.2015   V1.5   move rigid earth tide simulation to subdirectory
#                        tides
# 
# ============================================================================

PROGRAMS=dispfield kette planefield coupledoscillators \
	   dispfieldx lamb lambx modeinterference \
	   siggen tesiff teswf hamres siggenx phasedsignals
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

FLAGS += $(MYFLAGS) -fPIC
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += $(FLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)

#----------------------------------------------------------------------

%.o: %.f
	$(FC) -o $@ -c $< -O2 $(FFLAGS)

coupledoscillators kette: %: %.o
	$(FC) $(FFLAGS) $< -o $@ -ltf -lsffu -lsff -llapack -lblas\
        $(LDFLAGS) -L$(LOCLIBDIR) 

tesiff teswf hamres lamb planefield dispfield: %: %.o
	$(FC) -o $@ $< -ltf -lsffu -lsff  $(LDFLAGS) -L$(LOCLIBDIR)

phasedsignals siggen: %: %.o
	$(FC) -o $@ $< -ltf -lsffu -lsff -lgsl -lgslcblas \
	  $(LDFLAGS) -L$(LOCLIBDIR)

lambx dispfieldx: %x: %.o
	$(FC) -o $@ $< -ltf -lsffu \
	  -lfapidxx -ldatrwxx -lsffxx -lgsexx -ltime++ -laff \
	 $(LDFLAGS)  -L$(LOCLIBDIR)

siggenx: %x: %.o
	$(FC) -o $@ $< -ltf -lsffu \
	  -lfapidxx -ldatrwxx -lsffxx -lgsexx -ltime++ -laff -lgsl -lgslcblas \
	  $(LDFLAGS) -L$(LOCLIBDIR)

modeinterference: modeinterference.o
	$(FC) -o $@ $< -ltf $(TF_LINK_PGPLOT) $(LDFLAGS) -L$(LOCLIBDIR)

.PHONY: list-libraries
list-libraries:
	grep ' -l' Makefile | tr ' ' '\n' | egrep '^-l' |sort | uniq
	grep ' -l' Makefile | tr ' ' '\n' | egrep 'PGP' |sort | uniq


#======================================================================
# test cases
# ----------

alldisp: dispfield.ps dispfield.bp10.fil.ps dispfield.bp100.fil.ps

dispfield.%.sff: % dispfield.sff
	stufi $< -o dispfield.sff
	mv dispfield.sff.sfi $@

dispfield.sff: Makefile dispfield
	dispfield $@ -t 1. -c 80.,180. -x 2.,40. -f 5.,150. -n 12 -N 20 \
                   -e 1. -T 0.4

REFRACTOPT=-Sx -0.,30. -Sa 0.8 \
              -Eg -St 0.,0.5 -Tx 'Zeit (s)' -Ty 'Offset (m)'  \
              -Tm F -Tl F -Ts 1.1 -Lw 3
REFRACTTRACE=t:1-14

dispfield.ps: dispfield.sff Makefile
	refract $(REFRACTOPT) -Tt 'Dispergierter Wellenzug' \
              -d $@/vps $< $(REFRACTTRACE)

dispfield.bp10.fil.ps: dispfield.bp10.fil.sff Makefile
	refract $(REFRACTOPT) -Tt 'Dispergierter Wellenzug (Bandpass bei 10Hz)' \
              -d $@/vps $< $(REFRACTTRACE)

dispfield.bp100.fil.ps: dispfield.bp100.fil.sff Makefile
	refract $(REFRACTOPT) -Tt 'Dispergierter Wellenzug (Bandpass bei 100Hz)' \
              -d $@/vps $< $(REFRACTTRACE)

dispfield.bp100.red.ps: dispfield.bp100.fil.sff Makefile
	refract $(REFRACTOPT) -Tt 'Dispergierter Wellenzug (f\dBandpass\u=100Hz, v\dred\u=114m/s)' \
              -Sr .114 -St -0.1,0.2 -d $@/vps $< $(REFRACTTRACE)

#----------------------------------------------------------------------

allplane: planefield.ps

planefield.sff: Makefile planefield
	planefield $@ -t 1. -c 80. -x 0.,40. -f 5. -n 1024 -N 21

planefield.ps: planefield.sff Makefile
	refract -Sx -3.,43. -Sa 1.2 -Eg -Tt 'Ebene Welle (f=5Hz, c=80.m/s)' \
              -Tx 'Zeit (s)' -Ty 'Offset (m)' -Tm F -Tl F -Ts 1.3 \
              -d $@/vps $< s:1,1,1 $< s:1,1,4 t:1,21

#----------------------------------------------------------------------

%.psp: %.ps; gv $<; /bin/rm -fv $<
#
# ----- END OF Makefile ----- 
