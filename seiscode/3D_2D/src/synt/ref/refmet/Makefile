# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
# 
# refmet and refread library
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
#    15/03/2002   V1.0   Thomas Forbriger
#    14/09/2008   V1.1   use gfortran
#    23/02/2009   V1.2   support creation of package
#    08/10/2010   V1.3   migrate to SVN environment
#                        discard f2c fallback option
#                        check environment
#                        discard package creation
#    14/01/2011   V1.4   implemented refmetx with libfapidxx interface
#    24/09/2014  thof:   part of the code in librefread is moved to librefpar,
#                        a dedicated library in the collection of libraries
#    10/11/2014  thof:   grema takes refmet_rrcv from librefpar
# 
# ============================================================================
#

PROGRAMS=refmet refmetx grema matbau refmat resus
LIBRARIES=librefread.a

.PHONY: all
all: install 

.PHONY: install
install: libraries programs

.PHONY: programs
programs: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(dir $@)
	/bin/mv -fv $< $@

.PHONY: libraries
libraries: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(dir $@)
	/bin/mv -fv $(word 2,$^) $@

#----------------------------------------------------------------------
# standard edit targets
flist: Makefile $(wildcard */*.f */Makefile sub/lmath.c) COPYING \
       $(wildcard examples/*.cfg) $(wildcard docu/*.tex) \
	 $(wildcard */README) README
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.o | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist $(PROGRAMS) $(LIBRARIES)

#----------------------------------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)

FLAGS += $(MYFLAGS)
CFLAGS += -O2 $(FLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-none $(FLAGS) \
  -fimplicit-none -pedantic -std=legacy

# use STATIC=-static to produce statically linked binaries
STATIC=
LDFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR))) $(STATIC)
CPPFLAGS=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

REAMPOBS=refmet_basinf.o 
REFMETOBS=refmet_output.o refmet_preptrace.o refmet_wtrace.o lmath.o\
   refmet_basinf.o refmet_rmain.o 
REFMUOBS=main/ref2mu.o
REFMATOBS=refmat_basinf.o refmet_rmain.o 
RESUSOBS=refmet_output.o refmet_preptrace.o refmet_wtrace.o lmath.o\
   resus_basinf.o refmet_rmain.o 
RESUSNOBS=refmet_output.o refmet_preptrace.o refmet_wtrace.o lmath.o\
   resusnoise_basinf.o refmet_rmain.o 
LIBREFMETOBS=$(addprefix sub/,refmet_basinf.o refmet_rmain.o)

.PHONY: install-include
install-include:

refmet: main/refmet.o $(addprefix sub/, $(REFMETOBS))
refmetx: main/refmet.o $(addprefix sub/, $(REFMETOBS))
resus: main/resus.o $(addprefix sub/, $(RESUSOBS))
resusnoise: main/resusnoise.o $(addprefix sub/, $(RESUSNOBS))
refmat: main/refmat.o $(addprefix sub/, $(REFMATOBS))
refmatsh: main/refmatsh.o $(addprefix sub/, $(REFMETOBS))

matbau: %: main/%.o
	$(FC) $< -o $@ $(LDFLAGS)

grema: %: main/%.o
	$(FC) $(FFLAGS) $^ -o $@ -lrefpar -lrefread -ltf -lsff \
        $(LDFLAGS)

refmatsh refmat resus refmet: 
	$(FC) $^ -o $@ -lrefpar -lsff -lemod -ltf \
        $(LDFLAGS) $(FFLAGS)

refmetx: 
	$(FC) $^ -o $@ -lrefpar -lemod -ltf \
	  -lfapidxx -ldatrwxx -lsffxx -lgsexx -ltime++ -laff \
        $(LDFLAGS) $(FFLAGS)

resusnoise: 
	$(FC) $^ -o $@ -lrefpar -lsff -lemod -ltf -lgsl -lgslcblas \
        $(LDFLAGS) $(FFLAGS)

librefread.a: $(LIBREFMETOBS)
	ar rcv librefread.a $(LIBREFMETOBS)
	ranlib librefread.a

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
	  PACKAGE=refmet \
	  PACKAGEEXPORT="trunk/src/synt/ref/refmet:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=refmetwithlibs \
	  PACKAGEEXPORT="trunk/src/synt/ref/refmet:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="librefpar libemod libsff libtf libsffu libts libaff libtime libgsexx libsffxx libdatrwxx libfapidxx"
endif

# ----- END OF Makefile ----- 
