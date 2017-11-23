# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2014 by Thomas Forbriger (BFO Schiltach) 
# 
# Makefile for librefread
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
# Please have a look at the README file coming along with the source code in
# this directory for further notes regarding provided functionality,
# prerequisites and installation.
#
# REVISIONS and CHANGES 
#    24/09/2014   V1.0   Thomas Forbriger
# 
# ============================================================================
#

LIBRARIES=librefpar.a

.PHONY: all
all: install 

# efa.doc still uses deprecated tcl script
.PHONY: doc
doc: 

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

.PHONY: reinstall
reinstall:
	$(MAKE) clean
	$(MAKE) install

.PHONY: install-include
install-include:

#----------------------------------------------------------------------

flist: Makefile $(wildcard *.f) README COPYING
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o *.a *.d 

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR)

FLAGS += $(MYFLAGS)
FFLAGS += -Wall -ff2c -ffixed-line-length-0 -fno-backslash $(FLAGS) \
  -fimplicit-none -pedantic
CFLAGS += -O3 $(FLAGS)
LDFLAGS+=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS+=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

#----------------------------------------------------------------------

LIBSRC=refmet_rmod.f refmet_rsource.f refmet_rrcv.f \
  refmet_intro.f refmet_comments.f

LIBOBS=$(patsubst %.f,%.o,$(LIBSRC))

#----------------------------------------------------------------------

%.o: %.f
	$(FC) -O2 -o $@ -c $< $(FFLAGS)

librefpar.a: $(LIBOBS)
	ar rcv librefpar.a $(LIBOBS)
	ranlib librefpar.a

#--------------------------------------------------------------------- 
# create package 
# --------------  
 
ifdef TF_MAKEPKG 
.PHONY: package 
package: $(TF_MAKEPKG) 
	$(MAKE) -f $< \
        PACKAGE=librefpar \
        PACKAGEEXPORT="trunk/src/libs/librefpar:src" \
        PACKAGETARGETS="src:install:" \
        PACKAGELIBS="-"
endif

# ----- END OF Makefile ----- 
