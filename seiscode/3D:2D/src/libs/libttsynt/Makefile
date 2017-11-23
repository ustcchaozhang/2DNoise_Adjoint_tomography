# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
# 
# synthetic traveltimes for dipping refractors
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
#    21/03/2002   V1.0   Thomas Forbriger
#    17/01/2011   V1.1   distinguish library creation and installation
# 
# ============================================================================
#

.PHONY: all
all: install doc

.PHONY: doc
doc: libttsynt.doc

LIBRARIES=libttsynt.a

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

.PHONY: reinstall
reinstall:
	$(MAKE) clean
	$(MAKE) install

# install-include where no header files are to be installed
.PHONY: install-include
install-include:
	
# check mandatory environment variable settings
# ---------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)

FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)

LIBSRC=$(wildcard *.f)
LIBOBS=$(patsubst %.f,%.o,$(LIBSRC))

#----------------------------------------------------------------------

flist: Makefile $(LIBSRC) $(wildcard *.inc) COPYING README
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o *.a *.d

#----------------------------------------------------------------------
# ASCII documentation (Fortran and C code)
# ----------------------------------------
libttsynt.doc: $(LIBSRC)
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^cS/ { hot=1; \
          print "  c"; \
	    print FILENAME; \
          print "  c"; \
          next; } \
        /^cE/ { hot=0; } \
        { if (hot==1) { print "  " $$0; } }' $^ > $@

#----------------------------------------------------------------------

libttsynt.a: $(LIBOBS)
	ar rcv libttsynt.a $(LIBOBS)
	ranlib libttsynt.a

%.d: %.f
	echo $<: $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
include $(patsubst %.f,%.d,$(LIBSRC))

%.o: %.f
	$(FC) -O2 -o $@ -c $< $(FFLAGS)
#----------------------------------------------------------------------
# create package
# --------------

ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	PACKAGE=libttsynt \
	PACKAGEEXPORT="trunk/src/libs/libttsynt:src" \
	PACKAGETARGETS="src:install: src:doc:" \
	PACKAGELIBS="-"
endif

# ----- END OF Makefile ----- 
