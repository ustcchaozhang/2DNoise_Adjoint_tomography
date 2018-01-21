# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
# 
# Makefile for libgrrefsub.a
#
# reflectivity code by J. Ungerer
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
# provides an API such that reflectivity can be used within programs like
# gremlin or syg
#
# Please have a look at the README file coming along with the source code in
# this directory for further notes regarding provided functionality,
# prerequisites and installation.
#
# REVISIONS and CHANGES 
#    15/09/2008   V1.0   Thomas Forbriger
#                        use gfortran
#    08/10/2010   V1.1   migrate to new SVN environment
#                        discard f2c fallback option
#                        check environment
#    17/11/2010   V1.2   avoid incdep and makefdoc.tcl
#    11/01/2011   V1.3   avoid newlib
#    17/01/2011   V1.4   distinguish library creation and installation
# 
# ============================================================================
#

.PHONY: all 
all: install doc

.PHONY: doc
doc: libgrrefsub.doc

LIBRARIES=libgrrefsub.a

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

# install-include where no header files are to be installed
.PHONY: install-include
install-include:

.PHONY: reinstall
reinstall:
	$(MAKE) clean
	$(MAKE) install

#----------------------------------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)

FLAGS += $(MYFLAGS)
CFLAGS += -O2 $(FLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-none $(FLAGS)

# use STATIC=-static to produce statically linked binaries
STATIC=
LDFLAGS=-L$(LOCLIBDIR) $(STATIC)
CPPFLAGS=-I$(LOCINCLUDEDIR) $(FLAGS)

LIBOBS=gr_refsub.o gr_mat.o gr_rtkc.o gr_setrheo.o gr_setsigma.o

#----------------------------------------------------------------------
# standard edit targets
clean:
	-/bin/rm *.o *.bak *.d *.a 

flist: $(wildcard *.f *.inc) Makefile COPYING README
	echo $^ | tr ' ' '\n' | sort > $@
.PHONY: edit
edit: flist; vim $<

#----------------------------------------------------------------------

libgrrefsub.doc: gr_refsub.f 
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^cS/ { hot=1; \
          print "  c"; \
	    print FILENAME; \
          print "  c"; \
          next; } \
        /^cE/ { hot=0; } \
        { if (hot==1) { print "  " $$0; } }' $^ > $@

#----------------------------------------------------------------------

%.d: %.f
	echo $<: $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
include $(patsubst %.f,%.d,$(wildcard *.f))

%.o: %.f; $(FC) -c -o $@ $< $(FFLAGS)

libgrrefsub.a: $(LIBOBS)
	ar rcv libgrrefsub.a $(LIBOBS)
	ranlib libgrrefsub.a
#----------------------------------------------------------------------
# create package
# --------------
ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	PACKAGE=libgrrefsub \
	PACKAGEEXPORT="trunk/src/libs/libgrrefsub:src" \
	PACKAGETARGETS="src:install: src:doc:" \
	PACKAGELIBS="-"
endif	
# ----- END OF Makefile ----- 
