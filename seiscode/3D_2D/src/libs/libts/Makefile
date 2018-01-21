# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
# 
# some small functions for time series analysis
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
# Please have a look at the README file coming along with the source code in
# this directory for further notes regarding provided functionality,
# prerequisites and installation.
#
# 
# REVISIONS and CHANGES 
#    05/06/2007   V1.0   Thomas Forbriger
#    06/12/2007   V1.1   g77 compilation is the default now
#    17/12/2007   V1.2   moved to gfortran
#    19/09/2010   V1.3   start migration to new SVN scheme
#                        discard f2c option (fallback)
#                        check variables
#                        creation of package not yet defined
#    14/11/2010   V1.4   replaced makefdoc.pl by awk script
#    17/01/2011   V1.5   distinguish library creation and installation
# 
# ============================================================================
#

.PHONY: all
all: install doc

.PHONY: doc
doc: libts.doc

LIBRARIES=libts.a

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

flist: Makefile $(wildcard *.f) COPYING README
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist
	-/bin/rm *.o *.bak *.o77 *.a
	 
# ============================================================================
#
%.doc: %.f
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^cS/ { hot=1; \
          print "  c"; \
	    print FILENAME; \
          print "  c"; \
          next; } \
        /^cE/ { hot=0; } \
        { if (hot==1) { print "  " $$0; } }' $^ > $@
# ============================================================================
CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR)

AR=ar
AS=as
RANLIB=ranlib

FLAGS += $(MYFLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += -O3 $(FLAGS)
LDFLAGS+=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS+=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

LIBTSOBS=libts.o

docs: $(DOCS)

%.o: %.f
	$(FC) -O2 -o $@ -c $< $(FFLAGS)

libts.doc: libts.f
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^cS/ { hot=1; \
	    print FILENAME; \
          print "  c"; \
          next; } \
        /^cE/ { hot=0; } \
        { if (hot==1) { print "  " $$0; } }' $^ > $@

libts.a: $(LIBTSOBS)
	$(AR) rcv $@ $^
	$(RANLIB) $@
#=============================================================================
# create package
# --------------

ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	PACKAGE=libts \
	PACKAGEEXPORT="trunk/src/libs/libts:src" \
	PACKAGETARGETS="src:install: src:doc:" \
	PACKAGELIBS="-"
endif
# ----- END OF Makefile ----- 
