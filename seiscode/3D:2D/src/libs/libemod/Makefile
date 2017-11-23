# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
# 
# compile earth model library
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
#    16/06/2005   V1.0   Thomas Forbriger
#    06/12/2007   V1.1   g77 is now default compiler
#    17/12/2007   V1.2   moved to gfortran
#    23/02/2009   V1.3   create package
#    18/09/2010   V1.4   migration to SVN scheme
#                        discard f2c option (fallback)
#                        check variables
#                        no package creation currently
#    17/01/2011   V1.5   distinguish library creation and installation
# 
# ============================================================================
#

LIBRARIES=libemod.a

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

LIBEMODSUB=psexdat.o readgemini.o efa.o readrefmet.o earthmod.o \
  readanigemini.o

#----------------------------------------------------------------------

doc: $(patsubst %.o,%.doc,$(LIBEMODSUB))

%.doc: %.f
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^cS/ { hot=1; \
          print "  c"; \
	    print FILENAME; \
          print "  c"; \
          next; } \
        /^cE/ { hot=0; } \
        { if (hot==1) { print "  " $$0; } }' $^ > $@

#----------------------------------------------------------------------

%.o: %.f
	$(FC) -O2 -o $@ -c $< $(FFLAGS)

libemod.a: $(LIBEMODSUB)
	ar rcv libemod.a $(LIBEMODSUB)
	ranlib libemod.a

efa.doc: %.doc: %.f
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^\/\*S\*\// { hot=1; next; } \
        /^\/\*E\*\// { hot=0; } \
        { if (hot==1) { print "  " $$0; } }' $^ > $@

#--------------------------------------------------------------------- 
# create package 
# --------------  
 
ifdef TF_MAKEPKG 
.PHONY: package 
package: $(TF_MAKEPKG) 
	$(MAKE) -f $< \
        PACKAGE=libemod \
        PACKAGEEXPORT="trunk/src/libs/libemod:src" \
	PACKAGETARGETS="src:install: src:doc:" \
        PACKAGELIBS="-"
endif
# ----- END OF Makefile ----- 
