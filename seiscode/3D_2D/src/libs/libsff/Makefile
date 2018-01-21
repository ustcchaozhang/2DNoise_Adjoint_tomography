# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2003 by Thomas Forbriger (BFO Schiltach) 
# 
# libsff: Stuttgart File Format I/O Fortran software library
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
#    23/12/2003   V1.0   Thomas Forbriger
#    22/03/2007   V1.1   create a distribution package
#    27/11/2007   V1.2   g77 compilation is now default
#    17/12/2007   V1.3   moved to gfortran
#    18/09/2010   V1.4   start migration to new SVN scheme
#                        discard f2c option (fallback)
#                        check variables
#                        creation of package not yet defined
#    14/11/2010   V1.5   provide C prototypes to stuff.f
#    17/01/2011   V1.6   distinguish library creation and installation
#    24/11/2011   V1.7   supports source code and binary package creation
#    26/03/2013   V1.8   added installation instructions and comments
# 
# ============================================================================
#

.PHONY: all
all: install doc $(TESTS) installdoc

.PHONY: doc
doc: libsff.doc sff.doc

LIBRARIES=libsff.a
TESTS=teststuff tfullread tfullwrite tservice tskipdata

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
	$(MAKE) clean-include
	$(MAKE) install

#----------------------------------------------------------------------

flist: Makefile $(wildcard *.f *.c *.h) README $(TF_EDIT) COPYING
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<
	
.PHONY: clean
clean:
	-/bin/rm *.o *.bak *.o77 *.f2c.o flist *.a *.d
	 
# ============================================================================
# a variable definition to check variable settings
# ------------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR)

#----------------------------------------------------------------------

AR=ar
AS=as
RANLIB=ranlib

FLAGS+=$(MYFLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-none $(FLAGS)
CFLAGS += -O2 $(FLAGS)
LDFLAGS+=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS+=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

#----------------------------------------------------------------------
# documentation

libsff.doc sff.doc: stuff.f fapidxx.f
	awk 'BEGIN { flagsff=0; flaglib=0; } \
           /^cA/ { flaglib=1; flagsff=0; next; } \
           /^cB/ { flaglib=0; flagsff=1; next; } \
           /^cC/ { flaglib=1; flagsff=0; next; } \
           /^cD/ { flaglib=1; flagsff=0; next; } \
           /^cE/ { flaglib=0; flagsff=0; next; } \
           { if (flagsff) { print > "sff.doc" ; } \
             if (flaglib) { print > "libsff.doc" ; } } \
           END { print "c ---- END OF sff.doc ----" > "sff.doc" ; \
                 print "c ---- END OF libsff.doc ----" > "libsff.doc" ;}' $^

# publish documentation
ifdef TF_PUBLICATIONPATH
installdoc: libsff.doc sff.doc README 
	mkdir -pv $(TF_PUBLICATIONPATH)/libsff
	cp -vpd $(word 1,$^) $(TF_PUBLICATIONPATH)/libsff/libsff.txt
	cp -vpd $(word 2,$^) $(TF_PUBLICATIONPATH)/libsff/sff.txt
	cp -vpd $(word 3,$^) $(TF_PUBLICATIONPATH)/libsff/README
else
installdoc:
endif

ifdef TF_WWWBASEDIR
installpackagedoc: libsff.doc sff.doc README 
	mkdir -pv $(TF_WWWBASEDIR)/libsff
	cp -vpd $^ $(TF_WWWBASEDIR)/libsff
else
installpackagedoc:
endif

#----------------------------------------------------------------------
# C prototypes
      
$(call CHECKVAR,LOCINCLUDEDIR)
$(LOCINCLUDEDIR)/sff.h: sff.h
	mkdir -vp $(dir $@)
	-rm -fv $@
	/bin/cp -vpd $< $@

.PHONY: install-include
install-include: $(LOCINCLUDEDIR)/sff.h

.PHONY: clean-include
clean-include:
	-/bin/rm -fv $(LOCINCLUDEDIR)/sff.h

#----------------------------------------------------------------------
# library

gse20.o: gse20.f
stuff.o: stuff.f
fapidxx.o: fapidxx.f
sunfortran.o: sunfortran.c

libsff.a: gse20.o stuff.o sunfortran.o fapidxx.o

%.o: %.f
	$(FC) $(FFLAGS) $< -c -o $@
%.a:
	$(AR) rcv $@ $^
	$(RANLIB) $@

#----------------------------------------------------------------------
# test code 

$(TESTS): %: %.o; $(CC) -o $@ $< -lsff 

.PHONY: tests
tests: $(TESTS)

#======================================================================
# create package
# --------------
# is delegated to Makefile.packages
# which still has to be coded
PACKAGE=libsff
ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=libsff \
	  PACKAGEEXPORT="trunk/src/libs/libsff:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="-" 
endif

# ----- END OF Makefile ----- 
