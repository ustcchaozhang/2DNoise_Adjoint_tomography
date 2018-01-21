# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
# 
# create some little helpers for SFF data manipulation
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
# originally started in 2000:
#     06/08/2000  added f77 library
#     16/11/2001  make library g77-proof
# 
# REVISIONS and CHANGES 
#    22/03/2007   V1.0   Thomas Forbriger
#    06/12/2007   V1.1   g77 is now default compiler
#    17/12/2007   V1.2   moved to gfortran
#    18/09/2010   V1.3   start migration to SVN scheme
#                        discard f2c option (fallback) entirely
#                        check variables
#    14/11/2010   V1.4   replaced makefdoc.pl
#                        added C dependency files
#    17/01/2011   V1.5   distinguish library creation and installation
# 
# ============================================================================
#

.PHONY: all
all: install doc

.PHONY: doc
doc: libsffu.doc

LIBRARIES=libsffu.a

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

.PHONY: reinstall
reinstall:
	$(MAKE) clean
	$(MAKE) clean-include
	$(MAKE) install

# ============================================================================
# a variable definition to check variable settings
# ------------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

# check for required variables
$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR)
#----------------------------------------------------------------------

LIBSFFUFOBS=libsffu.o sffutilf.o sffu_simplewrite.o sffu_simpleread.o \
            sffu_fullargread.o sffu_rsta2.o
LIBSFFUCOBS=sffutil.o 
LIBSFFUOBS=$(LIBSFFUFOBS) $(LIBSFFUCOBS)

DOCS=libsffu.doc

LIBS=libsffu.a 

#----------------------------------------------------------------------
# Fortran dependencies
%.d: %.f
	echo $< $(patsubst %.f,%.o,$<): \
        $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
include $(patsubst %.f,%.d,$(wildcard *.f))

#----------------------------------------------------------------------
# C dependencies
%.d: %.c
	$(SHELL) -ec '$(CC) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

include $(patsubst %.c,%.d,$(patsubst %.o,%.c,$(LIBSFFUCOBS))) 

HEADERS=$(wildcard *.h)
INSTHEADER=$(addprefix $(LOCINCLUDEDIR)/,$(HEADERS))

# install-include where no header files are to be installed
$(LOCINCLUDEDIR)/%.h: %.h
	mkdir -vp $(dir $@)
	-rm -fv $@
	/bin/cp -vpd $< $@

# install header files
.PHONY: install-include
install-include: $(INSTHEADER)

.PHONY: clean-include
clean-include: 
	-/bin/rm -fv $(INSTHEADER)

#----------------------------------------------------------------------

flist: Makefile $(LIBSFFUCOBS:.o=.c) $(LIBSFFUFOBS:.o=.f) *.h \
  COPYING README $(TF_EDIT)
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -fv flist *.o *.bak *.o77 *.a *.d

#----------------------------------------------------------------------

FLAGS += $(MYFLAGS)
CFLAGS += -O2 $(FLAGS)
FFLAGS += -ff2c -ffixed-line-length-none -Wall $(FLAGS)
LDFLAGS+=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS+=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

#----------------------------------------------------------------------

%.o: %.c
	$(CC) -O2 -o $@ -c $< -Wall $(CFLAGS) $(CPPFLAGS) 
        
%.o: %.f
	$(FC) -O2 -o $@ -c $< $(FFLAGS)

libsffu.a: $(LIBSFFUOBS)

%.a: ; ar rcv $@ $^ ; ranlib $@ 

#======================================================================
# documentation
docs: $(DOCS)
libsffu.doc: $(LIBSFFUFOBS:.o=.f)
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^cS/ { hot=1; \
	    print FILENAME; \
          print "  c\n  c -----------------------------------------\n  c"; \
          next; } \
        /^cE/ { hot=0; } \
        { if (hot==1) { print "  " $$0; } }' $^ > $@

#======================================================================
# create package
# --------------
ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	PACKAGE=libsffu \
	PACKAGEEXPORT="trunk/src/libs/libsffu:src" \
	PACKAGETARGETS="src:install: src:doc:" \
	PACKAGELIBS="-"

.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	PACKAGE=libsffuwithlibs \
	PACKAGEEXPORT="trunk/src/libs/libsffu:src" \
	PACKAGETARGETS="src:install: src:doc:" \
	PACKAGELIBS="libsff libtime"
endif

# ----- END OF Makefile ----- 
