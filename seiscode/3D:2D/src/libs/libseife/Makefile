# this is <Makefile>
# ----------------------------------------------------------------------------
#   ($Id$)
# 
# 25/10/2000 by Thomas Forbriger (IMGF Frankfurt)
# 
# libseife Makefile
# 
# seife: seismic waveform filters
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
#=============================================================================
# 
# REVISIONS and CHANGES
#    25/10/2000   V1.0   Thomas Forbriger
#    14/12/2007   V1.1   g77 compilation is the default now
#    17/12/2007   V1.2   moved to gfortran
#    18/09/2010   V1.3   start migration to new SVN scheme
#                        discard f2c option (fallback)
#                        check variables
#    15/11/2010   V1.4   do not use fdep.sh
#    17/11/2010   V1.5   libseifemon.a is out of use
#    17/01/2011   V1.6   distinguish library creation and installation
# 
# ============================================================================
# 

.PHONY: all
all: install doc

.PHONY: doc
doc: 

LIBRARIES=libcseife.a libseife.a

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)
	 
# ============================================================================

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR)

#----------------------------------------------------------------------

CLIBSRC=$(wildcard cseife*.c)
HEADERS=$(shell find . -name \*.h)
INCINSTALLPATH=$(LOCINCLUDEDIR)
LIBINSTALLPATH=$(LOCLIBDIR)
# name of installed (exported) header files
INSTHEADER=$(addprefix $(INCINSTALLPATH)/,$(notdir $(HEADERS)))

.PHONY: clean edit
clean:
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.o | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.o77 | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -fv flist
	-/bin/rm -fv install-include *.a

edit: flist ; vim flist
flist: $(wildcard subs/*.f *.f Makefile *.c *.h subs/*.inc) COPYING \
  $(TF_EDIT) README
	echo $^ | tr ' ' '\n' | sort > $@

AR=ar
AS=as
RANLIB=ranlib

FLAGS += $(MYFLAGS)
CFLAGS += -O3 -Wall $(FLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash -Isubs $(FLAGS)
LDFLAGS+=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS+=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

SUBS=$(patsubst %.f,%.o,$(wildcard subs/*.f))

%.o: %.f
	$(FC) -O2 -o $@ -c $< $(FFLAGS)
.c.o:
	$(CC) $(CFLAGS) $< -c $(CPPFLAGS)

#======================================================================
# Fortran include dependencies
%.d: %.f
	echo $<: $(addprefix subs/,$(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq)) > $@
include $(patsubst %.f,%.d,$(wildcard subs/*.f))

#======================================================================

$(INCINSTALLPATH)/%.h: %.h
	mkdir -p $(INCINSTALLPATH)
	-rm -fv $@
	/bin/cp -vpd $< $@

.PHONY: install-include
install-include: $(INSTHEADER)

#======================================================================
# reinstall target
# is necessary in case of header file problems (e.g. remcmmnt not installed)
.PHONY: clean-include
clean-include: 
	/bin/rm -fv $(INSTHEADER)

.PHONY: reinstall
reinstall:
	$(MAKE) clean
	$(MAKE) clean-include
	$(MAKE) install

#======================================================================
# pattern rules
# -------------

%.d: %.c
	$(SHELL) -ec '$(CC) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

#======================================================================
# library part
# ------------

include $(patsubst %.c,%.d,$(CLIBSRC))

libcseife.a: $(patsubst %.c,%.o,$(CLIBSRC))
libseife.a: $(SUBS)

libcseife.a libseife.a: 
	$(AR) rcv $@ $^
	$(RANLIB) $@

#======================================================================
# create package
# --------------
# is delegated to Makefile.packages
ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=libseife \
	  PACKAGEEXPORT="trunk/src/libs/libseife:src" \
	  PACKAGETARGETS="src:all:" \
	  PACKAGELIBS="-"
endif
# 
# ----- END OF Makefile ----- 
