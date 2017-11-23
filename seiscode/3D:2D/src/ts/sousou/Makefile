# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
# 
# sousou - sound source
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
#    10/05/2002   V1.0   Thomas Forbriger
# 
# ============================================================================
#
 
PROGRAMS=sousou sousoux

.PHONY: all
all: install doc

.PHONY: doc
doc: sousou.doc

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

#----------------------------------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_BROWSER TF_WWWBASEDIR)
$(call CHECKVARS,TF_LINK_PGPLOT)

FLAGS += $(MYFLAGS) -fPIC
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += $(FLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS+=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS+=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

#----------------------------------------------------------------------

flist: Makefile COPYING README $(wildcard *.f *.inc)
	echo $^ | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o *.d *.doc $(PROGRAMS)

#----------------------------------------------------------------------

OBS=sousou.o sousou_readdata.o sousou_skipdata.o sousou_checks.o \
   sousou_prepare.o sousou_writedata.o sousou_analysis.o sousou_writeslow.o

%.d: %.f
	echo $<: $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
include $(patsubst %.o,%.d,$(OBS))

sousou.doc: $(patsubst %.o,%.f,$(OBS))
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^cS/ { hot=1; \
	    print FILENAME; \
          print "  c\n  c"; \
          next; } \
        /^cE/ { hot=0; } \
        { if (hot==1) { print "  " $$0; } }' $^ > $@

sousou: $(OBS)
	$(FC) -o $@ $(OBS) \
        -ltf -lts -lsffu -lsff -ltime \
        $(TF_LINK_PGPLOT) -llapack -lblas \
       $(LDFLAGS) -L$(LOCLIBDIR) 

sousoux: $(OBS)
	$(FC) -o $@ $(OBS) \
        -ltf -lts -lsffu -lfapidxx -ldatrwxx -lsffxx -lgsexx -laff -ltime++ \
        $(TF_LINK_PGPLOT) -llapack -lblas \
        $(LDFLAGS) -L$(LOCLIBDIR) 

.PHONY: list-libraries
list-libraries:
	grep ' -l' Makefile | tr ' ' '\n' | egrep '^-l' |sort | uniq
	grep ' -l' Makefile | tr ' ' '\n' | egrep 'PGP' |sort | uniq



#======================================================================
# create package
# --------------
# is delegated to Makefile.packages
ifdef TF_MAKEPKG
# export PACKAGELINKSFORTRAN=yes
export PACKAGENEEDSPGPLOT=yes
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	PACKAGE=sousou \
	PACKAGEEXPORT="trunk/src/ts/sousou:src" \
	PACKAGETARGETS="src:all" \
	PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	PACKAGE=sousouwithlibs \
	PACKAGEEXPORT="trunk/src/ts/sousou:src" \
	PACKAGETARGETS="src:all" \
	PACKAGELIBS="libsff libtf libts libsffu libaff libtime libgsexx libsffxx libdatrwxx libfapidxx"
endif

# ----- END OF Makefile ----- 
