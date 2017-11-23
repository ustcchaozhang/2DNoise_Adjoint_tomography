# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
# 
# main Makefile for Fortran77 gremlin versions
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
#    01/03/2002   V1.0   Thomas Forbriger
#    30/12/2002   V1.1   added C++ directory
#    09/02/2010   V1.2   transition to gfortran
#    17/11/2010   V1.3   migrated to SVN scheme; avoid incdep
#    10/01/2010   V1.4   libgin.a must be compiled before gremlin is going to
#                        be linked
#    14/01/2011   V1.5   call cxx/Makefile in top-level targets
#    10/11/2014   V1.6   librefpar replaces librefread
# 
# ============================================================================
#
#PROGRAMS=gremlin wgremlin gredim
PROGRAMS=gremlin gredim

.PHONY: all
all: install

.PHONY: install
install: 
	(cd cxx; $(MAKE) install)
	$(MAKE) libraries
	$(MAKE) programs

.PHONY: libraries
libraries: 
	(cd cxx; $(MAKE) install)
	(cd libs; $(MAKE) libs)

.PHONY: programs
programs: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

.PHONY: doc
doc: gremlin.pdf 
	
# check mandatory environment variable settings
# ---------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)

LINLIB=-llapack -lblas

FLAGS += $(MYFLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += $(FLAGS)
CPPFLAGS=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR)))
LDFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))

AR=ar
RANLIB=ranlib

#----------------------------------------------------------------------

.PHONY: clean edit

clean:
	-/bin/rm -vf *.o *.bak flist make.incdep *.d
	-/bin/rm -vf *.aux *.toc *.log *.dvi
	-cd docu; /bin/rm -vf *.aux *.toc *.log *.dvi *.bak *.blg *.bbl
	-cd libs; make clean

edit: flist
	vim flist

flist: $(wildcard *.f */*.f *.inc */*.inc */*.h */*.cc) \
  $(wildcard Makefile */Makefile) COPYING README \
  $(wildcard */COPYING */README) \
  $(wildcard docu/*.tex docu/*.bib docu/pepi.???)
	echo $(filter %Makefile,$^) | tr ' ' '\n' | sort > $@
	echo $(filter %README,$^) | tr ' ' '\n' | sort >> $@
	echo $(filter %COPYING,$^) | tr ' ' '\n' | sort >> $@
	echo '----------' >> $@
	echo $(filter-out %Makefile %COPYING %README cxx/% tools/% docu/% libs/%,$^) | tr ' ' '\n' | sort >> $@
	echo '----------' >> $@
	echo $(filter libs/%,$^) | tr ' ' '\n' | sort >> $@
	echo '----------' >> $@
	echo $(filter cxx/%,$^) | tr ' ' '\n' | sort >> $@
	echo '----------' >> $@
	echo $(filter tools/%,$^) | tr ' ' '\n' | sort >> $@
	echo '----------' >> $@
	echo $(filter docu/%,$^) | tr ' ' '\n' | sort >> $@
	echo '----------' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

#----------------------------------------------------------------------
# Fortran dependencies
# --------------------
%.d: %.f
	echo $<: $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
-include $(patsubst %.f,%.d,$(wildcard *.f))

libs/glq_dim.inc: libs/glq_dimstd.inc; /bin/cp -v $< $@

#----------------------------------------------------------------------

%.o: %.f
	$(FC) -o $@ -c $< $(FFLAGS)

gremlin: gremlin.o gremlin_help.o
	$(FC) -o gremlin $^ \
	  -L$(LOCLIBDIR) \
        -lgin \
        -lgrrefsub -lrefpar \
        $(LINLIB) -ltf  \
        $(BUGGY) -L$(LOCLIBDIR) $(TF_LINK_PGPLOT) $(LDFLAGS)

# version using wang code
wgremlin: gremlin.o gremlin_help.o
	$(FC) -o $@ $^ \
        -lgin \
        -lwrefsub -lrefpar \
        $(LINLIB) -ltf $(TF_LINK_PGPLOT) \
        $(BUGGY) -L$(LOCLIBDIR) $(LDFLAGS)

gredim_comments.xxx: libs/glq_dimstd.inc
	awk "BEGIN { printf \"      print *,'%s'\n\", \"comments from $<:\"; } \
	  /^c glq[md]_/ { printf \"      call repline('%s')\n\", \$$0; }" $< > $@

gredim: gredim.o gredim_comments.xxx
	$(FC) -o gredim gredim.o $(GFLAG) $(F2CLIB) $(LDFLAGS)

#======================================================================
# documentation
docu/gremlin.dvi: docu/gremlin.tex
	cd docu; latex gremlin; bibtex gremlin; latex gremlin; latex gremlin
gremlin.pdf: docu/gremlin.dvi
	cd docu; pdflatex gremlin
	/bin/cp -vpd docu/gremlin.pdf $@

#======================================================================
# create package
# --------------
# is delegated to Makefile.packages
ifdef TF_MAKEPKG
#export PACKAGELINKSFORTRAN
export PACKAGENEEDSPGPLOT=yes
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
          PACKAGE=gremlin \
          PACKAGEEXPORT="trunk/src/green/gremlin1:src" \
          PACKAGETARGETS="src:all" \
          PACKAGELIBS="-"
.PHONY: fullpackage
fullpackage: $(TF_MAKEPKG)
	$(MAKE) -f $< \
          PACKAGE=gremlinwithlibs \
          PACKAGEEXPORT="trunk/src/green/gremlin1:src" \
          PACKAGETARGETS="src:all" \
          PACKAGELIBS="libtime libgrrefsub libwrefsub librefpar \
	      libsff libsffu libtf \
	      libaff libgsexx libdatrwxx libfapidxx libfourier libsffxx \
		libtsxx libtfxx"
endif

# ----- END OF Makefile ----- 
