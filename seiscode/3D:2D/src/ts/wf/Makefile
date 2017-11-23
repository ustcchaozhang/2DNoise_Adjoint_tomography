# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
# 
# time series waveform filters
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
#
# Makefile fuer tools /src/ts/wf
#
# 22/01/2001      introduced smoos and did some "tidy up"
# 24/10/2001      introduced siggen
# 24/05/2001      introduced tsfilt
# 
# REVISIONS and CHANGES 
#    11/07/2005   V1.0   Thomas Forbriger (thof)
#    07/12/2007   V1.1   default Fortran compiler now is g77
#    17/12/2007   V1.2   moved to gfortran
#    08/10/2010   V1.3   migrate to new SVN environment
#                        - discard f2c fallback
#                        - use FFTW3
#    10/02/2011   V1.4   distinguish between compilation and installation
#    24/06/2011   V1.5   moved tests to distinguished Makefiles
#    25/05/2012   V1.6	 link siggen against libfapidxx (damb)
#    03/06/2012   V1.7   to be consistent with other programs: distinguish
#                        siggen and siggenx (being linked against libsff and
#                        libfapidxx, respectively)
#    11/01/2013   V1.8   added sigscale to program targets
#    17/04/2013   V1.9   lisousi has its own place now (moved source code)
#    28/10/2013   V1.10  implemented libfapidxx-version of suseix
#    02/01/2014   V1.11  removed signal generators, added tisli suspect
#    01/02/2014 thof:    rewrite all linker rules with libraries in
#                        proper order
#    19/03/2016 thof:    tidofi requires libgsl for filter GaussianNoise
# 
# ============================================================================

PROGRAMS=tsfilt stufi rotate coro xyz2uvw susei suseix evelo \
  smoos dise foutra teseco resaseda gatherdiff \
  autocorr cross tidofi sigfit noisymize sigval fidasexx soutifu \
  deconv geophone sigscale tisli suspect evelox

.PHONY: all
all: install doc

.PHONY: list-programs
list-programs:
	echo $(PROGRAMS) | tr ' ' '\n' | sort

.PHONY: doc
doc: doxydoc

.PHONY: install
install: $(addprefix $(LOCBINDIR)/,$(PROGRAMS))
$(LOCBINDIR)/%: %
	mkdir -pv $(LOCBINDIR)
	/bin/mv -fv $< $(LOCBINDIR)

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_LINK_FORTRAN TF_LINK_PGPLOT)

FORTRANLIB=$(TF_LINK_FORTRAN)

# use STATIC=-static to produce statically linked binaries
STATIC=
FLAGS += $(MYFLAGS)
CPPFLAGS=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR))) $(STATIC)

#----------------------------------------------------------------------
# standard edit targets
.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o *.xxx *.ps *~ $(PROGRAMS)

TESTCASEMAKE=$(filter-out %.bak,$(wildcard testcases/Makefile*))

# the frame of doxygen documentation is placed in text files
DOXYTXT=$(wildcard doxygen*.txt */doxygen*txt)  

EDITFILES=Makefile README $(wildcard *.cfg) COPYING $(DOXYTXT) \
	    $(filter-out %.bak,$(wildcard */README*))
EDITSRC=$(wildcard *.cc *.h *.c *.f *.gpt *.inc)
EDITTESTS=$(wildcard testcases/*.par) $(wildcard testcases/*.tpl) \
    $(TESTCASEMAKE) $(wildcard testcases/*.gpt)

flist: $(wildcard *.txt *.c *.f *.h *.inc *_text.txt Makefile *.cc *.gpt) \
  doxydoc.cfg README COPYING $(DOXYTXT) $(EDITFILES) \
  $(wildcard testcases/*.par) $(wildcard testcases/*.tpl) \
  $(TESTCASEMAKE) $(wildcard testcases/*.gpt) $(TF_EDIT)
	echo $(filter $(EDITFILES),$^) | tr ' ' '\n' | sort > $@
	echo "---- source code ----" >> $@
	echo $(filter $(EDITSRC),$^) | tr ' ' '\n' | sort >> $@
	echo "---- Fortran source files only ----" >> $@
	echo $(sort $(filter %.f,$^)) | tr ' ' '\n' >> $@
	echo "---- C++ source files only ----" >> $@
	echo $(sort $(filter %.cc,$^)) | tr ' ' '\n' >> $@
	echo "---- test cases ----" >> $@
	echo $(sort $(filter $(EDITTESTS),$^)) | tr ' ' '\n' >> $@
	echo "----" >> $@
	echo $(filter-out $(EDITFILES) $(EDITSRC) $(EDITTESTS),$^) \
	  | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<
#----------------------------------------------------------------------
# pattern rules

%.o: %.f; $(FC) -c -o $@ $< $(FFLAGS)

#----------------------------------------------------------------------
# description and online texts
%.cc %.h: %_help.txt
	echo "// DO NOT EDIT: this file is automatically derived from $<" \
	  > $(patsubst %.txt,%.h,$<)
	echo "extern char $(patsubst %.txt,%,$<)[];" >> $(patsubst %.txt,%.h,$<)
	echo "// DO NOT EDIT: this file is automatically derived from $<" \
	  > $(patsubst %.txt,%.cc,$<)
	echo "#include \"$(patsubst %.txt,%.h,$<)\"" >> $(patsubst %.txt,%.cc,$<)
	echo "char $(patsubst %.txt,%,$<)[]=" >> $(patsubst %.txt,%.cc,$<)
	echo "{" >> $(patsubst %.txt,%.cc,$<)
	cat $< | egrep -v '^#' | sed -e 's/"/\\"/g' \
	  | sed -e 's/$$/\\n"/' | sed -e 's/^/  "/'\
	  >> $(patsubst %.txt,%.cc,$<)
	echo "};" >> $(patsubst %.txt,%.cc,$<)

#----------------------------------------------------------------------
# dependencies
%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

include $(patsubst %.txt,%.d,$(wildcard *_help.txt)) 
include $(patsubst %.cc,%.d,$(wildcard *.cc)) 

#----------------------------------------------------------------------
# typical combinations of libraries
LIBDATRWXX=-ldatrwxx -lsffxx -ltime++ -lgsexx -laff
LIBFAPIDXX=-lfapidxx $(LIBDATRWXX)
LIBTSIOXX=-ltsioxx -ltsxx -ltfxx $(LIBDATRWXX) -lgsl -lgslcblas

#----------------------------------------------------------------------
# binary executable targets
# -------------------------
#
# Fortran programs
# ----------------
#

xyz2uvw tisli susei smoos rotate evelo coro: %: %.o
	$(LINK.f) $^ $(LOADLIBES) $(LDLIBS) -o $@ \
	  -lsff -ltf

tsfilt stufi: %: %.o
	$(LINK.f) $^ $(LOADLIBES) $(LDLIBS) -o $@ \
	  -lseife -lsff -ltf

suspect dise: %: %.o
	$(LINK.f) $^ $(LOADLIBES) $(LDLIBS) -o $@ \
	  -lsffu -ltime -lsff -ltf
	 
# Fortran programs with libfapidxx support
# ----------------------------------------

evelox suseix: %x: %.o
	$(LINK.f) $^ $(LOADLIBES) $(LDLIBS) -o $@ \
	  -ltf  $(LIBFAPIDXX) 
#
# C++ programs
# ------------

sigval sigscale geophone gatherdiff autocorr: \
  %: %.o
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@ \
	  $(LIBDATRWXX) -ltfxx

tidofi: \
  %: %.o
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@ \
	  -ltsxx -lcseife $(LIBDATRWXX) -ltfxx -lgsl -lgslcblas

fidasexx: \
  %: %.o
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@ \
	  -llinearxx -lpgplotxx -lcpgplot \
	  $(LIBDATRWXX) -ltfxx \
	  $(TF_LINK_PGPLOT) $(TF_LINK_FORTRAN) \
	  -llapack

sigfit: \
  %: %.o
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@ \
	  -llinearxx \
	  $(LIBDATRWXX) -ltfxx \
	  -llapack 

resaseda: \
  %: %.o
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@ \
	  -ltsxx $(LIBDATRWXX) -ltfxx

teseco: \
  %: %.o
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@ \
	  $(LIBTSIOXX)

noisymize: \
  %: %.o
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@ \
	  $(LIBTSIOXX) 

cross fregra foutra deconv: \
  %: %.o
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@ \
	  -lfourierxx $(LIBTSIOXX) -lfftw3 

soutifu: \
  %: %.o
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@ \
	  -lstfinv -lfourierxx $(LIBTSIOXX) -lfftw3

#======================================================================
# documentation part
# ------------------
#
# targets commonly used:
# ----------------------
#
# make doxyclean        removes all documentation
# make doxydoc          creates doxygen documentation in the DOXYWWWPATH
# make doxyview         creates doxygen documentation and launches netscape to
#                       browse in the documentation
# make doxyconf         edit the doxygen configuration file
#
# If you launch "make doxydoc" the documentation will be written to
# DOXYWWWPATH (see below). This is meant to export the documentation through
# your homepage. The doxyfull directory is just a symbolic link to this
# directory.
#

$(call CHECKVARS,TF_WWWBASEDIR TF_BROWSER)

DOXYWWWPATH=$(TF_WWWBASEDIR)/tswf

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH)

DOXYSRC=$(DOXYTXT) $(wildcard *.h *.cc *.f)

# create doxygen intermediate configuration
PWD=$(shell env pwd)
doxydoc.xxx: doxydoc.cfg
	sed 's,<OUTPUTDIRECTORY>,$(DOXYWWWPATH),g;s,<STRIPFROMPATH>,$(PWD),g' \
	  $< > $@
# create commented version of doxygen configuration
doxycomm.xxx: doxydoc.cfg
	/bin/cp -vf $< $@; doxygen -u $@

$(DOXYWWWPATH)/html/index.html: doxydoc.xxx $(DOXYSRC)
	mkdir -vp $(DOXYWWWPATH)
	doxygen $<

doxydoc: $(DOXYWWWPATH)/html/index.html

doxyview: $(DOXYWWWPATH)/html/index.html
	$(TF_BROWSER) file:$< &

# ----- END OF Makefile ----- 
