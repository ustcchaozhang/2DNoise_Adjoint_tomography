# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
# 
# create library for source wavelet correction filter determination (libstfinv)
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
# See the README and the doxygen documentation for a list of libraries
# required in addition to libstfinv.
# 
# REVISIONS and CHANGES 
#    05/05/2011   V1.0   Thomas Forbriger (thof)
#    03/02/2014 thof:	 do not strip comments from header files
#
# ============================================================================
#

.PHONY: all
all: install doxydoc

.PHONY: doc
doc: doxydoc

LIBRARIES=libstfinv.a

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

# ============================================================================
# select groups of files 
# they are used for editing and to create object file lists and dependencies

# whereever we find a README, we will use it
README=$(filter-out %.bak,$(shell find . -name README\*))
# the frame of doxygen documentation is palced in text files
DOXYTXT=$(shell find . -name doxygen\*.txt)
# C++ and C source code
SOURCEFILES=$(filter-out %usage.h, $(filter-out %usage.cc, \
		$(wildcard *.h *.cc *.c tests/*.cc tests/*.c tests/*.inp) \
	    $(wildcard test*/src/*.c) \
	    $(wildcard test*/src/*.h) \
	    $(wildcard test*/src/LEGAL*h)))
# LaTeX formatted documentation
DOCFILES=$(wildcard doc/*.tex doc/Makefile)
# end-user usage information
USAGEFILES=$(wildcard usage/*usage.txt)
# editional files to be edited
EDITFILES=Makefile $(README) \
	    $(DOXYTXT) $(DOCFILES) $(USAGEFILES) \
	    $(SOURCEFILES) \
	    $(wildcard *.cfg) \
	    $(wildcard test*/src/Makefile) \
	    COPYING
# ============================================================================
# check for settings expected in the environment
# ----------------------------------------------
#
CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))
#

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_BROWSER TF_WWWBASEDIR)

# ======================================================================
#
LIBHEADERS=$(filter-out none,$(wildcard *.h)) \
	     $(patsubst usage/%.txt,%.h,$(USAGEFILES))

LIBCCSRC=$(filter-out none,$(wildcard *.cc)) \
	   $(patsubst usage/%.txt,%.cc,$(USAGEFILES))

LIBCSRC=$(filter-out none,$(wildcard *.c))

TESTHEADERS=$(wildcard tests/*.h)

TESTCCSRC=$(wildcard tests/*.cc)
TESTCSRC=$(wildcard tests/*.c)

TESTCCBIN=$(patsubst tests/%.cc,%,$(TESTCCSRC))
TESTCBIN=$(patsubst tests/%.c,%,$(TESTCSRC))

INCINSTALLPATH=$(LOCINCLUDEDIR)/stfinv
LIBINSTALLPATH=$(LOCLIBDIR)

# name of installed (exported) header files
INSTHEADER=$(addprefix $(INCINSTALLPATH)/,$(notdir $(LIBHEADERS)))


#
# compiler flags
# --------------

FLAGS=
FLAGS+=-fPIC $(MYFLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS+=-L$(LOCLIBDIR)
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)
# 
# source code editing and directory clean-up
# ------------------------------------------

flist: $(EDITFILES) $(TF_EDIT)
	echo $(filter-out $(DOCFILES) $(SOURCEFILES) \
	  $(USAGEFILES),$(EDITFILES)) \
	  | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(filter-out test%,$(SOURCEFILES)) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(USAGEFILES) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(filter test%,$(SOURCEFILES)) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(DOCFILES) | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(TF_EDIT) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.o | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.hd | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.a *.so *.xxx $(TESTCBIN) $(TESTCCBIN)
	-/bin/rm -fv $(patsubst %.txt,%.h,$(USAGEFILES))
	-/bin/rm -fv $(patsubst %.txt,%.cc,$(USAGEFILES))

#======================================================================
# pattern rules
# -------------

%.d: %.cc
	$(SHELL) -ec '$(CXX) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

%.d: %.c
	$(SHELL) -ec '$(CC) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

#======================================================================
# library part
# ------------

include $(patsubst %.cc,%.d,$(LIBCCSRC) $(TESTCCSRC)) \
        $(patsubst %.c,%.d,$(LIBCSRC) $(TESTCSRC))

libstfinv.a: $(patsubst %.cc,%.o,$(LIBCCSRC)) \
             $(patsubst %.c,%.o,$(LIBCSRC))
	ar rcv $@ $^
	ranlib $@

#----------------------------------------------------------------------

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


# ======================================================================
# source code generators
# ======================
#
# description and online texts
# ----------------------------
#  The following rules convert ordinary ASCII text files into C++ source code,
#  providing functions to output usage information (online help)
#
#  Such text files use names with the pattern *usage.txt
#  Automatically generated source code is excluded from editing and versioning
#
%usage.cc %usage.h: usage/%usage.txt
	echo "// DO NOT EDIT: this file is automatically derived from $<" \
	  > $(patsubst usage/%.txt,%.h,$<)
	echo "extern char $(patsubst usage/%.txt,%,$<)[];" \
	  >> $(patsubst usage/%.txt,%.h,$<)
	echo "// DO NOT EDIT: this file is automatically derived from $<" \
	  > $(patsubst usage/%.txt,%.cc,$<)
	echo "#include \"$(patsubst usage/%.txt,%.h,$<)\"" \
	  >> $(patsubst usage/%.txt,%.cc,$<)
	echo "char $(patsubst usage/%.txt,%,$<)[]=" \
	  >> $(patsubst usage/%.txt,%.cc,$<)
	echo "{" >> $(patsubst usage/%.txt,%.cc,$<)
	cat $< | egrep -v '^#' | sed -e 's/\\/\\\\/g' | sed -e 's/"/\\"/g' \
	  | sed -e 's/$$/\\n"/' | sed -e 's/^/  "/'\
	  >> $(patsubst usage/%.txt,%.cc,$<)
	echo "};" >> $(patsubst usage/%.txt,%.cc,$<)

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

DOXYWWWPATH=$(TF_WWWBASEDIR)/libstfinv

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH) doxydoc.xxx

DOXYSRC=$(DOXYTXT) $(LIBCCSRC) $(LIBCSRC) $(LIBHEADERS)\
	  $(TESTCCSRC) $(TESTCSRC) \
        Makefile

PWD=$(shell env pwd)
# create doxygen intermediate configuration
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

#======================================================================
# test code
# ---------

$(TESTCCBIN): %: tests/%.o libstfinv.a
	$(CXX) $(FLAGS) -o $@ $^ -I$(LOCINCLUDEDIR) \
        -ltfxx -lfourierxx -lfftw3 -laff \
        -L$(LOCLIBDIR)

# ----- END OF Makefile ----- 
