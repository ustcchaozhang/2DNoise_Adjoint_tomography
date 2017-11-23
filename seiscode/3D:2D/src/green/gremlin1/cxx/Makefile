# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
# 
# C++ interface to gremlin1
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
# ============================================================================
#
# REVISIONS and CHANGES 
#    30/12/2002   V1.0   Thomas Forbriger (thof)
#    09/02/2010   V1.1   checked for new compiler
#    14/01/2011   V1.2   provide reinstall target
#    03/02/2014 thof:    removed comment stripping
#    30/06/2016 thof:	 pass proper inlcude path to CXX compiler when
#                        creating dependency files
# 
# ============================================================================
#
# You will find the installed library header files in $(LOCINCLUDEDIR)/aff

.PHONY: all
all: install doxydoc

.PHONY: doc
doc: doxydoc

# check mandatory environment variable settings
# ---------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_BROWSER TF_WWWBASEDIR)

# compiler and preprocessor flags
FLAGS=
CXXFLAGS=-Wall -pedantic $(FLAGS) -O3
CXXFLAGS=-Wall $(FLAGS) -O3
LDFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR)))

# files
# -----

# all header files used within this project
HEADERS=$(shell find . -name \*.h)
# all source code to be compiled to object files and to be included
# in the binary version of the library
# (see below for the configuration of a preinstantiated version of template
# code)
SRC=polymodel.cc greenspec.cc srcires.cc
# test programs are placed in a subdirectory
TESTS=$(wildcard tests/*.cc)
# whereever we find a README, we will use it
README=$(shell find . -name README) 

# place where we will copy header files
INCINSTALLPATH=$(LOCINCLUDEDIR)/gremlin1
# place where we will copy the binary library
LIBINSTALLPATH=$(LOCLIBDIR)
#
# name of installed (exported) header files (these are the names in your
# include directory)
INSTHEADER=$(addprefix $(INCINSTALLPATH)/,$(filter-out ./tests/%,$(HEADERS)))

#----------------------------------------------------------------------

# files which are to be edited
flist: Makefile doxydoc.cfg $(README) $(HEADERS) $(SRC)
	echo $^ | tr ' ' '\n' | sort >> $@

# Offers you a list of files to edit (ask Thomas for appropriate vim
# configuration).
.PHONY: edit
edit: flist; vim $<

# Target clean removes everything except the source code, headers and
# documentation. (See also doxyclean)
.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.o | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o install-include libgremlin1xx.a *.xxx junk*

#======================================================================
# library part
# ============
#
# create the binary library
# -------------------------
LIBOBS=$(ALLOBS) $(patsubst %.cc,%.o,$(SRC))
LIBOBS=$(patsubst %.cc,%.o,$(SRC))

libgremlin1xx.a: install-include $(LIBOBS)
	ar rcv $@ $(LIBOBS)
	ranlib $@

LIBRARIES=libgremlin1xx.a

$(LOCLIBDIR): ; mkdir -pv $@
.PHONY: install
install: $(LOCLIBDIR)
	$(MAKE) install-include
	$(MAKE) $(LIBRARIES)
	/bin/mv -fv $(LIBRARIES) $(LOCLIBDIR)

#======================================================================
# dependencies
# ------------
#
# The compiler is used to create dependency files, which are included below.

%.d: %.cc
	$(SHELL) -ec '$(CXX) $(CPPFLAGS) -M  $(TMPPARDEF) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

-include $(patsubst %.cc,%.d,$(SRC)) 

#======================================================================
# header files
# ------------
#
      
$(INCINSTALLPATH)/%.h: %.h
	mkdir -vp $(dir $@)
	-rm -fv $@
	/bin/cp -vpd $$(pwd)/$< $@

# install header files
.PHONY: install-include
install-include: $(INSTHEADER)
	echo $^ | tr ' ' '\n'

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

DOXYWWWPATH=$(TF_WWWBASEDIR)/libgremlin1xx

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH)

DOXYSRC=$(README) $(HEADERS) $(SRC) \

# create doxygen intermediate configuration
PWD=$(shell env pwd)
doxydoc.xxx: doxydoc.cfg
	sed 's,<OUTPUTDIRECTORY>,$(DOXYWWWPATH),g;s,<STRIPFROMPATH>,$(PWD),g' \
	  $< > $@

$(DOXYWWWPATH)/html/index.html: doxydoc.xxx $(DOXYSRC)
	mkdir -vp $(DOXYWWWPATH)
	doxygen $<

doxydoc: $(DOXYWWWPATH)/html/index.html

doxyview: $(DOXYWWWPATH)/html/index.html
	$(TF_BROWSER) file:$< &

# ----- END OF Makefile ----- 
