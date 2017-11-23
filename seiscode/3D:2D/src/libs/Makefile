# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
# 
# recompile all libraries
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
# ----------------------------------------------------------------------------
# 
# Standard make targets 
# ---------------------
# 
# Makefiles in the library subdirectories and the master Makefile in
# trunk/src/libs should provide the following targets:
# 
# all
#     installs header files, the binary libraries and documentation 
# install
#     just install the header files and binary libraries 
# install-include
#     just install the header files 
# reinstall
#     reinstall header files and binary libraries by removing old versions of
#     the header files first 
# doc
#     create documentation files where available (this should also call the
#     doxydoc target, which is currently [Jan 2011] not yet the case) 
# doxydoc
#     create and install doxygen documentation where available 
# clean
#     removes all local intermediate files (like binary object files or
#     dependency lists) 
# 
# 
# REVISIONS and CHANGES 
#    05/07/2005   V1.0   Thomas Forbriger (thof)
#    20/11/2007   V1.1   discontinued libbuggy
#    19/12/2007   V1.2   first install headers then compile
#                        first remove .d-files then the rest
#    12/01/2010   V1.3   provide target install for consistency
#    31/01/2014          added libtsioxx, re-ordered flist entries
#    10/02/2014 thof:    use new package installation scheme with install.sh
#    24/09/2014 thof:	 add librefpar
#
# ============================================================================
#
LIBDIRS= \
  libtfxx \
  libaff \
  libtime \
  libsff \
  libsffu  \
  libgsexx  \
  libsffxx \
  libdatrwxx \
  libemod \
  libfourier  \
  liblinearxx \
  libpgplotxx \
  libseife \
  libtf \
  libts \
  libtsxx \
  libfapidxx \
  librefpar \
  libgrrefsub \
  librheology \
  libstfinv \
  libttsynt \
  libserialxx \
  libtsioxx

# libraries with snapshot package make rules
PACKAGEDIRS=libaff \
  libtime \
  libsff \
  libdatrwxx \
  libstfinv \
  libtsioxx

TARGETS=$(addsuffix .install,$(LIBDIRS))
REINSTALL=$(addsuffix .reinstall,$(LIBDIRS))
INSTINCLUDE=$(addsuffix .install-include,$(LIBDIRS))
DOCTARGETS=$(addsuffix .doc,$(LIBDIRS))
PACKAGETARGETS=$(addsuffix .package,$(PACKAGEDIRS))
CLEANTARGETS=$(addsuffix .clean,$(LIBDIRS))

.PHONY: all
all: install doc
.PHONY: install
install: $(INSTINCLUDE) $(TARGETS)
.PHONY: install-include
install-include: $(INSTINCLUDE)
.PHONY: reinstall
reinstall: $(REINSTALL)
.PHONY: doc
doc: $(DOCTARGETS)
.PHONY: packages
packages: $(PACKAGETARGETS)
.PHONY: show
show: ; @echo $(TARGETS)

EDITFILES=Makefile $(filter-out %.bak,$(wildcard README*)) COPYING \
	    doxydoc.cfg 
ALLMAKEFILES=$(patsubst ./%,%,$(shell find . -name Makefile))
flist: $(EDITFILES) $(ALLMAKEFILES) $(TF_EDIT)
	echo $(EDITFILES) | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(filter-out $(EDITFILES),$(ALLMAKEFILES)) \
	  | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(filter-out $(EDITFILES) $(ALLMAKEFILES),$^) \
	  | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean:
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist
	$(MAKE) $(CLEANTARGETS) 

%.install: %; cd $<; $(MAKE) install
%.package: %; cd $<; $(MAKE) package
%.reinstall: %
	echo
	echo '$$Id$$'
	echo $(MAKE) $@
	cd $<; $(MAKE) reinstall
%.install-include: %; -(cd $<; $(MAKE) install-include)
%.doc: %
	-(cd $<; $(MAKE) doc)
	-(cd $<; $(MAKE) doxydoc)
%.clean: %; cd $<; $(MAKE) clean

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

DOXYWWWPATH=$(TF_WWWBASEDIR)/SeitoshLibraries

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH) doxydoc.xxx

DOXYSRC=$(DOXYTXT) $(HEADERS) $(SRC)

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
