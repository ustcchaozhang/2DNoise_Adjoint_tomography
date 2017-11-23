# this is <Makefile>
# ----------------------------------------------------------------------------
# Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
# 
# A Fortran API to libdatreadxx and libdatwritexx
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
#    17/11/2010   V1.0   Thomas Forbriger (thof)
#    17/01/2011   V1.1   distinguish library creation and installation
#    26/01/2014          (thof) no longer strip comments from header files
# 
# ============================================================================
#

.PHONY: all
all: install doxydoc

.PHONY: doc
doc: doxydoc

LIBRARIES=libfapidxx.a libfapidxx.so

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

# check mandatory environment variable settings
# ---------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)
$(call CHECKVARS,TF_BROWSER TF_WWWBASEDIR)

INCINSTALLPATH=$(LOCINCLUDEDIR)/fapidxx
LIBINSTALLPATH=$(LOCLIBDIR)
HEADERS=$(wildcard *.h)
CCSRC=$(wildcard *.cc)
FSRC=$(wildcard *.f)
LIBSRC=$(CCSRC) $(filter fapid_%,$(FSRC))

# whereever we find a README, we will use it
README=$(shell find . -name README)
# the frame of doxygen documentation is palced in text files
DOXYTXT=$(shell find . -name doxygen\*.txt)

FLAGS += $(MYFLAGS) -fPIC
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += $(FLAGS)
CXXFLAGS+=-Wall $(FLAGS)
LDFLAGS+=-L$(LOCLIBDIR)
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)

#----------------------------------------------------------------------

flist: Makefile $(HEADERS) $(README) $(LIBSRC) doxydoc.cfg \
  $(FSRC) $(DOXYTXT) COPYING
	echo $^ | tr ' ' '\n' | sort | uniq > $@
	echo '----' >> $@
	ls -1 ../libsff/*.[cfh] | tr ' ' '\n' | sort | uniq >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.o fapidtest *.a *.so *.xxx

#======================================================================
# dependencies
# ------------
#
# The compiler is used to create dependency files, which are included below.

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

include $(patsubst %.cc,%.d,$(SRCFILES)) 

#======================================================================
# header files
# ------------
#
      
$(call CHECKVAR,INCINSTALLPATH)
$(INCINSTALLPATH)/%.h: %.h
	@mkdir -vp $(dir $@)
	-@rm -fv $@
	@/bin/cp -vpd $< $@

# install header files
# INSTHEADER contains full qualified paths to library header files
# like /home/thof/include/libfapidxx/series.h
# usually then INCINSTALLPATH=/home/thof/include/libfapidxx
INSTHEADER=$(addprefix $(INCINSTALLPATH)/,$(HEADERS))
.PHONY: install-include
install-include: $(INSTHEADER)

.PHONY: clean-include
clean-include: 
	/bin/rm -fv $(INSTHEADER)

.PHONY: reinstall
reinstall:
	$(MAKE) clean
	$(MAKE) clean-include
	$(MAKE) install

#======================================================================
# create the binary library
# -------------------------
LIBOBS=$(patsubst %.f,%.o,$(patsubst %.cc,%.o,$(LIBSRC)))

# INSTHEADER contains full qualified paths to library header files
# like /home/thof/include/aff/series.h
libfapidxx.a: $(INSTHEADER) $(LIBOBS)
	ar rcv $@ $(LIBOBS)
	ranlib $@

libfapidxx.so: $(INSTHEADER) $(LIBOBS)
	$(CXX) $(CXXFLAGS) -shared -o $@ $(LIBOBS)

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

DOXYWWWPATH=$(TF_WWWBASEDIR)/libfapidxx

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH) doxydoc.xxx

DOXYSRC=$(DOXYTXT) $(HEADERS) $(LIBSRC) 

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

.PHONY: doc
doc: doxydoc
.PHONY: doxydoc
doxydoc: $(DOXYWWWPATH)/html/index.html

doxyview: $(DOXYWWWPATH)/html/index.html
	$(TF_BROWSER) file:$< &

#======================================================================
# test code

fapidtestx: fapidtest.o libfapidxx.a
	$(FC) -o $@ $< -lfapidxx -ldatrwxx -lsffxx -lgsexx -ltime++ \
	  -laff -lsffu -ltf -L$(LOCLIBDIR) -lstdc++
	/bin/mv -fv $@ $(LOCBINDIR)

fapidtest: fapidtest.o 
	$(FC) -o $@ $< -lsff -lsffu  -ltf -L$(LOCLIBDIR) 
	/bin/mv -fv $@ $(LOCBINDIR)

# ----- END OF Makefile ----- 
