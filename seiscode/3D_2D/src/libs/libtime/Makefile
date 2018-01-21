# this is <Makefile>
# ----------------------------------------------------------------------------
# Makefile for work/src/libs/libtime
#
# how to build all flavours of libtime
# 
# 06/08/2000 by Thomas Forbriger (IfG Stuttgart)
# 
# ----
# libtime is free software; you can redistribute it and/or modify
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
#    06/08/2000   V1.0   Thomas Forbriger
#    12/10/2000   V1.1   get BINPREFIX from environment
#    22/12/2000   V1.2   introduced example++
#    29/03/2001   V1.3   added a package output and default target 'all'
#    28/03/2006   V1.4   added target package
#    29/03/2006   V1.5   no SERVER dirs; add tfmacros.h to package
#    04/04/2006   V1.6   provide global FLAGS
#    05/12/2007   V1.7   migration to fenoglio with new environment variables
#                        and new Fortan compilation default: g77
#    13/12/2007   V1.8   renamed target: install-include and corrected
#                        installation command for header files
#                        libtime_trad.a is continued
#                        timeutil.h is still needed by some programs
#    17/12/2007   V1.9   all components are tested with their test programs
#                        against results on system prior to migration
#                        moved to gfortran
#    18/09/2010   V1.10  start migration to new SVN repository scheme
#                        fallback solution is removed
#                        f2c version is discarded
#                        doxygen docu uses new scheme
#                        delegate package creation
#    17/01/2011   V1.11  distinguish library creation and installation
#    10/02/2011   V1.12  publish package package 
#    22/03/2013   V1.13  do not set default compilers
# 
# ============================================================================
#

.PHONY: all
all: install doc example exectests

.PHONY: example
example: tests/example++; tests/example++

.PHONY: exectests
exectests: tests

.PHONY: doc
doc: libtime.doc doxydoc

LIBRARIES=libtime_trad.a libtime.a libctime.a libtime++.a libtime++.so

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

# ============================================================================
# a variable definition to check variable settings
# ------------------------------------------------

# if you want to check whether variable TF_PUBLICATIONPATH is non-empty,
# insert line
#
## $(call CHECKVAR,TF_PUBLICATIONPATH)
#
# in your Makefile
#
# if you want to check whether variable AVAR, BVAR, and CVAR are non-empty,
# insert line
#
## $(call CHECKVARS,AVAR BVAR CVAR)
#
# in your Makefile

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,TF_WWWBASEDIR TF_BROWSER)
$(call CHECKVARS,LOCLIBDIR LOCBINDIR LOCINCLUDEDIR)

#----------------------------------------------------------------------
# variables
#
AR?=ar
AS?=as
RANLIB?=ranlib

FLAGS=
FLAGS+=-fPIC $(MYFLAGS)
F2CFLAGS=-f -u
FFLAGS += -Wall -ff2c $(FLAGS)
CFLAGS += -O2 $(FLAGS)
LDFLAGS+=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
CPPFLAGS+=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)

# how to link the different varieties
LIBTIME=-ltime -L$(LOCLIBDIR)
LIBCTIME=-lctime -L$(LOCLIBDIR)
LIBCXXTIME=-ltime++ -L$(LOCLIBDIR)
LIBTIMETRAD=-ltime_trad -L$(LOCLIBDIR)

# external homemade library (needed by testlibtime)
LIBSFFU=-lsffu -L$(LDFLAGS)

#======================================================================
#
# source code package definitions
# -------------------------------

# the libtime kernel written in FORTRAN
KERNEL=kernel/time_add.f kernel/time_clear.f kernel/time_compare.f \
       kernel/time_copy.f kernel/time_div.f kernel/time_finish.f \
       kernel/time_fullyear.f kernel/time_getdate.f kernel/time_isleapyear.f \
       kernel/time_libversion.f kernel/time_mul.f kernel/time_nfit.f \
       kernel/time_norm.f kernel/time_setdoy.f kernel/time_sub.f \
	 kernel/time_util_warning_report_time.f

# language specific code
# Fortran version
FCODE=    kernel/time_sprint.f kernel/time_read.f kernel/time_info.f
FHANDLER= kernel/time_util_fatal.f kernel/time_util_warning.f

# C: wrapper functions call underlying Fortran functions
CWRAPPER=Cwrapper/ctime_add.c Cwrapper/ctime_clear.c \
         Cwrapper/ctime_compare.c Cwrapper/ctime_copy.c Cwrapper/ctime_div.c \
         Cwrapper/ctime_finish.c Cwrapper/ctime_fullyear.c \
         Cwrapper/ctime_getdate.c Cwrapper/ctime_isleapyear.c \
         Cwrapper/ctime_libversion.c Cwrapper/ctime_mul.c \
         Cwrapper/ctime_nfit.c Cwrapper/ctime_norm.c Cwrapper/ctime_setdoy.c \
         Cwrapper/ctime_sub.c

# language specific code
# C version
CCODE=   Cwrapper/ctime_sprint.c Cwrapper/ctime_read.c 
CHANDLER=Cwrapper/ctime_util_fatal.c Cwrapper/ctime_util_warning.c
 
# language specific code
# C++ version
CXXHANDLER=C++wrapper/cxxtime_util_fatal.cc C++wrapper/cxxtime_util_warning.cc

# C++ functions
CXXCODE= C++wrapper/base_constr.cc \
  C++wrapper/convert.cc \
  C++wrapper/now.cc \
  C++wrapper/dump.cc \
  C++wrapper/ranges.cc \
  C++wrapper/error.cc \
  C++wrapper/usage.cc \
  C++wrapper/cxxtime_util_fatal.cc \
  C++wrapper/hierarchicaltimestring.cc \
  C++wrapper/cxxtime_util_warning.cc \
  C++wrapper/formatted_timestring.cc

#----------------------------------------------------------------------
# test code
CTESTS=  tests/ctlibtime.c tests/timer.c tests/timeutil.c 
FTESTS=  tests/testlibtime.f tests/ttime_read.f
CXXTESTS=tests/testtime++.cc tests/example++.cc

#----------------------------------------------------------------------
# header files
HEADERS=libtime.h libtime++.h timeutil.h

#----------------------------------------------------------------------
# assembled file collections
ALLCODE=$(KERNEL) $(FCODE) $(FHANDLER) $(CWRAPPER) $(CCODE) $(CHANDLER) \
        $(CXXCODE) $(CTESTS) $(FTESTS) $(CXXTESTS) timeutil.c \
        $(HEADERS)

README=README README.changes README.files README.unsplit COPYING

ALLFILES=$(KERNEL) $(FCODE) $(CWRAPPER) $(CCODE) Makefile $(README) \
  timeutil.c timeutil.h libtime.h $(FHANDLER) $(CHANDLER) \
  $(CTESTS) $(FTESTS) libtime++.h $(CXXTESTS) $(CXXCODE) doxydoc.cfg

#======================================================================
#
# dependencies
# ------------
%.d: %.c 
	$(SHELL) -ec '$(CC) -M $(CPPFLAGS) $< \
                     | sed '\''s,\($*\)\.o[ :]*,\1.o $@ : ,g'\'' > $@; \
                     [ -s $@ ] || rm -f $@'
%.d: %.cc
	$(SHELL) -ec '$(CC) -M $(CPPFLAGS) $< \
                     | sed '\''s,\($*\)\.o[ :]*,\1.o $@ : ,g'\'' > $@; \
                     [ -s $@ ] || rm -f $@'

include $(patsubst %.c,%.d,$(CWRAPPER) $(CCODE) $(CHANDLER) $(CTESTS))
include $(patsubst %.cc,%.d,$(CXXTESTS) $(CXXCODE))

#======================================================================
# 
# rules
# -----

# how to create pure Fortran object files
%.o: %.f; $(FC) $(FFLAGS) $(FLAGS) -c -o $@ $^

# C++ stuff
%.o: %.cc; $(CXX) $(FLAGS) -o $@ $< -c -I$(LOCINCLUDEDIR)

# how to build a library
%.a: ; $(AR) rcv $@ $^ ; $(RANLIB) $@ 

#======================================================================
#
# misc targets
# ------------
.PHONY: clean edit install-include

clean:
	find . \( -name \*.bak -o -name \*.o -o -name \*.o77 -o -name \*.d \) | \
          xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -v flist libtime.doc *.tmp *.htmp *.out *.doc *.so *.a *.xxx

flist: $(ALLFILES)
	echo $^ | tr ' ' '\n' | sort > $@

edit: flist; vim $<

#======================================================================
# install headers
$(call CHECKVAR,LOCINCLUDEDIR)
$(LOCINCLUDEDIR)/%.h: %.h
	mkdir -vp $(dir $@)
	-rm -fv $@
	/bin/cp -vpd $< $@

# install header files
.PHONY: install-include
install-include: $(addprefix $(LOCINCLUDEDIR)/,$(HEADERS))

#======================================================================
# reinstall target
# is necessary in case of header file problems (e.g. remcmmnt not installed)
.PHONY: clean-include
clean-include: 
	/bin/rm -fv $(addprefix $(LOCINCLUDEDIR)/,$(HEADERS))

.PHONY: reinstall
reinstall:
	$(MAKE) clean
	$(MAKE) clean-include
	$(MAKE) install

#======================================================================
#
# library dependency definitions
# ------------------------------

# pure FORTRAN libtime (g77 version)
libtime.a: $(patsubst %.f,%.o,$(KERNEL) $(FCODE) $(FHANDLER))

# traditional libtime
libtime_trad.a: $(patsubst %.f,%.o,$(KERNEL) $(FCODE) $(FHANDLER)) \
                $(patsubst %.c,%.o,$(CWRAPPER) $(CCODE)) timeutil.o

# pure C libtime
libctime.a: $(patsubst %.f,%.o,$(KERNEL)) \
            $(patsubst %.c,%.o,$(CWRAPPER) $(CCODE) $(CHANDLER))

# C++ classlib
libtime++.a: $(patsubst %.f,%.o,$(KERNEL)) \
            $(patsubst %.c,%.o,$(CWRAPPER) $(CCODE)) \
            $(patsubst %.cc,%.o,$(CXXCODE) $(CXXHANDLER))

libtime++.so:  $(patsubst %.f,%.o,$(KERNEL)) \
            $(patsubst %.c,%.o,$(CWRAPPER) $(CCODE)) \
            $(patsubst %.cc,%.o,$(CXXCODE) $(CXXHANDLER))
	$(CXX) $(CXXFLAGS) -shared -o $@ $^

#======================================================================
#
# test code
# ---------

$(patsubst %.f,%,$(FTESTS)): %: %.o libtime.a
	$(FC) -o $@ $< $(LIBTIME) $(LIBSFFU) $(LDFLAGS)

tests/timer: %: %.o; $(CC) -o $@ $< $(LDFLAGS)
tests/ctlibtime: %: %.o; $(CC) -o $@ $< $(LIBCTIME) $(LDFLAGS)
tests/timeutil: %: %.o; $(CC) -o $@ $< -static $(LIBTIMETRAD) $(LDFLAGS)
tests/example++ tests/testtime++: %: %.o libtime++.a
	$(CXX) -o $@ $< $(LIBCXXTIME) $(LDFLAGS)

TESTS=$(patsubst %.c,%,$(CTESTS)) $(patsubst %.f,%,$(FTESTS)) \
       $(patsubst %.cc,%,$(CXXTESTS))

testsexe: $(TESTS)
	for d in $^; do printf "\n\nGO FOR $$d?\n"; read a; $$d; done

%.run: tests/%; $<; /bin/rm -fv $<
%.out: tests/%; ( $< 2>&1 ) > $@; /bin/rm -fv $<

# diff source:
DIFFREFERENCE=/home/thof/tmp/libtimeout/
%.diff: %.out $(DIFFREFERENCE)/%.out; diff $^

testresults: $(addsuffix .out,$(patsubst tests/%,%,$(TESTS)))

#======================================================================
#
# ASCII documentation (Fortran and C code)
# ----------------------------------------

kernel.tmp: $(KERNEL)
fortran.tmp: $(FCODE)

%.tmp:
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^cS/ { hot=1; \
          print "  c\n  c -----------------------------------------\n  c"; \
          next; } \
        /^cE/ { hot=0; } \
        { if (hot==1) { print "  " $$0; } }' $^ > $@

%.htmp: %.h
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^\/\*S\*\// { hot=1; next; } \
        /^\/\*E\*\// { hot=0; } \
        { if (hot==1) { print "  " $$0; } }' $^ > $@

libtime.doc: README kernel.tmp fortran.tmp libtime.htmp timeutil.htmp \
             libtime++.htmp $(ALLCODE)
	cat README > $@
	printf " \n \nDOCUMENTATION OF THE FORTRAN LIBRARAY KERNEL" >> $@
	printf "\n============================================\n\n" >> $@
	cat kernel.tmp >> $@
	printf ' \n \nDOCUMENTATION OF THE FORTRAN SPECIFIC CODE' >> $@
	printf      '\n==========================================\n\n' >> $@
	cat fortran.tmp >> $@
	printf ' \n \nDOCUMENTATION OF THE C WRAPPER CODE' >> $@
	printf          '\n===================================\n\n' >> $@
	cat libtime.htmp >> $@
	printf ' \n \nDOCUMENTATION OF THE TIMEUTIL C++ CLASSES' >> $@
	printf        '\n=========================================\n\n' >> $@
	cat libtime++.htmp >> $@
	printf ' \n \nDOCUMENTATION OF THE OLD TIMEUTIL C CODE' >> $@
	printf        '\n========================================\n\n' >> $@
	cat timeutil.htmp >> $@

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

DOXYWWWPATH=$(TF_WWWBASEDIR)/libtimexx

.PHONY: doxyclean doxyview doxydoc doxyconf

doxyclean: ;/bin/rm -rfv $(DOXYWWWPATH) doxydoc.xxx

DOXYSRC=$(CXXCODE) $(CXXTESTS) $(CXXHANDLER) $(HEADERS) 

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

#======================================================================
# WWW distribution
ifdef TF_PUBLICATIONPATH
WWWPATH=$(TF_PUBLICATIONPATH)/libtime
wwwdistrib: libtime.doc $(README)
	mkdir -pv $(WWWPATH)
	cd $(WWWPATH); /bin/rm -fv libtime*.tar.* 
	cd $(WWWPATH); ln -sfv ../libtime*.tar.* .
	/bin/cp -vf libtime.doc $(WWWPATH)/libtime.txt
	/bin/cp -vf tests/example++.cc $(README) $(WWWPATH)
	/usr/bin/yes end | $(MAKE) testsexe 
	/usr/bin/yes end | ( $(MAKE) testsexe 2>&1 ) > $(WWWPATH)/tests.out
	( tests/example++ 2>&1 ) > $(WWWPATH)/example.out
endif

#======================================================================
# create package
# --------------
# is delegated to Makefile.packages
ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
	  PACKAGE=libtime \
	  PACKAGEEXPORT="trunk/src/libs/libtime:src" \
	  PACKAGETARGETS="src:all" \
	  PACKAGELIBS="-"
endif

# ----- END OF Makefile ----- 
