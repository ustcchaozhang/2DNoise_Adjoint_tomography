# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
# 
# some test cases for libtfxx
#
# ----
# This file is part of libtfxx.
#
# libtfxx is free software; you can redistribute it and/or modify
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
# REVISIONS and CHANGES 
#  - 15/11/2002   V1.0   Thomas Forbriger (thof)
#  - 19/11/2002   V1.1   provide f2c rules too for tests on AIX
#  - 28/11/2002   V1.2   added blitztest
#  - 28/06/2005   V1.3   disabled blitz
#  - 05/12/2007   V1.4   provide test for regex
#    12/01/2014   V1.5   use FFLAGS instead of FCFLAGS conforming to GNU make
#                        default rules
#    03/02/2014 thof:	 expand server path variables; remove variable
#                        TF_LINK_REGEX
# 
# ============================================================================
#
LDFLAGS+=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR))) 
CPPFLAGS+=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR))) \
  $(FLAGS)
CFLAGS=$(CPPFLAGS)
CXXFLAGS=-Wall $(FLAGS)
#CXXFLAGS=$(FLAGS)

FC=g77
FFLAGS=-Wall -ffixed-line-length-0 -fno-backslash

CC=gcc
F2CFLAGS=-f -u

all:

flist: Makefile
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.o | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.d | xargs --no-run-if-empty /bin/rm -v
	-find . -name \*.o77 | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist junk* *.out
	-find . -type f -perm +0111 | xargs --no-run-if-empty /bin/rm -v

# all that only need templates from the library
STANDARDTEST=
# all that need the binary library
BINARYTEST=commandlinetest fortraniotest blitztest stringtest \
  handletest regextest filetest rangetest fstest exceptiontest \
  misctest
# F77 part of the I/O test
FORTRANTEST=fortranF77

CXXTESTS=$(STANDARDTEST) $(BINARYTEST)
ALLTESTS=$(CXXTESTS) $(FORTRANTEST)

$(addsuffix .o,$(FORTRANTEST)): %.o: %.f
	f2c $(F2CFLAGS) $<
	$(CC) -c $(CFLAGS) $(<:.f=.c)
	@rm  $(<:.f=.c)

$(addsuffix .o77,$(FORTRANTEST)): %.o77: %.f
	$(FC) -c -o $@ $< $(FFLAGS) $(CPPFLAGS) $(FLAGS)

$(addsuffix .o,$(CXXTESTS)): %.o: %.cc
	$(CXX) -c -o $@ $< $(CXXFLAGS) $(CPPFLAGS) $(FLAGS)

$(STANDARDTEST): %: %.o; $(CXX) -o $@ $< $(LDFLAGS)
$(BINARYTEST): %: %.o; $(CXX) -o $@ $< -ltfxx $(LDFLAGS)
$(FORTRANTEST): %: %.o; $(CC) -o $@ $< -ltf $(LDFLAGS) -lf2c -lm
$(addsuffix .g77,$(FORTRANTEST)): %.g77: %.o77
	$(FC) -o $@ $< $(LDFLAGS) -ltf77

#
# target to execute tests
#
# put all commandline arguments in $(ARG)
$(addsuffix .run,$(ALLTESTS)): %.run: %; ./$< $(ARG); rm -fv $< $<.o

#----------------------------------------------------------------------
# special rule for AIX test subroutine
TFLIBPATH=$(HOME)/work/src/libs/libtf
TFLIBSRC=$(addprefix $(TFLIBPATH)/,tf_cmdline.f tf_magic.f tf_bytesex.f)
fortranF77.aix: fortranF77.f $(TFLIBSRC)
	xlf -o $@ $^ -qfixed=100

#----------------------------------------------------------------------
# fortranio tests
fortraniotestaix: fortranF77.aix
	uname -a; echo
	-./fortranF77.aix -v -w junkaix
	-./fortranF77.aix -v -r junkaix
	-./fortranF77.aix -v -r junkintel
fortraniotestintel: fortranF77.g77 fortraniotest
	uname -a; echo
	-./fortranF77.g77 -v -w junkintel
	-./fortranF77.g77 -v -r junkintel
	-./fortranF77.g77 -v -r junkaix
	-./fortraniotest -v -c -s -C -w=junkintelxx -r=junkintelxx
	-./fortraniotest -v -r=junkintel
	-./fortraniotest -v -r=junkaix
	-./fortranF77.g77 -v -r junkintelxx

fortraniotest%.out: 
	$(MAKE) $(patsubst %.out,%,$@) > $@ 2>&1

# ----- END OF Makefile ----- 
