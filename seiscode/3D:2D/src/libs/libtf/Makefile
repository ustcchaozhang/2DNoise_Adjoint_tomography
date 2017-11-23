# this is <Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
# 
# source code of libtf.a
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
#    05/06/2007   V1.0   Thomas Forbriger
#    27/11/2007   V1.1   g77 compilation is the default now
#    17/12/2007   V1.2   moved to gfortran
#    19/09/2010   V1.3   start migration to new SVN scheme
#                        discard f2c option (fallback)
#                        check variables
#                        creation of package not yet defined
#    13/11/2010   V1.4   added Fortran interface to GSL Bessel functions
#                 V1.5   avoid makefdoc.pl
#    17/01/2011   V1.6   distinguish library creation and installation
# 
# ============================================================================
#

.PHONY: all
all: install doc

.PHONY: doc
doc: libtf.doc

LIBRARIES=libtf.a

.PHONY: install
install: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: install-include %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $(word 2,$^) $(LOCLIBDIR)

.PHONY: reinstall
reinstall:
	$(MAKE) clean
	$(MAKE) clean-include
	$(MAKE) install

#----------------------------------------------------------------------

.PHONY: clean edit
clean:
	-/bin/rm -fv *.o *.bak *.o77 flist *.xxx *.f2c.o *.a *.d
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist testlibtf77 testlibf2ctf testlibtf

edit: flist ; vim flist
flist: *.c *.h *.f Makefile README COPYING README.PGPLOT.copyright.notice
	echo $^ | tr ' ' '\n' | sort > $@
	 
# ============================================================================
CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR)

INCLUDEFLAGS=$(addprefix -I,$(LOCINCLUDEDIR) $(subst :, ,$(SERVERINCLUDEDIR)))
LIBFLAGS=$(addprefix -L,$(LOCLIBDIR) $(subst :, ,$(SERVERLIBDIR)))
FLAGS += $(MYFLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += -O3 $(INCLUDEFLAGS) $(FLAGS)
CPPFLAGS += $(INCLUDEFLAGS) $(FLAGS)
LDFLAGS += $(LIBFLAGS) $(FLAGS)

AR=ar
RANLIB=ranlib

SUBS=tf_cmdline.o tf_costap.o tf_doubletoint.o tf_fork.o tf_inttodouble.o\
     tf_inttoreal.o tf_listselect.o tf_realtoint.o tf_rectint.o\
     tf_bess.o pgp.o gpt.o tf_magic.o tf_bytesex.o tf_swap.o tf_dfork.o\
     tf_nice.o tf_chain.o tf_gauss.o tf_cbytesex.o tf_cswap.o tf_random.o \
     tf_time.o tf_rcol.o tf_ttaper.o tfgmt_makexyz.o tffile_newtmp.o \
     tfstr_trimlen.o tfstr_trimbeg.o tf_nameext.o tf_hpbut.o tf_lpbut.o \
     tf_pglogwedg.o tf_readrc.o tf_gsl_rnd.o tf_gsl_bess.o

#======================================================================
# documentation
DOCSRC=tf_cmdline.f tf_listselect.f tf_chain.f tf_nameext.f\
       tf_rcol.f\
       tf_bytesex.f tf_magic.f tf_swap.f\
       tf_realtoint.f tf_inttoreal.f tf_doubletoint.f tf_inttodouble.f\
       tf_ttaper.f \
       tf_costap.f tf_rectint.f tf_nice.f\
       tf_hpbut.f tf_lpbut.f\
       tf_fork.f tf_dfork.f tf_gauss.f\
       pgp.f gpt.f tffile_newtmp.f tfstr_trimlen.f tfstr_trimbeg.f \
       tfgmt_makexyz.f tf_pglogwedg.f tf_readrc.f

docs: libtf.doc 

libtf.doc: $(DOCSRC)
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^cS/ { hot=1; \
          print "c\nc -----------------------------------------\nc"; \
          next; } \
        /^cE/ { hot=0; } \
        { if (hot==1) { print $$0; } }' $^ > $@

#======================================================================
# C header files

HEADERS=$(wildcard *.h)
INSTHEADER=$(addprefix $(LOCINCLUDEDIR)/,$(HEADERS))

.PHONY: install-include
install-include:  $(INSTHEADER)
$(LOCINCLUDEDIR)/%.h: %.h
	mkdir -pv $(dir $@)
	/bin/cp -fv $< $@

.PHONY: clean-include
clean-include: libtf.h
	-/bin/rm -vf $(addprefix $(LOCINCLUDEDIR),$^)

%.d: %.c
	$(SHELL) -ec '$(CC) -M $(CPPFLAGS) $< \
      | sed '\''s,\($(notdir $*)\)\.o[ :]*,$(dir $@)\1.o $@ : ,g'\'' \
        > $@; \
      [ -s $@ ] || rm -f $@'

include $(patsubst %.c,%.d,$(wildcard *.c)) 

#======================================================================
# compile

%.o: %.c
	$(CC) -O2 -o $@ -c $< -Wall $(CFLAGS) $(CPPFLAGS)
%.o: %.f
	$(FC) -O2 -o $@ -c $< $(FFLAGS)

libtf.a: $(SUBS)
	$(AR) rcv libtf.a $(SUBS)
	$(RANLIB) libtf.a

testlibtf: testlibtf.o install
	$(FC) -o $@ $< -ltf -lm -lgsl -lgslcblas $(FFLAGS) $(LDFLAGS)

#======================================================================
# testcode
%.sort.xxx: %.xxx; sort -n $< > $@
%.plot.xxx: %.xxx
	echo 'plot "$<" ' > $@
	echo 'pause -1 "press return to continue"' >> $@
%.plot: %.plot.xxx; gnuplot $<

rngtest: testlibtf
	./testlibtf -rng -f -n 9900
	$(MAKE) test_gaussian.sort.plot test_uniform.sort.plot

#======================================================================
# create package
# --------------

ifdef TF_MAKEPKG
.PHONY: package
package: $(TF_MAKEPKG)
	$(MAKE) -f $< \
        PACKAGE=libtf \
        PACKAGEEXPORT="trunk/src/libs/libtf:src" \
        PACKAGETARGETS="src:install: src:doc:" \
        PACKAGELIBS="-"
endif
# ----- END OF Makefile ----- 
