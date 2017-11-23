# this is <libs/Makefile>
# ----------------------------------------------------------------------------
# $Id$
# 
# Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
# 
# Makefile for all Fortran77 gremlin library subroutines
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
# linearized least squares for greens matrix
#
# 01/03/2002      copy glq_dim.inc from standard if not present
#
# 
# REVISIONS and CHANGES 
#    01/03/2002   V1.0   Thomas Forbriger
#    16/04/2002   V1.1   introduced variance version of res_opt
#    03/05/2002   V1.2   introduced external parselect
#    04/07/2002   V1.3   new mseisfk offset removal
#    09/02/2010   V1.4   transition to gfortran
#    15/11/2010   V1.5   avoid makefdoc.pl
#    17/11/2010   V1.6   migrated to SVN scheme; avoid incdep
# 
# ============================================================================
#

.PHONY: all
all: install doc
.PHONY: install
install: libs
.PHONY: doc
doc: docs

LIBRARIES=libginmod.a libgindat.a libgininv.a libginpar.a libginpg.a libgin.a \
  libginres.a

.PHONY: libs
libs: $(addprefix $(LOCLIBDIR)/,$(LIBRARIES))
$(LOCLIBDIR)/%: %
	mkdir -pv $(LOCLIBDIR)
	/bin/mv -fv $< $(LOCLIBDIR)
	
# check mandatory environment variable settings
# ---------------------------------------------

CHECKVAR=$(if $($(1)),,$(error ERROR: missing variable $(1)))
CHECKVARS=$(foreach var,$(1),$(call CHECKVAR,$(var)))

$(call CHECKVARS,LOCINCLUDEDIR LOCLIBDIR LOCBINDIR)

FLAGS += $(MYFLAGS)
FFLAGS += -ff2c -Wall -ffixed-line-length-0 -fno-backslash $(FLAGS)
CFLAGS += $(FLAGS)
LDFLAGS+=-L$(LOCLIBDIR)
CPPFLAGS+=-I$(LOCINCLUDEDIR) $(FLAGS)

AR=ar
RANLIB=ranlib

DATOBS=dat_cgreen.o dat_cref.o dat_ctt.o dat_dcpc.o dat_dctt.o dat_dmode.o \
  dat_famp.o dat_fcamp.o dat_fmamp.o dat_ftt.o dat_rgreen.o dat_rtt.o \
  dat_rweight.o dat_synt.o dat_x2.o dat_wresp.o dat_dctt1.o dat_mtt.o \
  dat_fmaxamp.o dat_mmweights.o dat_wtweight.o dat_wgweight.o \
  dat_mscorr.o dat_mscprep.o
INVOBS=inv_addtolist.o inv_dss.o inv_dssd.o inv_dssdelta.o inv_linsynt.o \
  inv_linx2.o inv_mat.o inv_min.o inv_model.o inv_part.o inv_x2.o \
  inv_x2hist.o inv_ds.o inv_repchange.o
MODOBS=mod_aclear.o mod_check.o mod_chop.o mod_clear.o mod_copy.o \
  mod_follow.o mod_isec.o mod_panic.o mod_parcor.o mod_pcheck.o mod_prep.o \
  mod_read.o mod_save.o mod_showparcor.o mod_weight.o mod_write.o \
  mod_identify.o mod_follow1.o mod_read1.o mod_eval.o mod_writeold.o \
  mod_track.o
PAROBS=par_chop.o par_definv.o par_defpg.o par_defverb.o par_feas.o \
  par_parcor.o par_read.o par_sano.o par_set.o par_setsrc.o par_showdat.o \
  par_showindex.o par_showinv.o par_showmod.o par_showsrc.o par_verbosity.o \
  pars_verb.o par_setdefinv.o par_setdefpg.o par_setdefverb.o \
  par_wdec.o par_wenc.o par_wread.o par_wsave.o par_wcheck.o par_wwrite.o \
  par_pardisp.o par_showmodes.o par_setdebug.o par_pgset.o par_showpg.o \
  par_pgapply.o par_pgmon.o par_setdefpgmon.o
PGOBS=pg_fullset.o pg_gdata.o pg_green.o pg_inv.o pg_invfix.o pg_linx2.o \
  pg_min.o pg_mod.o pg_opt.o pg_selvp.o pg_tdata.o pg_tt.o pg_x2.o \
  pgs_addval.o pgs_curve.o pgs_curve2.o pgs_par.o pg_gpart.o pg_tpart.o \
  pg_resmod.o pg_sresmod.o pgs_par1.o pg_scoli.o pg_colis.o pg_zcoli.o \
  pg_tapgreen.o pgmon_update.o pgmon_togupdt.o pgmon_tmgupdt.o \
  pgmon_trgupdt.o
RESOBS=res_part.o res_opt.o res_orth.o res_wgpart.o res_wresmod.o \
  res_optrms.o res_optsqr.o res_parselect.o
OLDOBS=mod_parcor1.o mod_chop1.o
OLGPGOBS=
ALLOBS=$(PGOBS) $(PAROBS) $(MODOBS) $(DATOBS) $(INVOBS) $(RESOBS)

#----------------------------------------------------------------------

clean:
	-/bin/rm -vf *.o *.bak *.o77 *.d *.a

#----------------------------------------------------------------------

docs: libmod.doc libdat.doc libpar.doc libpg.doc libinv.doc libres.doc

libdat.doc: $(patsubst %.o,%.f,$(DATOBS)) glq_data.inc dat_oldhead.f

libmod.doc: $(patsubst %.o,%.f,$(MODOBS)) glq_model.inc mod_oldhead.f glq_strings.inc

libinv.doc: $(patsubst %.o,%.f,$(INVOBS)) glq_inv.inc glq_invpara.inc inv_oldhead.f

libpar.doc: $(patsubst %.o,%.f,$(PAROBS)) glq_para.inc par_oldhead.f

libpg.doc: $(patsubst %.o,%.f,$(PGOBS)) glq_pg.inc glq_pgpara.inc pg_oldhead.f

libres.doc: $(patsubst %.o,%.f,$(RESOBS)) glq_invres.inc

%.doc:
	/usr/bin/awk 'BEGIN{ hot=0; } \
        /^cS/ { hot=1; \
	    print FILENAME; \
          print "  c\n  c"; \
          next; } \
        /^cE/ { hot=0; } \
        { if (hot==1) { print "  " $$0; } }' $^ > $@

#----------------------------------------------------------------------

glq_dim.inc: glq_dimstd.inc; /bin/cp -v $< $@
	 
# Fortran dependencies
# --------------------
%.d: %.f
	echo $<: $(shell cat $< | egrep '^ +include' | cut -f 2 -d \' | sort | uniq) > $@
-include $(patsubst %.f,%.d,$(wildcard *.f))

libginres.a: $(RESOBS)
libginmod.a: $(MODOBS)
libgindat.a: $(DATOBS)
libgininv.a: $(INVOBS)
libginpar.a: $(PAROBS)
libgin.a: $(PGOBS) $(PAROBS) $(MODOBS) $(DATOBS) $(INVOBS) $(RESOBS)
libginpg.a: $(PGOBS)

$(LIBRARIES): %.a:
	ar rcv $@ $^
	ranlib $@

allgremlin:
	make libgin.a
	(cd ..; make gremlin)

# ----- END OF libs/Makefile ----- 
