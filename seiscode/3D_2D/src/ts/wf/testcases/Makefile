# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
# 
# test cases for waveform filters
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
# REVISIONS and CHANGES 
#    09/06/2011   V1.0   Thomas Forbriger
#    30/09/2011   V1.1   added soutifu test for the convolution of additional
#                        time series
# 
# ============================================================================
#

all:

TESTCASEMAKE=$(filter-out %.bak,$(wildcard Makefile*))
READMEFILES=$(filter-out %.bak,$(wildcard README*))
flist: $(TESTCASEMAKE) $(READMEFILES) $(wildcard *.par) $(wildcard *.tpl) \
  $(wildcard *.gpt)
	echo $(TESTCASEMAKE) | tr ' ' '\n' | sort > $@
	echo '----' >> $@
	echo $(filter-out $(TESTCASEMAKE) $(READMEFILES),$^) \
	  | tr ' ' '\n' | sort >> $@
	echo '----' >> $@
	echo $(READMEFILES) | tr ' ' '\n' | sort >> $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.xxx *.xxx.* *.sff *.TZ *.TR *.grn *.su *.bin
	-/bin/rm -vf *.ps *.pdf *.asc *.ascii 

#----------------------------------------------------------------------

# test cases for soutifu
soutifu%: Makefile.soutifu ; $(MAKE) -f $< $@

# soutifu test cases
SOUTIFUCASES=1 1_add 2a 2b 2c 3a 3b 3c
SOUTIFURESULTS=$(addprefix soutifu,$(addsuffix _report,$(SOUTIFUCASES)))
soutifutests: 
	$(MAKE) clean
	/bin/rm -fv soutifu* 
	$(MAKE) $(addsuffix .pdf,$(SOUTIFURESULTS)) 

#----------------------------------------------------------------------

# test cases for foutra
noise.xxx.ps: Makefile.foutra 
	/bin/rm -fv noise.xxx.*
	$(MAKE) -f $< $@ 
noise.sig.xxx.ps: Makefile.foutra 
	/bin/rm -fv noise.xxx.*
	$(MAKE) -f $< $@ 
harmspec.xxx.ps: Makefile.foutra 
	/bin/rm -fv sine?.xxx.sff
	$(MAKE) -f $< $@ 

#----------------------------------------------------------------------

# test cases for filter programs
# e.g.: filter.sig.lp2.ps
filter.%.ps: Makefile.filters
	/bin/rm -fv *.xxx.fil *.xxx.rules
	$(MAKE) -f $< $@ 

#----------------------------------------------------------------------

# test autocorr
autocorr.ps: Makefile.autocorr
	/bin/rm -fv *.sff
	$(MAKE) -f $< $@ 

#----------------------------------------------------------------------

# test deconvolution
# % should be replaced by trace number for tesiff trace
# e.g.: deco1.ps
deco%.ps: Makefile.deconv
	/bin/rm -fv *.xxx.fil *.xxx.rules
	$(MAKE) -f $< $@ 
codeco%.ps: Makefile.deconv
	/bin/rm -fv *.xxx.fil *.xxx.rules
	$(MAKE) -f $< $@ 

#----------------------------------------------------------------------

# test phasedsignals
# % should be replaced 0, m, or r (argument to parameter -p)
phasetest%.ps: Makefile.phasedsignals
	$(MAKE) -f $< $@ 
Pphasetest%.ps: Makefile.phasedsignals
	$(MAKE) -f $< $@ 

#----------------------------------------------------------------------

# test teseco
#
# tragets are
#
# t1_sig.xxx.ps
# t1_teseco1.xxx.ps 
# t1_teseco2.xxx.ps
#
t1_%.ps: Makefile.teseco
	$(MAKE) -f $< $@ 

#----------------------------------------------------------------------

# test resaseda
rsa.ps: Makefile.resaseda
	$(MAKE) -f $< $@ 

#----------------------------------------------------------------------

# test convolution
convolution%.ps: Makefile.convolution
	$(MAKE) -f $< $@ 

#----------------------------------------------------------------------

# test cross-correlation
cross%.ps: Makefile.crosscorrelation
	$(MAKE) -f $< $@ 

#----------------------------------------------------------------------

%.psp: %.ps; gv $<; /bin/rm -fv $<
%.pdf: %.ps; ps2pdf $<
%.rw.ps: %.ps; gs -sDEVICE=pswrite -dNOPAUSE -dBATCH -sOutputFile=$@ $< 
%.fix.ps: %.ps; fixps $< > $@
%.2up.ps: %.ps; psnup -r -l -2 -Pa4 -pa4 $< > $@
%.rs.ps: %.ps; psresize -Pa4 -pa5 $< > $@
%.pngp: %.png; xv $<; /bin/rm -fv $<
	 
# ----- END OF Makefile ----- 
