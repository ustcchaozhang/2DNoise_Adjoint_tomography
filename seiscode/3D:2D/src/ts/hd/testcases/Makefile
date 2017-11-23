# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
# 
# test cases for header modifiers
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
#    13/12/2012   V1.0   Thomas Forbriger
# 
# ============================================================================
#

all:

TESTCASEMAKE=$(filter-out %.bak,$(wildcard Makefile*))
flist: $(TESTCASEMAKE) $(wildcard *.par) $(wildcard *.tpl)
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist *.xxx *.xxx.* *.sff *.TZ *.TR *.grn *.su
	-/bin/rm -vf *.ps *.pdf *.asc *.bin

#----------------------------------------------------------------------

# test cases for soutifu
tijerasxx.ps: Makefile.tijerasxx ; $(MAKE) -f $< $@

#----------------------------------------------------------------------

%.psp: %.ps; gv -infoSilent $<; /bin/rm -fv $<
%.pdf: %.ps; ps2pdf $<
%.rw.ps: %.ps; gs -sDEVICE=pswrite -dNOPAUSE -dBATCH -sOutputFile=$@ $< 
%.fix.ps: %.ps; fixps $< > $@
%.2up.ps: %.ps; psnup -r -l -2 -Pa4 -pa4 $< > $@
%.rs.ps: %.ps; psresize -Pa4 -pa5 $< > $@
%.pngp: %.png; xv $<; /bin/rm -fv $<

# ----- END OF Makefile ----- 
