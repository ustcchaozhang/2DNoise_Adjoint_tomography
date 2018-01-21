# this is <Makefile>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
# 
# tests for libsffu
# 
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
# along with this program. If not, see <http://www.gnu.org/licenses/>.

# ----
#
# REVISIONS and CHANGES 
#    01/12/2016   V1.0   Thomas Forbriger
# 
# ============================================================================
#

all:

flist: Makefile $(wildcard *.f)
	echo $^ | tr ' ' '\n' | sort > $@

.PHONY: edit
edit: flist; vim $<

.PHONY: clean
clean: ; 
	-find . -name \*.bak | xargs --no-run-if-empty /bin/rm -v
	-/bin/rm -vf flist

acostest: acostest.o
	$(FC) $(FFLAGS) $^ -o $@ 

# ----- END OF Makefile ----- 
