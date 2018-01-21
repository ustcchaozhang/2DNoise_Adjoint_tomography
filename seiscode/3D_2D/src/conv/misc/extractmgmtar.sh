#!/bin/bash
# this is <extractmgmtar.sh>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
# 
# extract tar file from MGM to local directory
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
#    20/11/2006   V1.0   Thomas Forbriger
# 
# ============================================================================
#
while [ z != z$1 ]
do
  a=$1
  if test -f $a -a -r $a
  then
    day=$(basename $a .tar.bz2)
    echo "extract archive " $a " for day " $day
    rm -rfv geo.dat geo.inf
    tar xvfpsj $a
    if test -r geo.inf -a -r geo.dat
    then
      mgm2sff -v -t -o geo $day.sff
    fi
  fi
  shift
done
# ----- END OF extractmgmtar.sh ----- 
