c this is <fapid_sff_trimlen.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
c
c sff_TrimLen function copied from stuff.f
c
c ----
c This program is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c 
c You should have received a copy of the GNU General Public License
c along with this program; if not, write to the Free Software
c Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
c ----
c
c
c REVISIONS and CHANGES
c    02/01/2011   V1.0   Thomas Forbriger
c
c ============================================================================
c
c This function is copied from stuff.f
c It is implemented in libfapidxx since it cannot be replaced by the intrinsic
c Fortran function index in all cases. index(string,' ')-1 finds the
c index of the last character only in cases where the string does not
c contain spaces.
c
cD
c----------------------------------------------------------------------
      subroutine sff_TrimLen(string,ntrim)
c
c  give length of a string excluding trailing blanks
c  Input:
c        string:  String to be trimmed
c  Output:
c        ntrim:   length of string excluding trailing blanks
c
      integer ntrim
      character string*(*)
cE
      do 10 ntrim=len(string),1,-1
 10     if(string(ntrim:ntrim).ne.' ') return
      ntrim = 1 
      return
      end
c 
c ----- END OF fapid_sff_trimlen.f ----- 
