c this is <seife_pad.f>
c------------------------------------------------------------------------------
c   ($Id$)
c
c Copyright 1984 by Erhard Wielandt
c This code was part of seife.f. A current version of seife.f can be obtained
c from http://www.software-for-seismometry.de/
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
c extracted from libseife.f
c
c REVISIONS and CHANGES
c    25/10/2000   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine seife_pad(nil,par,x,n,msg)
c  Nullen ergaenzen
      real*8 x(n)
      character par*35,msg*(*)
      logical nil
      read(par,*) nul
      do 1 j=n+1,nul
    1 x(j)=0.d0
      n=nul
      write(msg,2) nul
    2 format("pad: Es wurden Nullen aufgefuellt bis Punkt",i8)
      nil=.false.
      return
      end
c
c ----- END OF seife_pad.f -----
