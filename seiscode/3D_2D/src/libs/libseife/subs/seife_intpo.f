c this is <seife_intpo.f>
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
      subroutine seife_intpo(nil,par,dt1,x,np1,msg)
      real*8 x(np1)
      logical nil
      character par*35,msg*(*)
      nil=.false.
      read(par,*) dt2
      if(dt2.le.dt1) then
      write(msg,'("ERROR: neues abtastintervall ist kleiner als altes - return")')
      return
      endif
      j2=0
    1 j2=j2+1
      t=(j2-1)*dt2
      j10=t/dt1
      frac=t/dt1-j10
      j11=j10+1
      j12=j11+1
      if(j12.gt.np1) goto 2
      x(j2)=(1.-frac)*x(j11)+frac*x(j12)
      goto 1
    2 np1=j2-1
      write(msg,3) dt1,dt2
    3 format("csi  ",2f10.3)
      dt1=dt2
      return
      end
c
c ----- END OF seife_intpo.f -----
