c this is <seife_timwin.f>
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
      subroutine seife_timwin(nil,par,x,n,dt,tmin,tsec,msg)
c  time window by time after midnight
      real*8 x(n)
      character par*35,msg*(*)
      logical nil
      read(par,*) tmin1,tsec1,tmin2,tsec2
      t0=60.*tmin+tsec
      t1=60.*tmin1+tsec1
      t2=60.*tmin2+tsec2
      np1=1.5+(t1-t0)/dt
      np2=0.5+(t2-t0)/dt
c 09/07/98 should be ok if selected times are outside dataset
      if((np1.lt.1).or.(np2.gt.n).or.(np2.lt.np1)) then
        write(msg,1) tmin1,tsec1,tmin2,tsec2
    1   format("ERROR: twi  ",4f8.1," *** fenster unmoeglich ***")
        if(np1.lt.1) then
          tsec1=tsec1+(1-np1)*dt
          np1=1
        endif
        if(np2.gt.n) then
          tsec2=tsec2-(np2-n)*dt
          np2=n
        endif
        if(np2.lt.np1) return
        write(msg,'("     neues fenster von punkt",i7," bis",i7)') np1,np2
      endif
      write(msg,2) tmin1,tsec1,tmin2,tsec2
    2 format('twi  ',4f10.1)
      np1=np1-1
      n=np2-np1
      do 3 j=1,n
    3 x(j)=x(np1+j)
      tsec=t0+np1*dt
      tmin=int(tsec/60.)
      tsec=tsec-60.*tmin
      nil=.false.
      return
      end
c
c ----- END OF seife_timwin.f -----
