c this is <seife_window.f>
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
      subroutine seife_window(nil,typ,par,x,n,dt,tmin,tsec,msg)
c  time window by sample number
      real*8 x(n)
      character par*35,msg*(*)
      character*3 typ
      logical nil
      if(typ.eq.'skp'.or.typ.eq.'cos'.or.typ.eq.'cob') then
      read(par,*) np1
      write(msg,7) typ,np1
      else
      read(par,*) np1,np2
      np1=max(np1,1)
      np2=min(np2,n)
      write(msg,7) typ,np1,np2
    7 format(a3,2x,2i10)
      np1=np1-1
      n=np2-np1
      endif
      if(typ.ne.'tap') goto 16
      n=np2
      pih=2.*atan(1.)
      fak=pih/(np2-np1)
      do 14 j=np1,np2
   14 x(j)=x(j)*cos(fak*(j-np1))**2
      goto 15
   16 if(typ.ne.'cos') goto 13
      n1=n+1
      pih=2.*atan(1.)
      fak=pih/np1
      do 17 j=1,np1
      taper=sin(fak*j)**2
      x(j)=x(j)*taper
   17 x(n1-j)=x(n1-j)*taper
      goto 15
   13 if (typ.ne.'cob') goto 18
      pih=2.*atan(1.)
      fak=pih/np1
      do j=1,np1
        taper=sin(fak*j)**2
        x(j)=x(j)*taper
      enddo
      goto 15
   18 if(typ.eq.'win') then
      do 11 j=1,n
   11 x(j)=x(np1+j)
      endif
      if(typ.eq.'sin'.or.typ.eq.'sis') then
      pi=4.*atan(1.)
      f=pi/(n+1)
      w2=sqrt(2.)
      nex=1
      if(typ.eq.'sis') nex=2
      do 12 j=1,n
   12 x(j)=w2*x(np1+j)*sin(f*j)**nex
      endif
      if(typ.eq.'skp') then
      do j=1,n
      x(j)=x(np1+j)
      enddo
      endif
      tsec=60.*tmin+tsec+np1*dt
      tmin=int(tsec/60.)
      tsec=tsec-60.*tmin
   15 nil=.false.
      return
      end
c
c ----- END OF seife_window.f -----
