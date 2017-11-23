c this is <seife_interp.f>
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
      subroutine seife_interp(nil,par,x,n,msg)
c  interpolate clipped samples
      real*8 x(n),clip,aik(4,4),xx(4),f(4),t(4)
      character par*35,msg*(*)
      logical nil
      data zero,one,two/0.d0,1.d0,2.d0/
c  define illegal samples
      read(par,*,end=99) clip,n1,n2,apex
      if(clip.eq.zero) clip=262000.d0
      n1=max(n1,1)
      if(n2.eq.0.or.n2.gt.n) n2=n
c  search for illegal samples
      naus=0
      nabt=0
   10 do 1 j=n1,n2
      if(abs(x(j)).gt.clip) goto 2
    1 continue
c  if no more, done
      nil=.false.
      write(msg,7) naus,nabt,clip,apex
    7 format("spl - an",i4," stellen wurden",i5," werte >",f9.0,
     & " interpoliert. apex =",f9.0)
      return
    2 j1=j
c  determine the last bad sample (two good ones must follow)
      do 3 j2=j1,n-2
      if(abs(x(j2+1)).lt.clip.and.abs(x(j2+2)).lt.clip) goto 4
    3 continue
      if(j1.lt.n1+2.or.j2.gt.n2-2) then
      write(msg,'("ERROR: *** spl ***  zu wenig stuetzwerte - return")')
      return
      endif
    4 if(apex.eq.zero) then
c  compute spline
      xx(1)=x(j1-2)
      t(1)=-two
      xx(2)=x(j1-1)
      t(2)=-one
      xx(3)=x(j2+1)
      t(3)=j2-j1+1
      xx(4)=x(j2+2)
      t(4)=t(3)+one
      do 6 j=1,4
      aik(j,1)=one
      do 6 k=2,4
    6 aik(j,k)=t(j)*aik(j,k-1)
      else
c  compute parabola
      xx(1)=x(j1-1)
      t(1)=-one
      xx(3)=x(j2+1)
      t(3)=j2-j1+1
      xx(2)=apex
      t(2)=(t(1)+t(3))/two
      do 16 j=1,3
      aik(j,1)=one
      aik(j,4)=zero
      aik(4,j)=0.d0
      do 16 k=2,3
   16 aik(j,k)=t(j)*aik(j,k-1)
      aik(4,4)=one
      xx(4)=zero
      endif
      call seife_gauss(aik,4,4,xx,f)
c  interpolate
      do 5 j=j1,j2
      tt=j-j1
    5 x(j)=f(1)+tt*(f(2)+tt*(f(3)+tt*f(4)))
      n1=j2+1
      naus=naus+1
      nabt=nabt+j2-j1+1
      goto 10
   99 write(msg,'("ERROR: spl braucht vier parameter: clp,n1,n2,apex")')
      return
      end
c
c ----- END OF seife_interp.f -----
