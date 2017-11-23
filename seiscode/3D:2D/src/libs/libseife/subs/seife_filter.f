c this is <seife_filter.f>
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
      subroutine seife_filter(nil,typ,par,x,n,dts,msg)
      implicit real*8 (a-h,o-z)
      character typ*3,typ1*3,typ2*3,par*35,msg*(*)
      dimension x(n)
      real*4 dts
      logical nil
      dt=dts
c  decode filter type and read parameters
      if(typ.eq.'lp1') then
      read(par,*) t0
      it=11
      else if(typ.eq.'hp1') then
      read(par,*) t0
      it=12
      else if(typ.eq.'lp2') then
      read(par,*) t0,h
      it=21
      else if(typ.eq.'hp2') then
      read(par,*) t0,h
      it=22
      else if(typ.eq.'bp2') then
      read(par,*) t0,h
      it=24
      else if(typ.eq.'int') then
      read(par,*) t0
      if(t0.eq.0.d0) t0=1.d0
      it= 1
      else if(typ.eq.'he1'.or.typ.eq.'le1') then
      read(par,*) t0s,t0
      it=13
      else if(typ.eq.'he2'.or.typ.eq.'le2') then
                     read(par,*) t0s,hs,t0,h
      it=23
      else
      goto 2
      endif
c  calculate filter weights
      call seife_rfk(it,t0/dt,h,t0s/dt,hs,f0,f1,f2,g1,g2)
      if(typ.eq.'le1') then
        fac=t0s/t0
      f0=f0*fac
      f1=f1*fac
      endif
      if(typ.eq.'le2') then
        fac=(t0s/t0)**2
      f0=f0*fac
      f1=f1*fac
      f2=f2*fac
      endif
c  define first sample as baseline level
      if(typ.eq.'hp1'.or.typ.eq.'hp2') call seife_first(x,n)
c  perform recursive filtration
      call seife_rekfl(x,x,n,f0,f1,f2,g1,g2)
    8 format(a3,2x,5f10.3)
c  confirm execution
      if(it.eq.1) write(msg,8) typ,t0
      if(it.eq.11.or.it.eq.12) write(msg,8) typ,t0
      if(it.eq.13) write(msg,8) typ,t0s,t0
      if(it.eq.21.or.it.eq.22.or.it.eq.24) write(msg,8) typ,t0,h
      if(it.eq.23) write(msg,8) typ,t0s,hs,t0,h
      nil=.false.
      return

c  Butterworth filters
    2 if(typ.eq.'lpb'.or.typ.eq.'hpb') then
        read(par,*) t0,m
        mm=m/2
        if(typ.eq.'lpb') then
          it1=11
          it2=21
          typ1='lp1'
          typ2='lp2'
        else
          it1=12
          it2=22
          typ1='hp1'
          typ2='hp2'
        call seife_first(x,n)
        endif
        if(m.gt.2*mm) then
          call seife_rfk(it1,t0/dt,h,t0s/dt,hs,f0,f1,f2,g1,g2)
          call seife_rekfl(x,x,n,f0,f1,f2,g1,g2)
          write(msg,8) typ1,t0
        endif
        pih=2.d0*datan(1.d0)
        do 3 j=1,mm
          h=dsin(pih*(2*j-1)/m)
          call seife_rfk(it2,t0/dt,h,t0s/dt,hs,f0,f1,f2,g1,g2)
          call seife_rekfl(x,x,n,f0,f1,f2,g1,g2)
          write(msg,8) typ2,t0,h
    3   continue
        write(msg,9) typ,t0,m
    9   format(a3,2x,f10.3,i6,'      complete')
      nil=.false.
      endif
      return
      end
c
c ----- END OF seife_filter.f -----
