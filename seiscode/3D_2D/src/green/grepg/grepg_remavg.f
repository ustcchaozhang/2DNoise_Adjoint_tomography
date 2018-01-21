c this is <grepg_remavg.f>
c------------------------------------------------------------------------------
cS
c
c 07/04/2000 by Thomas Forbriger (IfG Stuttgart)
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
c remove average
c
c REVISIONS and CHANGES
c    07/04/2000   V1.0   Thomas Forbriger
c
c    $Log: not supported by cvs2svn $
c    Revision 1.1  2000/04/07 16:19:54  thof
c    added some modification options to grepg
c
c
c==============================================================================
c
      subroutine grepg_remavg(ns,nf,d,v,ms,mf)
c
c declare parameters
      integer ns,nf,ms,mf
      complex d(ms,mf)
      logical v
c
cE
c declare local variables
      integer mw,i,j
      parameter(mw=1000)
      double precision dr(mw),di(mw),w(mw),avg,sum
c
c------------------------------------------------------------------------------
c go
c
      if (mw.lt.ns) stop 'ERROR (grepg_remavg): workspace too small'
      if (v) print *,'remove moving average'
      avg=0.d0
      do j=1,ns
        do i=1,nf
          dr(i)=real(d(i,j))
          di(i)=aimag(d(i,j))
        enddo
        call rema(dr,w,nf,avg)
        sum=sum+avg
        call rema(di,w,nf,avg)
        sum=sum+avg
        do i=1,nf
          d(i,j)=cmplx(sngl(dr(i)),sngl(di(i)))
        enddo
      enddo
      if (v) print *,'mean average: ',sum/dble(2.d0*ns)
      return
      end
c
c----------------------------------------------------------------------
c remove moving average
      subroutine rema(d,w,n,sumavg)
      integer n
      double precision d(n), w(n), sum, ssum, pi, si, sumavg
      parameter (pi=3.1415926535897931159)
      integer i,j,nn
      sumavg=0.d0
      do i=1,n
        sum=0.d0
        ssum=0.d0
        nn=2*i+3
        do j=1,min(nn,n)
          si=sin(pi*dble(j)/dble(nn))
          ssum=ssum+si
          sum=sum+si*d(j)
        enddo
        sum=sum/ssum
        sumavg=sumavg+sum
        w(i)=d(i)-sum
      enddo
      do i=1,n
        d(i)=w(i)
      enddo
      sumavg=sumavg/dble(n)
      return
      end
c
c ----- END OF grepg_remavg.f -----
