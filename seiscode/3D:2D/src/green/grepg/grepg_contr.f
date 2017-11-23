c this is <grepg_contr.f>
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
c increase contrast
c
c REVISIONS and CHANGES
c    07/04/2000   V1.0   Thomas Forbriger
c
c    $Log: not supported by cvs2svn $
c    Revision 1.2  2000/04/07 18:47:13  thof
c    did a lot of data modification enhancements in gremlin1
c
c    Revision 1.1  2000/04/07 16:19:54  thof
c    added some modification options to grepg
c
c
c==============================================================================
c
      subroutine grepg_contr(ns,nf,d,ms,mf,t,v,o)
c
c declare parameters
      real t,o
      logical v
      integer ns,nf,ms,mf
      complex d(ms,mf)
c
cE
c declare local variables
      double precision maxval, x, xd
      integer i,j
c 
      double precision f
      f(o,x)=log10(1.d0+((10.d0**o)-1.d0)*x)/dble(o)
c
c------------------------------------------------------------------------------
c go
      if (v) print *,'increase contrast with threshold (order: ',o,
     &               ' threshold: ',t,')'
      do j=1,nf
        maxval=1.d-70
        do i=1,ns
          maxval=max(maxval,abs(d(i,j)))
        enddo
        do i=1,ns
          xd=abs(d(i,j))
          x=xd/maxval
          if (x.gt.t) then
            x=(x-t)/(1.d0-t)
            x=f(o,x)*(1.d0-t)+t
          else
            x=-1.d0*(x-t)/t
            x=t-f(o,x)*t
          endif
          d(i,j)=cmplx(maxval*x*d(i,j)/xd)
        enddo
      enddo
c
      return
      end
c
c ----- END OF grepg_contr.f -----
