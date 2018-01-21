c this is <pgs_curve.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
cS
c----------------------------------------------------------------------
c
      subroutine pgs_curve(ivp, n ,x ,y ,xlab, ylab)
c
c plot a simple curve
c
      include 'glq_pgpara.inc'
      include 'glq_verbose.inc'
c 
      integer ivp, n
      real x(n), y(n)
      character*(*) xlab, ylab
c
cE
      real minx, maxx, miny, maxy, minval, maxval
      integer i
c 
      if (verb_subaction) print *,'ENTER pgs_curve(',ivp,','
     &  ,n,',x,y,',xlab,',',ylab,')'
c 
      if (n.gt.0) then
        minval=x(1)
        maxval=x(1)
        do i=1,n
          minval=min(minval,x(i))
          maxval=max(maxval,x(i))
        enddo
        call pgrnge(minval, maxval, minx, maxx)
c 
        minval=y(1)
        maxval=y(1)
        do i=1,n
          minval=min(minval,y(i))
          maxval=max(maxval,y(i))
        enddo
        call pgrnge(minval, maxval, miny, maxy)
        if (abs(miny-maxy).lt.1.e-15) then
          if (abs(miny).lt.1.e-20) then
            maxy=1.e-10
            miny=-1.e-10
          else
            miny=miny-1.e-15
            maxy=maxy+1.e-15
          endif
        endif
c 
        call pg_selvp(ivp)
        call pgsci(pg_colind)
        call pgslw(pg_lw)
c 
        call pgswin(minx, maxx, miny, maxy)
        call pgbox('BCTNS', 0.,0, 'BCTNS', 0.,0)
        call pgsls(4)
        call pgbox('BCGTS', 0.,0, 'BCGTS', 0.,0)
        call pgsls(1)
        call pglab(xlab, ylab, ' ')
        call pgline(n, x, y)
        call pgupdt
      endif
c 
      if (verb_subaction) print *,'LEAVE pgs_curve'
c 
      return
      end
c
c ----- END OF pgs_curve.f -----
