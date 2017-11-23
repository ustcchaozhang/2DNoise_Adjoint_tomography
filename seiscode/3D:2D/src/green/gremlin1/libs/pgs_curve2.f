c this is <pgs_curve2.f>
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
      subroutine pgs_curve2(ivp, n ,x ,y ,xlab, ylab, title, f, fm)
c
c plot a simple curve
c
      include 'glq_pgpara.inc'
      include 'glq_verbose.inc'
c 
      integer ivp, n
      real x(n), y(n)
      character*(*) xlab, ylab, title
      logical f, fm
c
cE
      real minx, maxx, miny, maxy, minval, maxval
      integer i, iextval, nti
c 
      integer ninterv
      parameter(ninterv=15)
c 
      if (verb_subaction) print *,'ENTER pgs_curve2(',ivp,',',n,
     &  ',x,y,',xlab,',',ylab,',',title,',',f,',',fm,')'
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
        iextval=1
        do i=1,n
          if (fm) then
            if (minval.gt.y(i)) iextval=i
          else
            if (maxval.lt.y(i)) iextval=i
          endif
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
        nti=int((maxx-minx)/ninterv)+1
        call pgswin(minx, maxx, miny, maxy)
        call pgbox('BCTNS', float(nti),nti, 'BCTNS', 0.,0)
        call pgsls(4)
        call pgbox('BCGTS', float(nti),nti, 'BCGTS', 0.,0)
        call pgsls(1)
        call pglab(xlab, ylab, title)
c 
        if (f) then
          call pgslw(pg_bestlw)
        else
          call pgslw(pg_lw)
        endif
        call pgsci(pg_alphacol)
        call pgline(iextval, x, y)
        if (iextval.lt.n) then
          call pgsci(pg_betacol)
          call pgline((n-iextval+1), x(iextval), y(iextval))
        endif
        call pgsci(pg_colind)
        call pgslw(pg_lw)
        call pgupdt
      endif
c 
      if (verb_subaction) print *,'LEAVE pgs_curve2'
c 
      return
      end
c
c ----- END OF pgs_curve2.f -----
