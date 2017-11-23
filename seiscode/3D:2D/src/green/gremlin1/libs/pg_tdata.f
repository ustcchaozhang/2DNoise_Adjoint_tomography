c this is <pg_tdata.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c plot travel time samples of read data
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
c    20/01/99   V1.1   added plot options
c
      subroutine pg_tdata
c
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_para.inc'
      include 'glq_pgpara.inc'
      include 'glq_verbose.inc'
c
cE
      real tmin, tmax, xmin, xmax
      real yd(glqd_mtts), x(glqd_mtts)
      integer i
      character*80 title
c 
      if (verb_subaction) print *,'ENTER pg_tdata'
c 
c select viewport
      call pgpage
      call pg_selvp(0)
c 
c copy data
      do i=1,data_ntts
        yd(i)=travt(i, di_read)
        x(i)=travx(i)
      enddo
c 
c find ranges
      tmin=yd(1)
      tmax=yd(1)
      xmin=x(1)
      xmax=x(1)
      do i=1,data_ntts
        tmin=min(tmin, yd(i))
        tmax=max(tmax, yd(i))
        xmin=min(xmin, x(i))
        xmax=max(xmax, x(i))
      enddo
      if ((xmax-xmin).lt.1.e-6) then
        xmin=xmin-1.e-3
        xmax=xmax+1.e-3
      endif
      if ((tmax-tmin).lt.1.e-6) then
        tmin=tmin-1.e-3
        tmax=tmax+1.e-3
      endif
c 
c plot
      title='travel time data'
c 
      call pgsch(pg_ch)
      call pgsci(pg_colind)
      call pgslw(pg_lw)
c 
      call pgswin(xmin*1000.,xmax*1000.,tmin*1000.,tmax*1000.)
      call pgbox('BCNTS',0.,0,'BCNTS',0.,0)
      call pgsch(pg_lch)
      if (pg_plottitle) then
        call pglabel('offset / m','time / ms',title)
      else
        call pglabel('offset / m','time / ms',' ')
      endif
      call pgsch(pg_ch)
      call pgswin(xmin,xmax,tmin,tmax)
c 
      call pgsci(pg_colind)
      call pgslw(pg_bestlw)
      call pgmove(x(rng_xmax), tmin)
      call pgdraw(x(rng_xmax), tmax)
      call pgslw(pg_lw)
c 
      call pgsci(pg_alphacol)
      call pgsch(pg_ach)
      call pgmtxt('R',2.,0.,0.,'data')
      call pgsch(pg_ch)
c      call pgslw(pg_bestlw)
c      call pgline(data_ntts, travx, travt(1, di_read))
c      call pgslw(pg_lw)
      call pgslw(pg_clw)
      do i=1,data_ntts
        call pgpt1(x(i), yd(i), -3)
        call pgerr1(6, x(i), yd(i), sngl(tterror), 1.)
      enddo
      call pgslw(pg_lw)
c 
      call pgupdt
c 
      if (verb_subaction) print *,'LEAVE pg_tdata'
c 
      return
      end
c
c ----- END OF pg_tdata.f -----
