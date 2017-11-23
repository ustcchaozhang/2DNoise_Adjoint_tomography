c this is <pg_tpart.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c plot partial dervivatives of travel time data
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
c the values are taken from lq_dss and must be ready (being calculated with
c inv_part and inv_ds)
c
c REVISIONS and CHANGES
c    08/04/98   V1.0   Thomas Forbriger
c    24/08/98   V1.1   corrected string synthetics
c    20/01/99   V1.2   added plot options
c    06/05/02   V1.3   - changed braces for units
c                      - use pure partial derivatives - no data weights!
c                      - scale to search range now
c
      subroutine pg_tpart(ivp, mi)
c
c ivp:          index of viewport to be used
c mi:           index of model parameter
c
      integer ivp, mi
c
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_para.inc'
      include 'glq_pgpara.inc'
      include 'glq_inv.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
c
cE
      real tmin, tmax, xmin, xmax
      real yd(glqd_mtts), x(glqd_mtts)
      integer ipara, i, iano, msec, mpar, mpol
      character*20 parname
      character*80 title
c
      if (verb_subaction) print *,'ENTER pg_tpart(',ivp,',',mi,')'
c 
      if (verb_subaction) then
        print *,'NOTICE (pg_tpart): ',
     &    'plot travel time partial derivatives for parameter ',
     &    mi,' to viewport ',ivp
      endif
c 
      ipara=mi
      if ((ipara.lt.1).or.(ipara.gt.mod_n)) then
        print *,'WARNING (pg_gpart): invalid model parameter index ',ipara
        return
      endif
c select viewport
      call pg_selvp(ivp)
c 
c
c copy data
      iano=(rng_smax-rng_smin+1)*(rng_fmax-rng_fmin+1)
      do i=1,rng_xmax
        iano=iano+1
        yd(i)=abs(lq_d(iano, ipara))*mweight(ipara)
        x(i)=travx(i)
      enddo
c 
c find ranges
      tmin=yd(1)
      tmax=yd(1)
      xmin=x(1)
      xmax=x(1)
      do i=1,rng_xmax
        tmin=min(tmin, yd(i))
        tmax=max(tmax, yd(i))
        xmin=min(xmin, x(i))
        xmax=max(xmax, x(i))
      enddo
      tmax=tmax*1.05
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
      call mod_identify(ipara, msec, mpol, mpar, parname)
      write (title, 50) ipara, mpol-1, 
     &  parname(1:index(parname,' ')), msec
c 
      call pgsch(pg_ch)
      call pgsci(pg_colind)
      call pgslw(pg_lw)
c 
      call pgswin(xmin*1000.,xmax*1000.,tmin*1000.,tmax*1000.)
      call pgbox('BCNTS',0.,0,'BCNTS',0.,0)
      call pgsch(pg_lch)
      if (pg_plottitle) then
        call pglabel('offset / m',
     &    'time/(search range) / ms/par.unit',title)
      else
        call pglabel('offset / m',
     &    'time/(search range) / ms/par.unit',' ')
      endif
      call pgsch(pg_ch)
      call pgswin(xmin,xmax,tmin,tmax)
c 
      call pgsci(pg_colind)
c      call pgslw(pg_bestlw)
c      call pgmove(x(rng_xmax), tmin)
c      call pgdraw(x(rng_xmax), tmax)
      call pgslw(pg_bestlw)
c 
      call pgsci(pg_alphacol)
c      call pgmtxt('R',2.,0.,0.,'real data')
c      call pgslw(pg_bestlw)
c      call pgline(data_ntts, travx, travt(1, di_read))
c      call pgslw(pg_lw)
c      do i=1,data_ntts
c        call pgpt1(x(i), yd(i), -3)
c        call pgerr1(6, x(i),yd(i), sngl(tterror), 1.)
c      enddo
c 
      call pgsci(pg_alphacol)
c      if (ref) then
c        call pgmtxt('R',2.,1.,1.,'reference synthetics')
c      else
c        call pgmtxt('R',2.,1.,1.,'synthetics')
c      endif
      call pgslw(pg_clw)
      call pgline(data_ntts, x, yd)
      call pgslw(pg_lw)
c 
      call pgupdt
c 
      if (verb_subaction) print *,'LEAVE pg_tpart'
c 
      return
   50 format('partial derivatives for parameter ',i3,': ord. ',i3,
     &  ' of ',a,'in section ',i3)
      end
c
c ----- END OF pg_tpart.f -----
