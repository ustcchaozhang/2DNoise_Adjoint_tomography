c this is <pg_tt.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c plot travel time samples together with synthetic traveltime curves
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
c    24/08/98   V1.1   corrected string synthetics
c    20/01/99   V1.2   added some plot options
c    05/03/99   V1.3   consider asphalt section when plotting synthetics
c    06/05/02   V1.4   changed brace type for physical units
c
      subroutine pg_tt(ivp, ref)
c
c ivp:          index of viewport to be used
c ref=.true.:   plot refrence synthetics
c
      integer ivp
      logical ref
c
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_para.inc'
      include 'glq_pgpara.inc'
      include 'glq_verbose.inc'
c
cE
      real tmin, tmax, xmin, xmax
      real yd(glqd_mtts), ys(glqd_mtts), x(glqd_mtts)
      integer data_index, i
      real dat_X2, X2result
      character*80 title
c 
      if (verb_subaction) print *,'ENTER pg_tt(',ivp,',',ref,')'
c 
      if (verb_subaction) then
        if (ref) then
          print *,
     &      'NOTICE (pg_tt): plot reference travel times to viewport ',ivp
        else
          print *,'NOTICE (pg_tt): plot travel times to viewport ',ivp
        endif
      endif
c select viewport
      call pg_selvp(ivp)
c 
      if (ref) then
        data_index=di_mref
      else
        data_index=di_mcalc
      endif
c 
c copy data
      do i=1,data_ntts
        yd(i)=travt(i, di_read)
        ys(i)=travt(i, data_index)
        x(i)=travx(i)
      enddo
c 
c find ranges
      tmin=yd(1)
      tmax=yd(1)
      xmin=x(1)
      xmax=x(1)
      do i=1,data_ntts
        tmin=min(tmin, yd(i), ys(i))
        tmax=max(tmax, yd(i), ys(i))
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
      X2result=dat_X2(ref)
      if (pg_shorti) then
        write(title, fmt=51) X2result
      else
        write(title, fmt=50) X2result
      endif
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
        call pgerr1(6, x(i),yd(i), sngl(tterror), 1.)
      enddo
      call pgslw(pg_lw)
c 
      call pgsci(pg_betacol)
      call pgsch(pg_ach)
      if (ref) then
        call pgmtxt('R',2.,1.,1.,'reference synthetics')
      else
        call pgmtxt('R',2.,1.,1.,'synthetics')
      endif
      call pgsch(pg_ch)
      call pgslw(pg_clw)
      if (data_ttsplit.gt.0) call pgline(data_ttsplit, x, ys)
      call pgline(data_ntts-data_ttsplit, 
     &            x(data_ttsplit+1), ys(data_ttsplit+1))
      call pgslw(pg_lw)
c 
      call pgupdt
c 
      if (verb_subaction) print *,'LEAVE pg_tt'
c 
      return
   50 format('travel times (\gx\u2\d\dtotal\u=',f12.7,')')
   51 format('travel times (\gx\u2\d\dtotal\u=',f5.2,')')
      end
c
c ----- END OF pg_tt.f -----
