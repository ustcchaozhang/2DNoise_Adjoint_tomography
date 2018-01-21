c this is <pg_mod.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c plot one model (with polynomial model)
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
c    16/04/98   V1.1   introduced plot range memory
c    20/04/98   V1.2   do not plot box and labels when overplotting
c    17/08/98   V1.3   introduced different line style plotting
c    20/01/99   V1.4   introduced new plot options:
c                      - pg_clw
c                      - reduced colour index
c                      - changed pgs_par calling convention
c    22/01/99   V1.5   - changed calling convention for pgs_par
c    06/05/02   V1.6   changed brace type for physical units
c
      subroutine pg_mod(imod)
c
c plot discrete model to viewports 1,2,3
c 
c mod>0:   plot also polynomial model mod
c mod<0:   plot also polynomial model -mod but keep pgwin settings
c 
      integer imod
      
c 
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_pgpara.inc'
      include 'glq_verbose.inc'
c 
cE
      real mindepth,maxdepth,parmin,parmax,vmin,vmax
      integer i, mod
      logical pol, takemem
c 
      if (verb_subaction) print *,'ENTER pg_mod(',mod,')'
c 
      if (verb_subaction) 
     &  print *,'NOTICE (pg_mod): plot model ',mod,' to viewports 1,2,3'
c      print *,'DEBUG: mod is ',imod
      if (imod.lt.0) then
        mod=-imod
        takemem=.true.
c        print *,'DEBUG: takemem'
      else
        mod=imod
        takemem=.false.
c        print *,'DEBUG: NO takemem'
      endif
      pol=.false.
      if ((pg_alphals.eq.1).and.(pg_betals.eq.1)) pol=.true.
c first get depth range
      parmin=0.
      parmax=dmodel(glqm_nlay, mi_depth)
      call pgrnge(parmin,parmax,mindepth,maxdepth)
      if ((maxdepth-mindepth).lt.1.e-10) then
        maxdepth=maxdepth+1.e-3
        mindepth=mindepth-1.e-3
      endif
      if (takemem) then
        maxdepth=pg_depmax
        mindepth=pg_depmin
      else
        pg_depmax=maxdepth
        pg_depmin=mindepth
      endif
c 
c plot velocities
      vmin=dmodel(2, mi_beta)
      vmax=dmodel(2, mi_alpha)
      do i=2,glqm_nlay
c        print *,'i,z,v',i,dmodel(i, mi_depth),dmodel(i, mi_alpha)
        vmin=min(vmin,dmodel(i, mi_alpha), dmodel(i, mi_beta))
        vmax=max(vmax,dmodel(i, mi_alpha), dmodel(i, mi_beta))
      enddo
      call pgrnge(vmin, vmax, parmin, parmax)
      if ((parmax-parmin).lt.1.e-6) then
        parmin=parmin-0.01
        parmax=parmax+0.01
      endif
      if (takemem) then
        parmin=pg_velmin
        parmax=pg_velmax
      else
        pg_velmin=parmin
        pg_velmax=parmax
      endif
c 
      call pg_selvp(1)
      call pgsci(pg_colind)
      call pgsls(pg_linestyle)
      call pgsch(pg_ch)
      call pgslw(pg_lw)
      call pgswin(parmin, parmax, maxdepth*1000., mindepth*1000.)
      if (.not.(takemem))
     &  call pgbox('BCNTS',0.,0,'BCNTS',0.,0)
      call pgswin(parmin, parmax, maxdepth, mindepth)
      if (.not.(takemem)) then
        call pgsch(pg_lch)
        call pglab('velocity / km s\u-1','depth / m',' ')
        call pgsch(pg_ch)
      endif
c
      call pgsch(pg_bch)
      call pgsci(pg_alphacol)
      call pgsls(pg_alphals)
      if (.not.(takemem))
     &  call pgmtxt('LV', 2., 1., 0.5, 'v\dP')
      call pgsci(pg_betacol)
      call pgsls(pg_betals)
      if (.not.(takemem))
     &  call pgmtxt('LV', 2., 0.0, 0.5, 'v\dS')
      call pgsch(pg_ch)
c 
      call pgsci(pg_alphacol)
      call pgsls(pg_alphals)
      call pgslw(pg_clw)
      call pgmove(sngl(dmodel(1, mi_alpha)), mindepth)
      do i=2,glqm_nlay
        call pg_scoli(mi_alpha, dmodel(i+1,mi_depth),
     &                dmodel(i,mi_depth), mod)
        call pgdraw(sngl(dmodel(i-1, mi_alpha)), 
     &              sngl(dmodel(i, mi_depth)))
        call pgdraw(sngl(dmodel(i, mi_alpha)), 
     &              sngl(dmodel(i, mi_depth)))
      enddo
      call pgdraw(sngl(dmodel(glqm_nlay, mi_alpha)), maxdepth)
      call pgslw(pg_lw)
c 
      call pgsci(pg_betacol)
      call pgsls(pg_betals)
      call pgslw(pg_clw)
      call pgmove(sngl(dmodel(1, mi_beta)), mindepth)
      do i=2,glqm_nlay
        call pg_scoli(mi_beta, dmodel(i-1,mi_depth),
     &                dmodel(i,mi_depth), mod)
        call pgdraw(sngl(dmodel(i-1, mi_beta)), 
     &              sngl(dmodel(i, mi_depth)))
        call pgdraw(sngl(dmodel(i, mi_beta)), 
     &              sngl(dmodel(i, mi_depth)))
      enddo
      call pgdraw(sngl(dmodel(glqm_nlay, mi_beta)), maxdepth)
      call pgslw(pg_lw)
c 
      if (pol) then
        call pgsci(pg_alphacol)
        call pgsls(pg_alphals)
        call pgs_par(mod, mi_alpha, takemem, maxdepth)
        call pgsci(pg_betacol)
        call pgsls(pg_betals)
        call pgs_par(mod, mi_beta, takemem, maxdepth)
      endif
c 
c plot density-values
      vmin=dmodel(2, mi_density)
      vmax=dmodel(2, mi_density)
      do i=2,glqm_nlay
        vmin=min(vmin,dmodel(i, mi_density))
        vmax=max(vmax,dmodel(i, mi_density))
      enddo
      call pgrnge(vmin, vmax, parmin, parmax)
      if ((parmax-parmin).lt.1.e-6) then
        parmin=parmin-0.01
        parmax=parmax+0.01
      endif
      if (takemem) then
        parmin=pg_denmin
        parmax=pg_denmax
      else
        pg_denmin=parmin
        pg_denmax=parmax
      endif
c 
      call pg_selvp(2)
      call pgsci(pg_colind)
      call pgsls(pg_linestyle)
      call pgsch(pg_ch)
      call pgslw(pg_lw)
      call pgswin(parmin, parmax, maxdepth, mindepth)
      if (.not.(takemem)) then
        call pgbox('BCNTS',0.,0,'BCTS',0.,0)
        call pgsch(pg_lch)
        call pglab('density / g cm\u-3\d',' ',' ')
        call pgsch(pg_ch)
      endif
c 
      call pgslw(pg_clw)
      call pgmove(sngl(dmodel(1, mi_density)), mindepth)
      do i=2,glqm_nlay
        call pg_scoli(mi_density, dmodel(i-1,mi_depth),
     &                dmodel(i,mi_depth), mod)
        call pgdraw(sngl(dmodel(i-1, mi_density)), 
     &              sngl(dmodel(i, mi_depth)))
        call pgdraw(sngl(dmodel(i, mi_density)), 
     &              sngl(dmodel(i, mi_depth)))
      enddo
      call pgdraw(sngl(dmodel(glqm_nlay, mi_density)), maxdepth)
      call pgslw(pg_lw)
c 
      if (pol) then
        call pgsci(pg_colind)
        call pgsls(pg_linestyle)
        call pgs_par(mod, mi_density, takemem, maxdepth)
      endif
c 
c plot Q-values
      vmin=dmodel(2, mi_Qbeta)
      vmax=dmodel(2, mi_Qalpha)
      do i=2,glqm_nlay
        vmin=min(vmin,dmodel(i, mi_Qalpha), dmodel(i, mi_Qbeta))
        vmax=max(vmax,dmodel(i, mi_Qalpha), dmodel(i, mi_Qbeta))
      enddo
      call pgrnge(vmin, vmax, parmin, parmax)
      if ((parmax-parmin).lt.1.e-6) then
        parmin=parmin-10.
        parmax=parmax+10.
      endif
      if (takemem) then
        parmin=pg_qmin
        parmax=pg_qmax
      else
        pg_qmin=parmin
        pg_qmax=parmax
      endif
c 
      call pg_selvp(3)
      call pgsci(pg_colind)
      call pgsls(pg_linestyle)
      call pgsch(pg_ch)
      call pgslw(pg_lw)
      call pgswin(parmin, parmax, maxdepth, mindepth)
      if (.not.(takemem)) then
        call pgbox('BCNTS',0.,0,'BCTS',0.,0)
        call pgsch(pg_lch)
        call pglab('quality',' ',' ')
        call pgsch(pg_ch)
      endif
c 
      call pgsci(pg_alphacol)
      call pgsls(pg_alphals)
      call pgslw(pg_clw)
      call pgmove(sngl(dmodel(1, mi_Qalpha)), mindepth)
      do i=2,glqm_nlay
        call pg_scoli(mi_Qalpha, dmodel(i-1,mi_depth),
     &                dmodel(i,mi_depth), mod)
        call pgdraw(sngl(dmodel(i-1, mi_Qalpha)), 
     &              sngl(dmodel(i, mi_depth)))
        call pgdraw(sngl(dmodel(i, mi_Qalpha)), 
     &              sngl(dmodel(i, mi_depth)))
      enddo
      call pgdraw(sngl(dmodel(glqm_nlay, mi_Qalpha)), maxdepth)
      call pgslw(pg_lw)
c 
      call pgsci(pg_betacol)
      call pgsls(pg_betals)
      call pgslw(pg_clw)
      call pgmove(sngl(dmodel(1, mi_Qbeta)), mindepth)
      do i=2,glqm_nlay
        call pg_scoli(mi_Qbeta, dmodel(i-1,mi_depth),
     &                dmodel(i,mi_depth), mod)
        call pgdraw(sngl(dmodel(i-1, mi_Qbeta)), 
     &              sngl(dmodel(i, mi_depth)))
        call pgdraw(sngl(dmodel(i, mi_Qbeta)), 
     &              sngl(dmodel(i, mi_depth)))
      enddo
      call pgdraw(sngl(dmodel(glqm_nlay, mi_Qbeta)), maxdepth)
      call pgslw(pg_lw)
c 
      if (pol) then
        call pgsci(pg_alphacol)
        call pgsls(pg_alphals)
        call pgs_par(mod, mi_Qalpha, takemem, maxdepth)
        call pgsci(pg_betacol)
        call pgsls(pg_betals)
        call pgs_par(mod, mi_Qbeta, takemem, maxdepth)
      endif
c 
      call pgsci(pg_colind)
      call pgsls(pg_linestyle)
c 
      call pgupdt
c 
      if (verb_subaction) print *,'LEAVE pg_mod'
c
      return
      end
c
c ----- END OF pg_mod.f -----
