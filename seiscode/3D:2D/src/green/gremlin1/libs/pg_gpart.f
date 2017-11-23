c this is <pg_gpart.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c plot partial dervatives of green data
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
c displayed values are weighted partial derivatives coming from lq_dss
c
c REVISIONS and CHANGES
c    08/04/98   V1.0   Thomas Forbriger
c    20/01/99   V1.1   added title and character height switching
c    06/05/02   V1.2   - changed braces for units
c                      - use pure partial derivatives - no data weights!
c                      - scale to search range now
c
      subroutine pg_gpart(ivp, mi)
c
c ivp:          viewport to use
c mi:           model parameter index
c
      integer ivp, mi
c
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_model.inc'
      include 'glq_inv.inc'
      include 'glq_para.inc'
      include 'glq_pgpara.inc'
      include 'glq_pg.inc'
      include 'glq_verbose.inc'
c
cE
      real smin, smax, fmin, fmax
      integer ipara
      character*80 title
      real ds, df, trans(6), minval, maxval
      integer islo, ifre
      integer mpar, msec, mpol, iano
      character*20 parname
c 
      if (verb_subaction) print *,'ENTER pg_gpart(',ivp,',',mi,')'
c 
      if (verb_subaction) print *,
     &  'NOTICE (pg_gpart): plot green partial derivatives for ',
     &  'parameter ', mi,' to viewport ',ivp
c select viewport
      call pg_selvp(ivp)
c 
c set model index
      ipara=mi
      if ((ipara.lt.1).or.(ipara.gt.mod_n)) then
        print *,'WARNING (pg_gpart): invalid model parameter index ',
     &    ipara
        return
      endif
c 
c set range
      smin=dat_slo(rng_smin)
      smax=dat_slo(rng_smax)
      fmin=dat_fre(rng_fmin)
      fmax=dat_fre(rng_fmax)
      if ((smax-smin).lt.1.e-3) then
        smin=smin-1.e-3
        smax=smax+1.e-3
      endif
      if ((fmax-fmin).lt.1.e-3) then
        fmin=fmin-1.e-3
        fmax=fmax+1.e-3
      endif
c 
      maxval=abs(lq_dss(1, ipara))
      iano=0
      do islo=rng_smin,rng_smax
        do ifre=rng_fmin,rng_fmax
          iano=iano+1
          pg_data(islo, ifre)=abs(lq_d(iano, ipara))*mweight(ipara)
          maxval=max(maxval,pg_data(islo, ifre))
        enddo
      enddo
c 
      df=dat_fre(2)-dat_fre(1)
      ds=dat_slo(2)-dat_slo(1)
      trans(1)=dat_fre(1)-df
      trans(2)=0.
      trans(3)=df
      trans(4)=dat_slo(1)-ds
      trans(5)=ds
      trans(6)=0.
c 
      minval=0.
      if (verb_subresult) print *,
     &  'NOTICE (pg_gpart): amplitudes of ',ipara,
     &  ': max min ',maxval,minval
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
      call pgswin(fmin,fmax,smin,smax)
c 
c plot box
      call pgslw(pg_lw)
      call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
c 
      call pgsch(pg_lch)
      if (pg_plottitle) then
        call pglabel('frequency / Hz','phase-slowness / s km\u-1',title)
      else
        call pglabel('frequency / Hz','phase-slowness / s km\u-1',' ')
      endif
      call pgsch(pg_ch)
c 
      call pgsitf(0)
      call pgsch(pg_wch)
      call pgwedg('RG',0.3,3.,maxval,minval,'amplitude/(search range)')
      call pgsch(pg_ch)
      call pggray(pg_data,glqd_mslo,glqd_mfre,
     &            rng_smin,rng_smax,rng_fmin,rng_fmax,
     &            maxval, minval, trans)
c plot grid
      call pgsls(4)
      call pgbox('STG',0.0,0,'SGT',0.0,0)
      call pgsls(1)
c 
      call pgsch(pg_ch)
      call pgsci(pg_colind)
      call pgslw(pg_lw)
      call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
c 
c 
      call pgupdt
c 
      if (verb_subaction) print *,'LEAVE pg_gpart'
c 
      return
   50 format('partial derivatives for parameter ',i3,': ord. ',i3,
     &  ' of ',a,'in section ',i3)
      end
c
c ----- END OF pg_gpart.f -----
