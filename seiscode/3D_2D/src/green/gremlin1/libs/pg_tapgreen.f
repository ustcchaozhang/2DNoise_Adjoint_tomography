c this is <pg_tapgreen.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c plot green matrix amplitudes in current datamode
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
c replace old pg_tapgreen with subroutine that is able to apply taper
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    20/01/99   V1.1   - added title switching and character height
c    06/05/02   V1.2   changed brace type for physical units
c 
c
      subroutine pg_tapgreen(ivp, di, dotaper)
c
c ivp:          viewport to use
c di:           dataindex
c               for di>2 X2-values will be calculated
c dotaper:      apply taper
c
      integer ivp, di
      logical dotaper
c
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_para.inc'
      include 'glq_pgpara.inc'
      include 'glq_pg.inc'
      include 'glq_verbose.inc'
c
cE
      real smin, smax, fmin, fmax
      integer data_index
      real dat_X2, X2result
      character*80 title
      real ds, df, trans(6), minval, maxval
      integer islo, ifre
c 
      if (verb_subaction) print *,'ENTER pg_tapgreen(',ivp,',',di,')'
c 
      if (verb_subaction) then
        print *,
     &  'NOTICE (pg_tapgreen): plot green dataset ',di,' to viewport ',ivp
        if (dotaper) print *,
     &  'NOTICE (pg_tapgreen): use weights as taper'
      endif
c select viewport
      call pg_selvp(ivp)
c 
c set data index
      data_index=di
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
      if (dotaper) then
        maxval=abs(green(rng_smin, rng_fmin, data_index))
        do islo=rng_smin,rng_smax
          do ifre=rng_fmin,rng_fmax
            pg_data(islo, ifre)=
     &        abs(green(islo, ifre, data_index))*rgweight(islo,ifre)
            maxval=max(maxval,pg_data(islo, ifre))
          enddo
        enddo
      else
        maxval=abs(green(rng_smin, rng_fmin, data_index))
        do islo=rng_smin,rng_smax
          do ifre=rng_fmin,rng_fmax
            pg_data(islo, ifre)=abs(green(islo, ifre, data_index))
            maxval=max(maxval,pg_data(islo, ifre))
          enddo
        enddo
      endif
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
     &  'NOTICE (pg_tapgreen): amplitudes of ',data_index,
     &  ': max min ',maxval,minval
c 
c plot
      if (data_index.eq.di_mref) then
        X2result=dat_X2(.true.)
        if (pg_shorti) then
          write(title, fmt=51) ' reference ', X2result
        else
          write(title, fmt=50) ' reference ', X2result
        endif
      elseif (data_index.eq.di_mcalc) then
        X2result=dat_X2(.false.)
        if (pg_shorti) then
          write(title, fmt=51) ' ', X2result
        else
          write(title, fmt=50) ' ', X2result
        endif
      elseif (data_index.eq.di_mread) then
        title='amplitudes of modified data'
      else
        title='amplitudes of data'
      endif
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
      call pgwedg('RG',0.3,3.,maxval,minval,'amplitude')
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
      if (verb_subaction) print *,'LEAVE pg_tapgreen'
c 
      return
   50 format(a,'synthetic amplitudes (\gx\u2\d\dtotal\u=',
     &       f12.7,')')
   51 format(a,'synthetics (\gx\u2\d\dtotal\u=',
     &       f5.2,')')
      end
c
c ----- END OF pg_tapgreen.f -----
