c this is <dat_dmode.f>
c------------------------------------------------------------------------------
c
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
c prepare data mode settings (data ranges, weights, green mode, etc.)
c
c read weights are taken from rgweight. inversion weights are calculated and
c stored in gweight.
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    07/04/98   V1.1   introduced green error estimate
c    20/08/98   V1.2   keep maximum amplitude of real green data
c    24/08/98   V1.3   now use real data to evaluate maximum amplitude
c    02/12/98   V1.4   data_maxamp has to be calculated BEFORE calling
c                      dat_dcpc
c    07/04/00   V1.5   changed number of available datamodes
c                      introduced new prefit mode
c
c==============================================================================
cS
c
      subroutine dat_dmode
c
c this routine defines how to use your data
c the runtime versions of the weight arrays will be set by this routine
c
c bounds for working around in the green:
c  flow:   lowest frequency (Hz)
c  fhigh:  largest frequency (Hz)
c  slow:   lowest slowness (s/km)
c  shigh:  largest slowness (s/km)
c 
c bound for travel time data:
c  xmax:   largest offset to look at travel time
c 
c how to balance between green data and travel time data
c  0 <= balance <= 1
c  balance=0:   only travel time data will influence the misfit
c  balance=1:   only green data will influence the misfit
c 
c datamode to be used:
c   see dat_dcpc.f for an explanation
c 
c travel time difference (in seconds) that will result in a misfit of 1:
c  tterror
c 
c prefit=.true.:  prefit the synthetic complex green amplitudes
c                 to match unmodified data
c 
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_data.inc'
      include 'glq_verbose.inc'
cE
c 
      integer i,j
      double precision norm
c
      if (verb_subaction) print *,'ENTER dat_dmode'
c 
      if (verb_subaction) print *,'NOTICE (dat_dmode): set data usage mode'
      if (verb_subinput) print *,
     &   'NOTICE (dat_dmode): slowness from ',val_smin,' to ',val_smax
      if (verb_subinput) print *,
     &   'NOTICE (dat_dmode): frequency from ',val_fmin,' to ',val_fmax
      if (verb_subinput) print *,
     &   'NOTICE (dat_dmode): travel times up to ',val_xmax,
     &        ' error ',tterror
      if (verb_subinput) print *,
     &   'NOTICE (dat_dmode): greenmode ',datamode,' balance ',balance
      if (verb_subinput) then
        if (prefit_mode.eq.0) then
          print *,'NOTICE (dat_dmode): do complex amplitude prefit'
        elseif (prefit_mode.eq.1) then
          print *,'NOTICE (dat_dmode): do real amplitude prefit'
        elseif (prefit_mode.eq.2) then
          print *,'NOTICE (dat_dmode): do mean real amplitude prefit'
        elseif (prefit_mode.eq.3) then
          print *,'NOTICE (dat_dmode): do maximum real amplitude prefit'
        else
          call mod_panic('ERROR (dat_dmode): unknown prefit mode')
        endif
      endif
c 
c test range
      if (val_fmin.ge.val_fmax) 
     &  call mod_panic( 'ERROR (dat_dmode): flow >= fhigh')
      if (val_smin.ge.val_smax) 
     &  call mod_panic( 'ERROR (dat_dmode): slow >= shigh')
      if (val_fmin.ge.dat_fre(data_nfre))
     &  call mod_panic( 'ERROR (dat_dmode): flow >= fmax')
      if (val_smin.ge.dat_slo(data_nslo))
     &  call mod_panic( 'ERROR (dat_dmode): slow >= smax')
      if (val_smax.le.dat_slo(1))
     &  call mod_panic( 'ERROR (dat_dmode): shigh <= smin')
      if (val_fmax.le.dat_fre(1)) 
     &  call mod_panic( 'ERROR (dat_dmode): fhigh <= fmin')
c 
c test other parameters
      if ((datamode.lt.1).or.(datamode.gt.35))
     &  call mod_panic('ERROR (dat_dmode): unknown greenmode')
      if ((balance.lt.0.).or.(balance.gt.1.))
     &  call mod_panic('ERROR (dat_dmode): invalid balance')
      if (tterror.le.0.)
     &  call mod_panic('ERROR (dat_dmode): invalid travel time errror estimate')
c 
c set range
      rng_smin=0
      rng_fmin=0
      rng_smax=0
      rng_fmax=0
      rng_xmax=0
c 
c there are always problems with frequencies about 0.Hz - just cancel them
      if (abs(dat_fre(1)).lt.1.e-6) then
        val_fmin=max(dat_fre(2), val_fmin)
        if (verb_substrategy) print *,'NOTICE (dat_dmode): ',
     &    'limit minimum frequency to ',val_fmin
      endif
c 
      do i=1,data_nfre
        if ((val_fmin.le.dat_fre(i)).and.(rng_fmin.eq.0)) rng_fmin=i
        if ((val_fmax.ge.dat_fre(data_nfre-i+1)).and.(rng_fmax.eq.0))
     &     rng_fmax=data_nfre-i+1
      enddo
      if (rng_fmin.eq.0) rng_fmin=1
      if (rng_fmax.eq.0) rng_fmax=data_nfre
c 
      do i=1,data_nslo
        if ((val_smin.le.dat_slo(i)).and.(rng_smin.eq.0)) rng_smin=i
        if ((val_smax.ge.dat_slo(data_nslo-i+1)).and.(rng_smax.eq.0))
     &     rng_smax=data_nslo-i+1
      enddo
      if (rng_smin.eq.0) rng_smin=1
      if (rng_smax.eq.0) rng_smax=data_nslo
c
      do i=1,data_ntts
        if ((val_xmax.le.travx(i)).and.(rng_xmax.eq.0)) rng_xmax=i
      enddo
      if (rng_xmax.eq.0) rng_xmax=data_ntts
c 
      if ((rng_fmin.eq.0).or.(rng_fmax.eq.0).or.
     &    (rng_smin.eq.0).or.(rng_smax.eq.0).or.(rng_xmax.eq.0))
     &    then  
        print *,'ERROR (dat_dmode): found no limits!'
        print *,'                   rng_fmin: ',rng_fmin
        print *,'                   rng_fmax: ',rng_fmax
        print *,'                   rng_smin: ',rng_smin
        print *,'                   rng_smax: ',rng_smax
        print *,'                   rng_xmax: ',rng_xmax
        call mod_panic('ERROR (dat_dmode): found no limits')
      endif
c 
c get maximum amplitude of input real data
      data_maxamp=real(green(rng_smin,rng_smax,di_read))**2+
     &    imag(green(rng_smin,rng_smax,di_read))**2
      do i=rng_smin,rng_smax
        do j=rng_fmin,rng_fmax
          data_maxamp=max(data_maxamp,(real(green(i,j,di_read))**2+
     &         imag(green(i,j,di_read))**2))
        enddo
      enddo
      data_maxamp=sqrt(data_maxamp)
c 
c copy rgreen to mgreen according to datamode
      call dat_dcpc(di_read, di_mread)
c
c copy travel times (just to be consistent in this version)
      do i=1,rng_xmax
        travt(i, di_mread)=travt(i, di_read)
      enddo
c 
c calculate new green weight arrays
      norm=0.
      do i=rng_smin,rng_smax
        do j=rng_fmin,rng_fmax
c          norm=norm+(real(green(i,j,di_mread))**2+
c     &         imag(green(i,j,di_mread))**2)*
c     &         rgweight(i,j)**2
          norm=norm+rgweight(i,j)**2
        enddo
      enddo
c      print *,'DEBUG: norm: ',norm
c      print *,'DEBUG: gerror: ',gerror
c 
      do i=rng_smin,rng_smax
        do j=rng_fmin,rng_fmax
c          gweight(i,j)=balance*rgweight(i,j)**2/norm
          gweight(i,j)=balance*rgweight(i,j)**2/(norm*gerror**2)
c          print *,'DEBUG: gweight(',i,',',j,')=',gweight(i,j)
        enddo
      enddo
c 
c calculate new runtime weight arrays
      norm=0.
      do i=1,rng_xmax
        norm=norm+rtweight(i)**2
      enddo
c 
      do i=1,rng_xmax
        tweight(i)=(1.-balance)*(rtweight(i)**2)/(norm*tterror**2)
c        print *,'DEBUG: tweight(',i,')=',tweight(i)
      enddo
c
      if (verb_subaction) print *,'LEAVE dat_dmode'
c
      return
      end
c
c ----- END OF dat_dmode.f -----
