c this is <interface.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c interface subroutines common too all
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
c    04/07/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c set model parameters
c only remembers source parameters
c
      subroutine gr_dsetmod(depth, alpha, beta, density, Qalpha, Qbeta,
     &  s_type, s_depth, s_amp, nlay)
c
c This subroutine sets model for subsequent calculation of greens
c coeficients. This routines calls gr_fsm before returning.
c 
c double precision version
c
c nlay:         number of layers described by model
c depth(i):     top of layer i in (kilometers)
c alpha(i):     Vp layer velocity (kilometers/second)
c beta(i):      Vs layer velocity (kilometers/second)
c density(i):   layer density (g/ccm)
c Qalpha(i):    Vp quality factor
c Qbeta(i):     Vs quality factor
c
c i=nlay will be the bottom halfspace parameters
c i=1    will be the top halfspace
c
c s_depth:      source depth in kilometer
c s_type:       source type: 1=vertical single force   2=explosion
c s_amp:        source amplitude in N (force) or N*m (explosion)
c
c parameters
      integer nlay
      double precision depth(nlay), alpha(nlay), beta(nlay), density(nlay)
      double precision Qalpha(nlay), Qbeta(nlay)
      integer s_type
      double precision s_depth, s_amp
c
cE
      integer i
      include 'common.inc'
      srctyp=s_type
      srcdep=s_depth*1.e3
      srcamp=s_amp
c
      mnlay=nlay
      if (mnlay.gt.maxlay) stop 'ERROR (gr_dsetmod): too many layers'
      do i=1,mnlay
        mdepth(i)=depth(i)
        malpha(i)=alpha(i)
        mbeta(i)=beta(i)
        mdensity(i)=density(i)
        mQalpha(i)=Qalpha(i)
        mQbeta(i)=Qbeta(i)
      enddo
c 
      return
      end
c
c----------------------------------------------------------------------
c finish source and model - does nothing
c
      subroutine gr_fsm
c
cE
c
      return 
      end
c
c----------------------------------------------------------------------
c
c prepare interface reflection coefficients for one slowness
c 
      subroutine gr_prep(u)
c 
      double precision u
c 
cE
      include 'common.inc'
      slowness=u
c 
      return
      end
c 
c----------------------------------------------------------------------
c
      subroutine gr_refmod(modelname, sourcename)
c
c parameters
      character modelname*(*)
      character sourcename*(*)
c
c source parameters
c =================
c
c src_depth    source depth in kilometer
c src_type     source type: 1=vertical single force   2=explosion
c src_amp      source amplitude in N (force) or N*m (explosion)
c
      double precision src_depth, src_amp
      integer src_type
c
c model parameters
c ================
c
c mod_mlay:     maximum number of layers
c
      integer mod_mlay
      parameter (mod_mlay=60)
c
c mod_nlay:     number of layers described by model
c mod_z:        top of layer in (kilometers)
c mod_a:        Vp layer velocity (kilometers/second)
c mod_b:        Vs layer velocity (kilometers/second)
c mod_r:        layer density (g/ccm)
c mod_Qa:       Vp quality factor
c mod_Qb:       Vs quality factor
c
      integer mod_nlay
      double precision mod_z(0:mod_mlay)
      double precision mod_a(0:mod_mlay), mod_b(0:mod_mlay)
      double precision mod_r(0:mod_mlay)
      double precision mod_Qa(0:mod_mlay), mod_Qb(0:mod_mlay)
c      common /gr_model/mod_aC, mod_bC, mod_z, mod_a, mod_b, mod_r, 
c     &              mod_Qa, mod_Qb, mod_t,
c     &              mod_nlay
c 
c parameters to be passed to flgevas
c ==================================
c
c  nk:         number of model nodes
c  mk:         number of model nodes
c  dampflag:   switch to select damping convention
c  flg_f:      reference frequency fref
c  flg_z:      node depth in km
c  flg_ro:     density
c  flg_vp:     P-velocity
c  flg_vs:     S-Velocity
c  flg_qk:     Qkappa (not 1/Q)
c  flg_qm:     Qmu
c
cE
c variables
      real inqa(0:mod_mlay), inqb(0:mod_mlay)
      character*80 infotext, junk
      double precision radius, nuref, Thd, The, M1,M2,M3,M4,M5,M6
      integer i, typ, outsig, srcsig
c 
c go model
      call refmet_rmod(modelname, infotext, mod_mlay, 
     &   nuref, radius, mod_nlay,
     &   mod_z, mod_a, mod_b, mod_r, inqa, inqb, 2, .false., 1)
c 
      print *,infotext
c
      if (radius.gt.0.d0) stop 'ERROR (gr_refmod): only flat models!'
      if (nuref.gt.0.d0) stop 'ERROR (gr_refmod): no dispersion!'
c 
      do i=0,mod_nlay
        mod_Qa(i)=inqa(i)
        mod_Qb(i)=inqb(i)
      enddo
c 
c go source
      call refmet_rsource(sourcename, infotext, junk,
     &   typ, outsig, srcsig, The, Thd, src_depth, src_amp,
     &   M1, M2, M3, M4, M5, M6, 2, 1, .false.)
c 
      print *,infotext
c 
      if (typ.eq.1) then
        src_type=2
        print *,'NOTICE: assuming explosion source'
      elseif (typ.eq.2) then
        src_type=1
      else
        stop 'ERROR (gr_refmod): unknown source type'
      endif
c
c source and subsurface model are available, noe pass them to the core
      call gr_dsetmod(mod_z, mod_a, mod_b, mod_r, mod_Qa, mod_Qb,
     &  src_type, src_depth, src_amp, mod_nlay+1)
c 
      return
      end
cS
c----------------------------------------------------------------------
c set imaginary part of angular frequency (not yet implemented)
c
      subroutine gr_setsigma(sigma)
c
      double precision sigma
c
cE
      return
      end
c 
c----------------------------------------------------------------------
c
c ----- END OF interface.f ----- 
