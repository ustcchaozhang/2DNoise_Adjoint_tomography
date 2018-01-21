c this is <gr_setrheo.f>
c------------------------------------------------------------------------------
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c set model values for rheological models
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
c    21/10/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
c list of subroutines in this file:
c
c   subroutine gr_slsfsm:  finish a source and a model for a standard linear
c                          solid
c                             this means: calculate the layer to which the
c                             source belongs and calculate the complex
c                             velocities and calculate layer thickness
c 
c----------------------------------------------------------------------
c 
c some remarks on the used units:
c
c   the model is given in units of
c      density:     g/ccm
c      velocity:    km/s
c      slowness:    s/km
c      frequency:   Hz (1/s)
c      layer depth: km
c
c   if you want to receive units appropriate for slowness in s/m
c   you will have to devide the green coefficients by 1.e6
c 
c======================================================================
c
c finish a model definition
c
c this modifies the complex velocities according to a standard linear solid
c
      subroutine gr_slsfsm(nu,nur,nup)
c
c nu        frequency to prepare model
c nur       frequency at which reference model is given
c nup       frequency for SLS time constant
c
      double precision nu,nur,nup
c
c read common blocks
      include 'gr_source.inc'
      include 'gr_model.inc'
cE
c rest of variable declaration
      integer i
      double complex ime
      parameter(ime=(0.d0,1.d0))
      double precision pi, om, omr, omp
      double complex rl_sls_v
      parameter(pi=3.141592653589793115997963468544185161590576171875d0)
c go
c
      om=2.d0*pi*nu
      omr=2.d0*pi*nur
      omp=2.d0*pi*nup
c
c calculate geometry coefficients
      if (src_type.eq.1) then
        src_dim=1.d-12
      elseif (src_type.eq.2) then
        src_dim=1.d-15
      else
        stop 'ERROR (gr_slsfsm): unknown source type'
      endif
c 
c now calculate layer thickness
      do i=0,mod_nlay-1
        mod_t(i)=mod_z(i+1)-mod_z(i)
      enddo
c 
c look for source layer index
      i=0
    1 i=i+1
      if ((mod_z(i).le.src_depth).and.(i.lt.mod_nlay)) goto 1
      src_layer=i-1
      if (src_depth.ge.mod_z(mod_nlay)) src_layer=mod_nlay
c      print *,'src_layer ',src_layer
c      print *,'src_depth ',src_depth
c      print *,'src_type ',src_type
c      print *,'src_amp ',src_amp
c
c now calculate the complex velocities
      do i=0,mod_nlay
         mod_aC(i)=rl_sls_v(om, omr, omp, mod_a(i), mod_Qa(i))
         mod_bC(i)=rl_sls_v(om, omr, omp, mod_b(i), mod_Qb(i))
      enddo
c
c do some precalculation
      FFI=src_amp*src_dim/(4.d0*pi*mod_r(src_layer))
c      print *,FFI
c 
c output
c      print *,'layer stack:'
c      do i=0,mod_nlay
c        print 50,i,mod_z(i),mod_a(i),mod_b(i),mod_r(i),mod_Qa(i),mod_Qb(i)
c      enddo
c      do i=0,mod_nlay
c        print *,i,mod_aC(i), mod_bC(i)
c      enddo
c 
      return
   50 format(i5, 6f10.3)
      end
cS 
c
c ----- END OF gr_setrheo.f -----
