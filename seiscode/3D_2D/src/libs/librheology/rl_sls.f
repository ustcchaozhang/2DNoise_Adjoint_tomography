c this is <rl_sls.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1998 by Thomas Forbriger (IfG Stuttgart)
c
c provide rheology of a standard linear solid
c
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
c    20/10/98   V1.0   Thomas Forbriger
c
c==============================================================================
cS
c
      double complex function rl_sls_v(om, omr, omp, vref, qref)
c
c calculate complex velocity acoording to a given angular frequency
c
c input:
c   om     angular frequency to calculate values of velocity and quality
c   omr    reference angular frequency at which vref and qref are given
c   omp    relaxation angular frequency for standard linear solid
c   vref   reference velocity at omr
c   qref   reference velocity-Q at omr
c
c output:
c   v      velocity at om
c   q      velocity-Q at om
c
      double precision om, omr, omp, vref, qref
c
cE
c
      double precision taue, taup, qmref, qm, phi, rfac, pi, angfacr
      double complex result
      double precision rl_qm_from_qv
      double precision rl_phi_from_qv
      double precision rl_phi_from_qm
      double precision rl_sls_qm
      double precision rl_sls_mfac
      double complex ime
      parameter(pi=3.14159265358979d0)
      parameter(ime=(0.d0,1.d0))
c
      qmref=rl_qm_from_qv(qref)
      call rl_taue(omr, omp, qmref, taue, taup)
      qm=rl_sls_qm(om, taue, taup, qmref)
      phi=rl_phi_from_qm(qm)
      angfacr=cos(0.5d0*rl_phi_from_qv(qref))
      rfac=vref/(angfacr*rl_sls_mfac(omr, taue, taup))
      result=rfac*rl_sls_mfac(om,taue,taup)*exp(ime*0.5d0*phi)
      rl_sls_v=result
c 
      return
      end
c
c----------------------------------------------------------------------
cS
c
      subroutine rl_sls(om, omr, omp, vref, qref, v, q)
c
c calculate velocity and velocity-Q acoording to a given angular frequency
c
c input:
c   om     angular frequency to calculate values of velocity and quality
c   omr    reference angular frequency at which vref and qref are given
c   omp    relaxation angular frequency for standard linear solid
c   vref   reference velocity at omr
c   qref   reference velocity-Q at omr
c
c output:
c   v      velocity at om
c   q      velocity-Q at om
c
      double precision om, omr, omp, vref, qref, v ,q
c
cE
c
      double precision taue, taup, qmref, qm, phi, angfac, rfac, pi, angfacr
      double precision rl_qm_from_qv
      double precision rl_qv_from_qm
      double precision rl_phi_from_qv
      double precision rl_phi_from_qm
      double precision rl_sls_qm
      double precision rl_sls_mfac
      parameter(pi=3.14159265358979d0)
c
      qmref=rl_qm_from_qv(qref)
      call rl_taue(omr, omp, qmref, taue, taup)
      qm=rl_sls_qm(om, taue, taup, qmref)
      phi=rl_phi_from_qm(qm)
      q=rl_qv_from_qm(qm)
      angfac=cos(0.5d0*phi)
      angfacr=cos(0.5d0*rl_phi_from_qv(qref))
      rfac=vref/(angfacr*rl_sls_mfac(omr, taue, taup))
      v=rfac*rl_sls_mfac(om,taue,taup)*angfac
c 
      return
      end
c
c----------------------------------------------------------------------
cS
c
      double precision function rl_sls_mfac(om, taue, taup)
c
c calculate modul-factor acoording to a given angular frequency
c
c input:
c   om     angular frequency to calculate values of velocity and quality
c   taue   strain relaxation time constant
c   taup   stress relaxation time constant
c
      double precision om, taue, taup
c
cE
c
      double precision result, pi
      parameter(pi=3.14159265358979d0)
c
      result=sqrt((1.d0+(om*taue)**2)/(1.d0+(om*taup)**2))
      rl_sls_mfac=result
c 
      return
      end
c
c----------------------------------------------------------------------
cS
c
      double precision function rl_sls_qm(om, taue, taup, qmref)
c
c calculate modul-Q acoording to a given angular frequency
c
c input:
c   om     angular frequency to calculate values of velocity and quality
c   taue   strain relaxation time constant
c   taup   stress relaxation time constant
c   qmref  reference modul-Q at omr
c
      double precision om, taue, taup, qmref
c
cE
c
      double precision result, pi
      parameter(pi=3.14159265358979d0)
c
      result=(1.d0+(om**2)*taue*taup)/(om*(taue-taup))
      rl_sls_qm=result
c 
      return
      end
c
c----------------------------------------------------------------------
cS
c
      subroutine rl_taue(omr, omp, qref, taue, taup)
c
c return reasonable values for TauE and TauP
c calculated for given modul-Q
c
      double precision omr, omp, qref, taue, taup
c
cE
      double precision b, tau
c
      tau=1/omp
      b=0.5d0*(tau/qref)*((1+(omr*tau)**2)/(omr*tau))
      taue=b+sqrt(b**2+tau**2)
      taup=(tau**2)/taue
c
      return
      end
c 
c----------------------------------------------------------------------
cS
c
      subroutine rl_taue12(omr, omp, qref, taue1, taue2)
c
c return both possible values for TauE
c
      double precision omr, omp, qref, taue1, taue2
c
cE
      double precision term1,term2,term3
c
      term1=omp+omr**2/omp
      term2=omp**2+(omr**2/omp)**2+2*omr**2+4*omr**2*qref**2
      term3=2*omr*qref*omp
c 
      taue1=(term1+sqrt(term2))/term3
      taue2=(term1-sqrt(term2))/term3
c
      return
      end
c 
c ----- END OF rl_sls.f -----
