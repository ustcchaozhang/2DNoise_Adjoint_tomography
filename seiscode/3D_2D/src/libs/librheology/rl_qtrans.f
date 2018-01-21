c this is <rl_qtrans.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1998 by Thomas Forbriger (IfG Stuttgart)
c
c transform Q-values
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
      double precision function rl_qm_from_phi(phi)
c 
c calculate modul-Q from phase angle
c
      double precision phi
c
      double precision result
c
      result=1/tan(phi)
      rl_qm_from_phi=result
c
      return
      end
c 
c----------------------------------------------------------------------
c
      double precision function rl_qv_from_phi(phi)
c 
c calculate velocity-Q from phase angle
c
      double precision phi
c
      double precision result
c
      result=1/tan(0.5d0*phi)
      rl_qv_from_phi=result
c
      return
      end
c 
c----------------------------------------------------------------------
c
      double precision function rl_phi_from_qm(qm)
c
c calculate phase angle from modul-Q
c
      double precision qm
c
      double precision result
c
      result=atan2(1.d0,qm)
      rl_phi_from_qm=result
c
      return
      end
c 
c----------------------------------------------------------------------
c
      double precision function rl_phi_from_qv(qv)
c
c calculate phase angle from velocity-Q
c
      double precision qv
c
      double precision result
c
      result=2.d0*atan2(1.d0,qv)
      rl_phi_from_qv=result
c
      return
      end
c 
c----------------------------------------------------------------------
c
      double precision function rl_qm_from_qv(qv)
c
c calculate modul-Q from velocity-Q
c
      double precision qv
c
      double precision result
c
      result=1.d0/(tan(2.d0*atan2(1.d0,qv)))
      rl_qm_from_qv=result
c
      return
      end
c 
c----------------------------------------------------------------------
c
      double precision function rl_qv_from_qm(qm)
c
c calculate velocity-Q from modul-Q
c
      double precision qm
c
      double precision result
c
      result=1.d0/tan(0.5d0*atan2(1.d0,qm))
      rl_qv_from_qm=result
c
      return
      end
c 
c
c ----- END OF rl_qtrans.f -----
