c this is <dat_famp.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c fit amplitudes for reflectivity green to match real data (ignore phase)
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
c (simple linear least squares)
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    07/04/00   V1.1   added refer paremeters
c
c==============================================================================
c
      subroutine dat_famp(ref,imod)
c
c ref:      refer to this dataspace index (di_read was usual)
c imod:     dataspace to modify (di_mcalc was usual)
c 
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c
      integer ref,imod
c
cE
      double precision denominator, synsqr
      double precision numerator, realsqr
      double precision factor
      integer ifre, islo
c 
      if (verb_subaction) print *,'ENTER dat_famp ',ref,imod
c 
      if (verb_subaction) print *,'NOTICE (dat_famp): do real prefit'
c
      do ifre=rng_fmin,rng_fmax
        denominator=0.d0
        numerator=0.d0
        do islo=rng_smin,rng_smax
          realsqr=real(green(islo,ifre,ref))**2+
     &            imag(green(islo,ifre,ref))**2
          synsqr=real(green(islo,ifre,imod))**2+
     &           imag(green(islo,ifre,imod))**2
          numerator=numerator+sqrt(synsqr*realsqr)
          denominator=denominator+synsqr
        enddo
c 
c safety valve (may open in case we missed all modes)
        if (denominator.lt.1e-20) then
          if (verb_allwarn) then
            print *,'WARNING (dat_famp): denominator too small!'
            print *,'         ifre ',ifre,' fre ',dat_fre(ifre)
          endif
          factor=0.d0
        else
c perfect - take it
          factor=numerator/denominator
        endif
        if (verb_substrategy) then
          print *,'NOTICE (dat_famp): ifre, rsq, ssq, fac: ',
     &      ifre, realsqr, synsqr, factor
        endif
c 
c calculate filtered coefficients
        do islo=rng_smin,rng_smax
          green(islo,ifre,imod)=factor*green(islo,ifre,imod)
        enddo
      enddo
c 
      if (verb_subaction) print *,'LEAVE dat_famp'
c 
      return
      end
c
c ----- END OF dat_famp.f -----
