c this is <dat_fmaxamp.f>
c------------------------------------------------------------------------------
cS
c   ($Id$)
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
c prefit to maximum amplitude
c
c REVISIONS and CHANGES
c    07/04/2000   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine dat_fmaxamp(ref,imod)
c 
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c
c ref:      refer to this dataset index (di_read was usual)
c imod:     dataspace index to modify
c
      integer ref,imod
c
cE
c declare local variables
      character*(*) dat_fmaxamp_id
      parameter (dat_fmaxamp_id='$Id$')
      double precision denominator, synsqr
      double precision numerator, realsqr
      double precision factor
      integer ifre, islo
c 
      if (verb_subaction) print *,'ENTER dat_fmaxamp ',ref,imod
c 
      if (verb_subaction) print *,'NOTICE (dat_fmaxamp): ',
     &                            'do maximum amplitudeprefit'
c
      do ifre=rng_fmin,rng_fmax
        denominator=0.d0
        numerator=0.d0
        realsqr=0.d0
        synsqr=0.d0
        do islo=rng_smin,rng_smax
          realsqr=max(realsqr, (real(green(islo,ifre,ref))**2+
     &            imag(green(islo,ifre,ref))**2))
          synsqr=max(synsqr, (real(green(islo,ifre,imod))**2+
     &           imag(green(islo,ifre,imod))**2))
        enddo
        numerator=sqrt(realsqr)
        denominator=sqrt(synsqr)
c 
c safety valve (may open in case we missed all modes)
        if (denominator.lt.1e-20) then
          if (verb_allwarn) then
            print *,'WARNING (dat_fmaxamp): denominator too small!'
            print *,'         ifre ',ifre,' fre ',dat_fre(ifre)
          endif
          factor=0.d0
        else
c perfect - take it
          factor=numerator/denominator
        endif
c 
c calculate filtered coefficients
        do islo=rng_smin,rng_smax
          green(islo,ifre,imod)=factor*green(islo,ifre,imod)
        enddo
      enddo
c 
      if (verb_subaction) print *,'LEAVE dat_fmaxamp'
c 
      return
c the following line prevents the linker from removing the ID string
   99 print *, dat_fmaxamp_id
      end
c
c ----- END OF dat_fmaxamp.f -----
