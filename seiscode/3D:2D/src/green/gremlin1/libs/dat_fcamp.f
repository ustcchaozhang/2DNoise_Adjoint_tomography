c this is <dat_fcamp.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c fit complex response between delta-source and real greens function
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
c    25/03/98   V1.1   remember transfer function
c    25/05/00   V1.2   calculate weighted least-squares
c
c==============================================================================
cS
c
      subroutine dat_fcamp
c
c fit complex amplitudes for reflectivity green to match real data
c (simple linear least squares)
c calculate filtered version of reflectivity green
c 
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c
cE
      double complex numerator
      double complex synamp
      double precision denominator
      double complex refconjg
      integer ifre, islo
c 
      if (verb_subaction) print *,'ENTER dat_fcamp'
c 
      if (verb_subaction) print *,'NOTICE (dat_fcamp): do complex prefit'
c 
c clear transfer function     
      do ifre=1,rng_fmin
        dat_response(ifre)=(0.,0.)
      enddo
      do ifre=rng_fmax,glqd_mfre
        dat_response(ifre)=(0.,0.)
      enddo
c
      do ifre=rng_fmin,rng_fmax
        numerator=(0.d0,0.d0)
        denominator=0.d0
        do islo=rng_smin,rng_smax
          refconjg=conjg(green(islo,ifre,di_mcalc))
          denominator=denominator+
     &      (green(islo,ifre,di_mcalc)*refconjg)*gweight(islo,ifre)
          numerator=numerator+
     &      (green(islo,ifre,di_read)*refconjg)*gweight(islo,ifre)
        enddo
c 
c safety valve (may open in case we missed all modes)
        if (denominator.lt.1e-200) then
          if (verb_allwarn) then
            print *,'WARNING (dat_fcamp): denominator too small!'
            print *,'         ifre ',ifre,' fre ',dat_fre(ifre)
          endif
          synamp=(0.d0,0.d0)
        else
c perfect - take it
          synamp=numerator/denominator
        endif
c remember
        dat_response(ifre)=synamp
c 
c calculate filtered coefficients
        do islo=rng_smin,rng_smax
          green(islo,ifre,di_mcalc)=synamp*green(islo,ifre,di_mcalc)
        enddo
      enddo
c 
      if (verb_subaction) print *,'LEAVE dat_fcamp'
c 
      return
      end
c
c ----- END OF dat_fcamp.f -----
