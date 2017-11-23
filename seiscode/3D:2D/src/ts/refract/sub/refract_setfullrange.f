c this is <refract_setfullrange.f>
c------------------------------------------------------------------------------
c
c 30/04/98 by Thomas Forbriger (IfG Stuttgart)
c
c set world coordinates according to the full scale of the whole set of
c seismograms
c
c ----
c refract is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c refract is distributed in the hope that it will be useful,
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
c    30/04/98   V1.0   Thomas Forbriger
c    26/06/98   V1.1   - use clipping region, not only real seismogram
c                        amplitudes
c                      - expo-factors already come with mpc-factors
c    04/07/98   V1.2   respect traveltime reduction
c
c==============================================================================
cS
c 
      subroutine setfullrange
c
c set world coordinates according to the full scale of the whole set of
c seismograms
c
      include 'refract_dim.inc'
      include 'refract_data.inc'
      include 'refract_seipar.inc'
      include 'refract_para.inc'
c
cE
c
      integer i
      real tmin, tmax, thistmax, rmin, rmax
      real redtime, plotoffset
c
c find most and least values
      rmin=plotoffset(1)
      rmax=plotoffset(1)
      tmin=redtime(toffset(1),plotoffset(1))
      tmax=redtime(toffset(1)+dt(1)*(nsamples(1)-1),plotoffset(1))
      do i=1,ntraces
        if (plpar_remav) then
          rmin=min(rmin,plotoffset(i)+max(((minval(i)-average(i))*
     &      trv_mpc(i)),-plpar_clip))
          rmax=max(rmax,plotoffset(i)+min(((maxval(i)-average(i))*
     &      trv_mpc(i)),plpar_clip))
        else
          rmin=min(rmin,roffset(i)+max((minval(i)*
     &      trv_mpc(i)),-plpar_clip))
          rmax=max(rmax,roffset(i)+min((maxval(i)*
     &      trv_mpc(i)),plpar_clip))
        endif
c 
        tmin=min(tmin,redtime(toffset(i),plotoffset(i)))
        thistmax=toffset(i)+dt(i)*(nsamples(i)-1)
        tmax=max(tmax,redtime(thistmax,plotoffset(i)))
      enddo
c 
      tov_tmin=tmin
      tov_tmax=tmax
      tov_rmin=rmin-0.02*(rmax-rmin)
      tov_rmax=rmax+0.02*(rmax-rmin)
c
      return
      end
c 
c ----- END OF refract_setfullrange.f -----
