c this is <refract_ttreduce.f>
c------------------------------------------------------------------------------
c
c 04/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c two functions that will calculate traveltime reduction
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
c    04/07/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
      real function redtime(intime, offset)
c 
c input is real seismogram time
c output is reduced time
c
      include 'refract_para.inc'
c 
      real intime, offset
c 
      real result
c 
      result=intime
      if (plflag_reduce) then
        result=intime-1.e-3*offset/plpar_vred
      endif
c
      redtime=result
c
      return
      end
c 
c----------------------------------------------------------------------
c
      real function realtime(intime, offset)
c 
c input is reduced time
c output is real seismogram time
c
      include 'refract_para.inc'
c 
      real intime, offset
c 
      real result
c 
      result=intime
      if (plflag_reduce) then
        result=intime+1.e-3*offset/plpar_vred
      endif
c
      realtime=result
c 
      return
      end
c
c ----- END OF refract_ttreduce.f -----
