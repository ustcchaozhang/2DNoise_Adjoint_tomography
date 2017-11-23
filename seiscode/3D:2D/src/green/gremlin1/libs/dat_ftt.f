c this is <dat_ftt.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c do a prefit on travel times
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
c the mean travel time error will be removed form the synthetics
c this corresponds to an error in the shot (zero) time
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine dat_ftt(ref)
c
c ref=.true.:   data should be stored in reference data array
c
      logical ref
c 
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c
cE
      double precision meanerror
      integer i, n, data_index
c 
      if (verb_subaction) print *,'ENTER dat_ftt(',ref,')'
c 
      if (ref) then
        data_index=di_mref
      else
        data_index=di_mcalc
      endif
c 
      n=min(rng_xmax, dottprefit)
      if (verb_subaction) print *,'NOTICE (dat_ftt): ',
     &  'prefit shot (zero) time by mean error over ',n,' samples'
c
      meanerror=0.d0
      do i=1,n
        meanerror=meanerror+travt(i, di_mread)-travt(i, data_index)
      enddo
      meanerror=meanerror/dble(n)
c 
      do i=1,rng_xmax
        travt(i,data_index)=travt(i,data_index)+meanerror
      enddo
      if (verb_subresult) print *,'NOTICE (dat_ftt): ',
     &  'synthetics are delayed by ',meanerror,' seconds.'
c 
      if (verb_subaction) print *,'LEAVE dat_ftt'
c 
      return
      end
c
c ----- END OF dat_ftt.f -----
