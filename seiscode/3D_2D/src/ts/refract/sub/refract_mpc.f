c this is <refract_mpc.f>
c------------------------------------------------------------------------------
c
c 30/04/98 by Thomas Forbriger (IfG Stuttgart)
c
c calculate mpc-factors for all traces
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
c (mpc means 'meters per count' and is in fact the amplitude scaling factor)
c
c REVISIONS and CHANGES
c    30/04/98   V1.0   Thomas Forbriger
c    03/07/98   V1.1   ok - after puzzling hard I admit that there is no way
c                      do find a global reference scale to a global reference 
c                      offset for scaling mode 3 as the actual offset
c                      dependency is unknown
c    20/11/12   V1.2   use field offset for scaling purposes, not plot
c                      offset
c    07/12/15   V1.3   mode 2 and 3 now refer scaling to average scaled
c                      amplitude in defined offset range
c
c==============================================================================
cS
c
      subroutine mpcfactors
c
c calculate mpc-factors for all traces
c
      include 'refract_dim.inc'
      include 'refract_data.inc'
      include 'refract_seipar.inc'
      include 'refract_para.inc'
c
cE
      integer i,j
      real refmpc, refamp
      real refract_avgamp
c
c scaling mode 1: individual scaling
c ----------------------------------
      if (plpar_mode.eq.1) then
        do j=1,ntraces
          if (plpar_remav) then
            refamp=max(abs(maxval(j)-average(j)),abs(minval(j)-average(j)))
          else
            refamp=max(abs(maxval(j)),abs(minval(j)))
          endif
          trv_mpc(j)=plpar_amp/refamp
          if (debug) print *,'DEBUG (mpcfactors): trace, mpc ',j,trv_mpc(j)
          if (debug) print *,'DEBUG (mpcfactors): refamp ',refamp
          if (debug) print *,'DEBUG (mpcfactors): maxval ',maxval(j)
          if (debug) print *,'DEBUG (mpcfactors): minval ',minval(j)
        enddo
c
c scaling mode 2: least offset trace is reference
c -----------------------------------------------
      elseif (plpar_mode.eq.2) then
        j=1
        refamp=refract_avgamp(j, .true.)
        refmpc=plpar_amp/refamp
c set mpc factors
        do j=1,ntraces
          trv_mpc(j)=refmpc*(fieldoffset(j)**plpar_expo)
        enddo
c
c scaling mode 3: least offset trace or average in given offset range
c                 in dataset is reference
c -------------------------------------------------------------------
      elseif (plpar_mode.eq.3) then
c go for files
        do i=1,nfiles
          refamp=refract_avgamp(i, .false.)
          refmpc=plpar_amp/refamp
c set mpc factors within file
          do j=1,ntraces
            if (fileindex(j).eq.i) then
              trv_mpc(j)=refmpc*(fieldoffset(j)**plpar_expo)
            endif
          enddo
        enddo
      else
        stop 'ERROR (mpcfactors): unknown scaling mode'
      endif
c 
      if (debug) then
        do j=1,ntraces
          print *,'DEBUG: trace ',j,' mpc: ',trv_mpc(j)
        enddo
      endif
c 
      return
      end
c
c ----- END OF refract_mpc.f -----
