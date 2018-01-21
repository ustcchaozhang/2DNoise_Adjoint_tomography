c this is <sub/refract_setrefrange.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2015 by Thomas Forbriger (BFO Schiltach) 
c
c Set offset range to take amplitude scaling reference from, if not set
c on command line.
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
c along with this program. If not, see <http://www.gnu.org/licenses/>.

c ----
c
c REVISIONS and CHANGES
c    07/12/2015   V1.0   Thomas Forbriger
c
c ============================================================================
c
cS
      subroutine refract_setrefrange
c
c set default scaling
c
      include 'refract_dim.inc'
      include 'refract_data.inc'
      include 'refract_para.inc'
      include 'refract_opt.inc'
cE
c
c The default is read from the command line parameter defaults. Make
c sure that the range at least includes on trace of each file.
c
      real rmin,rmax
      integer i
      real reffraction
c fraction of total offset range to be used for reference
      parameter(reffraction=0.1)
c
c adjust offset range only if not specified on command line
      if (.not.opt_Savgref) then
c
c find overall minimum and maximum field offset value
        rmin=fieldoffset(1)
        rmax=fieldoffset(1)
        do i=1,ntraces
        rmin=min(rmin,fieldoffset(i))
        rmax=max(rmax,fieldoffset(i))
        enddo
c
c set reference to the smalles reffraction fraction
        plpar_avgrefxmin=rmin
        plpar_avgrefxmax=rmin+(rmax-rmin)*reffraction
c
c check for at least one trace of each file being present in the
c selected range
        do i=1,nfiles
          rmin=plpar_avgrefxmin
          do j=1,ntraces
            if (fileindex(j).eq.i) then
              rmin=min(rmin,fieldoffset(j))
            endif
          enddo
          plpar_avgrefxmax=max(plpar_avgrefxmax,rmin)
        enddo
c
        if (verbose) then
          print *,'offset range to which amplitude scaling is referred:'
          print *,'  ',plpar_avgrefxmin,' m to ',plpar_avgrefxmax,' m'
        endif
      endif
c
      return
      end
c
c ----- END OF sub/refract_setrefrange.f ----- 
