c this is <sub/refract_avgamp.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2015 by Thomas Forbriger (BFO Schiltach) 
c
c calculate average amplitude in offset range (used by subroutine mpcfactors)
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
      real function refract_avgamp(ifile, allfiles)
c
      integer ifile
      logical allfiles
c
c Return average amplitude for traces in file ifile in amplitude
c reference range. 
c
c ifile: file index of file to calculate average amplitude for
c allfiles: use all files not just file ifile
c
c return: average scaled amplitude value
cE
c 
      include 'refract_dim.inc'
      include 'refract_data.inc'
      include 'refract_para.inc'
c 
      integer navg, j
      real maxamp, thisamp
      logical usefile
c
      navg=0
      maxamp=0.
      do j=1,ntraces
        usefile=allfiles
        if (.not.usefile) usefile=(fileindex(j).eq.ifile)
        if (usefile
     &      .and.(fieldoffset(j).ge.plpar_avgrefxmin)
     &      .and.(fieldoffset(j).le.plpar_avgrefxmax)) then
          navg=navg+1
          if (plpar_remav) then
            thisamp=max(abs(maxval(j)-average(j)),
     &                  abs(minval(j)-average(j)))
          else
            thisamp=max(abs(maxval(j)),abs(minval(j)))
          endif
          maxamp=maxamp+thisamp*(fieldoffset(j)**plpar_expo)
        endif
      enddo
      if (navg.lt.1) then
        if (allfiles) then
          print *,'ERROR (refract_avgamp): ',
     &      'no traces in offset range selected by -S3 or -SN'
        else
          print *,'ERROR (refract_avgamp): ',
     &      'file ',filename(ifile)(1:index(filename(ifile),' ')-1),
     &      ' has no traces in offset range selected by -S3 or -SN'
        endif
        stop 'aborting...'
      endif
      refract_avgamp=maxamp/navg
c
      return
      end
c
c ----- END OF sub/refract_avgamp.f ----- 
