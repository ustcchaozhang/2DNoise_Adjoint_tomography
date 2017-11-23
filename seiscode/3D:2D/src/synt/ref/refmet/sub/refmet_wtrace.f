c this is <sub/refmet_wtrace.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
c
c write a single SFF trace
c
c ============================================================================
c
c this is part of the REFMET reflectivity program
c (for comments and revisions see also the main source refmet.f)
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
c 27/06/97   T.F.   changed coordinate output to meters (as defined in sff)
c 25/02/99   thof   introduced spherical coordinates
c 26/02/99   thof   sensible date setting
c
c======================================================================
c 
c write a single sff trace
c
c----------------------------------------------------------------------
c
c write one sff trace
c
      subroutine writetrace(com, lu, NS, cl_debug, srcdate, dt, E, T, r, p,
     &  radius, cmp, idata, fdata, MS, last)
c 
      character com*(*), cmp*(*)
      integer lu, NS, E, MS
      integer idata(MS)
      real fdata(MS)
      real*8 dt, T, r, p, radius
      logical last, cl_debug
      integer srcdate(3)
c 
      integer hour, minute, ierr, nstack, i
      real second, rate, c1, c2
      real*8 pi
      parameter(pi=3.14159265358979d0)
      character*80 free(1)
      character wid2line*132, component*10, station*10, cs*1
c 
c free block
      free(1)=com
c prepare time of first sample
      hour=int(T/3600) 
      minute=int((T-3600.d0*hour)/60)
      second=sngl(T-3600.d0*hour-60.d0*minute)
c prepare station info
      write(station, '(i3.3)') E
c compoenent info
      component=cmp
c sampling rate
      rate=sngl(1./dt)
c receiver info
      if (radius.gt.0.d0) then
        cs='S'
        c1=90.-180.*r/radius/pi
        c2=p
      else
        cs='C'
        c1=r*cos(sngl(pi*p/180.))*1000.
        c2=r*sin(sngl(pi*p/180.))*1000.
      endif
      nstack=0
c prepare wid2line
      if (cl_debug) print *,'DEBUG: call PrepWid2'
      call sff_PrepWid2(NS, rate, station, srcdate(3), srcdate(2), srcdate(1), 
     &  hour, minute, component, 'NSP', 'NSP', second, -1., -1.,
     &  -1., -1., wid2line, ierr)
      if (ierr.ne.0) stop 'ERROR: preparing WID2 line'
c write dataset
      if (cl_debug) then
        print *,'DEBUG: call WTraceFI'
        print *,'DEBUG: lu, NS, last, cs, c1 ,c2, nstack, 1 ',
     &   lu, NS, last, cs, c1, c2, nstack, 1
        print *,'DEBUG: wid2line: ', wid2line
        print *,'DEBUG: fdata: '
        print 50,(fdata(i), i=1,NS)
      endif
      call sff_WTraceFI(lu, wid2line, NS, fdata, idata, last, 1, free,
     &  cs, c1, c2, 0., nstack, ierr)
      if (ierr.ne.0) stop 'ERROR: writing dataset'
      if (cl_debug) print *,'DEBUG: return to calling'
      return
   50 format (7(e9.3,1x),e9.3)
      end
c
c ----- END OF sub/refmet_wtrace.f ----- 
