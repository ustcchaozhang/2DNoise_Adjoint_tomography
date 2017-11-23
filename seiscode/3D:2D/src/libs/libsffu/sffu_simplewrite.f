c this is <sffu_simplewrite.f>
c------------------------------------------------------------------------------
cS
c Copyright (c) 2001 by Thomas Forbriger (IfG Stuttgart)
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
c some routines for easy to use file writing
c
c REVISIONS and CHANGES
c    09/02/2001   V1.0   Thomas Forbriger
c    03/06/2012   V1.1   added simplewrite_external_ws
c
c==============================================================================
c 
      subroutine sffu_simpleopen(lu, filename)
c
c open file with defined source time and source location
c
c declare parameters
      integer lu
      character filename*(*)
c
cE
c declare local variables
      integer ierr
      character type*20
      character cs*1
      real c1,c2,c3
      character date*6, time*10
c
      type='NSP'
      cs='C'
      c1=0.
      c2=0.
      c3=0.
      date='010209'
      time='000000.000'
c
      call sff_WOpenS(lu, filename, type, cs, c1, c2, c3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR (sffu_simpleopen): opening file'
c 
      return
      end
c
c------------------------------------------------------------------------------
cS
c
      subroutine sffu_simplewrite_external_ws(lu, last, x, n, dt, r, 
     &  ix, mi)
c
c write a single trace with offset 
c accept workspace for data sample conversion from user
c
c lu:       file unit
c last:     true if file is to be closed
c x:        time series
c n:        number of samples
c r:        receiver offset value
c dt:       sampling interval 
c ix:       workspace for data conversion
c mi:       dimension of workspace array
c
c declare parameters
      integer lu
      logical last
      integer n, mi
      real x(n)
      integer ix(mi)
      real r, dt
c
cE
c declare local variables
      integer ierr
      real rate, second, calib, calper, hang, vang
      character*10 nsp,station,comp,auxid,instype
      character*132 wid2line
      parameter(nsp='NSP')
      integer year, month, day, hour, minute, nstack
      real c1,c2,c3
      character*1 cs
c 
      if (n.gt.mi) 
     &  stop 'ERROR (sffu_simplewrite_external_ws): too many samples'
c 
      rate=1./dt
      station=nsp
      comp=nsp
      auxid=nsp
      instype=nsp
      year=2001
      month=02
      day=09
      hour=0
      minute=0
      second=0.
      calib=1.
      calper=-1.
      hang=-1.
      vang=-1.
c 
      call sff_PrepWID2(n, rate, station, year, month,
     &    day, hour, minute, comp, auxid, instype, second, calib,
     &    calper, hang, vang, wid2line, ierr)
      if (ierr.ne.0) 
     &  stop 'ERROR (sffu_simplewrite_external_ws): preparing WID2 line'
c 
      c1=r
      c2=0.
      c3=0.
      cs='C'
      nstack=1
c 
      call sff_WTraceI(lu, wid2line, n, x, ix, last,
     &    cs, c1, c2, c3, nstack, ierr)
      if (ierr.ne.0) 
     &  stop 'ERROR (sffu_simplewrite_external_ws): writing trace'
c 
      return
      end
c
c------------------------------------------------------------------------------
cS
c
      subroutine sffu_simplewrite(lu, last, x, n, dt, r)
c
c write a single trace with offset 
c
c lu:       file unit
c last:     true if file is to be closed
c x:        time series
c n:        number of samples
c r:        receiver offset value
c dt:       sampling interval 
c
c declare parameters
      integer lu
      logical last
      integer n
      real x(n)
      real r, dt
c
cE
c declare local variables
      integer maxint
      parameter (maxint=100000)
      integer ix(maxint)

      call sffu_simplewrite_external_ws(lu, last, x, n, dt, r, 
     &  ix, maxint)
c 
      return
      end
c
c ----- END OF sffu_simplewrite.f -----
