c this is <coro.f>
c------------------------------------------------------------------------------
c
c 27/05/98 by Thomas Forbriger (IfG Stuttgart)
c
c seismogram COmponent ROtation
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
c    27/05/98   V1.0   Thomas Forbriger
c    06/12/04   V1.1   set component name
c
c==============================================================================
c
      program coro
c
      character*79 version
      parameter(version='CORO   V1.1   seismogram COmponent ROtation')
c
c dimensions
      integer maxsamps, maxfree
      parameter(maxsamps=66000, maxfree=20)
c parameters
      real vx,vy,vz,amp
      character*80 para
c 
      integer i
c input data
      real xdata(maxsamps), ydata(maxsamps), zdata(maxsamps)
      integer ixdata(maxsamps), iydata(maxsamps), izdata(maxsamps)
      equivalence(xdata,ixdata)
      equivalence(ydata,iydata)
      equivalence(zdata,izdata)
      character*80 inxname, inyname, inzname
      integer lux, luy, luz
      parameter(lux=10, luy=11, luz=12)
c output data
      real data(maxsamps)
      integer idata(maxsamps)
      equivalence(data,idata)
      character*80 outname
      character*132 wid2line
      integer luo
      parameter(luo=13)
c sff extras
      character*80 free(maxfree)
      integer nfree
      real libversion
      character timestamp*13, code*10, type*20, cs*1, date*6, time*10
      character code2*10, cs2*1
      real c1,c2,c3,c12,c22,c32
      integer ierr,nsamp,nsamp2,nstack,nstack2
      logical last,last2
      real tanf,dt
      character*132 wid2line2
      character*40 compname
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=2)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug
c here are the keys to our commandline options
      data optid/2h-d,2h-n/
      data opthasarg/.FALSE.,.TRUE./
      data optarg/1h-,3hNSP/
c
c------------------------------------------------------------------------------
c basic information
c
      print *,version
      print *,'Usage: coro x,y,z inx iny inz out [-n name]'
      print *,'   or: coro -help'
c
      if (iargc().lt.1) stop 'ERROR: missing arguments'
      call getarg(1, para)
      if (para(1:5).eq.'-help') then
        print *,' '
        print *,'seismogram COmponent ROtation'
        print *,' '
        print *,'x,y,z        components of a vactor that defines the'
        print *,'             direction of the resulting component'
        print *,'inx          filename of input seismograms for x-component'
        print *,'iny          filename of input seismograms for y-component'
        print *,'inz          filename of input seismograms for z-component'
        print *,'out          filename of output seismograms'
        print *,' '
        print *,'-n name      set name of new component'
        print *,' '
        print *,'All information about source and receiver location will'
        print *,'be taken from inx. No plausibility checks are performed!'
        print *,' '
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(6, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      compname=optarg(2)
c
      call getarg(1, para)
      call getarg(2, inxname)
      call getarg(3, inyname)
      call getarg(4, inzname)
      call getarg(5, outname)
c 
c------------------------------------------------------------------------------
c 
c calculate direction
      read(para, *) vx,vy,vz
      amp=sqrt(vx*vx+vy*vy+vz*vz)
      vx=vx/amp
      vy=vy/amp
      vz=vz/amp
c 
      free(1)=version
      write(free(2), '(3hvx=,f10.6,2x,3hvy=,f10.6,2x,3hvz=,f10.6)')
     &  vx,vy,vz
      free(3)='x-data from '//inxname(1:index(inxname, ' '))
      free(4)='y-data from '//inyname(1:index(inyname, ' '))
      free(5)='z-data from '//inzname(1:index(inzname, ' '))
      free(6)='all information is taken from x-data file'
      free(7)='no plausibility checks were performed'
      nfree=7
c 
      call sff_ROpenS(lux, inxname,
     &  libversion, timestamp, code,
     &  type, cs, c1, c2, c3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening x-component'
      call sff_New(luo, outname, ierr)
      call sff_WOpenFS(luo, outname,
     &  free, nfree,
     &  type, cs, c1, c2, c3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening output file'
      call sff_ROpenS(luy, inyname,
     &  libversion, timestamp, code,
     &  type, cs, c1, c2, c3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening y-component'
      call sff_ROpenS(luz, inzname,
     &  libversion, timestamp, code,
     &  type, cs, c1, c2, c3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening z-component'
c 
      last=.false.
      do while (.not.(last))
        nsamp=maxsamps
        call sff_RTraceI(lux, tanf, dt, wid2line, nsamp, xdata, ixdata,
     &    code, last, cs, c1, c2, c3, nstack, ierr)
        if (ierr.ne.0) stop 'ERROR: reading x-data'
        nsamp2=maxsamps
        call sff_RTraceI(luy, tanf, dt, wid2line2, nsamp2, ydata, iydata,
     &    code2, last2, cs2, c12, c22, c32, nstack2, ierr)
        if (ierr.ne.0) stop 'ERROR: reading y-data'
        if (nsamp.ne.nsamp2) stop 'ERROR: wrong number of samples in y-data'
        last=(last.or.last2)
        nsamp2=maxsamps
        call sff_RTraceI(luz, tanf, dt, wid2line2, nsamp2, zdata, izdata,
     &    code2, last2, cs2, c12, c22, c32, nstack2, ierr)
        if (ierr.ne.0) stop 'ERROR: reading z-data'
        if (nsamp.ne.nsamp2) stop 'ERROR: wrong number of samples in z-data'
        last=(last.or.last2)
c 
        do i=1,nsamp
          data(i)=vx*xdata(i)+vy*ydata(i)+vz*zdata(i)
        enddo
        wid2line(36:38)=compname(1:3)
c 
        call sff_WTraceI(luo, wid2line, nsamp, data, idata, last,
     &    cs, c1, c2, c3, nstack, ierr)
        if (ierr.ne.0) stop 'ERROR: writing trace'
      enddo
c
      stop
      end
c
c ----- END OF coro.f -----
