c this is <epi.f> by Thomas Forbriger
c
c Copyright 2010 by Thomas Forbriger
c
c Just a quick and dirty hack to print out epicentral distances
c of traces in a sff data file
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
c----------------------------------------------------------------------
      program epi
c
c 
      character*70 version
      parameter(version='EPI   V1.0   epicentral distances')
c 
      integer maxsamp
      parameter(maxsamp=100000)
      real fdata(maxsamp)
      integer idata(maxsamp)
      equivalence (fdata,idata)
      integer ierr, trace, iargc, lu, nstack, nsamp
      logical last
      parameter(lu=10)
      character cs*1, timestamp*20, code*10, filename*80, line*80
      character wid2line*132
      real sffvers, tanf, dt, c1, c2, c3, radius
c 
      print *,version
      if ((iargc().lt.1).or.(iargc().gt.2)) then
        print *,'Usage: epi file [radius]'
        print *,' '
        print *,'The given sff data file will be read and the contents of all'
        print *,'info lines will be printed. If there is an additional earth'
        print *,'radius given the distances will be transformed from flat'
        print *,'to spherical.'
        print *,' '
        print *,'The distances are give raltive to the origin of the'
        print *,'coordinate system.'
        stop
      endif
c 
      call getarg(1, filename)
      if (iargc().gt.1) then
        call getarg(2, line)
        read(line, '(f10.3)') radius
      else
        radius=0.
      endif
c 
      call sff_ROpen(lu, filename, sffvers, timestamp, code, ierr)
      if (ierr.ne.0) stop 'ERROR: opening file'
      trace=0
    1 continue
        trace=trace+1
        nsamp=maxsamp
        call sff_RTraceI(lu, tanf, dt, wid2line, nsamp, fdata, idata,
     &    code, last, cs, c1, c2, c3, nstack, ierr)
        if (ierr.ne.0) stop 'ERROR: reading trace'
        if (index(code,'I').gt.0) then
          call coordout(radius, trace, cs, c1, c2, c3)
        else
          print *,'no INFO line for trace ',trace
        endif
        if (.not.(last)) goto 1
      stop
      end

c----------------------------------------------------------------------
      subroutine coordout(radius, trace, cs, c1, c2, c3)
      real radius, c1, c2, c3
      integer trace
      character cs*1
c 
      logical trans, spher
      real lat, lon, hei, rad, x, y, r, delta
c 
      trans=.false.
      spher=.true.
      if (radius.gt.0.) trans=.true. 
      if (cs.eq.'C') spher=.false.
c 
      if (spher) then
        lat=c1
        lon=c2
        hei=c3
      else
        x=c1/1000.
        y=c2/1000.
        hei=c3/1000.
        r=sqrt(x*x+y*y)
      endif
c 
      if (trans) then
        rad=radius-hei
        if (spher) then
          delta=(90.-lat)
          r=delta*radius*3.1415/180.
          print 50, trace, delta, r, lat, lon, hei, rad
        else
          delta=r/radius/3.1415*180.
          print 51, trace, delta, r, x, y, hei, rad
        endif
      else
        if (spher) then
          delta=(90.-lat)
          print 52, trace, delta, lat, lon, hei
        else
          print 53, trace, r, x, y, hei
        endif
      endif
c 
      return
   50 format(/'trace ',i3,
     &       t20,'epicentral distance:',f10.3,'° = ',f10.3,'km',
     &      /t10,'latitude: ',f10.3,     t40,'longitude: ',f10.3
     &      /t10,'   depth: ',f10.3,'km',t40,'   radius: ',f10.3,'km')
   51 format(/'trace ',i3,
     &       t20,'epicentral distance:',f10.3,'° = ',f10.3,'km',
     &      /t10,'       X: ',f10.3,'km',t40,'        Y: ',f10.3,'km',
     &      /t10,'   depth: ',f10.3,'km',t40,'   radius: ',f10.3,'km')
   52 format(/'trace ',i3,
     &       t20,'epicentral distance:',f10.3,'°',
     &      /t10,'latitude: ',f10.3,     t40,'longitude: ',f10.3
     &      /t10,'   depth: ',f10.3,'km')
   53 format(/'trace ',i3,
     &       t20,'epicentral distance:',f10.3,'km',
     &      /t10,'       X: ',f10.3,'km',t40,'        Y: ',f10.3,'km',
     &      /t10,'   depth: ',f10.3,'km')
      end
