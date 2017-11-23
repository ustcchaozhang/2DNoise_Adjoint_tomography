c this is <cmt2refmet.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 1995 by Jörg Dalkolmo
c Copyright (c) 2003 by Thomas Forbriger (BFO Schiltach) 
c
c convert CMT earthquake parameters to reflectivity source file
c
c Some parts of the source code are copied from gemini by Jörg Dalkolmo.
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
c
c REVISIONS and CHANGES
c    12/09/2003   V1.0   Thomas Forbriger
c
c ============================================================================
c
      program cmt2refmet
c
      character*(*) version
      parameter(version=
     & 'CMT2REFMET   V1.0   convert CMT earthquake parameters to refmet source file')
c
      real rmom(6), clat, clon, cdepth, M0
c 
      character*80 cmtfile,srcfile
      character*24 eventname
      character*8 code
      integer lu
      parameter(lu=10)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=2)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v/
      data opthasarg/2*.FALSE./
      data optarg/2*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.2)) then
        print *,version
        print *,'Usage: cmt2refmet cmtfile srcfile'
        print *,'   or: cmt2refmet -help'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'convert CMT earthquake parameters to refmet source file'
        print *,' '
        print *,'cmtfile      earthquake parameters in CMT format'
        print *,'srcfile      source parameters for refmet'
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call getarg(1, cmtfile)
      call getarg(2, srcfile)
      call tf_cmdline(3, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
c
c------------------------------------------------------------------------------
c go
      call readCMT(cmtfile, rmom, clat, clon, cdepth, M0, eventname, code)
      open(unit=lu, file=srcfile, err=99)
      write(lu, '(a,1x,a/)', err=98) code, eventname
      write(lu, '(3(a8),3(a10))', err=98) 'typ','outsig','srcsig',
     &  'Tonset', 'Tdur','Zsrc'
      write(lu, '(3i8,3(1x,f9.3)/)', err=98) 1, 2, 2, 0., 0., cdepth
      write(lu, '(7a8)', err=98) 'M0','Mxx','Myy','Mzz',
     &  'Mxy','Mxz','Myz'
      write(lu, '(e8.2,6(1x,f7.3)/)', err=98) M0, rmom(2), rmom(3), rmom(1),
     &  rmom(6), rmom(5), rmom(4)
      write(lu, '(a)', err=98) 'seismogram units will be:'
      write(lu, '(a)', err=98) 'nm/s'
      close(lu,err=97)
c 
      print *,' '
      print '(1x,a,1x,a)', code, eventname
      print *,' centroid latitude:', clat
      print *,'centroid longitude:', clon
      print *,'    centroid depth:', cdepth
c
      stop
   99 stop 'ERROR: opening output file'
   98 stop 'ERROR: writing output file'
   97 stop 'ERROR: closing output file'
      end
c
c======================================================================
c
c The following code was copied from the GEMINI package by Jörg Dalkolmo
c and was slightly modified to match the purpose of this program
c
      Subroutine readCMT (cevent,rmom,clat,clon,cdepth,M0, region, code)
c----                                                 ---------------
c Subroutine to read source file containing moment tensor and 
c earthquake parameters in  H a r v a r d  Catalogue Format.
c With the factors 'dyne2Nm' and 'Nm2myUnit' the resulting seimograms
c will have the unit 'nm/s' (velocity) or 'nm' (displacement).
c--------------------------------------------------------------------

      character*80 cevent
      character*2 year,month,day,hour,min
      character  time*10, date*6
      character code*(*)
      character region*(*)
      character*3 epsrc
      real lat,lon,mb,ms,ev(3),dyne2Nm,Nm2myUnit,totsec
      real rmom(6),emrr,emss,emee,emrs,emre,emse,M0
      real mwst,mwrec,mwcoff,sec,depth,ahdur,smo,
     1            dt,edt,clat,eclat,clon,eclon,cdepth,ecdepth
      integer bwst,bwrec,bwcoff,expo,strike(2),dip(2),rake(2)
      integer evpl(3),evaz(3),i,inut,ihour,inmin,ios
c CMTs are given in dyn*cm
c refmet uses N*m and outputs m/s
c we want nm/s
      parameter(dyne2Nm=1.e-7,Nm2myUnit=1.e9)
c      common / cmt_date / date,time,code

c-----------------------------------------------------------------
c  Read source file in Harvard Catalog Format with moment tensor, |
c  See the file 'CMT_explained' for further information.          |
c-----------------------------------------------------------------

      inut = 1
      OPEN(inut,file=cevent,iostat=ios,status='old',err=999)
      read(inut,1001)code,month,day,year,hour,min,sec,lat,lon,depth,
     1            mb,ms,region
 1001 format(a8,5(1x,a2),1x,f4.1,f7.2,f8.2,f6.1,2f3.1,a24)     
      read(inut,1002)epsrc,bwst,bwrec,bwcoff,mwst,mwrec,mwcoff,
     1            dt,edt,clat,eclat,clon,eclon,cdepth,ecdepth
 1002 format(a3,2(4x,i2,i3,i4),4x,f6.1,f4.1,
     1       f7.2,f5.2,f8.2,f5.2,f6.1,f5.1)
      read(inut,1003)ahdur,expo,rmom(1),emrr,rmom(2),emss,rmom(3),emee,
     1            rmom(4),emrs,rmom(5),emre,rmom(6),emse
 1003 format(4x,f4.1,4x,i2,6(f6.2,f5.2))
      read(inut,1004)ev(1),evpl(1),evaz(1),ev(2),evpl(2),evaz(2),
     2              ev(3),evpl(3),evaz(3),smo,strike(1),dip(1),rake(1),
     4              strike(2),dip(2),rake(2)
 1004 format(3(f7.2,i3,i4),f7.2,2(i4,i3,i5)) 
      print *
      print *, 'Centroid moment tensor and coordinates:'
      print '(a,i3,6f7.3)', 'Moment tensor:', expo, (rmom(i),i=1,6)
      print '(a,3f8.3)', 'Centroid-Lat/Long/Depth:', clat,clon,cdepth

c-----------------------------------------------------------------
c Apply scale factor to the tensor elements and convert from Dyne |
c to Nm.                                                          |
c Correct time for centroid-delay.                                |
c-----------------------------------------------------------------

c      do i=1,6
c       rmom(i) = rmom(i)*(10.**expo)*dyne2Nm*Nm2myUnit
c      enddo
c
c seismogram units will be nm or nm/s
      M0=(10.**expo)*dyne2Nm*Nm2myUnit
c Mrr -> Mzz
      rmom(1)=rmom(1)
c Mss -> Mxx
      rmom(2)=rmom(2)
c Mee -> Myy
      rmom(3)=rmom(3)
c -Mrs -> Mzx
      rmom(4)=-rmom(4)
c Mre -> Mzy
      rmom(5)=rmom(5)
c -Mse -> Mxy
      rmom(6)=-rmom(6)
c
      read(hour,'(i2)') ihour
      read(min,'(i2)') inmin
      totsec = float(ihour)*3600. + float(inmin)*60. + sec + dt
      ihour = int(totsec/3600.)
      inmin = int(mod(totsec,3600.)/60.)
      sec = mod(totsec,60.)
      print *,'PDE time:', hour,min,sec
      print *,'CMT time:', ihour,inmin,sec
      write(time,'(2i2,f6.3)') ihour,inmin,sec
      date = year//month//day

      close(inut)
      return

999   print *,'File status=',ios
      pause 'Error in readCMT!!'
      end


c
c ----- END OF cmt2refmet.f ----- 
