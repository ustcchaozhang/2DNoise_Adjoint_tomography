c this is <sub/refmet_rrcv.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
c
c read receiver coordinates
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
c 28/01/1997   new select mode 3
c 15/02/1997   new select mode 4
c
c======================================================================
c 
c read receiver coordinates
c 
c receiver coordinates may be specified as follows:
c   select=1  give each epicetral distance and azimuth angle
c   select=2  give a range of distance and azimuth angle
c   select=3  give a range of distance in angles and azimuth angle
c   select=4  give each epicetral distance and azimuth angles
c
      subroutine refmet_rrcv(receiverfile, receivertext,
     &  Vred, Tli, Tre, NE, ME, r, phi, radius,
     &  cl_vlevel, lev2, cl_debug)
c 
      character receiverfile*(*), receivertext*(*)
      real*8 Vred, Tli, Tre, radius
      integer NE, ME
      real*8 r(ME), phi(ME)
      integer cl_vlevel, lev2
      logical cl_debug
c 
      integer select, e
      integer rcvlu, rcvlen
      parameter(rcvlu=11)
      real*8 pi
      parameter (pi=3.14159265358979d0)
c 
      rcvlen=index(receiverfile, ' ')-1
      if (cl_vlevel.gt.lev2) print 52,'opening ',receiverfile(1:rcvlen)
      open(rcvlu, file=receiverfile, status='old', err=99)
      read(rcvlu, '(a72)', err=98, end=97) receivertext
      read(rcvlu, '(/)', err=98, end=97)
      read(rcvlu, *, err=98, end=97) select, Vred, Tli, Tre, NE
      if (NE.gt.ME) then
        print *,'WARNING: too many receivers: ',NE
        print *,'WARNING: will take only: ',ME
        NE=ME
      endif
      read(rcvlu, '(/)', err=98, end=97)
      if (select.eq.1) then
        do E=1,NE
          read(rcvlu, *, err=98, end=97) r(E), phi(E)
        enddo
      elseif(select.eq.2) then
        read(rcvlu, *, err=98, end=97) r(1), r(NE), phi(1)
        do E=1,NE
          r(E)=r(1)+(r(NE)-r(1))*dble(E-1)/dble(NE-1)
          phi(E)=phi(1)
        enddo
      elseif(select.eq.3) then
        read(rcvlu, *, err=98, end=97) r(1), r(NE), phi(1)
        if (radius.lt.1.) then
          print *,'ERROR: receiver coordinates given for spherical model'
          stop 'ERROR: but using flat one'
        endif
        r(1)=r(1)*pi*radius/180.d0
        r(NE)=r(NE)*pi*radius/180.d0
        do E=1,NE
          r(E)=r(1)+(r(NE)-r(1))*dble(E-1)/dble(NE-1)
          phi(E)=phi(1)
        enddo
      elseif (select.eq.4) then
        do E=1,NE
          read(rcvlu, *, err=98, end=97) r(E), phi(E)
        enddo
        if (radius.lt.1.) then
          print *,'ERROR: receiver coordinates given for spherical model'
          stop 'ERROR: but using flat one'
        endif
        do E=1,NE
          r(E)=r(E)*pi*radius/180.d0
        enddo
      else
        print *,'ERROR: you did select coordinate setting mode:',select
        stop 'ERROR: coordinate setting mode is unknown'
      endif
      if (cl_vlevel.gt.lev2) print 52,'closing ',receiverfile(1:rcvlen)
      close(rcvlu, err=96)
      if (cl_debug) then
        print *,'DEBUG: receivers ', (r(E),E=1,NE)
        print *,'DEBUG: angles ', (phi(E),E=1,NE)
      endif
      return
   52 format(/a,1x,a)
   99 stop 'ERROR opening receiver file'
   98 stop 'ERROR reading receiver file'
   97 stop 'ERROR reading receiver file - unexpected end of file'
   96 stop 'ERROR closing receiver file'
      end
c----------------------------------------------------------------------
c 
c Information on receiver file structure
c
      subroutine refmet_rcvinf
c     
      print 50,' '
      print 50,'How to build a receiver configuration file'
      print 50,'=========================================='
      print 50,' '
      print 51,'line','contents'

      print 52,1,'text'
      print 53,'text','a72','any comment on receiver configuration'

      print 52,4,'mode, Vred, Tl, Tr, Nr'
      print 53,'mode','i','=1: specify each receiver individual'
      print 55,'=2: choose equidistant receivers in one directions by'
      print 55,'    a range given in km'
      print 55,'=3: choose equidistant receivers in one directions by'
      print 55,'    a range given in degress (only for spherical models)'
      print 55,'=4: like 1 but distances are given in degress in sphere'
      print 54,'Vred','f','traveltime reduction velocity in km/s'
      print 55,'(A value of 0 km/s means no reduction)'
      print 54,'Tl','f','time in seconds that seismomgram should start before'
      print 55,'reduced time (left adjust)'
      print 54,'Tr','f','time in seconds that seismomgram should start after'
      print 55,'reduced time (right adjust)'
      print 54,'Nr','i','number of receivers'
     

      print 50,' '
      print 50,'individually specified receivers:'
      print 52,4,'r, phi'
      print 53,'r','f','epicentral distance of receiver one (in km)'
      print 54,'phi','f','azimuth angle of receiver one in source'
      print 55,'coordinate system (in degrees)'
      print 50,'append one other line for each receiver up to number Nr'
      print 50,' '
      print 50,'equidistant receivers:'
      print 52,4,'rmin, rmax, phi'
      print 53,'rmin','f','epicentral distance of first receiver given'
      print 55,'in km or degrees (see mode selection)'
      print 54,'rmax','f','epicentral distance of last receiver given'
      print 55,'in km or degrees (see mode selection)'
      print 54,'phi','f','azimuth angle of all receivers in source'
      print 55,'coordinate system (in degrees)'
c 
      return
   50 format(1x,a)
   51 format(1x,a4,1x,a)
   52 format(/1x,i4,1x,a)
   53 format(/6x,a8,1x,1h(,a3,1h),1x,a)
   54 format(6x,a8,1x,1h(,a3,1h),1x,a)
   55 format(21x,a)
      end
 

c
c ----- END OF sub/refmet_rrcv.f ----- 
