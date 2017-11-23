c this is <sub/refmet_rmain.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
c
c read main control file
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
c 09/02/97   changed to be able to select a dominating period
c            for velocity dispersion
c 16/07/98   check SL to be smaller (!) than MSL
c
c======================================================================
c
c read main control file
c
      subroutine refmet_rmain(mainfile, modelfile, sourcefile, receiverfile,
     &  cl_vlevel, cl_debug, lev2, umin, uwil, uwir, umax, Nu, 
     &  fmin, fwil, fwir, fmax, Dt, SL, MSL, text)
c 
      character mainfile*(*), modelfile*(*), sourcefile*(*)
      character receiverfile*(*), text*(*)
      integer cl_vlevel, lev2
      logical cl_debug
      real*8 umin, uwil, uwir, umax
      real*8 fmin, fwil, fwir, fmax, Dt
      integer Nu, SL, MSL
c 
      real*8 length, exlen
      integer expo, mainlen, mainlu
      parameter(mainlu=10)
c 
      mainlen=index(mainfile,' ')-1
      if (cl_vlevel.gt.lev2) print 52,'opening ',mainfile(1:mainlen)
      open(mainlu, file=mainfile, status='old', err=99)
      read(mainlu, '(a70)', err=98, end=97) text
      if (cl_debug) print *,'DEBUG: ',text
      read(mainlu, '(//a80)', err=98, end=97) modelfile
      if (cl_debug) print *,'DEBUG: ',modelfile
      read(mainlu, '(//a80)', err=98, end=97) sourcefile
      if (cl_debug) print *,'DEBUG: ',sourcefile
      read(mainlu, '(//a80)', err=98, end=97) receiverfile
      if (cl_debug) print *,'DEBUG: ',receiverfile
      read(mainlu, '(/)', err=98, end=97)
      if (cl_debug) print *,'DEBUG: lines skipped'
      read(mainlu, *, err=98, end=97) umin,uwil,uwir,umax,Nu
      if (cl_debug) print *,'DEBUG: ',umin,uwil,uwir,umax,Nu
      read(mainlu, '(/)', err=98, end=97)
      if (cl_debug) print *,'DEBUG: lines skipped'
      read(mainlu, *, err=98, end=97) fmin,fwil,fwir,fmax,Dt,length
      if (cl_debug) print *,'DEBUG: ',fmin,fwil,fwir,fmax,Dt,length
      if (cl_vlevel.gt.lev2) print 52,'closing ',mainfile(1:mainlen)
      close(mainlu, err=96)
c----------------------------------------------------------------------
c check value of SL
      expo=0
    1 continue
        SL=(2**expo)
        exlen=dt*SL
        if (cl_debug) print *,'DEBUG: expo, SL, exlen, length ',
     &    expo,SL,exlen,length
        if (((SL*2).lt.MSL).and.(exlen.lt.length)) then
          expo=expo+1
          goto 1
        endif
      if (exlen.lt.length) then
        print *,'WARNING: we do not reach the required seismogram length'
        print *,'WARNING:        selected sampling interval: ',dt,'s'
        print *,'WARNING:        required seismogram length: ',length,'s'
        print *,'WARNING: maximum allowed number of samples: ',MSL
        print *,'WARNING:        achieved seismogram length: ',exlen,'s'
      endif
      
c----------------------------------------------------------------------
      return
   52 format(/a,1x,a)
   99 stop 'ERROR opening main file'
   98 stop 'ERROR reading main file'
   97 stop 'ERROR reading main file - unexpected end of file'
   96 stop 'ERROR closing main file'
      end

c----------------------------------------------------------------------
c 
c Information on main configuration file strcuture
c
      subroutine refmet_maininf
c 
      print 50,' '
      print 50,'How to build a main configuration file'
      print 50,'======================================'
      print 50,' '
      print 51,'line','contents'

      print 52,1,'text'
      print 53,'text','a72','any comment on main configuration'

      print 52,4,'model'
      print 53,'model','a80','name of file containing earth model'

      print 52,7,'source'
      print 53,'source','a80','name of file containing source configuration'

      print 52,10,'rcv'
      print 53,'rcv','a80','name of file containing receiver configuration'

      print 52,13,'umin, uwil, uwir, umax, Nu'
      print 53,'umin','f','minimum slowness in s/km'
      print 54,'uwil','f','left edge of slowness taper (in s/km)'
      print 54,'uwir','f','right edge of slowness taper (in s/km)'
      print 54,'umax','f','maximum slowness in s/km'
      print 54,'Nu','i','Number of slownesses to be calculated'

      print 52,16,'fmin, fwil, fwir, fmax, dt, T'
      print 53,'fmin','f','minimum frequency in Hz'
      print 54,'fwil','f','left edge of frequency taper (in Hz)'
      print 54,'fwir','f','right edge of frequency taper (in Hz)'
      print 54,'fmax','f','maximum frequency in Hz'
      print 54,'dt','f','seismogram sampling interval in seconds'
      print 54,'T','f','minimum length of seismogram in seconds'
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
c ----- END OF sub/refmet_rmain.f ----- 
