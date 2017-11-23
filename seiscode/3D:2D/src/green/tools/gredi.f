c this is <gredi.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c calculate difference between two green files
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
c    03/11/1997   V1.0   Thomas Forbriger
c    29/05/2002   V1.1   check of relative change
c
c ============================================================================
c
      program gredi
c
      character*(*) version
      parameter(version=
     &  'GREDI   V1.1   calculate difference between two green files')
c
c 
      integer maxslo, maxfreq, i, j
      parameter (maxslo=300, maxfreq=300)
      complex green1(maxfreq, maxslo), divis, diff
      real om1(maxfreq), slo1(maxslo)
      complex green2(maxfreq, maxslo)
      real om2(maxfreq), slo2(maxslo)
      integer nom1, nom2, nslo1, nslo2
      character*80 infile1, infile2, outfile
      double precision rms1, rms2, rms3, max1, max2, max3
c magic number for binary file identification
      integer magic
      character*4 cmagic
      parameter(cmagic='1234')
c file
      integer lu
      parameter(lu=20)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=3)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose, relative
c here are the keys to our commandline options
      data optid/'-d', '-v', '-r'/
      data opthasarg/3*.FALSE./
      data optarg/3*'-'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.3)) then
        print *,version
        print *,'Usage: gredi file1 file2 diff [-r]'
        print *,'   or: gredi -help'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'file1        green input file 1'
        print *,'file2        green input file 2'
        print *,'diff         file1-file2 (green output)'
        print *,' '
        print *,'-r           calculate relative vartiation'
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(4, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      relative=optset(3)

      call getarg(1, infile1)
      call getarg(2, infile2)
      call getarg(3, outfile)
c
c------------------------------------------------------------------------------
c go
c
c 

      print *,' '
      call greenread(infile1, 
     &     maxslo, maxfreq, slo1, om1,
     &     nslo1, nom1, green1)
      print *,' '
      call greenread(infile2, 
     &     maxslo, maxfreq, slo2, om2,
     &     nslo2, nom2, green2)

      if (nslo1.ne.nslo2) stop 'ERROR: different number of slownesses'
      if (nom1.ne.nom2) stop 'ERROR: different number of frequencies'

      if (abs((om1(1)-om2(1))/om1(1)).gt.0.01) 
     &   stop 'ERROR: frequency values differ more than 1%'
      if (abs((om1(nom1)-om2(nom1))/om1(nom1)).gt.0.01) 
     &   stop 'ERROR: frequency values differ more than 1%'
      if (abs((slo1(1)-slo2(1))/slo1(1)).gt.0.01) 
     &   stop 'ERROR: frequency values differ more than 1%'
      if (abs((slo1(nslo1)-slo2(nslo1))/slo1(nslo1)).gt.0.01) 
     &   stop 'ERROR: frequency values differ more than 1%'

      if (relative) then
        print *,' '
        print *,'calculating relative change'
      endif
      rms1=0.d0
      rms2=0.d0
      rms3=0.d0
      max1=0.d0
      max2=0.d0
      max3=0.d0
      do i=1,nslo1
        do j=1,nom1
          if (j.ne.1) then
            rms1=dble(green1(j,i)*conjg(green1(j,i))+rms1)
            rms2=dble(green2(j,i)*conjg(green2(j,i))+rms2)
            max1=max(max1,abs(green1(j,i)))
            max2=max(max2,abs(green2(j,i)))
          endif
          if (relative) then
            divis=green1(j,i)
            diff=green1(j,i)-green2(j,i)
            if (abs(divis).le.(1.e-30)) divis=1.e-30
            green1(j,i)=diff/divis
          else
            green1(j,i)=green1(j,i)-green2(j,i)
          endif
          if (j.ne.1) then
            rms3=dble(green1(j,i)*conjg(green1(j,i))+rms3)
            max3=max(max3,abs(green1(j,i)))
          endif
        enddo
      enddo
      rms1=rms1/(nom1*nslo1)
      rms2=rms2/(nom1*nslo1)
      rms3=rms3/(nom1*nslo1)
      rms1=sqrt(rms1)
      rms2=sqrt(rms2)
      rms3=sqrt(rms3)
      print *,' '
      print *,infile1(1:index(infile1,' '))
      print 50,'  rms: ',rms1
      print 50,'  max: ',max1
      print *,infile2(1:index(infile2,' '))
      print 50,'  rms: ',rms2
      print 50,'  max: ',max2
      print *,outfile(1:index(outfile,' '))
      print 50,'  rms: ',rms3
      print 50,'  max: ',max3
      if (.not.(relative)) then
        print 50,'  relative change rms: ',rms3/rms1
        print 50,'  relative change max: ',max3/max1
      endif
      print *,' '

c write green code (easy to use)
c 
      print *,' '
      print *,'opening green file ',outfile(1:index(outfile,' ')),
     &    ' - overwrite mode'
      open(lu, file=outfile, form='unformatted', err=98)
      call tf_magic(cmagic, magic)
      write(lu, err=97) magic
      write(lu, err=97) nom1, nslo1
      write(lu, err=97) (om1(i), i=1,nom1), (slo1(i), i=1,nslo1)
      write(lu, err=97) ((green1(i,j), i=1,nom1), j=1,nslo1)
      close(lu, err=96)
c 
      stop
   50 format(a,e13.6)
   98 stop 'ERROR: opening green file'
   97 stop 'ERROR: writing green file'
   96 stop 'ERROR: closing green file'
      end
c 
c======================================================================
c 
c some subroutines
c
c----------------------------------------------------------------------
c 
c this routine reads in the data
c
      subroutine greenread(filename, 
     &     maxslo, maxfreq, slo, om,
     &     nslo, nom, green)
c
      character filename*(*)
      integer maxslo, maxfreq
      complex green(maxfreq, maxslo)
      real om(maxfreq), slo(maxslo)
      integer nslo, nom
      integer inmagic, cpu, match
      character*4 cmagic, incmagic
      parameter(cmagic='1234')
      equivalence(incmagic, inmagic)
      integer lu
      parameter(lu=20)
      integer i,j
c 
      print *,'read green file ',filename(1:index(filename, ' '))
      open(lu, file=filename, form='unformatted', status='old', err=99)

c check byte sex
      read(lu, err=98, end=97) inmagic
      call tf_bytesex(cmagic, inmagic, cpu, match)
      if (cpu.eq.1) then
        print *,'running on Intel...'
      elseif (cpu.eq.2) then
        print *,'running on Motorola...'
      else
        stop 'ERROR: unknown processor type...'
      endif
      if (match.eq.1) then
        print *,'matching bytesex - good...'
      elseif (match.eq.2) then 
        print *,'bytesex not matching - we will have to swap!'
        stop 'ERROR: do not know how to do that...'
      else
        close(lu, err=96)
        print *,'bytesex read is ',incmagic
        stop 'ERROR: bytesex is unkown - oh oh...'
      endif
      read(lu, err=98, end=97) nom, nslo
      if ((nom.gt.maxfreq).or.(nslo.gt.maxslo)) then
        close(lu, err=96)
        stop 'ERROR: data exceeds array bounds'
      endif
      read(lu, err=98, end=97) (om(i), i=1,nom), (slo(i), i=1,nslo)
      read(lu, err=98, end=97) ((green(i,j), i=1,nom), j=1,nslo)
      close(lu, err=96)
      print *,'green file read and closed'

      return
   99 stop 'ERROR: opening green file'
   98 stop 'ERROR: reading green file'
   97 stop 'ERROR: reading green file - unexpected end'
   96 stop 'ERROR: closing green file'
      end
       
c
c ----- END OF gredi.f -----
