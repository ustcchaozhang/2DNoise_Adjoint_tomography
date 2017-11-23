c this is <hivexpanco.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
c
c calculate H/V for Fourier Bessel expansion coefficients
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
c    11/05/2007   V1.0   Thomas Forbriger
c
c ============================================================================
c
      program hivexpanco
c
      character*(*) version
      parameter(version=
     &  'HIVEXPANCO   V1.0'//
     &  '   calculate H/V for Fourier Bessel expansion coefficients')
c
c datasets
      character*80 hinfile, vinfile, outfile
      integer lu
      parameter(lu=12)
c magic number for binary file identification
      integer magic
      character*4 cmagic
      parameter(cmagic='1234')
c greens function
      integer maxp,maxom
      parameter(maxp=3000,maxom=maxp)
      complex hgreen(maxp, maxom)
      complex vgreen(maxp, maxom)
      complex hvgreen(maxp, maxom)
      real htime(maxp), hom(maxom)
      real vtime(maxp), vom(maxom)
      real hivtime(maxp), hivom(maxom)
c 
c processing
      integer hnp,hnom
      integer vnp,vnom
      integer hivnp,hivnom
      logical overwrite, waterlevel
      real water, rms
c
      integer i,j
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=4)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-o, 2h-w/
      data opthasarg/3*.FALSE.,.true./
      data optarg/3*1h-,2h1./
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.3)) then
        print *,version
        print *,'Usage: hivexpanco [-v] [-o] [-w val]'
        print *,'                  Hinfile Vinfile outfile'
        print *,'   or: hivexpanco -help'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'Hinfile      horizontal component input file from syg'
        print *,'Vinfile      vertical component input file from syg'
        print *,'outfile      output file name'
        print *,' '
        print *,'The program calculates the complex H/V ratio of for'
        print *,'Fourier Bessel coefficients as computed by sug.'
        print *,'All files are in the format produced by syg.f'
        print *,' '
        print *,'Results can be displayed by grepg'
        print *,' '
        print *,'-v     be verbose'
        print *,'-o     overwrite existing output file'
        print *,'-w val water level for denominator as a fraction'
        print *,'       of signal rms for a given time'
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      overwrite=optset(3)
      waterlevel=optset(4)
      read (optarg(4), *) water
      if (iargc().lt.(lastarg+3)) stop 'ERROR: missing filename!'
      call getarg(lastarg+1, hinfile)
      call getarg(lastarg+2, vinfile)
      call getarg(lastarg+3, outfile)
c
c------------------------------------------------------------------------------
c go
      call greenread(hinfile, verbose, maxp, maxom,
     &  htime, hom, hnp, hnom, hgreen)
      call greenread(vinfile, verbose, maxp, maxom,
     &  vtime, vom, vnp, vnom, vgreen)
c 
c check consistency
      if (vnp.ne.hnp)
     &  stop 'ERROR: inconsistent number of samples!'
      hivnp=hnp
      if (vnom.ne.hnom)
     &  stop 'ERROR: inconsistent number of samples!'
      hivnom=hnom
      do i=1,hnom
        if (hom(i).lt.1.e-20) then
          if (abs(hom(i)-vom(i)).gt.1.e-20)
     &      stop 'ERROR: inconsistent frequencies!'
        else
          if (abs(1.-(hom(i)/vom(i))).gt.1.e-5)
     &      stop 'ERROR: inconsistent frequencies!'
        endif
        hivom(i)=hom(i)
      enddo
      do i=1,hnp
        if (htime(i).lt.1.e-20) then
          if (abs(htime(i)-vtime(i)).gt.1.e-20)
     &      stop 'ERROR: inconsistent frequencies!'
        else
          if (abs(1.-(htime(i)/vtime(i))).gt.1.e-5)
     &      stop 'ERROR: inconsistent frequencies!'
        endif
        hivtime(i)=htime(i)
      enddo
c 
c calculate ratio
      if (verbose.and.waterlevel)
     &  print *,'use waterlevel of ',water,' *rms'
      do j=1,hivnp
        if (waterlevel) then
          rms=0.
          do i=1,hivnom
            rms=rms+abs(hgreen(j,i)*conjg(hgreen(j,i)))
            rms=rms+abs(vgreen(j,i)*conjg(vgreen(j,i)))
          enddo
          rms=sqrt(rms)/(2.*hivnom)
          do i=1,hivnom
            hvgreen(j,i)=hgreen(j,i)/(vgreen(j,i)+water*rms)
          enddo
        else
          do i=1,hivnom
            hvgreen(j,i)=hgreen(j,i)/vgreen(j,i)
          enddo
        endif
      enddo
c 
c write green file (easy to use)
c 
      if (overwrite) then
        if (verbose) print *,'opening green file ',
     &    outfile(1:index(outfile,' ')),' - overwrite mode'
        open(lu, file=outfile, form='unformatted', err=98)
      else
        if (verbose) print *,'opening green file ',
     &    outfile(1:index(outfile,' '))
        open(lu, file=outfile, status='new', form='unformatted', err=98)
      endif
      call tf_magic(cmagic, magic)
      write(lu, err=97) magic
      write(lu, err=97) hivnom, hivnp
      write(lu, err=97) (hivom(i), i=1,hivnom), 
     &                  (hivtime(i), i=1,hivnp)
      write(lu, err=97) ((hvgreen(j,i), i=1,hivnom), j=1,hivnp)
      close(lu, err=96)
c
      stop
   98 stop 'ERROR: opening green file'
   97 stop 'ERROR: writing green file'
   96 stop 'ERROR: closing green file'
      end
c
c======================================================================
c 
c this routine reads in the data
c 
c 01/07/99   allow taper files
c
      subroutine greenread(filename, verbose,
     &     maxp, maxfreq, slo, om,
     &     np, nom, green)
c
      character filename*(*)
      logical verbose
      integer maxp, maxfreq
      complex green(maxp, maxfreq)
      real om(maxfreq), slo(maxp)
      integer np, nom
      integer inmagic, cpu, match
      equivalence(incmagic, inmagic)
      integer lu
      parameter(lu=20)
      integer i,j
      character*4 incmagic, cmagic
      parameter(cmagic='1234')
      real pi2
      parameter(pi2=2.*3.141592653589)
c 
      if (verbose) print *,'read green file ',
     &  filename(1:index(filename, ' '))
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
        print *,'magic number read (',incmagic,
     &    ') does not indicate green file!'
        stop 'ERROR: wrong file type!'
      endif
c 
      read(lu, err=98, end=97) nom, np
      if ((nom.gt.maxfreq).or.(np.gt.maxp)) then
        close(lu, err=96)
        stop 'ERROR: data exceeds array bounds'
      endif
      read(lu, err=98, end=97) (om(i), i=1,nom), (slo(i), i=1,np)
      read(lu, err=98, end=97) ((green(j,i), i=1,nom), j=1,np)
      close(lu, err=96)
      print *,'green file read and closed'
      return
   99 stop 'ERROR: opening green file'
   98 stop 'ERROR: reading green file'
   97 stop 'ERROR: reading green file - unexpected end'
   96 stop 'ERROR: closing green file'
      end

c ----- END OF hivexpanco.f ----- 
