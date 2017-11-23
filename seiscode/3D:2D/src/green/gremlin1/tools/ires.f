c this is <ires.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2011 by Thomas Forbriger (IfG Stuttgart)
c
c calculate impuls response from complex response function
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
c    06/04/98   V1.0   Thomas Forbriger
c    29/05/00   V1.1   provide absolute time and offset coordinates
c    18/10/01   V1.2   allow handling of absolute amplitudes
c    20/10/01   V1.3   justified factor
c    09/01/03      -   just checked correctness
c
c==============================================================================
c
      program ires
c
      character*79 version
      parameter(version='IRES   V1.3   Impuls RESponse')
c
c SFF stuff
      character sffdate*10, sfftime*12, nspstring*80
      integer odate(7)
      parameter(nspstring='NSP')
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=3)
      character*3 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c 
      logical absolute
c
c data
      integer maxsamples, nsamples
      parameter(maxsamples=2050)
      complex spe(maxsamples)
      real fdata(maxsamples), cdata(maxsamples)
      integer idata(maxsamples)
      equivalence(idata, fdata)
      real omega(maxsamples), dom
c help
      real a,b,c, rueck, pi, dt, norm
      parameter(rueck=+1., pi=3.14159265358979)
      integer newnsamp, insamp, i, ierr
c files
      character*80 infile,outfile,fileformat
      character*132 wid2line
      integer lu
      parameter(lu=12)
c debugging
      logical debug
c here are the keys to our commandline options
      data optid/'-d','-a','-ty'/
      data opthasarg/2*.FALSE.,.true./
      data optarg/2*'-','sff'/
c
c------------------------------------------------------------------------------
c basic information
c
      print *,version
      print *,'Usage: ires infile outfile [-a] [-ty f]'
      print *,'   or: ires -help'
      print *,'   or: ires -xhelp'
c
      if (iargc().lt.1) stop 'ERROR: missing arguments'
      call getarg(1, argument)
      if (argument(1:6).eq.'-xhelp') then
        call sff_help_details
        stop
      else if (argument(1:5).eq.'-help') then
        print *,' '
        print *,'calculate impuls response from complex response function'
        print *,' '
        print *,'infile       input file written by gremlin'
        print *,'outfile      output file (SFF time series)'
        print *,' '
        print *,'-a           spectral coefficients are values of the'
        print *,'             Fourier-integral transform'
        print *,'             (if not used, time series will be normalized)'
        print *,'-ty f        write time series in file format f'
        print *,' '
        print *,'$Id$'
        print *,' '
        call sff_help_formats
        stop
      endif
      if (iargc().lt.2) stop 'ERROR: wrong number of arguments'
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(3, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      absolute=optset(2)
      fileformat=optarg(3)
c 
      call getarg(1,infile)
      call getarg(2,outfile)
c
c 
      if (absolute) then
        print *,'do NOT normalize'
      else
        print *,'do normalize'
      endif
c 
      call sff_select_output_format(fileformat, ierr)
      if (ierr.ne.0) stop 'ERROR: selecting output format'
c
c------------------------------------------------------------------------------
c go
c
c read data
      print *,'read input file ',infile(1:index(infile, ' '))
      open(lu, file=infile, err=99)
      read(lu, 50, err=98, end=97)
      nsamples=0
    1 read(lu, *, err=98, end=2) a,b,c
      nsamples=nsamples+1
      if (nsamples.gt.int(maxsamples/2)) stop 'ERROR: too many samples'
      omega(nsamples)=a
      spe(nsamples)=cmplx(b,c)
      goto 1
    2 continue
      close(lu, err=96)
      if (nsamples.eq.0) stop 'ERROR: no sample read'
c 
      print *,'read ',nsamples,' samples'
      print *,'minimum angular frequency: ',omega(1),' 1/s'
      print *,'maximum angular frequency: ',omega(nsamples),' 1/s'
      dom=(omega(nsamples)-omega(1))/float(nsamples-1)
      if ((omega(1)/dom).gt.1.e-10) 
     &  stop 'ERROR: first angular frequency schould be zero'
      print *,'angular frequency stepsize: ',dom
c 
c find number of samples for result
      insamp=1
      newnsamp=2**insamp
      do while (newnsamp.le.maxsamples)
        insamp=insamp+1
        newnsamp=2**insamp
      enddo
      newnsamp=newnsamp/2
      if ((2*nsamples).gt.newnsamp) stop 'ERROR: too many samples read'
      do i=(nsamples+1),newnsamp
        spe(i)=(0.,0.)
      enddo
      print *,'output will have ',newnsamp,' samples'
c 
      dt=2.*pi/(newnsamp*dom)
      print *,'time interval will be ',dt,' seconds'
      print *,'seismogram length will be ',newnsamp*dt,' seconds'
c 
c make symmetric
      do i=0,newnsamp/2-2
        spe(newnsamp-i)=conjg(spe(i+2))
      enddo
c 
c transform
      print *,'transform'
      call tf_fork(newnsamp, spe, rueck)
c
      norm=0
c extract time series
      do i=1,newnsamp
        fdata(i)=real(spe(i))
        norm=max(norm,abs(fdata(i)))
      enddo
c
c 09/01/2003: just checked correctness
c we divide by norm and 1/norm=1/(dt*sqrt(newnsamp))=sqrt(newnsamp)*df
c   = sqrt(newnsamp)*domega/(2*pi)
c is the correct scling factor for fork
      if (absolute) then
        norm=dt*sqrt(float(newnsamp))
      endif
c if not: explicitely norm to 1 to make clear that
c amplitude has no physical meaning
      print *,'normalize with ',norm
      do i=1,newnsamp
        fdata(i)=fdata(i)/norm
      enddo
      do i=1,newnsamp/2
        cdata(i)=fdata(i+newnsamp/2)
        cdata(i+newnsamp/2)=fdata(i)
      enddo
c
c build SFF header information
      call sffu_dttotime(newnsamp*dt/2., odate)
      odate(2)=1
      call time_finish(odate)
      call sff_PrepWid2(newnsamp, 1./dt, 'NSP   ', -1, -1, -1, -1, -1, 'NSP   ',
     &  'NSP   ', 'NSP   ', 0., -1., -1., -1., -1., wid2line, ierr)
      if (ierr.ne.0) stop 'ERROR: creating WID2 line'
      call sffu_setwid2time(wid2line, odate)
      call sffu_srcetime(odate, sffdate, sfftime)
c 
c write it
      print *,'write output file ',outfile(1:index(outfile, ' '))
      call sff_WOpenS(lu, outfile, nspstring, 'C', 0., 0., 0.,
     &                sffdate, sfftime, ierr)
      if (ierr.ne.0) stop 'ERROR: opening output file'
      call sff_WTraceI(lu, wid2line, newnsamp, fdata, idata, .false.,
     &                 'C', 1.,0.,0.,1,ierr)
      if (ierr.ne.0) stop 'ERROR: writing first trace'
      call time_clear(odate)
      odate(1)=2000
      odate(2)=1
      call time_finish(odate)
      call sffu_setwid2time(wid2line, odate)
      call sff_WTraceI(lu, wid2line, newnsamp, cdata, idata, .true., 
     &                 'C', 2.,0.,0.,1,ierr)
      if (ierr.ne.0) stop 'ERROR: writing second trace'
c
      stop
   99 stop 'ERROR opening input file'
   98 stop 'ERROR reading input file'
   97 stop 'ERROR reading input file - unexpected end'
   96 stop 'ERROR closing input file'
   95 stop 'ERROR opening output file'
   94 stop 'ERROR closing output file'
   50 format(//////)
      end
c
c ----- END OF ires.f -----
