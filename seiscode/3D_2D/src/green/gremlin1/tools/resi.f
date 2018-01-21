c this is <resi.f>
c------------------------------------------------------------------------------
c   ($Id$)
c
c Copyright 2001, 2011 by Thomas Forbriger (IMGF Frankfurt)
c
c the inverse operation to ires
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
c    02/11/2001   V1.0   Thomas Forbriger
c
c==============================================================================
c
      program resi
c
      character*(*) version
      parameter(version='RESI   V1.0   the inverse operation to ires')
      character*(*) RESI_CVS_ID
      parameter(RESI_CVS_ID='$Id$')
c
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=4)
      character*3 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c
      character*80 infile,outfile,fileformat
      integer maxsamples
      parameter(maxsamples=100000)
      integer nsamples,insamples,ntraces,nom,iom
      real data(maxsamples), realpart,imagpart,dom
      real toffset,roffset,dt
      integer idata(maxsamples), firstsample, lu,i
      equivalence(data,idata)
      double complex cdata(maxsamples)
      parameter(lu=10)
      double precision pi,tofourier
      parameter(pi=3.1415926535897931159979)
      parameter(tofourier=-1.)
c 
      logical overwrite
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-o,'-ty'/
      data opthasarg/3*.FALSE.,.true./
      data optarg/3*1h-,'sff'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:6).eq.'-xhelp')) then
        call sff_help_details
        stop
      else if ((argument(1:5).eq.'-help').or.(iargc().lt.2)) then
        print *,version
        print *,'Usage: resi infile outfile -o -v'
        print *,'   or: resi -help'
        print *,'   or: resi -xhelp'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'the inverse operation to ires'
        print *,' '
        print *,'infile       input waveform (SFF)'
        print *,'outfile      output response spectrum (gremlin format)'
        print *,' '
        print *,'-v           be verbose'
        print *,'-o           overwrite'
        print *,'-ty f        read time series from file format f'
        print *,' '
        print *,RESI_CVS_ID
        print *,' '
        call sff_help_formats
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(3, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      overwrite=optset(3)
      fileformat=optarg(4)
c 
      call getarg(1, infile)
      call getarg(2, outfile)
c
c------------------------------------------------------------------------------
c go
      call sff_select_input_format(fileformat)
c 
      call sffu_simpleread(lu, infile, 1, maxsamples, 
     &  data, idata, toffset, dt, roffset, insamples, firstsample, ntraces,
     &  verbose)
c
      nsamples=1
      do while (nsamples.lt.insamples)
        nsamples=nsamples*2
      enddo
c 
      do i=1,nsamples
        if (i.gt.insamples) then
          cdata(i)=(0.d0,0.d0)
        else
          cdata(i)=data(i)*sqrt(float(nsamples))*dt
        endif
      enddo
c 
      if (verbose) then
        print 50,'number of samples: ',nsamples,' '
        print 51,'length of time series: ',nsamples*dt,'s'
        print 51,'sampling interval: ',dt,'s'
        print 51,'Nyquist frequency: ',0.5/dt,'Hz'
        print 51,'frequency interval: ',1./(nsamples*dt),'Hz'
      endif
      dom=2.*pi/(nsamples*dt)
      nom=nsamples/2
c 
      call tf_dfork(nsamples,cdata,tofourier)
c 
c write to disk now
      if (verbose) print *, 'opening file',outfile(1:index(outfile,' '))
      if (overwrite) then
        open(lu, file=outfile, err=99)
      else
        open(lu, file=outfile, status='new', err=99)
      endif
      write(lu, '(a)', err=98) VERSION
      write(lu, '(a)', err=98) 'columns below row number 7:'
      write(lu, '(a)', err=98) '  1: angular frequency (1/s)'
      write(lu, '(a)', err=98) '  2: real part'
      write(lu, '(a)', err=98) '  3: imaginary part'
      write(lu, '(f10.3,a)', err=98) dom, ' angular frequency step width'
      write(lu, '(i5,a)', err=98) nom, ' number of frequency samples'
      do iom=1,nom
        realpart=real(cdata(iom))
        imagpart=imag(cdata(iom))
        write(lu, '(3g15.7)', err=98) (iom-1)*dom, realpart, imagpart
      enddo
      close(lu, err=97)
      if (verbose) print *, 'file written and closed'

      stop
   50 format(a30,1x,i10,1x,a)
   51 format(a30,1x,f10.5,1x,a)
   99 stop 'ERROR: opening output file'
   98 stop 'ERROR: writing output file'
   97 stop 'ERROR: closing output file'
      end
c
c ----- END OF resi.f -----
