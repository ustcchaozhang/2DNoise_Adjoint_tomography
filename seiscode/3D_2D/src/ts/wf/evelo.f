c this is <evelo.f>
c------------------------------------------------------------------------------
c
c 28/01/99 by Thomas Forbriger (IfG Stuttgart)
c
c calculate EnVELOpe of time series
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
c    28/01/99   V1.0   Thomas Forbriger
c    23/02/99   V1.1   there was a factor of two missing
c    28/02/99   V1.2   allow calculation of hilbert transform
c    01/03/99   V1.3   calculate spectral coefficients
c    25/06/01   V1.4   depart from GSE sampling rate standard formst if
c                      necessary
c    04/12/09   V1.5   corrected comment
c    05/07/14   V1.6   provide access to other data formats
c    14/02/17   V1.7   fix option default (set envelope to .false.)
c
c==============================================================================
c
      program evelo
c
      character*79 version
      parameter(version=
     &  'EVELO   V1.7   calculate EnVELOpe of time series')
c
      integer maxsamples,nsamp,nspa,i, npow, limit
      parameter(maxsamples=310000)
c 
      real fdata(maxsamples)
      real scalefact,pi
      integer idata(maxsamples)
      equivalence(fdata,idata)
      double complex ftarray(maxsamples)
      parameter(pi=3.14159265358979311)
c 
      character*80 infile
      character*80 outfile
      character*80 comment
c 
      integer luin, luout
      parameter(luin=10, luout=11)
c 
      logical last, hilbert, realspec, imagspec, ampspec, phasespec
      logical envelope, pi4shift
c 
      real tanf,dt
      real c1,c2,c3,readversion
      integer nstack,ierr
      character*1 cs
      character*14 timestamp
      character*10 code
      character*132 wid2line
      character*20 srctype
      character*6 srcdate
      character*10 srctime
c 
      character*80 inputformat, outputformat
c 
      integer maxfree, maxreadfree, nfree, lenmax
      parameter(maxfree=200, maxreadfree=maxfree-1)
      character*80 free(maxfree)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=11)
      character*3 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/'-d', '-v', '-H', '-R', '-I', '-A', '-P', '-4',
     &    '-ty', '-it', '-ot'/
      data opthasarg/8*.FALSE.,3*.TRUE./
      data optarg/8*'-',3*'sff'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.2)) then
        print *,version
        print *,'Usage: evelo infile outfile [-H|-R|-I|-A|-P|-4]'
        print *,'             [-ty f] [-it f] [-ot f]'
        print *,'   or: evelo -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'calculate EnVELOpe of time series'
        print *,' '
        print *,'-H           Calculate Hilbert transform of input'
        print *,'             signal rather than envelope.'
        print *,'-R           calculate real part of Fourier spectrum'
        print *,'-I           calculate imaginary part of Fourier spectrum'
        print *,'-A           calculate Fourier amplitude spectrum'
        print *,'-P           calculate Fourier phase spectrum'
        print *,'-4           phase shift by pi/4'
        print *,' '
        print *,'-ty f  choose file format ''f'' instead of SFF'
        print *,'       for input and output files'
        print *,'-it f  choose input file format ''f'' instead of SFF'
        print *,'-ot f  choose output file format ''f'' instead of SFF'
        print *,' '
        print *,'The switches are exclusive with priority in the order'
        print *,'listed above.'
        print *,' '
        print *,'In case of Fourier output one second means one Hertz.'
        print *,' '
        print *,'The output Fourier spectrum is normalized to be'
        print *,'equivalent to the corresponding integral transform.'
        print *,'Thus Fourier coeffients and spectral amplitudes are'
        print *,'given in [amplitude]/Hz, where [amplitude] is'
        print *,'the unit of the input samples.'
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
      hilbert=optset(3)
c 
      envelope=.false.
      realspec=.false.
      imagspec=.false.
      ampspec=.false.
      phasespec=.false.
      pi4shift=.false.
c 
      if (.not.(hilbert)) then
        realspec=optset(4)
        if (.not.(realspec)) then
          imagspec=optset(5)
          if (.not.(imagspec)) then
            ampspec=optset(6)
            if (.not.(ampspec)) then
              phasespec=optset(7)
              if (.not.(phasespec)) then
                pi4shift=optset(8)
                if (.not.(pi4shift)) envelope=.true.
              endif
            endif
          endif
        endif
      endif
      inputformat=optarg(9)
      outputformat=optarg(9)
      if (optset(10)) inputformat=optarg(10)
      if (optset(11)) outputformat=optarg(11)
c 
c 
      call getarg(1, infile)
      call getarg(2, outfile)
c
      call sff_select_input_format(inputformat, ierr)
      if (ierr.ne.0) stop 'ERROR: selected input format is not supported'
      call sff_select_output_format(outputformat, ierr)
      if (ierr.ne.0) stop 'ERROR: selected output format is not supported'
c 
c------------------------------------------------------------------------------
c go
c 
      call sff_ROpenFS(luin, infile, readversion, timestamp, code, 
     &  nfree, free, lenmax, maxreadfree,
     &  srctype, cs, c1, c2, c3, srcdate, srctime, ierr)
      if (ierr.ne.0) stop 'ERROR: opening input file'
c 
      nfree=min(nfree+1, maxfree)
      free(nfree)=version
      comment='ERROR in algorithm selection code'
      if (envelope) comment='calculate envelope'
      if (hilbert) comment='calculate Hilbert transform'
      if (pi4shift) comment='shift by pi/4'
      if (realspec) comment='calculate real part of Fourier spectrum'
      if (imagspec) comment=
     &   'calculate imaginary part of Fourier spectrum'
      if (ampspec) comment='calculate Fourier amplitude spectrum'
      if (phasespec) comment='calculate Fourier phase spectrum'
      print *,comment
      if (comment(1:6).eq.'ERROR ') stop
      nfree=min(nfree+1, maxfree)
      free(nfree)=comment

      if (envelope)  print *,'envelop option (default) is selected'
      if (hilbert)   print *,'hilbert option -H is selected'
      if (pi4shift)  print *,'pi4shift option -4 is selected'
      if (realspec)  print *,'realspec option -R is selected'
      if (imagspec)  print *,'imagspec option -I is selected'
      if (ampspec)   print *,'ampspec option -A is selected'
      if (phasespec) print *,'phasespec option -P is selected'
c 
      call sff_WOpenFS(luout, outfile,
     &  free, nfree, srctype, cs, c1, c2, c3, srcdate, srctime, ierr)
      if (ierr.ne.0) stop 'ERROR: opening output file'
c 
      last=.false.
      do while(.not.(last))
        nsamp=maxsamples
        call sff_RTraceFI(luin, tanf, dt,
     &    wid2line, nsamp, fdata, idata, code, last,
     &    nfree, free, maxreadfree, lenmax,
     &    cs, c1, c2, c3, nstack, ierr)
        if (ierr.ne.0) stop 'ERROR: reading input file'
c 
        nfree=min(nfree+1, maxfree)
        free(nfree)=version
        nfree=min(nfree+1, maxfree)
        free(nfree)=comment
c 
        npow=0
        nspa=2**npow
        do while (nspa.lt.nsamp)
          npow=npow+1
          nspa=2**npow
        enddo
        if (nspa.gt.maxsamples) stop 'ERROR: too many samples'
c 
        do i=1,nspa
          ftarray(i)=(0.d0,0.d0)
          if (i.le.nsamp) ftarray(i)=cmplx(fdata(i), 0.)
        enddo
c 
        call tf_dfork(nspa, ftarray, -1.d0)
c 
        if (hilbert) then
          limit=nspa/2+1
          do i=2,limit
            ftarray(i)=(0.d0,1.d0)*ftarray(i)
          enddo
          ftarray(limit)=(0.d0,0.d0)
          do i=limit+2,nspa
            ftarray(i)=(0.d0,-1.d0)*ftarray(i)
          enddo
        elseif (pi4shift) then
          limit=nspa/2+1
          do i=2,limit
            ftarray(i)=sqrt(1/2.)*(1.d0,1.d0)*ftarray(i)
          enddo
          ftarray(limit)=(0.d0,0.d0)
          do i=limit+2,nspa
            ftarray(i)=sqrt(1/2.)*(1.d0,-1.d0)*ftarray(i)
          enddo
        elseif (envelope) then
          limit=nspa/2+1
          do i=limit+1,nspa
            ftarray(i)=(0.d0,0.d0)
          enddo
        endif
c 
        if ((envelope).or.(hilbert).or.(pi4shift)) 
     &    call tf_dfork(nspa, ftarray, 1.d0)
c 
        scalefact=dt*sqrt(float(nspa))
        if (hilbert.or.pi4shift) then
          do i=1,nsamp
            fdata(i)=sngl(real(ftarray(i)))
          enddo
        elseif (envelope) then
          do i=1,nsamp
            fdata(i)=sngl(2.d0*abs(ftarray(i)))
          enddo
        elseif (realspec) then
          nsamp=nspa
          do i=1,nsamp
            fdata(i)=sngl(scalefact*real(ftarray(i)))
          enddo
        elseif (imagspec) then
          nsamp=nspa
          do i=1,nsamp
            fdata(i)=sngl(scalefact*imag(ftarray(i)))
          enddo
        elseif (ampspec) then
          nsamp=nspa
          do i=1,nsamp
            fdata(i)=sngl(scalefact*abs(ftarray(i)))
          enddo
        elseif (phasespec) then
          nsamp=nspa
          do i=1,nsamp
            fdata(i)=sngl(atan2(imag(ftarray(i)),real(ftarray(i))))*
     &               180./pi
          enddo
        else
          stop 'ERROR in selection code'
        endif
c 
        if ((phasespec).or.(ampspec).or.(realspec).or.(imagspec)) then
          call sff_ModWid2samps(wid2line,nsamp)
          if ((nsamp*dt).lt.1.e3) then
            call sff_ModWid2samprat(wid2line, (nsamp*dt))
          else
            write(wid2line(58:68), '(e11.6)') (nsamp*dt)
          endif
        endif
c 
        call sff_WTraceFI(luout, 
     &    wid2line, nsamp, fdata, idata, last, 
     &    nfree, free, cs, c1, c2, c3, nstack, ierr)
        if (ierr.ne.0) stop 'ERROR: writing output file'
      enddo
c
      stop
      end
c
c ----- END OF evelo.f -----
