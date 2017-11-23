c this is <fidase.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c FIt DAtaSets - make datasets homogeneous
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
c    09/07/98   V1.0   Thomas Forbriger
c    12/08/98   V1.1   improved help text
c    25/11/98   V1.2   now does resampling
c    27/02/99   V1.3   scale energies offset dependent
c    23/10/01   V1.4   destack option
c    06/12/02   V1.5   support flgevask inversion inv1d 
c    09/09/04   V1.6   time window
c    05/02/10   V1.7   explain trace selection
c    14/01/11   V1.8   use libfapidxx interface to select file format
c
c==============================================================================
c
      program fidase
c
      character*79 version
      parameter(version=
     &  'FIDASE   V1.8   FIt DAtaSets - make datasets homogeneous')
c
c common blocks
      include 'fidase_para.inc'
      include 'fidase_strings.inc'
      include 'fidase_dim.inc'
      include 'fidase_data.inc'
c options
      logical overwrite, opttaper, optenfit, optdestack
      logical optrenamestat, optwritesingle
      character*80 optfileformat
c 
      real enfminoff, enfmindelta, enfexpo
      logical optresamp
c 
      real newdt
      integer newsamppow, i, itrace
      character*5 newstationid
c filenames
      character*80 outfile, tapfile
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=13)
      character*3 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c here are the keys to our commandline options
      data optid/2h-D,2h-v,2h-o,2h-t,2h-e,2h-d,2h-r,2h-E,2h-s,2h-R,2h-S,
     &           2h-w,'-ty'/
      data opthasarg/3*.FALSE.,.TRUE.,4*.TRUE.,3*.FALSE.,2*.TRUE./
      data optarg/3*1h-,1h-,1h-,3hx11,1h-,2h1.,4*1h-,'sff'/
c
c------------------------------------------------------------------------------
c basic information
c
      argument=' '
c      print *,'iargc: ',iargc()
      if (iargc().eq.1) call getarg(1, argument)
c
      if (argument(1:6).eq.'-xhelp') then
        call sff_help_details
        stop
      elseif ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: fidase [-D] [-v] [-o] [-t file] [-e min,mind]'
        print *,'              [-d device] [-r n,dt] [-E exp] [-s]'
        print *,'              [-R] [-S] [-w l,r] [-ty f]'
        print *,'              file1 [t:list] file2 [t:list] ... target'
        print *,'   or: fidase -help'
        print *,'   or: fidase -xhelp'
        if (iargc().lt.1) stop 'ERROR: missing arguments'
        print *,' '
        print *,'FIt DAtaSets - make datasets homogeneous'
        print *,' '
        print *,'-D           Print debug information.'
        print *,'-v           Be verbose.'
        print *,'-o           Overwrite existing output file.'
        print *,'-ty f        select data file format f'
        print *,'-t file      Read quad taper from file.'
        print *,'-e min,mind  Fit amplitudes to smooth energy decay.'
        print *,'             Seismograms with receiver offset larger than'
        print *,'             ''min'' are used to determine scaling factors'
        print *,'             for the different datasets. Receivers that are'
        print *,'             closer to each other than ''mind'' will be used'
        print *,'             as one receiver location.'
        print *,'             Avoid conflicts by selecting ''mind'' small'
        print *,'             enough: Different traces of one file MUST'
        print *,'             be seperated!'
        print *,'-d device    chosse output device for energy fit'
        print *,'-r n,dt      resample time series to 2**n samples with'
        print *,'             sampling interval dt'
        print *,'-E exp       calculated energies will be scaled with'
        print *,'             offset**exp (default: ',optarg(8)(1:3),')'
        print *,'-s           destack traces'
        print *,'             samples values will be divided by the'
        print *,'             stack counter upon read and the stack'
        print *,'             counter will be reset to 1'
        print *,'-R           rename all station id to make them unique'
        print *,'             within the output file'
        print *,'-S           write each trace to a single file'
        print *,'-w l,r       extract time window from input traces'
        print *,'             l: time of first sample'
        print *,'             r: time of last sample'
        print *,'             times in seconds relative to source'
        print *,' '
        print *,'file [t:l]   input datasets'
        print *,'target       filename for homogeneus output dataset'
        print *,' '
        print *,'I recommend to give the trace with the largest decay'
        print *,'(which will be the trace closest to the source) as'
        print *,'the last one as it will be then used as the reference.'
        print *,'This prevents the algorithm from make second derivatives'
        print *,'small just be setting this trace to zero.'
        print *,' '
        print *,'Each input datafile name may be followed by a list of'
        print *,'traces. This list selects a range of traces in'
        print *,'the file which will be processed. This list may'
        print *,'contain no blank (which is the separator to the'
        print *,'next filname). The traces will always be processed'
        print *,'in the order they appear in the data file.'
        print *,' '
        print *,'Examples:'
        print *,'  t:2           will select only trace 2'
        print *,'  t:4-6,2,4     will select traces 2, 4, 5 and 6'
        print *,'  t:9,8,10,14   will select traces 8, 9, 10 and 14'
        print *,' '
        print *,'$Id$'
        print *,' '
        print *,'This version is compiled for:'
        print *,'number of traces: ',maxtraces
        print *,'total number of samples: ',maxsamples
        print *,'number of files: ',maxfiles
        call sff_help_formats
        stop
      endif
c
c keep version string
      string_version=version
c 
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      overwrite=optset(3)
      opttaper=optset(4)
      if (opttaper) tapfile=optarg(4)
      optenfit=optset(5)
      if (optenfit) read (optarg(5), *, err=99) enfminoff, enfmindelta
      pgdevice=optarg(6)
      optresamp=optset(7)
      if (optresamp) read (optarg(7), *, err=99) newsamppow,newdt
      read(optarg(8), *, err=99) enfexpo
      optdestack=optset(9)
      optrenamestat=optset(10)
      optwritesingle=optset(11)
      optwinset=optset(12)
      optfileformat=optarg(13)
      if (optwinset) read (optarg(12), *, err=99) optwinl,optwinr
c 
      if ((iargc()-lastarg).lt.2) stop 'ERROR: too few filenames'
c 
      call getarg(iargc(), outfile)
c
c------------------------------------------------------------------------------
c go
      call sff_select_format(optfileformat, i)
      if (i.ne.0) stop 'ERROR: selecting file format'
      call readdata(lastarg)
      if (debug) print *,'DEBUG: read ',ntraces,' traces'
c
      if (optdestack) call destack
c 
      if (optresamp) call resamp(newsamppow, newdt)
c 
c eval tapers
      if (opttaper) then
        call readtaper(tapfile)
        call evaltaper
      endif
c 
c scale amplitudes
      if (optenfit) call enfit(enfminoff, enfmindelta, enfexpo)
c
c rename stations
      if (optrenamestat) then
        if (verbose) print *,'rename stations:'
        i=firstinchain
        do itrace=1,ntraces
          write(newstationid, 50) itrace
          if (verbose) print *,'  ',station(i),' --> ',newstationid
          station(i)=newstationid
          i=chain(i)
        enddo
      endif
c
c write result
      call writedata(outfile, overwrite, optwritesingle)
c 
      stop
   50 format('B',i3.3,' ')
   99 stop 'ERROR: reading min,mind from arguments'
      end
c
c ----- END OF fidase.f -----
