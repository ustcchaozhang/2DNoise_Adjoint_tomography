c this is <sub/resus_basinf.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
c
c usage information for resus
c
c ============================================================================
c
c this is part of the RESUS reflectivity program
c (for comments and revisions see also the main source resus.f)
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
c 10/02/97   included information on component selection
c 10/06/97   copied and changed from refmet_basinf.f to serve special
c            needs of resus.
c 22/12/98   info about option -i
c
c======================================================================
c 
c give basic information
c 
      subroutine resus_basinf(version, mainfile, lev1, lev2, lev3, lev4,
     &  ME, MS, MSL, Mf, sff_maxfree, hfktstr)
c 
      character version*(*), mainfile*(*), hfktstr*(*)
      integer lev1, lev2, lev3, lev4
      integer ME, MSL, MS, Mf, sff_maxfree
c 
      integer iargc
c 
      print *, version
      print *, 'Usage: resus [-d] [-v level] [-o basename] [-c]'
      print *, '              [-s select] [-p] [-i] -m file file'
      print *,'              [-l n,f,o[,f,o,...]] [-h n,f,o,[,f,o,...]]'
      print *, '   or: resus -help'
      if (iargc().lt.1) stop 'ERROR: missing parameters'
      call getarg(1, mainfile)
      if (mainfile.eq.'-help') then
        call refmet_intro
        print *,' '
        print *,' NOTICE: This is not refmet this is RESUS.'
        print *,' NOTICE: This will need a precalculated response'
        print *,'         matrix to serve you with seismograms!'
        print *,'         (use refmat to calculate a response matrix)'
        print *,' '
        print *,'commandline parameters are:'
        print *,' '
        print *,'-d           Give debugging output.'
        print *,'-v level     Set verbosity level. The parameter may be'
        print *,'             any integer value. The higher the value'
        print *,'             the more output will be produced.'
        print *,'-o basename  Define output files basename.'
        print *,'             (default is: refmet.out)'
        print *,'-c           Output will be sortet one component per file'
        print *,'             instead of one receiver per file.'
        print *,'-s select    This option selects components that should not'
        print *,'             be written to the output files. The string select'
        print *,'             is any set of two-character combinations that'
        print *,'             specify components: TZ, TR, TT, FZ, FR, FT,'
        print *,'             NZ, NR and NT are allowed. T stands for total'
        print *,'             field, F for far field and N for near field.'
        print *,'             Z means vertical component, R radial component'
        print *,'             and T transverse component.'
        print *,'             In the case of a vertical single force select'
        print *,'             is set to TTFZFRFTNZNRNT by default, which'
        print *,'             disables the transverse component and all'
        print *,'             near field and far field output.'
        print *,'-m file      Name of matrix file created by refmat.' 
        print *,'-p           Introduce a polar phase shift by the heuristic'
        print *,'             trick to apply a hilbert transform to all'
        print *,'             waveforms behind the antipode. In addition the'
        print *,'             coordinate system for seismograms behind the'
        print *,'             antipode point is rotated by 180° in the'
        print *,'             horizontal plane.'
        print *,'-i           Invert sign of all horizontal coordinates'
        print *,'             behind 180°, but do not apply polar phase'
        print *,'             shift.'
        print *,'-l n,f,o[,f,o,...] Define lowpass Butterworth filters.'
        print *,'             The output signal will be filtered with n'
        print *,'             stages of butterworth filters. Each stage has'
        print *,'             to be defined by an eigenfrequency f (Hz) and'
        print *,'             its order o.'
        print *,'-h n,f,o[,f,o,...] Define highpass Butterworth filters.'
        print *,'             The output signal will be filtered with n'
        print *,'             stages of butterworth filters. Each stage has'
        print *,'             to be defined by an eigenfrequency f (Hz) and'
        print *,'             its order o.'
        print *,' '
        print *,'file         Is the name of the main configuration file.'
        print *,'             It contains the names of the three file'
        print *,'             containing the earth model, the source model'
        print *,'             and the receiver coordinates. In addition there'
        print *,'             must be given some numerical parameters for the'
        print *,'             calculation.'
        print *,' '
        print *,'This programs uses a precalculated response matrix (see '
        print *,'refmat) for layered media to build synthetic seismograms. '
        print *,'Therefore we do not use a modelfile. You must provide this '
        print *,'program with a main configuration file which must set '
        print *,'a source configuration and a receiver configuration file. '
        print *,' '
        print *,'The following parameters give in your configuration will '
        print *,'be overwritten by the values stored in the response matrix '
        print *,'file: '
        print *,' '
        print *,' - the sampling interval '
        print *,' - the seismogram length '
        print *,' - the minimum and maximum frequency '
        print *,' - the minimum and maximum slowness '
        print *,' - the number of slowness steps'
        print *,' - the source depth'
        print *,' - the earth radius'
        print *,' - and (as you did expect) all earth model parameters'
        print *,' '
        print *,'You have still got the freedom to apply any taper '
        print *,'in the frequency and slowness domain. '
        print *,' '
        call refmet_comments
        print *,' '
        print *,'Verbosity levels are: '
        print *,'  =',lev1,'    no output'
        print *,'  >',lev1,'    report basic configuration'
        print *,'  >',lev2,'    report reading and writing files'
        print *,'  >',lev3,'    model, receivers and source are reported'
        print *,'  >',lev4,'    report on results'
        print *,'The same values but with negative sign will cause a'
        print *,'report on calculation progress.'
        print *,' '
        print *,'Array dimensions compiled into this version:'
        print *,'    maximum number of receivers: ',ME
        print *,'      maximum number of samples: ',MSL
        print *,'  maximum number of frequnecies: ',Mf
        print *,'   maximum number of FREE lines: ',sff_maxfree
c call other info routine
        call refmet_maininf
        call refmet_sourinf(hfktstr)
        call refmet_rcvinf
        stop
      endif
      return
      end
c
c ----- END OF sub/resus_basinf.f ----- 
