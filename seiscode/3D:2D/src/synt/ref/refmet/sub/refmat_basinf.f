c this is <sub/refmat_basinf.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
c
c online documentation for refmat
c
c ============================================================================
c
c this is part of the REFMAT reflectivity program
c (for comments and revisions see also the main source refmat.f)
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
c 10/06/97   copied and changed from refmet_basinf.f to serve
c            special needs by refmat
c
c======================================================================
c 
c give basic information
c 
      subroutine refmat_basinf(version, mainfile, lev1, lev2, lev3, lev4,
     &  ME, MS, MSL, Mf, sff_maxfree, hfktstr)
c 
      character version*(*), mainfile*(*), hfktstr*(*)
      integer lev1, lev2, lev3, lev4
      integer ME, MSL, MS, Mf, sff_maxfree
c 
      integer iargc
c 
      print *, version
      print *, 'Usage: refmat [-d] [-v level] -m file file'
      print *, '   or: refmat -help'
      if (iargc().lt.1) stop 'ERROR: missing parameters'
      call getarg(1, mainfile)
      if (mainfile.eq.'-help') then
        call refmet_intro
        print *,' '
        print *,' NOTICE: This is not refmet this is REFMAT.'
        print *,' NOTICE: I will not calculate seismograms!'
        print *,' '
        print *,'commandline parameters are:'
        print *,' '
        print *,'-d           Give debugging output.'
        print *,'-v level     Set verbosity level. The parameter may be'
        print *,'             any integer value. The higher the value'
        print *,'             the more output will be produced.'
        print *,'-m file      Name of file in which the response matrix'
        print *,'             should be saved.'
        print *,' '
        print *,'file         Is the name of the main configuration file.'
        print *,'             It contains the names of the three file'
        print *,'             containing the earth model, the source model'
        print *,'             and the receiver coordinates. In addition there'
        print *,'             must be given some numerical parameters for the'
        print *,'             calculation.'
        print *,' '
        print *,' This special version of the method will only calculate'
        print *,' the response matrix of the layered medium. This is still'
        print *,' source and receiver independent. The only source'
        print *,' parameter used here is the source depth. Therefore'
        print *,' refmat uses only two configuration files (apart from'
        print *,' the main configuration file: The earth model and the'
        print *,' source configuration (for source depth). Numerical'
        print *,' parameters from the main file only set ranges. No'
        print *,' tapering will be done here. The frequency and slowness'
        print *,' tapers may be applied when using resus to get the'
        print *,' seismic time series.'
        print *,' '
        print *,' The matrix file is written in Fortran binary format and'
        print *,' is therefore CPU specific!'
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
        print *,'       maximum number of layers: ',MS
        print *,'      maximum number of samples: ',MSL
        print *,'  maximum number of frequnecies: ',Mf
        print *,'   maximum number of FREE lines: ',sff_maxfree
c call other info routine
        call refmet_maininf
        call refmet_modinf
        call refmet_sourinf(hfktstr)
        stop
      endif
      return
      end
c
c ----- END OF sub/refmat_basinf.f ----- 
