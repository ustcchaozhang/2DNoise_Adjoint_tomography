c this is <sub/refmet_basinf.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
c
c online documentation for refmet
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
c 10/02/97   included information on component selection
c 10/06/97   no matrix writing for refmet
c 12/10/11   notes regarding line source
c 14/10/11   line source option -l in short help
c
c======================================================================
c 
c give basic information
c 
      subroutine refmet_basinf(version, mainfile, lev1, lev2, lev3, lev4,
     &  ME, MS, MSL, Mf, sff_maxfree, hfktstr)
c 
      character version*(*), mainfile*(*), hfktstr*(*)
      integer lev1, lev2, lev3, lev4
      integer ME, MSL, MS, Mf, sff_maxfree
c 
      integer iargc
c 
      print *, version
      print *, 'Usage: refmet [-d] [-v level] [-o basename] [-c] [-l]'
      print *, '              [-s select] [-1] [-2] [-p u] [-ty f] file'
      print *, '   or: refmet -help'
      print *, '   or: refmet -xhelp'
      if (iargc().lt.1) stop 'ERROR: missing parameters'
      call getarg(1, mainfile)
      if (mainfile.eq.'-xhelp') then
        call sff_help_details
        stop
      else if (mainfile.eq.'-help') then
        call refmet_intro
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
        print *,'-1           use first Hankel function H^(1)_m instead of'
        print *,'             Bessel function J_m for wavefield expansion.'
        print *,'-2           use second Hankel function H^(2)_m instead of'
        print *,'             Bessel function J_m for wavefield expansion.'
        print *,'-p u         use Hankel functions only for slowness values'
        print *,'             greater than ''u'' (in s/km).'
        print *,'-l           use line source (handle with care; see below)'
        print *,'-ty f        select output file format f'
        print *,' '
        print *,'file         Is the name of the main configuration file.'
        print *,'             It contains the names of the three file'
        print *,'             containing the earth model, the source model'
        print *,'             and the receiver coordinates. In addition there'
        print *,'             must be given some numerical parameters for the'
        print *,'             calculation.'
        print *,' '
        call refmet_comments
        print *,' '
        print *,'Line source'
        print *,'-----------'
        print *,'  Seismograms for a line source are calculated if'
        print *,'  option ''-l'' is selected.'
        print *,'  This is a quick and dirty modification, which'
        print *,'  can work for vertical single forces and isotropic'
        print *,'  moment tensors (explosions) only. Notice that'
        print *,'  no consistency checks are performed. Providing'
        print *,'  a moment tensor with deviatoric or CLVD components'
        print *,'  will result in meaningless output.'
        print *,' '
        print *,'  The Bessel-function kernels are simply replaced:'
        print *,'  J0(w*u*r) -> 2.*cos(w*u*r)/max(1.d-50,w*u)'
        print *,'  J1(w*u*r) -> 2.*sign(r)*sin(w*u*r)/max(1.d-50,w*u)'
        print *,'  J2(w*u*r) -> 0'
        print *,' '
        print *,'  Regarding coordinates:'
        print *,'  Radial coordinate r by definition is positive. The'
        print *,'  location in the plane is defined by r together with'
        print *,'  azimuth angle phi. In the case of a line source,'
        print *,'  specify coordinates perpendicular to the line source'
        print *,'  by r (which then can be positive and negative) and'
        print *,'  set phi=0 for all receivers. Again: no consistency'
        print *,'  checks are applied to the input parameter files!'
        print *,' '
        print *,'  Units:'
        print *,'  In the case of a line source, the force defined'
        print *,'  in the source parameter file is understood as a'
        print *,'  force per unit distance with'
        print *,'  [F0] = 1 N/m.'
        print *,'  The resulting seismograms then are displacment given'
        print *,'  in meters.'
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
        print *,'       maximum number of layers: ',MS
        print *,'      maximum number of samples: ',MSL
        print *,'  maximum number of frequnecies: ',Mf
        print *,'   maximum number of FREE lines: ',sff_maxfree
c call other info routine
        call refmet_maininf
        call refmet_modinf
        call refmet_sourinf(hfktstr)
        call refmet_rcvinf
        print *,' '
        call sff_help_formats
        stop
      endif
      return
      end
c
c ----- END OF sub/refmet_basinf.f ----- 
