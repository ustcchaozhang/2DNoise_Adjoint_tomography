c this is <modrecipes.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2003 by Thomas Forbriger (IMG Frankfurt) 
c
c conversion between different recipes for model definitions
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
c    11/02/2003   V1.0   Thomas Forbriger
c
c ============================================================================
c
      program modrecipes
c
      character*(*) version
      parameter(version=
     &  'MODRECIPES   V1.0   '//
     &  'conversion between different recipes for model definitions')
c
c model parameters
      integer n, ms
      parameter(ms=100)
      double precision nuref, radius
      double precision z(0:MS), rho(0:MS)
      double precision beta(0:MS), alpha(0:MS)
      real qa(0:MS), qb(0:MS)
      double precision conv_beta(0:MS), conv_alpha(0:MS)
      real conv_qa(0:MS), conv_qb(0:MS)
      double precision rel_beta(0:MS), rel_alpha(0:MS)
      real rel_qa(0:MS), rel_qb(0:MS)
      double complex cplx_beta(0:MS), cplx_alpha(0:MS)
c misc
      integer ilay
      double complex ime
      parameter(ime=(0.d0,1.d0))
      logical optrelvar
      character*80 infile,outfile,argrelvar,recipe,comment,outcomment
      integer cl_vlevel, lev2
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=3)
      character*2 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-r/
      data opthasarg/2*.FALSE.,.true./
      data optarg/2*1h-,4hjunk/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.3)) then
        print *,version
        print *,'Usage: modrecipes infile outfile recipe [-v] [-r file]'
        print *,'   or: modrecipes -help'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'This program converts refmet model files that are'
        print *,'defined for different recipes for calculating complex'
        print *,'velocities from real parameter values. The output is'
        print *,'the corresponding model file to be interpreted by our'
        print *,'recipe. For recipe definitions see below.'
        print *,' '
        print *,'infile       input refmet model file'
        print *,'outfile      output refmet model file'
        print *,'recipe       key of recipe to use (A, B, C, or D)'
        print *,' '
        print *,'-v           be verbose'
        print *,'-r file      write relative variation of parameters'
        print *,'             in % to ''file'' '
        print *,' '
        print *,'our recipe (recipe I in F&F 2005)'
        print *,'----------'
        print *,' '
        print *,'  c=v*sqrt(1+i/Q)'
        print *,' '
        print *,'  where'
        print *,'    c: complex wave velocity in wave equation'
        print *,'    v: real tabulated velocity'
        print *,'    Q: tabulated quality for velocity'
        print *,'    i: imaginary unit'
        print *,' '
        print *,'recipe A: (recipe V in F&F 2005)'
        print *,'---------'
        print *,'(tabulated velocity is modulus of c)'
        print *,' '
        print *,'  c=v*exp(i*atan(1/Q)/2.)'
        print *,' '
        print *,'recipe B: (recipe III in F&F 2005)'
        print *,'---------'
        print *,'(tabulated velocity is real part of c)'
        print *,' '
        print *,'  c=v*(1+i/(2*Q))'
        print *,' '
        print *,'recipe C: (recipe IV in F&F 2005)'
        print *,'---------'
        print *,'(tabulated velocity is phase velocity of plane wave)'
        print *,' '
        print *,'  c=v*cos(atan(1/Q)/2)*exp(i*atan(1/Q)/2)'
        print *,' '
        print *,'recipe D: (recipe II in F&F 2005)'
        print *,'---------'
        print *,'(tabulated velocity is real part of complex velocity)'
        print *,' '
        print *,'  c=v*exp(i*atan(1/Q)/2)/cos(atan(1/Q)/2)'
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
      optrelvar=optset(3)
      argrelvar=optarg(3)
c
      call getarg(1, infile)
      call getarg(2, outfile)
      call getarg(3, recipe)
c
c------------------------------------------------------------------------------
c go
      lev2=0
      cl_vlevel=lev2
      if (verbose) cl_vlevel=lev2+1
      call refmet_rmod(infile, comment, ms, nuref,
     &    radius, n, z, alpha, beta, rho, qa, qb, cl_vlevel, debug,
     &    lev2)
                
      do ilay=1,n
        if (recipe(1:1).eq.'A') then
          cplx_beta(ilay)=beta(ilay)*exp(ime*atan(1/qb(ilay))/2.d0)
          cplx_alpha(ilay)=alpha(ilay)*exp(ime*atan(1/qa(ilay))/2.d0)
        elseif (recipe(1:1).eq.'B') then
          cplx_beta(ilay)=beta(ilay)*(1.d0+ime/(2.d0*qb(ilay)))
          cplx_alpha(ilay)=alpha(ilay)*(1.d0+ime/(2.d0*qa(ilay)))
        elseif (recipe(1:1).eq.'C') then
          cplx_beta(ilay)=beta(ilay)*cos(atan(1.d0/qb(ilay))/2.d0)*
     &                      exp(ime*atan(1.d0/qb(ilay))/2.d0)
          cplx_alpha(ilay)=alpha(ilay)*cos(atan(1.d0/qa(ilay))/2.d0)*
     &                      exp(ime*atan(1.d0/qa(ilay))/2.d0)
        elseif (recipe(1:1).eq.'D') then
          cplx_beta(ilay)=beta(ilay)*
     &                      exp(ime*atan(1.d0/qb(ilay))/2.d0)/
     &                      cos(atan(1.d0/qb(ilay))/2.d0)
          cplx_alpha(ilay)=alpha(ilay)*
     &                      exp(ime*atan(1.d0/qa(ilay))/2.d0)/
     &                      cos(atan(1.d0/qa(ilay))/2.d0)
        else
          stop 'ERROR: unknown recipe key'
        endif
        conv_qb(ilay)=real(cplx_beta(ilay)**2)/
     &                dimag(cplx_beta(ilay)**2)
        conv_beta(ilay)=sqrt(real(cplx_beta(ilay)**2))
        conv_qa(ilay)=real(cplx_alpha(ilay)**2)/
     &                dimag(cplx_alpha(ilay)**2)
        conv_alpha(ilay)=sqrt(real(cplx_alpha(ilay)**2))
        rel_qb(ilay)=100.*abs(1.d0-conv_qb(ilay)/qb(ilay))
        rel_qa(ilay)=100.*abs(1.d0-conv_qa(ilay)/qa(ilay))
        rel_alpha(ilay)=100.*abs(1.d0-conv_alpha(ilay)/alpha(ilay))
        rel_beta(ilay)=100.*abs(1.d0-conv_beta(ilay)/beta(ilay))
      enddo

      write (outcomment, 50) recipe(1:1), comment
      call write_refmet(outfile, outcomment, ms, nuref, radius,
     &  n, z, conv_alpha, conv_beta, rho, conv_qa, conv_qb)

      if (optrelvar) then
        write (outcomment, 51) recipe(1:1), comment
        call write_refmet(argrelvar, outcomment, ms, nuref, radius,
     &    n, z, rel_alpha, rel_beta, rho, rel_qa, rel_qb)
      endif
c
      stop
   50 format('recipe ',a1,': ',a)
   51 format('rel. var. recipe ',a1,': ',a)
      end
c
c
c======================================================================
c 
      subroutine write_refmet(outfile, comment, ms, nuref, radius,
     &  n, z, alpha, beta, rho, qa, qb)
c 
c write refmet output file
c
      character outfile*(*), comment*(*)
      integer n, ms, lu
      parameter(lu=12)
      double precision nuref, radius
      double precision z(0:MS), alpha(0:MS)
      double precision beta(0:MS), rho(0:MS)
      real qa(0:MS), qb(0:MS)
      integer layer
c open output
      print *,'open output file: ',outfile(1:index(outfile,' ')-1)
      open(lu, file=outfile, err=99)
      write(lu, '(a72)', err=98) comment
c----------------------------------------------------------------------
c write model
      write(lu, 55, err=98) 
     &  'number of layers:',n,' earth radius:',radius,
     &  'reference frequency (Hz):', nuref
      write(lu, 50, err=98) 'Zb','alpha','beta','rho','Qalpha','Qbeta','Rb'
c write top halfspace (atmosphere) needed by refmet.f
      write(lu, 51, err=98) 0.d0, 0.3318d0, 0.d0, 0.0013d0, 1000.d0, -1.d0, -1. 
      do layer=1,n-1
        write(lu, 51, err=98) z(layer+1), alpha(layer), beta(layer), 
     &                rho(layer), qa(layer), qb(layer), -1.
      enddo
      write(lu, 54, err=98) 'halfspace:', alpha(n), beta(n), 
     &              rho(n), qa(n), qb(n), -1.
c----------------------------------------------------------------------
c output additional comments
      write(lu, '(1x)', err=98)
      write(lu, 52, err=98) 'par','units','description'
      write(lu, '(76(1h-))', err=98) 
      write(lu, 52, err=98) 'Zb','km','bottom-of-layer depth'
      write(lu, 52, err=98) 'alpha','km/s','P-wave layer velocity'
      write(lu, 52, err=98) 'beta','km/s','S-wave layer velocity'
      write(lu, 52, err=98) 'rho','g/cm^3','layer density'
      write(lu, 52, err=98) 'Qalpha',' ','quality factor related to alpha'
      write(lu, 52, err=98) 'Qbeta',' ','quality factor related to beta'
      write(lu, 52, err=98) 'Rb','km','radius corresponding to bottom of layer'
      write(lu, '(//"This file was generated by:"/a70/)', err=98) comment
      write(lu, '(/a)', err=98) 
     &  'A reference frequency of 0Hz means values are frequency independent.'
c 
      return
   50 format(a10,6(1x,a10)/77(1h-))
   51 format(f10.7,2(1x,f10.5),1x,f10.6,3(1x,f10.3))
   52 format(a8,a8,2x,a)
   53 format(18x,a)
   54 format(a10,2(1x,f10.5),1x,f10.6,3(1x,f10.3))
   55 format(/a30,i10/a30,f10.3,a30,f10.3/)
   56 format(/'transformed velocities according to a simples constant-Q',
     &       /'dispersion law to period ',f10.4,'sec for a reference',
     &       /'freuqency of ',f10.5,'Hz')
   99 stop 'ERROR: opening output file'
   98 stop 'ERROR: writing output file'
   97 stop 'ERROR: closing output file'
      end
c 
c ----- END OF modrecipes.f ----- 
