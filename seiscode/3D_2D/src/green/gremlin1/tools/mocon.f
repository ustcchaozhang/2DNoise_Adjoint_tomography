c this is <mocon.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1997, 2011 by Thomas Forbriger (IfG Stuttgart)
c
c MOdel CONversion
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
c    18/12/97   V1.0   Thomas Forbriger
c    14/01/99   V1.1   model definition is changed (see glq_model.inc)
c                      - no coph_hs anymore
c                        argument means now bottom of deepest section
c                      - section depth means now bottom of section
c    28/11/99   V1.2   - introduced chopping master
c    17/11/10   V1.3   - use correct include path
c
c==============================================================================
c
      program mocon
c
      character*79 version
      parameter(version='MOCON   V1.3   MOdel CONversion')
c
      include '../libs/glq_dim.inc'
      include '../libs/glq_model.inc'
      include '../libs/glq_para.inc'
c 
      character*1 mode
      character*80 infile,outfile,comment
c 
      integer n, ms
      parameter(ms=200)
      real*8 radius, z(0:ms), alpha(0:ms)
      real*8 beta(0:ms), rho(0:ms), nuref
      real qa(0:ms), qb(0:ms)
      integer cl_vlevel, lev2
      double precision lastbot
c 
      integer i, j, k
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=4)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug
c here are the keys to our commandline options
      data optid/2h-D,2h-h,2h-s,2h-m/
      data opthasarg/.FALSE.,3*.TRUE./
      data optarg/1h-,3h-1.,3h0.1,1h2/
c
c------------------------------------------------------------------------------
c basic information
c
      print *,version
      print *,'Usage: mocon P|D infile outfile [-h depth] [-s step]'
      print *,'             [-m master]'
      print *,'   or: mocon -help'
c
      if (iargc().lt.1) stop 'ERROR: missing arguments'
      call getarg(1, argument)
      if (argument(1:5).eq.'-help') then
        print *,' '
        print *,'MOCON converts discrete refmet models into polynomial'
        print *,'models used by the glq-package and vice versa.'
        print *,' '
        print *,'P|D          P: input model is polynomial, output model'
        print *,'                will be a discrete one'
        print *,'             D: input model is a discrete, output will'
        print *,'                be a polynomial model'
        print *,'infile       file to read as input model'
        print *,'outfile      file to create as output model'
        print *,' '
        print *,'-h depth     bottom depth of deepest section for'
        print *,'             discrete->polynomial'
        print *,'             (default: 1.1*depth of deepest interface)'
        print *,'-s step      chopping stepsize '
        print *,'             (default: ',optarg(3)(1:5),')'
        print *,'-m master    chopping master'
        print *,'             (default: ',optarg(4)(1:5),')'
        print *,' '
        call refmet_modinf
        stop
      endif
c 
      if (iargc().lt.3) stop 'ERROR: wrong number of arguments'
      call getarg(1, mode)
      call getarg(2, infile)
      call getarg(3, outfile)
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(4, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      read(optarg(2), *) lastbot
      read(optarg(3), *) chop_step
      read(optarg(4), *) chop_master
c 
      if (DEBUG) print *,'DEBUG: debug is on'
c
c------------------------------------------------------------------------------
c go
c
      if (mode.eq.'D') then
c discrete -> polynomial
        lev2=1
        cl_vlevel=4
        call refmet_rmod(infile, comment, ms, nuref,
     &    radius, n, z, alpha, beta, rho, qa, qb, cl_vlevel, debug, lev2)
        if (nuref.ne.0.d0) 
     &    print *,'NOTICE: reference frequency ',nuref,' will be ignored'
        if (radius.gt.0.) 
     &    print *,'NOTICE: model has radius ',radius,
     &           ' - will be regarded flat'
c convert
        glqm_nsec=n
        do i=1,n
          if (i.lt.n) then
            mdepth(i, 1)=z(i+1)*1.e3
          else
            mdepth(i, 1)=z(i)*1.1e3
            if (lastbot.gt.0.d0) mdepth(i, 1)=lastbot
            if (mdepth(i,1).lt.(z(i)*1.e3)) then
              mdepth(i,1)=z(i)*1.1e3
              print *,'WARNING: bottom depth of deepest section is too shallow'
              print *,'         will be set to ',mdepth(i,1),' m'
            endif
          endif
          if (debug) print *,'DEBUG: i depth ',i,mdepth(i,1)
          do k=1,glqm_mpar
            glqm_npol(i, k)=1
            do j=2,3
              model(j, i, k, 1)=0.
            enddo
          enddo
          model(1, i, mi_alpha, 1)=alpha(i)
          if (debug) print *,'DEBUG: i alpha ',i,alpha(i)
          model(1, i, mi_beta, 1)=beta(i)
          model(1, i, mi_density, 1)=rho(i)
          model(1, i, mi_Qalpha, 1)=qa(i)
          model(1, i, mi_Qbeta, 1)=qb(i)
        enddo
c write 
        call mod_save(outfile, 1, .true., comment)
      elseif (mode.eq.'P') then
c polynomial -> discrete
        call mod_read(infile, 1)
        call mod_chop(1)
        n=glqm_nlay
c 
        if (debug) then
          do i=1,n
            print *,'DEBUG: i, depth, alpha ',i,dmodel(i,mi_depth),
     &          dmodel(i,mi_alpha)
          enddo
        endif
c convert
        if (n.gt.ms) stop 'ERROR: too many layers'
        do i=2,n
          z(i-1)=dmodel(i, mi_depth)
          alpha(i-1)=dmodel(i, mi_alpha)
          beta(i-1)=dmodel(i, mi_beta)
          rho(i-1)=dmodel(i, mi_density)
          qa(i-1)=dmodel(i, mi_Qalpha)
          qb(i-1)=dmodel(i, mi_Qbeta)
        enddo
c write
        call write_refmet(outfile, version, ms, 
     &    n-1, z, alpha, beta, rho, qa, qb)
      else
        stop 'ERROR: unknown conversion mode'
      endif
c 
      stop
      end
c
c======================================================================
c 
      subroutine write_refmet(outfile, comment, ms, 
     &  n, z, alpha, beta, rho, qa, qb)
c 
c write refmet output file
c
      include '../libs/glq_dim.inc'
      include '../libs/glq_para.inc'
c 
      character outfile*(*), comment*(*)
      integer n, ms, lu
      parameter(lu=12)
      real*8 z(0:MS), alpha(0:MS)
      real*8 beta(0:MS), rho(0:MS)
      real qa(0:MS), qb(0:MS)
      integer layer
c open output
      print *,'open output file: ',outfile(1:index(outfile,' ')-1)
      open(lu, file=outfile, err=99)
      write(lu, '(a72)', err=98) comment
c----------------------------------------------------------------------
c write model
      write(lu, 55, err=98) 
     &  'number of layers:',n,' this is a flat model',-1.,
     &  'reference frequency (Hz):', 0.
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
      write(lu, '("Minimum allowed relative parameter stepsize: "f6.4)',
     &  err=98) chop_step
c      write(lu, '("Halfspace depth: "f8.3"m")', err=98) chop_hs
      write(lu, '(/a)', err=98) 
     &  'A reference frequency of 0Hz means values are frequency independent.'
      write(lu, '(/a)', err=98) 
     &  'This was and is still a flat model.'
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
c ----- END OF mocon.f -----
