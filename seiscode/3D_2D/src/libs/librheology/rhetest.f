c this is <rhetest.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1998 by Thomas Forbriger (IfG Stuttgart)
c
c RHEology TESTer
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
c    20/10/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
      program rhetest
c
      character*79 version
      parameter(version='RHETEST   V1.0   RHEology TESTer')
c
      double precision om, omr, omp, vref, qref, taue, taup, v, q, om1,om2
      double precision qmref
      double precision rl_qm_from_qv
      integer i, m
      parameter (m=80)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument, testtype
      parameter(maxopt=2)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v/
      data opthasarg/2*.FALSE./
      data optarg/2*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: rhetest arguments'
        print *,'   or: rhetest -help'
        if (iargc().lt.1) stop 'ERROR: missing arguments'
        print *,' '
        print *,'RHEology TESTer'
        print *,' '
        print *,'-S om1,om2,omr,omp,vref,qref '
        print *,'       go from om1 to om2 in 24 steps and print SLS values'
        print *,'       for omr and omp and vref and qref'
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
c
c------------------------------------------------------------------------------
c go
c
      call getarg(1, testtype)
      if (testtype(1:2).eq.'-S') then
        call getarg(2, argument)
        read(argument, *) om1,om2,omr,omp,vref,qref
c 
        print *,'reference angular frequency:  ',omr
        print *,'rheology angular frequency:   ',omp
        print *,'reference velocity:           ',vref
        print *,'reference velocity-Q:         ',qref
c 
        qmref=rl_qm_from_qv(qref)
        call rl_taue(omr, omp, qmref, taue, taup)
c 
        print *,'reference modul-Q:            ',qmref
        print *,'strain time constant:         ',taue
        print *,'stress time constant:         ',taup
c 
        print 50,'om','v','q','1/q'
        do i=1,m
          om=10**((i-1)*(log10(om2)-log10(om1))/(m-1)+log10(om1))
          call rl_sls(om, omr, omp, vref, qref, v, q)
          print 51,om,v,q,1./q
        enddo
      endif
c 
      stop
   50 format(3x,4a12)
   51 format(4f12.5)
      end
c
c ----- END OF rhetest.f -----
