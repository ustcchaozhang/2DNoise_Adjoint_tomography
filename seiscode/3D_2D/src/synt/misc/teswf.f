c this is <teswf.f>
c------------------------------------------------------------------------------
c   ($Id$)
c
c Copyright 2000, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c test wave field
c
c REVISIONS and CHANGES
c    17/04/2000   V1.0   Thomas Forbriger
c
c==============================================================================
c
      program teswf
c
      character*(*) version
      parameter(version='TESWF   V1.0   test wave field')
      character*(*) TESWF_CVS_ID
      parameter(TESWF_CVS_ID='$Id$')
c
      integer m
      parameter(m=1024)
      real d(m)
      integer id(m)
      equivalence(d,id)
      integer n,i,j,s,k
      logical gauss
      real p,dx,dt
c 
      integer lu, ierr
      parameter(lu=10)
      character date*7, time*12,type*22, wid2line*132, nil*10
      parameter(nil='NIL')
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=4)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-p, 2h-o/
      data opthasarg/2*.FALSE.,.TRUE.,.FALSE./
      data optarg/2*1h-,2h1.,1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: teswf filename [-p p] [-o]'
        print *,'   or: teswf -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'test wave field'
        print *,' '
        print *,'-p p         set slowness to p s/km'
        print *,'-o           overwrite file'
        print *,' '
        print *,TESWF_CVS_ID
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
      call getarg(1, argument)
c
c------------------------------------------------------------------------------
c go
      gauss=.false.
      dx=1.
      dt=1.e-3
      n=100
      if (optset(3)) then
        read(optarg(3), *) p
        p=p/1.e3
      else
        p=(dt*m)/(dx*n)
      endif
      type='synthetic'
      date='990417'
      time='222700.000'
      call sff_PrepWid2(m, 1./dt, NIL, 1999, 04, 17, 22, 27, NIL,
     &                  NIL, NIL, 0., -1., -1., -1., -1., wid2line, ierr)
      if (ierr.ne.0) stop 'ERROR: preparing wid2line'
      if (optset(4)) call sff_New(lu, argument, ierr)
      if (ierr.ne.0) stop 'ERROR: cleaning file'
      call sff_WOpenS(lu, argument, type, 'C', 0., 0., 0., date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening file'
      if (gauss) then
        stop 'not yet implemented'
      else
        s=int((dx*p)/dt)
        k=s
        do i=1,n
          do j=1,m
            d(j)=0.
          enddo
          if (k.le.m) d(k)=1./sqrt(i*dx)
          k=k+s
          if (i.eq.n) then
            call sff_WTraceI(lu, wid2line, m, d, id, .true., 'C',
     &                       i*dx, 0., 0., 1, ierr)
          else
            call sff_WTraceI(lu, wid2line, m, d, id, .false., 'C',
     &                       i*dx, 0., 0., 1, ierr)
          endif
          if (ierr.ne.0) stop 'ERROR: writing trace'
        enddo
      endif
c
      stop
      end
c
c ----- END OF teswf.f -----
