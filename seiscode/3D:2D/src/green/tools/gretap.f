c this is <gretap.f>
c------------------------------------------------------------------------------
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c GREen files TAPer
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
c    28/06/99   V1.0   Thomas Forbriger
c    30/09/99   V1.1   added sin and cos taper
c    14/02/17   V1.2   optionally use 0. as default weight
c
c==============================================================================
c
      program gretap
c
      character*79 version
      parameter(version='GRETAP   V1.2   GREen files TAPer')
c 
      include 'gretap.inc'
c
c magic number for binary file identification
      integer inmagic, cpu, match
      character*4 cmagic, incmagic, wcmagic
      parameter(cmagic='1234')
      equivalence(incmagic, inmagic)
      parameter(wcmagic='123S')
      integer magic
c 
      integer i, j
      real defaultweight
c 
      real pi2
      parameter(pi2=2.*3.1415926535897931159979634685)
c file
      integer lu
      parameter(lu=20)
c
      integer iarg, iargc
c commandline
      integer maxopt, lastarg
      character*80 argument,greenfile,outfile
      parameter(maxopt=3)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/'-d', '-v', '-0'/
      data opthasarg/3*.FALSE./
      data optarg/3*'-'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: gretap greenfile taperfile [-v] [-d]'
        print *,'              command [command ...]'
        print *,'   or: gretap -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'GREen files TAPer'
        print *,' '
        print *,'-v           verbose'
        print *,'-s           debug'
        print *,'-0           set default weight to 0. (see below)'
        print *,' '
        print *,'greenfile    greens data file which defines the matrix'
        print *,'             dimensions'
        print *,'taperfile    output file to which taper values will be'
        print *,'             written'
        print *,' '
        print *,'commands may be:'
        print *,'  ga10:file        Take first two curves from ''file'' and'
        print *,'                   place gaussian taper in between with a'
        print *,'                   value of 10% at each curve.'
        print *,'  ga50:file        Take first two curves from ''file'' and'
        print *,'                   place gaussian taper in between with a'
        print *,'                   value of 50% at each curve.'
        print *,'  el:file          Take first curve from ''file'' and'
        print *,'                   build a cosine taper from the curve'
        print *,'                   to the lower slowness edge.'
        print *,'  eh:file          Take first curve from ''file'' and'
        print *,'                   build a cosine taper from the curve'
        print *,'                   to the higher slowness edge.'
        print *,'  sin:file         Take first two curves from ''file'' and'
        print *,'                   place a sin(p)**2 taper in between.'
        print *,'  cos:file         Take first two curves from ''file'' and'
        print *,'                   place a cos(p)**2 taper in between.'
        print *,'  inv:             Invert taper.'
        print *,'  or:              Switch to or-mode which means that new'
        print *,'                   values will override old values if they'
        print *,'                   are larger.'
        print *,'  mul:             Switch to mul-mode which means that'
        print *,'                   new and old values will be multiplied.'
        print *,' '
        print *,'If not otherwise specified the default weight is 1.'
        print *,'This means, that at frequencies where curves are not'
        print *,'defined, weights for all slowness values will be set'
        print *,'to 1. To apply a taper definition only to a limited'
        print *,'frequency band and to suppress all other frequencies'
        print *,'use option -0 set or: mode prior to first taper'
        print *,'definitions and switch back to mul: mode after first'
        print *,'taper definition.'
        print *,' '
        print *,'compiled array dimensions:'
        print *,'  number of frequencies:     ',maxfreq
        print *,'  number of slowness values: ',maxslo
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
      defaultweight=1.
      if (optset(3)) then 
        defaultweight=0.
      endif
      print *,'defaultweight ',defaultweight
      call getarg(1, greenfile)
      call getarg(2, outfile)
c
c------------------------------------------------------------------------------
c go
c
      print *,'read green file ',greenfile(1:index(greenfile, ' '))
      open(lu, file=greenfile, form='unformatted', status='old', err=89)
c check byte sex
      read(lu, err=88, end=87) inmagic
      call tf_bytesex(cmagic, inmagic, cpu, match)
      if (cpu.eq.1) then
        print *,'running on Intel...'
      elseif (cpu.eq.2) then
        print *,'running on Motorola...'
      else
        stop 'ERROR: unknown processor type...'
      endif
      if (match.eq.1) then
        print *,'matching bytesex - good...'
      elseif (match.eq.2) then 
        print *,'bytesex not matching - we will have to swap!'
        stop 'ERROR: do not know how to do that...'
      else
        close(lu, err=86)
        print *,'bytesex read is ',incmagic
        stop 'ERROR: bytesex is unkown - oh oh...'
      endif
      read(lu, err=88, end=87) nom, nslo
      if ((nom.gt.maxfreq).or.(nslo.gt.maxslo)) then
        close(lu, err=86)
        stop 'ERROR: data exceeds array bounds'
      endif
      read(lu, err=88, end=87) (om(i), i=1,nom), (slo(i), i=1,nslo)
      close(lu, err=86)
      print *,'green file closed'
      do i=1,nslo
        slo(i)=slo(i)*1.e3
      enddo
      do i=1,nom
        om(i)=om(i)/pi2
      enddo
c 
c----------------------------------------------------------------------
c clear weigth array
      do i=1,nom
        do j=1,nslo
          weight(i,j)=defaultweight
        enddo
      enddo
      setmode=mode_mul

c======================================================================
c 
      lastarg=max(3,lastarg+1)
      do iarg=lastarg,iargc()
        call getarg(iarg, argument)
        print *,'process command ''',argument(1:index(argument,' ')),''''
        if (argument(1:5).eq.'ga10:') then
c ga10      - 10% edge gaussian taper from first two curves
          call tap_ga10(argument)
        elseif (argument(1:5).eq.'ga50:') then
c ga50      - 50% edge gaussian taper from first two curves
          call tap_ga50(argument)
        elseif (argument(1:3).eq.'el:') then
c el        - low phase-slowness end edge from first curve
          call tap_el(argument)
        elseif (argument(1:3).eq.'eh:') then
c eh        - high phase-slowness end edge from first curve
          call tap_eh(argument)
        elseif (argument(1:4).eq.'sin:') then
c sin       - sin**2 phase-slowness from first two curves
          call tap_sin(argument)
        elseif (argument(1:4).eq.'cos:') then
c cos       - cos**2 phase-slowness from first two curves
          call tap_cos(argument)
        elseif (argument(1:4).eq.'inv:') then
c inv       - reverse taper matrix
          call tap_inv
        elseif (argument(1:3).eq.'or:') then
c or        - change mode (uses larger of old or new taper value)
          setmode=mode_or
          print *,'    changed mode to ''or'''
        elseif (argument(1:4).eq.'mul:') then
c mul       - change mode (uses old*new taper value)
          setmode=mode_mul
          print *,'    changed mode to ''mul'''
        else
c no match
          print *,'ERROR: unknown command: ''',
     &      argument(1:index(argument,':')),''''
        endif
      enddo
c
c----------------------------------------------------------------------
c
c write taper code (easy to use)
c 
      print *,' '
      print *,'opening taper file ',outfile(1:index(outfile,' ')),
     &    ' - overwrite mode'
      open(lu, file=outfile, form='unformatted', err=98)
      call tf_magic(wcmagic, magic)
      write(lu, err=97) magic
      write(lu, err=97) nom, nslo
      write(lu, err=97) ((weight(i,j), i=1,nom), j=1,nslo)
      close(lu, err=96)
      print *,'taper file written and closed'
c 
      stop
   98 stop 'ERROR: opening taper file'
   97 stop 'ERROR: writing taper file'
   96 stop 'ERROR: closing taper file'
   89 stop 'ERROR: opening green file'
   88 stop 'ERROR: reading green file'
   87 stop 'ERROR: reading green file - unexpected end'
   86 stop 'ERROR: closing green file'
      end
c
c======================================================================
c 
c some subroutines
c
c----------------------------------------------------------------------
c
      subroutine readpicks(pickfile)
c
c read picks from file
c
      include 'gretap.inc'
c
      integer iset
c
      character pickfile*(*)
      integer lu, i, ntoread
      parameter(lu=10)
c
      print *,'    reading from ',pickfile(1:index(pickfile,' '))
c
      open(lu, file=pickfile, status='old', err=99)
      read(lu, '(/,i10)', err=98, end=97) ntoread
      print *,'      going to read ',ntoread,' curves'
      if (ntoread.gt.maxpicktypes) stop 'ERROR: too many curves'
c
      do iset=1,maxpicktypes
        npicks(iset)=0
      enddo
c 
      do iset=1,ntoread
        read(lu, '(i10)', err=98, end=97) npicks(iset)
c
        if (npicks(iset).le.maxpicks) then
          do i=1,npicks(iset)
            read(lu, *, err=98, end=97) spicks(i,iset), fpicks(i,iset)
          enddo
c
          print *,'      ',npicks(iset),' picks read to set ',iset
        else
          stop 'ERROR: too many samples'
        endif
      enddo
c
      close(lu, err=96)
c
      return
   99 stop 'ERROR (readpicks): opening dispersion curve file'
   98 stop 'ERROR (readpicks): reading dispersion curve file'
   97 stop 'ERROR (readpicks): reading dispersion curve file - unexpected end'
   96 stop 'ERROR (readpicks): closing dispersion curve file'
      end
c
c----------------------------------------------------------------------
c
      subroutine tap_ga10(cmd)
c
      include 'gretap.inc'
c
      character*(*) cmd
c 
      integer i,j
      real p1,p2,value,m,w2
c 
      print *,'  apply 10% gaussian taper defined by first two curves from'
      print *,'  ',cmd(6:index(cmd,' '))
c 
      call readpicks(cmd(6:))
c 
      do i=1,nom
        call svalue(1,om(i),p1)
        call svalue(2,om(i),p2)
        if ((p1.gt.0.).and.(p2.gt.0.)) then
          m=(p1+p2)/2.
          w2=(p1-p2)**2/4./log(.1)
          do j=1,nslo
            value=exp((slo(j)-m)**2/w2)
            call setvalue(i,j,value)
          enddo
        endif
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine tap_cos(cmd)
c
      include 'gretap.inc'
c
      character*(*) cmd
c 
      integer i,j
      real p1,p2,value,pi2
      parameter(pi2=0.5*3.141592653589793115)
c 
      print *,'  apply cos**2 taper defined by first two curves from'
      print *,'  ',cmd(5:index(cmd,' '))
c 
      call readpicks(cmd(5:))
c 
      do i=1,nom
        call svalue(1,om(i),p1)
        call svalue(2,om(i),p2)
        if ((p1.gt.0.).and.(p2.gt.0.)) then
          if (p2.lt.p1) then
            value=p1
            p1=p2
            p2=value
          endif
          do j=1,nslo
            if ((slo(j).ge.p1).and.(slo(j).le.p2)) then
              value=cos(pi2*(slo(j)-p1)/(p2-p1))**2
            elseif (slo(j).gt.p2) then
              value=0.
            else
              value=1.
            endif
            call setvalue(i,j,value)
          enddo
        endif
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine tap_sin(cmd)
c
      include 'gretap.inc'
c
      character*(*) cmd
c 
      integer i,j
      real p1,p2,value,pi2
      parameter(pi2=0.5*3.141592653589793115)
c 
      print *,'  apply sin**2 taper defined by first two curves from'
      print *,'  ',cmd(5:index(cmd,' '))
c 
      call readpicks(cmd(5:))
c 
      do i=1,nom
        call svalue(1,om(i),p1)
        call svalue(2,om(i),p2)
        if ((p1.gt.0.).and.(p2.gt.0.)) then
          if (p2.lt.p1) then
            value=p1
            p1=p2
            p2=value
          endif
          do j=1,nslo
            if ((slo(j).ge.p1).and.(slo(j).le.p2)) then
              value=sin(pi2*(slo(j)-p1)/(p2-p1))**2
            elseif (slo(j).gt.p2) then
              value=1.
            else
              value=0.
            endif
            call setvalue(i,j,value)
          enddo
        endif
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine tap_ga50(cmd)
c
      include 'gretap.inc'
c
      character*(*) cmd
c 
      integer i,j
      real p1,p2,value,m,w2
c 
      print *,'  apply 50% gaussian taper defined by first two curves from'
      print *,'  ',cmd(6:index(cmd,' '))
c 
      call readpicks(cmd(6:))
c 
      do i=1,nom
        call svalue(1,om(i),p1)
        call svalue(2,om(i),p2)
        if ((p1.gt.0.).and.(p2.gt.0.)) then
          m=(p1+p2)/2.
          w2=(p1-p2)**2/4./log(.5)
          do j=1,nslo
            value=exp((slo(j)-m)**2/w2)
            call setvalue(i,j,value)
          enddo
        endif
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine tap_el(cmd)
c
      include 'gretap.inc'
c
      character*(*) cmd
c 
      integer i,j
      real p,pi,c, value
      parameter(pi=3.14159265358979311599796)
c 
      print *,
     &  '  apply lower slowness edge cosine taper defined by first curve from'
      print *,'  ',cmd(4:index(cmd,' '))
c 
      call readpicks(cmd(4:))
c 
      do i=1,nom
        call svalue(1,om(i),p)
        if (p.gt.0.) then
          c=pi/p
          do j=1,nslo
            if (slo(j).lt.p) then
              value=1.-0.5*(cos(slo(j)*c)+1.)
            else
              value=1.
            endif
            call setvalue(i,j,value)
          enddo
        endif
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine tap_eh(cmd)
c
      include 'gretap.inc'
c
      character*(*) cmd
c 
      integer i,j
      real p,pi,c,sm, value
      parameter(pi=3.14159265358979311599796)
c 
      print *,
     &  '  apply higher slowness edge cosine taper defined by first curve from'
      print *,'  ',cmd(4:index(cmd,' '))
c 
      call readpicks(cmd(4:))
c 
      sm=slo(nslo)
      do i=1,nom
        call svalue(1,om(i),p)
        if (p.gt.0.) then
          c=pi/(sm-p)
          do j=1,nslo
            if (slo(j).gt.p) then
              value=1.-0.5*(cos((sm-slo(j))*c)+1.)
            else
              value=1.
            endif
            call setvalue(i,j,value)
          enddo
        endif
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine tap_inv
c
      include 'gretap.inc'
c 
      integer i,j
c 
      print *,'  inverting weights...'
c
      do i=1,nom
        do j=1,nslo
          weight(i,j)=1.-weight(i,j)
        enddo
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine setvalue(iom,islo,value)
c
      include 'gretap.inc'
c 
      integer iom,islo
      real value
c 
      if (setmode.eq.mode_or) then
        weight(iom,islo)=max(weight(iom,islo),value)
      elseif (setmode.eq.mode_mul) then
        weight(iom,islo)=weight(iom,islo)*value
      else
        stop 'ERROR: unknown setmode'
      endif
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine svalue(i,f,pc)
c 
      real f,pc
      integer i
c
      include 'gretap.inc'
c 
      integer j
c 
      pc=-1.
c      print *,'svalue search for curve ',i,' at f=',f
      if (f.gt.fpicks(1,i)) then
c        print *,'f is greater than fmin=',fpicks(1,i)
        j=1
        do while ((j.le.npicks(i)).and.(fpicks(j,i).lt.f))
c          print *,'f is still greater than fpicks(',j,',',i,')=',fpicks(j,i)
          j=j+1
        enddo
        if (j.le.npicks(i)) then
          pc=spicks(j-1,i)+(f-fpicks(j-1,i))*
     &      (spicks(j,i)-spicks(j-1,i))/(fpicks(j,i)-fpicks(j-1,i))
        endif
      endif
c 
      return
      end
c
c ----- END OF gretap.f -----
