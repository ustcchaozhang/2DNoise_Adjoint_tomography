c this is <dig.f>
c------------------------------------------------------------------------------
c
c Coypright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c DIspersion curves to Greens file
c
c REVISIONS and CHANGES
c    26/11/98   V1.0   Thomas Forbriger
c                      took this code from syg
c    28/02/99   V1.1   no comes with optional mask mode
c    07/04/00   V1.2   introduced option -n
c    09/05/00   V1.3   fixe curve width
c
c==============================================================================
c
      program dig
c
c variables
      character*79 version
      parameter (version='DIG   V1.3   DIspersion curves to Greens file')
c
      integer mu, mom
      parameter (mu=1000, mom=1000)
      integer nu, nom
      complex green(mom, mu)
      real thisgreen
c 
      integer magic
      character*4 cmagic
      parameter(cmagic='1234')
c
      integer lu
      parameter (lu=20)
c 
      character*80 curvename, lengthstr, greenname, line
      real fmax, umax, length, fixom
      real om(mom), u(mu)
c 
      integer i, j, index
      real pi
      parameter(pi=3.14159265)
c
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=5)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose, maskmode, normalize, dofixom
      double precision normval, maxval
c here are the keys to our commandline options
      data optid/'-d', '-v', '-M', '-n', '-O'/
      data opthasarg/3*.FALSE.,2*.TRUE./
      data optarg/3*'-',2*'1.'/
c 
c go
      print *,version
      print *,'Usage: dig curves greenfile length'
      print *,'           -P nf,nu,fmax,umax | -F file [-M] [-n v] [-O f]'
      if (iargc().ge.5) then
        call getarg(4, line)
      else
        line='none'
      endif
      if ((line(1:2).ne.'-P').and.(line(1:2).ne.'-F')) then
        print *,' '
        print *,'  curves       dispersion curves in a file created by grepg'
        print *,'  greenfile    greda style greens matrix (output)'
        print *,'  length       apparent profile length to calculate'
        print *,'               resolution from'
        print *,' '
        print *,'  -P           take parameters from command line'
        print *,
     &   '  -F file      choose parameters matching to greenfile ''file'' '
        print *,' '
        print *,'  nf           number of frequencies'
        print *,'  nu           number of phase slowness values'
        print *,'  fmax         maximum frequency (Hz)'
        print *,'  umax         maximum slowness (s/km)'
        print *,' '
        print *,'  -M           select mask mode (norms to maximum of one'
        print *,'               rather than integral of one)'
        print *,' '
        print *,' -n v          normalize maximum to v'
        print *,' '
        print *,' -O f          always use angular frequency f to calculate' 
        print *,'               curve width'
        stop
      endif
c 
c read command line arguments
c
      call tf_cmdline(6, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      maskmode=optset(3)
      normalize=optset(4)
      read(optarg(4), *) normval
      dofixom=optset(5)
      read(optarg(5), *) fixom
c 
c get command line parameters
      call getarg(1, curvename)
      call getarg(2, greenname)
      call getarg(3, lengthstr)
c 
      call readpicks(curvename)
      read(lengthstr, *, err=95) length

      if (line(1:2).eq.'-P') then
        call getarg(5, line)
        read(line, *) nom, nu, fmax, umax
c 
        if (nom.gt.mom) stop 'ERROR: too many frequencies'
        if (nu.gt.mu) stop 'ERROR: too many slownesses'
c 
c prepare frequency and slowness values to calculate greens function
        fmax=2.*pi*fmax
        do i=1,nom
          om(i)=float(i-1)*fmax/(nom-1)
        enddo
c 
        do i=1,nu
          u(i)=float(i-1)*umax/(nu-1)
        enddo
      elseif (line(1:2).eq.'-F') then
        call getarg(5, line)
        call greenread(line, 
     &       mu, mom, u, om,
     &       nu, nom)
      else
        stop 'ERROR: there is something wrong with your command line'
      endif
c 
      if (maskmode) then
        print *,'selected mask-mode'
        print *,'output will be normalized to a maximum amplitude of 1'
        print *,'use gaussian curve type'
      else
        print *,'selected green mode'
        print *,'output will be normalized to a per mode slowness'
        print *,'integral of 1'
        print *,'use Lorentz curve type'
      endif
c 
c report on range
      print *,'calculating ',nu,' slowness steps from ',u(1),' s/km to ',
     &        u(nu),' s/km'
      print *,'calculating ',nom,' frequency steps from ',om(1)/2./pi,
     &       ' Hz to ', om(nom)/2./pi,' Hz'
c 
      if (maskmode) then
c go through all slowness values
        do i=1,nu
c
          do j=1,nom
c go through all frequencies and get the expansion coefficients
c which are the green matrix elements
c 
            call maskvalue(om(j), u(i), length, thisgreen)
c scale from s/km to s/m units
            green(j,i)=thisgreen
          enddo
        enddo
      else
c go through all slowness values
        do i=1,nu
c
          do j=1,nom
c go through all frequencies and get the expansion coefficients
c which are the green matrix elements
c 
            if (dofixom) then
              call digvalue(om(j), u(i), length, thisgreen,fixom)
            else
              call digvalue(om(j), u(i), length, thisgreen,om(j))
            endif
c scale from s/km to s/m units
            green(j,i)=thisgreen
          enddo
        enddo
      endif
c
c normalize
      if (normalize) then
        print *,'normalize to ',normval
        do j=1,nom
          maxval=0.d0
          do i=1,nu
c            print *,j,nom,i,nu,maxval
            maxval=max(maxval,abs(green(j,i)))
          enddo
          if (maxval.le.1.d-70) maxval=1.d-50
c          print *, j,nom,maxval
          do i=1,nu
            green(j,i)=sngl(normval/maxval)*green(j,i)
c            print *,j,nom,i,nu,normval,maxval,green(j,i)
          enddo
        enddo
      endif
c 
c change scale (from s/km to s/m)
      do i=1,nu
        u(i)=u(i)*1.e-3
      enddo
c 
c write green code (easy to use)
      print *,'opening green file ',greenname(1:index(greenname,' ')),
     &    ' - overwrite mode'
      open(lu, file=greenname, form='unformatted', err=98)
      call tf_magic(cmagic, magic)
      write(lu, err=97) magic
      write(lu, err=97) nom, nu
      write(lu, err=97) (om(i), i=1,nom), (u(i), i=1,nu)
      write(lu, err=97) ((green(i,j), i=1,nom), j=1,nu)
      close(lu, err=96)
c 
      stop
   98 stop 'ERROR: opening green file'
   97 stop 'ERROR: writing green file'
   96 stop 'ERROR: closing green file'
   95 stop 'ERROR: in commandline'
      end
c
c======================================================================
c 
c some subroutines
c
c----------------------------------------------------------------------
c 
c this routine reads in the data
c
      subroutine greenread(filename, 
     &     maxslo, maxfreq, slo, om,
     &     nslo, nom)
c
      character filename*(*)
      integer maxslo, maxfreq
      real om(maxfreq), slo(maxslo)
      integer nslo, nom
      integer inmagic, cpu, match
      character*4 cmagic, incmagic
      parameter(cmagic='1234')
      equivalence(incmagic, inmagic)
      integer lu
      parameter(lu=20)
      integer i
c 
      print *,' '
      print *,'read green file ',filename(1:index(filename, ' '))
      open(lu, file=filename, form='unformatted', status='old', err=99)

c check byte sex
      read(lu, err=98, end=97) inmagic
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
        close(lu, err=96)
        print *,'bytesex read is ',incmagic
        stop 'ERROR: bytesex is unkown - oh oh...'
      endif
      read(lu, err=98, end=97) nom, nslo
      if ((nom.gt.maxfreq).or.(nslo.gt.maxslo)) then
        close(lu, err=96)
        stop 'ERROR: data exceeds array bounds'
      endif
      read(lu, err=98, end=97) (om(i), i=1,nom), (slo(i), i=1,nslo)
      close(lu, err=96)
      print *,'green file read and closed'
      
      do i=1,nslo
        slo(i)=slo(i)*1000.
      enddo

      return
   99 stop 'ERROR: opening green file'
   98 stop 'ERROR: reading green file'
   97 stop 'ERROR: reading green file - unexpected end'
   96 stop 'ERROR: closing green file'
      end
c       
c----------------------------------------------------------------------
c
      subroutine readpicks(pickfile)
c
c read picks from file
c
      include 'dig.inc'
c
      integer iset
c
      character pickfile*(*)
      integer lu, i, ntoread
      parameter(lu=10)
c
      print *,'reading from ',pickfile(1:index(pickfile,' '))
c
      open(lu, file=pickfile, status='old', err=99)
      read(lu, '(/,i10)', err=98, end=97) ntoread
      print *,'  going to read ',ntoread,' curves'
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
          print *,'  ',npicks(iset),' picks read to set ',iset
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
      subroutine maskvalue(om,p,length,value)
c
      real om,p,length
      real value
c 
      include 'dig.inc'
c 
      real pi,f,pc,pw,thisval
      integer i
      parameter(pi=3.1415729)
c 
      f=0.5*om/pi
      if (om.lt.1.e-10) then
        pw=1000./length
      else
        pw=1000./f/length
      endif
c 
      value=0.
c      print *,'om',om,' f',f,' p',p,' length',length,' pw',pw
      do i=1,maxpicktypes
        if (npicks(i).gt.1) then
          call svalue(i,f,pc)
          if (pc.gt.0.) then
            thisval=exp(-1.*((p-pc)/pw)**2)
c            print *,'i',i,' pc',pc,' thisval',thisval
            value=max(value,thisval)
          endif
        endif
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine digvalue(om,p,length,value,wom)
c
      real om,p,length,wom
      real value
c 
      include 'dig.inc'
c 
      real pi,f,pc,pw,thisval,wf
      integer i
      parameter(pi=3.1415729)
c 
      f=0.5*om/pi
      wf=0.5*wom/pi
      if (wom.lt.1.e-10) then
        pw=1000./length
      else
        pw=1000./wf/length
      endif
c 
      value=0.
c      print *,'om',om,' f',f,' p',p,' length',length,' pw',pw
      do i=1,maxpicktypes
        if (npicks(i).gt.1) then
          call svalue(i,f,pc)
          if (pc.gt.0.) then
            thisval=pw/pi/((p-pc)*(p-pc)+pw*pw)
c            print *,'i',i,' pc',pc,' thisval',thisval
            value=value+thisval
          endif
        endif
      enddo
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
      include 'dig.inc'
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
c ----- END OF dig.f -----
