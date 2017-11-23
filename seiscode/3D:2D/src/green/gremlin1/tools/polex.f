c this is <polex.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2011 by Thomas Forbriger (IfG Stuttgart)
c
c calculate polynomial expansion coefficients
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
c    02/01/98   V1.0   Thomas Forbriger
c    24/01/99   V1.1   added mit of section feature
c    08/08/06   V1.2   support gradient too
c
c==============================================================================
c
      program polex
c
      character*79 version
      parameter(version=
     &  'POLEX   V1.2   calculate polynomial expansion coefficients')
c
c calc
      integer mord, mdim, nord
      parameter(mord=3,mdim=12)
      real matrix(mdim,mdim), a(mdim), 
     &  p(mord), rs(mdim), x(mord), z(mord), res(mord)
      real ztop,zbottom,zmid
      integer deriv(mord)
c 
      logical anothergame
      character*80 line
      integer i,j
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=2)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
      logical linear
c debugging
      logical debug
c here are the keys to our commandline options
      data optid/2h-d,2h-l/
      data opthasarg/2*.FALSE./
      data optarg/2*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
      print *,version
      print *,'Usage: polex [-l]'
      print *,'   or: polex -help'
c
c      if (iargc().lt.1) stop 'ERROR: missing arguments'
      if (iargc().ge.1) then
        call getarg(1, argument)
        if (argument(1:5).eq.'-help') then
          print *,' '
          print *,'calculate polynomial expansion coefficients'
          print *,' '
          print *,'polynomial: y(x)=c(0)*x+c(1)*x+c(2)*x**2'
          print *,' '
          print *,'-l   use linear function (polnymial of degree 1):'
          print *,'     y(x)=c(0)+c(1)*x'
          print *,' '
          print *,'x is calculated from coordinates as follows:'
          print *,'  zmid=0.5*(ztop+zbottom)'
          print *,'  x=z-zmid'
          print *,' '
          print *,'Find polynomial coefficients to satisfy data values.'
          print *,'Provide three data values in the format:'
          print *,' '
          print *,'   z  p  d'
          print *,' '
          print *,'  z: coordinate value to calculate x from'
          print *,'  d: =0: p is the polynomial value'
          print *,'     =1: p is the first derivative of the polynomial'
          stop
        endif
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      linear=optset(2)
c
c------------------------------------------------------------------------------
c go
c
      nord=mord
      if (linear) nord=2
      anothergame=.true.
      do while (anothergame)
c 
        print *,'enter reference coordinates (top, bottom):'
        read (5, *, err=99, end=98) ztop,zbottom
        zmid=0.5d0*(ztop+zbottom)
        print *,'mid is ',zmid
c    
        print *,'enter z, p, d (',nord,' times):'
        print *,' '
        do i=1,nord
          read (5, *, err=99, end=98) z(i), p(i), deriv(i)
          x(i)=z(i)-zmid
          rs(i)=p(i)
        enddo
c 
        do i=1,nord
          if (deriv(i).eq.0) then
            do j=1,nord
              matrix(i, j)=x(i)**(j-1)
            enddo
          else
            deriv(i)=1
            do j=1,nord
              matrix(i, j)=(j-1)*x(i)**(j-2)
            enddo
          endif
        enddo
c 
        print *,' '
        do i=1,nord
          print 53,(matrix(i,j), j=1,nord)
        enddo
c 
        call tf_gauss(matrix, nord, rs, a)
c 
        print 51, nord-1
        do i=1,nord
          print 50, i-1,a(i)
        enddo
c 
        do i=1,nord
          res(i)=0.
          do j=1,nord
            if (deriv(i).eq.0) then
              res(i)=res(i)+x(i)**(j-1)*a(j)
            else
              res(i)=res(i)+x(i)**(j-2)*a(j)*(j-1)
            endif
          enddo
        enddo
c 
        do i=1,nord
          print 52,deriv(i),x(i),res(i),p(i)
        enddo
c 
        print *,' '
        print *,'do you want another game? (yes/no)'
        read (5, '(a80)') line
        if (line(1:1).ne.'y') anothergame=.false.
c 
      enddo
c 
      stop
   50 format(' c(',i1,') = ',g20.10)
   51 format(/' y(x) = sum_(i=0)^(',i1,') c(i) * x**i')
   52 format(' y^(',i1,') (',g12.5,') = ',g12.5,'   -   sould be ',g12.5)
   53 format(3(2x,g12.5))
   99 stop 'ERROR: reading input line'
   98 stop 'ERROR: unexpected end of input line'
      end
c
c ----- END OF polex.f -----
