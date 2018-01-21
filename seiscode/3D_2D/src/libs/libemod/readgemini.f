c this is <readgemini.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
c Copyright (c) 1997 by Joerg Dalkolmo (IfG Stuttgart)
c
c this file contains routines to read GEMINI related file
c
c gemini 2.2 is put into the public domain at
c http://www.quest-itn.org/library/software/gemini-greens-function-of-the-earth-by-minor-integration
c
c No copying, usage or modification restrictions are given in
c gemini-2.2.tgz
c
c The file in its current form is put under the 
c GNU General Public License
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
c    01/01/1997   V1.0   Thomas Forbriger
c    11/12/1999   V1.1   declare ifoc and nlay
c    23/02/2009   V1.2   modification to satisfy gfortran
c
c ============================================================================
c
cS
      subroutine gemini_getmod(filename, lu, maxlayer, 
     &  rb, qm, qk, rho, alpha, beta, nlayer, iflso, nco, text)
c 
c this routines reads a gemini model file
c
      character filename*(*), text*72
	integer nlayer, maxlayer, lu
      double precision qm(maxlayer), qk(maxlayer), rho(maxlayer, 4)
      double precision alpha(maxlayer, 4), beta(maxlayer, 4)
      double precision rb(0:maxlayer)
      integer iflso(maxlayer), nco(maxlayer), ifoc
cE
      character cnlay*2,form*11
      integer i,j
c
	open(lu,file=filename,status='old',err=99)
c
c Read header
c
      read(lu,'(/a72//i2)', err=98, end=97) text,nlayer
      print *,' '
      print *,'reading ',text
      print *,'from file ',filename(1:index(filename,' ')-1)
      write(cnlay,'(i2)') nlayer
      form='('//cnlay//'i3)'
      read(lu,form, err=98, end=97) (iflso(i),i=1,nlayer)
      read(lu,form, err=98, end=97) (nco(i),i=1,nlayer)
      read(lu,'(1x)', err=98, end=97) 
      read(lu,'(1x/)', err=98, end=97)
c
      do i=1,4
       do j=1,maxlayer
        alpha(j,i)=0.d0
        beta(j,i)=0.d0
        rho(j,i)=0.d0
       enddo
      enddo
      do j=1,maxlayer
        qm(j)=0.d0
        qk(j)=0.d0
      enddo
c
c Reading radii and polynom coefficients
c
      do i=1,nlayer
        read(lu,21, err=98, end=97) 
     &    rb(i-1),rho(i,1),alpha(i,1),beta(i,1),qm(i),qk(i)
        do j=2,nco(i)
          read(lu,22, err=98, end=97)
     &      rho(i,j),alpha(i,j),beta(i,j)
        enddo
      enddo
      continue
 21   format(/f10.3,f10.4,2f11.5,f9.1,f10.1)
 22   format(10x,f10.4,2f11.5)
c
      read(lu,'(/f10.3)', err=98, end=97) rb(nlayer)
      ifoc=iflso(nlayer)
c
	close(lu, err=96)
      return
   99 stop 'gemini_getmod: ERROR opening modelfile'
   98 stop 'gemini_getmod: ERROR reading modelfile'
   97 stop 'gemini_getmod: ERROR reading modelfile - unexpected end of file'
   96 stop 'gemini_getmod: ERROR closing modelfile'
      end
cS
c----------------------------------------------------------------------
      integer function gemini_layer(rb, r, maxlayer, nlayer)
c
c return index of layer corresponding to radius r
c
      integer nlayer, maxlayer
      double precision rb(0:maxlayer), r
cE
      integer i, nl
      nl=nlayer
      i=nlayer-1
    1 if (r.lt.rb(i)) then
        nl=i
        i=i-1
        goto 1
      endif
      gemini_layer=nl
      return
      end
cS
c----------------------------------------------------------------------
      double precision function gemini_par(par, rb, r, maxlayer, nlayer)
c
c return parameter value of array par at depth r
c
      integer maxlayer, nlayer
      double precision par(maxlayer, 4), rb(0:maxlayer), r
      integer gemini_layer
cE
      double precision x, param
      integer nl, i
      nl=gemini_layer(rb, r, maxlayer, nlayer)
      x=r/rb(nlayer)
      param=par(nl, 4)
      do i=3,1,-1
        param=par(nl, i)+x*param
      enddo
      gemini_par=param
      return
      end
c
c
c ----- END OF readgemini.f ----- 
