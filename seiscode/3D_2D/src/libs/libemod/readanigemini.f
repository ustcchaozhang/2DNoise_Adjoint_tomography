c this is <readanigemini.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
c Copyright (c) 1997 by Joerg Dalkolmo (IfG Stuttgart)
c
c read transversal isotropic gemini model
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
c    16/06/2005   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine gemini_getani(filename, lu, maxlayer, eta,
     &  rb, qm, qk, rho, vpv, vph, vsv, vsh, nlayer, iflso, nco, text)
c 
c this routines reads a gemini model file
c
      character filename*(*), text*(*)
      integer nlayer, maxlayer, lu
      double precision qm(maxlayer), qk(maxlayer), rho(maxlayer, 4)
      double precision vpv(maxlayer, 4), vsv(maxlayer, 4)
      double precision vph(maxlayer, 4), vsh(maxlayer, 4)
      double precision eta(maxlayer, 4)
      double precision rb(0:maxlayer)
      integer iflso(maxlayer), nco(maxlayer)
cE
      character cnlay*2,form*11
      double precision fref, rboc
      integer i,j,ifanis,nloc
      character junk*80
c
      open(lu,file=filename,status='old',err=99)
c
c------------------------------------------------------
c code was taken from:
c
c              G T M A N I S 
c
c Reads tranversal isotropic Earth model parameters described by polynomials
c from file.
c See comments at the read-statements for the meaning of the parameter.
c
c------------------------------------------------------

      do i=1,maxlayer
        qm(i)=0.d0
        qk(i)=0.d0
        rb(i)=0.d0
        iflso(i)=0
        nco(i)=0
        do j=1,4
          rho(i,j)=0.d0
          vpv(i,j)=0.d0
          vph(i,j)=0.d0
          vsv(i,j)=0.d0
          vsh(i,j)=0.d0
          eta(i,j)=0.d0
        enddo
      enddo

c          Read header with comments and print them on stdout
c          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      text='-'
  1   read(lu,'(a72)') junk
      if (text.eq.'-') text=junk
      if (junk(1:1).eq.'#') then
c       print '(a72)', text
        goto 1
      endif
      backspace lu


      read(lu,'(i2)') nlayer                ! Number of layers
      if (nlayer.gt.maxlayer) stop 'ERROR: too many layers!'

      write(cnlay,'(i2)') nlayer               ! 
      form='('//cnlay//'i2)'                 ! Number of polynomal
      read(lu,form) (nco(i),i=1,nlayer)     ! coefficients for each layer 
      
      read(lu,*) fref               ! reference frequency of Qs in Hertz
      read(lu,*) ifanis             ! Transversal isotropic? 1=y, else=n
      read(lu,'(1x/1x/)')


c  Reading radii, density, P-velo-vert, P-velo-hori, S-velo-vert, 
c          S-velo-hori, Qmu, Qkappa, eta

      do i = 1, nlayer

        read(lu,*) rb(i-1),rho(i,1),vpv(i,1),vph(i,1),vsv(i,1),vsh(i,1),
     &                qm(i),qk(i),eta(i,1)
c       write(88,*) rb(i-1),rho(i,1),vpv(i,1),vph(i,1),vsv(i,1),vsh(i,1),
c     &                qm(i),qk(i),eta(i,1)

        if (qm(i).le.0.) then          !
            qm(i) = 0.                 !
            iflso(i)=1                 !
            nloc = i                   ! layer number of outer core
        else                           ! 
            iflso(i)=0                 !
        endif                          ! 

        do j = 2, nco(i)               ! polynomal coefficients
        read(lu,*) rho(i,j),vpv(i,j),vph(i,j),vsv(i,j),vsh(i,j),eta(i,j)
c       write(88,*) rho(i,j),vpv(i,j),vph(i,j),vsv(i,j),vsh(i,j),eta(i,j)
        enddo
        read(lu,'(1x)')
c       write(88,'(1x)')

      enddo


      read(lu,*) rb(nlayer)           ! Get Earth radius

c  Check for ocean.
      if(iflso(nlayer).eq.1) stop '~~~~ I don`t like oceans. ~~~~'
      rboc = rb(nloc)                  ! Radius of outer core

      close(lu)
      return
   99 stop 'ERROR (gemini_getani): opening file'
      end
c
c ----- END OF readanigemini.f ----- 
