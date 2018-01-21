c this is <readrefmet.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
c
c read and reconfigure earth model from file
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
c    09/02/1997   V1.0   Thomas Forbriger
c    23/02/2009   V1.1   corrections to satisfy gfortran
c
c ============================================================================
c 
c read and reconfigure earth model from file
c
      subroutine readrefmet(modelfile, modeltext, MS,
     &  radius, n, z, alpha, beta, rho, qa, qb)
c 
      integer MS, N
      character modelfile*(*), modeltext*(*)
      double precision radius, z(0:MS), alpha(0:MS), beta(0:MS), rho(0:MS)
      real qa(0:MS), qb(0:MS)
c 
      character*80 line
      integer i, modlu, modellen
      parameter(modlu=11)
c 
      modellen=index(modelfile, ' ')-1
      print *,'READREFMET: read refmet layer model'
      print 52,'opening ',modelfile(1:modellen)
      open(modlu, file=modelfile, status='old', err=99)

      read(modlu, '(a72)', err=98, end=97) modeltext

      read(modlu, '(//30x,f10.6)', err=98, end=97) radius

      read(modlu, '(//)', err=98, end=97)

      n=0
  100 continue
        read(modlu, '(a80)', err=98, end=97) line
        if (line(1:10).ne.'halfspace:') then
          read(line, '(f10.3,5(1x,f10.6))', err=98, end=97)
     &      z(n),alpha(n),beta(n),rho(n),qa(n),qb(n)
          if (n.eq.MS) then
            print *,'WARNING: too many layers'
            print *,'WARNING: stopped model reading at depth ',z(n)
            print *,'WARNING: layer ',n,' will be used as lower halfspace'
          else
            n=n+1
            goto 100
          endif
        endif
      if (n.lt.MS) then
        read(line, '(10x,5(1x,f10.6))', err=98, end=97)
     &    alpha(n),beta(n),rho(n),qa(n),qb(n)
c depth with halfspace index will be set below
      endif
      if (n.lt.1) stop 'ERROR we need at least a top and a bottom halfspace'
        
      print 52,'closing ',modelfile(1:modellen)
      close(modlu, err=96)

c z-values must be top of layer
      do i=n,1,-1
        z(i)=z(i-1)
      enddo
c fix special case values for reflectivity use
      do i=0,n
        if (qb(i).lt.0.) qb(i)=1.
        if (beta(i).lt.1.d-5) beta(i)=1.d-5
      enddo
      return
   52 format(/a,1x,a)
   99 stop 'ERROR opening model file'
   98 stop 'ERROR reading model file'
   97 stop 'ERROR reading model file - unexpected end of file'
   96 stop 'ERROR closing model file'
      end

c
c ----- END OF readrefmet.f ----- 
