c this is <tfgmt_makexyz.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1999 by Thomas Forbriger (IfG Stuttgart)
c
c create xyz-dataset file from two dimensional array
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
c    19/01/99   V1.0   Thomas Forbriger
c
c==============================================================================
cS
c
      subroutine tfgmt_makexyz(xyzfile, data, xdim, ydim, nx, ny,
     &  x0, y0, dx, dy, order)
c
c The two dimensional array data defines an x,y,z-dataset with
c   z=data(i,j)     if order is .true.
c   z=data(j,i)     if order is .false.
c   x=x0+(i-1)*dx
c   y=y0+(i-1)*dy
c
c The array is expected to be declared as data(xdim, ydim)
c 
c The used values will be 
c    1 <= i <= nx
c    1 <= j <= ny
c
c declare parameters
      character*(*) xyzfile
      integer xdim, ydim, nx, ny
      real data(xdim, ydim)
      real x0, y0, dx, dy
      logical order
c 
cE
c declare local variables
      real xl,yl
      integer i,j,lu
      parameter(lu=10)
c
c------------------------------------------------------------------------------
c go
c
      open(lu, file=xyzfile, status='new', err=99)
c 
      xl=x0+dx*(nx-1)
      yl=y0+dy*(ny-1)
c 
      write(lu, 50) nx,ny,dx,dy,x0,xl,y0,yl
c 
      if (order) then
        do j=ny,1,-1
          do i=1,nx
            write(lu, *, err=98) data(i,j)
          enddo
        enddo
      else
        do j=ny,1,-1
          do i=1,nx
            write(lu, *, err=98) data(j,i)
          enddo
        enddo
      endif
c
      close(lu, err=97)
c 
      return
   99 stop 'ERROR (tfgmt_makexyz): opening xyz file'
   98 stop 'ERROR (tfgmt_makexyz): writing xyz file'
   97 stop 'ERROR (tfgmt_makexyz): closing xyz file'
   50 format('GMT single column xyz-file created by tfgmt_makexyz',/
     &       'header:',t10,8x,'10',/
     &       'nx:',t10,i10,/
     &       'ny:',t10,i10,/
     &       'dx:',t10,g10.4,/
     &       'dy:',t10,g10.4,/
     &       'x0:',t10,g10.4,/
     &       'xl:',t10,g10.4,/
     &       'y0:',t10,g10.4,/
     &       'yl:',t10,g10.4)
      end
c
c ----- END OF tfgmt_makexyz.f -----
