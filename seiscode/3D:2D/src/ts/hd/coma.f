c this is <coma.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
c create coordinate table
c
c REVISIONS and CHANGES
c    17/12/2010   V1.0   Thomas Forbriger
c
c ============================================================================
c
c a quick hack to generate files containing 3D coordinates
c 
      program coma
      integer n,i,iargc
      real x1,x2,x3,d1,d2,d3
      character*30 a,b,c
c
      if (iargc().ne.3) then
        print *,'Usage: coma x1,x2,x3 d1,d2,d3 n'
        print *,' '
        print *,'  x1,x2,x3  3D vector for the first coordiante triple'
        print *,'  d1,d2,d3  3D vector pointing to the next coordinate'
        print *,'            triple'
        print *,'  n         number of triples to write to stdout in free'
        print *,'            format'
        print *,' '
        stop 'ERROR: wrong number of arguments'
      endif
      call getarg(1,a)
      call getarg(2,b)
      call getarg(3,c)
      read(a, *) x1,x2,x3
      read(b, *) d1,d2,d3
      read(c, *) n
      do 1 i=0,n-1
        write(6, '(3(e15.9,1x))', err=99) x1+i*d1, x2+i*d2, x3+i*d3
    1 continue
      stop
   99 stop 'ERROR (coma): writing to stdout'
      end
c
c ----- END OF coma.f ----- 
