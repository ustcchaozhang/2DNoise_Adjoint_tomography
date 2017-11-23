c this is <tf_rcol.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1998 by Thomas Forbriger (IfG Stuttgart)
c
c read column orientated data (might be produced by gnuplot)
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
c    17/12/07   V1.1   repositioned label in tf_rcol2gp
c
c==============================================================================
cS
c
      subroutine tf_rcol2(filename, x, dt, n, m)
c
c will read two columns from file 'filename'
c
c x:   return values from column 2
c dt:  difference of the first two values in column 1
c n:   number of datapoints returned
c m:   dimension of x in calling program
c
      character*(*) filename
      integer n,m
      real x(m),dt
c
cE
      integer lu
      real t1, t2
      parameter(lu=10)
c 
      print *,'NOTICE (tf_rcol2): open file ',filename(1:index(filename, ' '))
      open(lu, file=filename, status='old', err=99)
c 
      n=0
      t1=0.
      do while (n.lt.m)
        n=n+1
        read(lu, *, err=98, end=1) t2, x(n)
        if (n.eq.2) dt=t2-t1
        t1=t2
      enddo
    1 continue
c 
      close(lu, err=97)
      print *,'NOTICE (tf_rcol2): file read and closed'
c 
      return
   99 stop 'ERROR (tf_rcol2): opening file'
   98 stop 'ERROR (tf_rcol2): reading file'
   97 stop 'ERROR (tf_rcol2): closing file'
      end
c----------------------------------------------------------------------
cS
c
      subroutine tf_rcol2gp(filename, x, dt, n, m)
c
c will read two columns from gnuplot-table-file 'filename'
c
c x:   return values from column 2
c dt:  difference of the first two values in column 1
c n:   number of datapoints returned
c m:   dimension of x in calling program
c
      character*(*) filename
      integer n,m
      real x(m),dt
c
cE
      integer lu, b1,e1,b2,e2
      real t1, t2
      character*80 line
      parameter(lu=10)
c 
      print *,'NOTICE (tf_rcol2gp): open file ',filename(1:index(filename, ' '))
      open(lu, file=filename, status='old', err=99)
c 
      t1=0.
      n=0
      read(lu, '(a80)', err=98, end=1) line
      if (line(1:6).ne.'Curve ') then
        print *,'ERROR (tf_rcol2pg): doesn''t seem to be a gnuplot file'
      else
        do while (n.lt.m)
          read(lu, '(a80)', err=98, end=1) line
          if (line(1:4).eq.'i x=') then
            n=n+1
            b1=index(line,'=')+1
            e1=index(line(b1:80),' ')+b1-2
            b2=index(line(b1:80),'=')+b1
            e2=index(line(b2:80),' ')+b2-2
            read(line(b1:e1), *, err=96, end=95) t2
            read(line(b2:e2), *, err=96, end=95) x(n)
            if (n.eq.2) dt=t2-t1
            t1=t2
          endif
        enddo
      endif
    1 continue
c 
      close(lu, err=97)
      print *,'NOTICE (tf_rcol2gp): file read and closed'
c 
      return
   99 stop 'ERROR (tf_rcol2gp): opening file'
   98 stop 'ERROR (tf_rcol2gp): reading file'
   97 stop 'ERROR (tf_rcol2gp): closing file'
   96 stop 'ERROR (tf_rcol2gp): reading line'
   95 stop 'ERROR (tf_rcol2gp): reading line - unexpected end'
      end
c
c ----- END OF tf_rcol.f -----
