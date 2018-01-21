c this is <Ffcommand.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c evaluate filters specified in text form
c
c ----
c libfourier is free software; you can redistribute it and/or modify
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
c
c REVISIONS and CHANGES
c    11/11/2002   V1.0   Thomas Forbriger
c    28/06/2016   V1.1   discard function foucmd_revision
c
cS
c ============================================================================
c
      subroutine foucmd_help(verbose)
c 
      logical verbose
c
c print command definitions
c
c verbose: tell library code revision
c
cE
c
      print *,'filter library commands:'
      print 50,'rem',' ',        'any comment (ignored)'
      print 50,'#  ',' ',        'any comment (ignored)'
      print 50,'dif',' ',        'differentiate'
      print 50,'int',' ',        'integrate'
      print 50,'lp2','in,h',
     &  'second order low-pass (see foufil_lp2)'
      print 50,'hp2','in,h',     
     &  'second order high-pass (see foufil_hp2)'
      print 50,'lpb','in,ord',   
     &  'Butterworth low-pass of order ord (see foufil_lpb)'
      print 50,'hpb','in,ord',   
     &  'Butterworth high-pass of order ord (see foufil_hpb)'
      print 50,'fac','factor',   'mutiply by factor'
      print 50,'mod','normal',   
     &  'switch back to normal filters (default)'
      print 50,'mod','inverse',  'switch to inverse filters'
      print 50,'mod','period',   
     &  'switch to specification by period (default)'
      print 50,'mod','frequency','switch to specification by frequency '
      print 50,'end',' ',        'terminate file reading'
c
      return
   50 format(1x,a3,1x,a,t18,1x,a)
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine foucmd_command(line)
c
c evaluate filter specification given as a line of text
c
c rem             any comment (ignored)
c #               any comment (ignored)
c dif             differentiate
c int             integrate
c lp2 in,h        second order low-pass (see foufil_lp2)
c hp2 in,h        second order high-pass (see foufil_hp2)
c lpb in,ord      Butterworth low-pass of order ord (see foufil_lpb)
c hpb in,ord      Butterworth high-pass of order ord (see foufil_hpb)
c fac factor      mutiply by factor
c mod normal      switch back to normal filters (default)
c mod inverse     switch to inverse filters
c mod period      switch to specification by period (default)
c mod frequency   switch to specification by frequency 
c
      character*(*) line
c
cE
      double precision v1,v2
      integer i1
c
      if ((line(1:3).ne.'rem').and.
     &    (line(1:1).ne.'#')) then
        if (line(1:4).eq.'mod ') then
          if (line(5:8).eq.'norm') then
            call fou_normal
          elseif (line(5:8).eq.'inve') then
            call fou_inverse
          elseif (line(5:8).eq.'freq') then
            call foufil_frequency
          elseif (line(5:8).eq.'peri') then
            call foufil_period
          else
            print *,'WARNING (foucmd_command): unknown mode'
            print *,line
          endif
        elseif (line(1:3).eq.'dif') then
          call foufil_dif
        elseif (line(1:3).eq.'int') then
          call foufil_int
        elseif (line(1:4).eq.'fac ') then
          read(line(5:), *, err=99,end=98) v1
          call fou_numer(v1)
        elseif (line(1:4).eq.'lp2 ') then
          read(line(5:), *, err=99,end=98) v1,v2
          call foufil_lp2(v1,v2)
        elseif (line(1:4).eq.'hp2 ') then
          read(line(5:), *, err=99,end=98) v1,v2
          call foufil_hp2(v1,v2)
        elseif (line(1:4).eq.'lpb ') then
          read(line(5:), *, err=99,end=98) v1,i1
          call foufil_lpb(v1,i1)
        elseif (line(1:4).eq.'hpb ') then
          read(line(5:), *, err=99,end=98) v1,i1
          call foufil_hpb(v1,i1)
        else
          print *,'WARNING (foucmd_command): unknown command'
          print *,line
        endif
      endif
c
      return
   99 stop 'ERROR (foucmd_command): reading arguments'
   98 stop 'ERROR (foucmd_command): too few arguments'
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine foucmd_unit(lu, verbose)
c
c read filter specifications from logical file unit lu
c
c end             terminates reading
c
c NOTICE: this subroutine does NOT call any of the initialization routines
c
      integer lu
      logical verbose
c
cE
      character*79 line
      line='    '
      do while (line(1:3).ne.'end')
        read(lu, '(a79)', err=99, end=98) line
        if (verbose) print *,line
        if (line(1:3).ne.'end') call foucmd_command(line)
      enddo
c
      return
   99 stop 'ERROR (foucmd_unit): reading line'
   98 stop 'ERROR (foucmd_unit): unexpected end'
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine foucmd_file(filename, verbose)
c
c read filter specifications from file
c
c NOTICE: this subroutine does NOT call any of the initialization routines
c
      character*(*) filename
      logical verbose
c
cE
      integer lu
      parameter(lu=21)
      if (verbose) print *,'read filter commands from ',
     &  filename(1:index(filename,' ')-1)
      open(lu, file=filename, err=99)
      call foucmd_unit(lu, verbose)
      close(lu, err=98)
c
      return
   99 stop 'ERROR (foucmd_file): opening file'
   98 stop 'ERROR (foucmd_file): closing file'
      end
c
c ----- END OF Ffcommand.f ----- 
