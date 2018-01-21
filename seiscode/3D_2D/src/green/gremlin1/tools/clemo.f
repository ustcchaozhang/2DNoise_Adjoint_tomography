c this is <clemo.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1997, 2011 by Thomas Forbriger (IfG Stuttgart)
c
c just rewrite a model file
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
c    02/12/97   V1.0   Thomas Forbriger
c    03/12/97   V1.1   emty model generation
c
c==============================================================================
c
      program clemo

      character*79 version
      parameter(version='CLEMO   V1.1    create/rewrite model')

      integer iargc, n
      character*80 filename, nchar

      print *,version
      if ((iargc().ne.1).and.(iargc().ne.2)) then
        print *,'Usage: clemo filename [n]'
        print *,' '
        print *,'filename     file to be rewritten'
        print *,'n            an empty model with n section will be created'
        stop 'ERROR: arguments?'
      endif

      call getarg(1, filename)
      call par_setdefverb

      if (iargc().eq.1) then
        call mod_read(filename, 1)
        call mod_follow(1)
        print *,' '
        call mod_save(filename, 1, .true., version)
      else
        call getarg(2, nchar)
        read(nchar, *) n
        call mod_clear(n, 1)
        print *,' '
        call mod_save(filename, 1, .false., version)
      endif

      stop
      end
c
c ----- END OF clemo.f -----
