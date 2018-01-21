c this is <fapidxx.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
c
c some functions/subroutines for compatibility with libfapidxx
c
c ----
c This file is part of libsff.
c
c libsff is free software; you can redistribute it and/or modify
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
c    26/11/2010   V1.0   Thomas Forbriger
c    23.12.2010   V1.1   distinguish input and output format
c
c ============================================================================
c
cD
c----------------------------------------------------------------------
      subroutine sff_select_input_format(formatid, ierr)
c 
c Select a file format for the next data file to be opened
c
c input:
c   filename        name of file
c output:
c   ierr            error status (ok: ierr=0)
c
      integer ierr
      character formatid*(*)
cE
      if (formatid.ne.'sff') then
        print *,'ERROR (libsff, sff_select_input_format):',
     &    ' only SFF is supported'
        ierr=1
      else
        ierr=0
      endif
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_select_output_format(formatid, ierr)
c 
c Select a file format for the next data file to be opened
c
c input:
c   filename        name of file
c output:
c   ierr            error status (ok: ierr=0)
c
      integer ierr
      character formatid*(*)
cE
      if (formatid.ne.'sff') then
        print *,'ERROR (libsff, sff_select_output_format):',
     &    ' only SFF is supported'
        ierr=1
      else
        ierr=0
      endif
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_select_format(formatid, ierr)
c 
c Select a file format for the next data file to be opened
c
c input:
c   filename        name of file
c output:
c   ierr            error status (ok: ierr=0)
c
      integer ierr
      character formatid*(*)
cE
      if (formatid.ne.'sff') then
        print *,'ERROR (libsff, sff_select_format):',
     &    ' only SFF is supported'
        ierr=1
      else
        ierr=0
      endif
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_help_formats()
c 
c Print online help
c
cE
      print *,'This program is linked against the Fortran version of'
      print *,'libsff. Only generic SFF data is supported.'
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_help_details()
c 
c Print online help
c
cE
      real sff_encode_libversion, sff_libversion
      print *,'The Fortran version of libsff can only handle ',
     &  'generic SFF data'
      print *,'The library is able to read data of SFF version ',
     &  sff_libversion()
      print *,'The library encodes data to SFF version ',
     &  sff_encode_libversion()
      return
      end
c
c ----- END OF fapidxx.f ----- 
