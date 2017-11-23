c this is <dat_rweight.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c read weight file for green data
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
c    24/03/98   V1.0   Thomas Forbriger
c
c----------------------------------------------------------------------
c
      subroutine dat_rweight(filename)
c
c read a file containing data weights
c has to be called after dat_rgreen
c 
      character filename*(*)
c 
c get common blocks
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_verbose.inc'
c 
cE
      integer inmagic, cpu, match
      character*4 cmagic, incmagic
      parameter(cmagic='123S')
      equivalence(incmagic, inmagic)
      integer lu
      parameter(lu=20)
      integer i,j
c 
      if (verb_subaction) print *,'ENTER dat_rweight(',filename,')'
c 
      if (verb_io) print *,
     &  'NOTICE (dat_rweight): read green data weights from file ',
     &  filename(1:index(filename, ' '))
      open(lu, file=filename, form='unformatted', status='old', err=99)

c check byte sex
      read(lu, err=98, end=97) inmagic
      call tf_bytesex(cmagic, inmagic, cpu, match)
      if (cpu.eq.1) then
        if (verb_io) print *,'NOTICE (dat_rweight): running on Intel...'
      elseif (cpu.eq.2) then
        if (verb_io) print *,'NOTICE (dat_rweight): running on Motorola...'
      else
        stop 'ERROR: unknown processor type...'
      endif
      if (match.eq.1) then
        print *,'NOTICE (dat_rweight): matching bytesex - good...'
      elseif (match.eq.2) then 
        print *,
     &     'NOTICE (dat_rweight): bytesex not matching - we will have to swap!'
        stop 'ERROR: do not know how to do that...'
      else
        close(lu, err=96)
        print *,'NOTICE (dat_rweight): bytesex read is ',incmagic
        stop 'ERROR (dat_rweight): bytesex is unkown - oh oh...'
      endif
c 
      read(lu, err=98, end=97) i, j
      if (i.ne.data_nfre) stop 'ERROR (dat_rweight): file dimensions do not fit'
      if (j.ne.data_nslo) stop 'ERROR (dat_rweight): file dimensions do not fit'
      read(lu, err=98, end=97) ((rgweight(j,i), i=1,data_nfre), j=1,data_nslo)
      close(lu, err=96)
c 
      if (verb_io) print *,'NOTICE (dat_rweight): file read and closed...'
c 
      if (verb_subaction) print *,'LEAVE dat_rweight'
c
      return
   99 stop 'ERROR (dat_rweight): opening green weight file'
   98 stop 'ERROR (dat_rweight): reading green weight file'
   97 stop 'ERROR (dat_rweight): reading green weight file - unexpected end'
   96 stop 'ERROR (dat_rweight): closing green weight file'
      end
c
c ----- END OF dat_rweight.f -----
