c this is <dat_rtt.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c read picked travel times
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
c    04/03/99   V1.1   set merge index
c    14/06/99   V1.2   data_ttsplit must be reset here and set in dat_mtt.f
c
c==============================================================================
c
      subroutine dat_rtt(filename)
c
c read travel time data from file
c
      character filename*(*)
c
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_verbose.inc'
c
cE
      integer lu, i
      parameter(lu=18)
c 
      if (verb_subaction) print *,'ENTER dat_rtt(',filename,')'
c 
      if (verb_io) print *,'NOTICE (dat_rtt): read travel time data from ',
     &  filename(1:index(filename, ' '))
c 
      open(lu, file=filename, status='old', err=99)
      read(lu, '(/i10)', err=98, end=97) data_ntts
      if (data_ntts.gt.glqd_mtts) stop
     &  'ERROR (dat_rtt): too many travel time samples'
      read(lu, *, err=98, end=97) (travx(i), travt(i, di_read), i=1,data_ntts)
      close(lu, err=96)
      if (verb_io) print *,'NOTICE (dat_rtt): file read and closed...'
c 
c set default weights
      do i=1,data_ntts
        rtweight(i)=1.
      enddo
c
      data_ttsplit=0
c 
      if (verb_subaction) print *,'LEAVE dat_rtt'
c 
      return
   99 stop 'ERROR (dat_rtt): opening file'
   98 stop 'ERROR (dat_rtt): reading file'
   97 stop 'ERROR (dat_rtt): reading file - unexpected end'
   96 stop 'ERROR (dat_rtt): closing file'
      end
c
c ----- END OF dat_rtt.f -----
