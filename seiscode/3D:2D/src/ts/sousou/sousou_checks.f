c this is <sousou_checks.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c perform plausibility checks
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
c    17/11/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine inpchecks
c
      include 'sousou_dim.inc'
      include 'sousou_data.inc'
      include 'sousou_options.inc'
c
      integer i
c 
      if (opt_verbose.gt.0) then
        print *,' '
        print *,'perform input dataset checks'
      endif
c
c check sampling intervals
      do i=1,ntraces
        if (abs((dt(i)/dt(1))-1.).gt.0.001) 
     &    stop 'ERROR: sampling interval differs more than 0.1%'
      enddo
c 
      if (opt_verbose.gt.0) 
     &  print *,'  sampling intervals are ok...'
c
c check number of samples
      do i=1,ntraces
        if (nsamples(i).ne.nsamples(1))
     &    stop 'ERROR: traces have different numbers of samples'
      enddo
c 
      if (opt_verbose.gt.0) 
     &  print *,'  numbers of samples are ok...'
c
      return
      end
c
c ----- END OF sousou_checks.f -----
