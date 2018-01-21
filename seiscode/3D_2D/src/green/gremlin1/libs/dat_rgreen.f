c this is <dat_rgreen.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c==============================================================================
c
c
cS
c----------------------------------------------------------------------
c
      subroutine dat_rgreen(filename)
c
c read a file containing a green matrix derived from real data
c 
c sets data weights to 1.
c
      character filename*(*)
c 
c get common block
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_verbose.inc'
c 
cE
      integer inmagic, cpu, match
      character*4 cmagic, incmagic
      parameter(cmagic='1234')
      equivalence(incmagic, inmagic)
      integer lu
      parameter(lu=20)
      integer i,j
      real pi2  
      parameter(pi2=2.*3.14159265)
c 
      if (verb_subaction) print *,'ENTER dat_rgreen(',filename,')'
c 
      if (verb_io) print *,'NOTICE (dat_rgreen): read green data from ',
     &  filename(1:index(filename, ' '))
c 
      open(lu, file=filename, form='unformatted', status='old', err=99)

c check byte sex
      read(lu, err=98, end=97) inmagic
      call tf_bytesex(cmagic, inmagic, cpu, match)
      if (cpu.eq.1) then
        if (verb_io) print *,'NOTICE (dat_rgreen): running on Intel...'
      elseif (cpu.eq.2) then
        if (verb_io) print *,'NOTICE (dat_rgreen): running on Motorola...'
      else
        stop 'ERROR: unknown processor type...'
      endif
      if (match.eq.1) then
        if (verb_io) print *,'NOTICE (dat_rgreen): matching bytesex - good...'
      elseif (match.eq.2) then 
c      if (match.eq.2) then 
        print *,
     &     'NOTICE (dat_rgreen): bytesex not matching - we will have to swap!'
        stop 'ERROR: do not know how to do that...'
      else
        close(lu, err=96)
        print *,'NOTICE (dat_rgreen): bytesex read is ',incmagic
        stop 'ERROR (dat_rgreen): bytesex is unkown - oh oh...'
      endif
c 
      read(lu, err=98, end=97) data_nfre, data_nslo
      if ((data_nfre.gt.glqd_mfre).or.(data_nslo.gt.glqd_mslo)) then
        close(lu, err=96)
        stop 'ERROR (dat_rgreen): data exceeds array bounds'
      endif
      read(lu, err=98, end=97) (read_dat_om(i), i=1,data_nfre),
     &                         (read_dat_slo(i), i=1,data_nslo)
      read(lu, err=98, end=97) 
     &  ((readgreen(j,i), i=1,data_nfre), j=1,data_nslo)
      close(lu, err=96)
c 
      do i=1,data_nfre
        dat_om(i)=read_dat_om(i)
        do j=1,data_nslo
          green(j,i,di_read)=readgreen(j,i)
        enddo
      enddo
c
c scale slowness values to s/km
      do i=1,data_nslo
        dat_slo(i)=read_dat_slo(i)*1.e3
      enddo
c
c calculate frequencies
      do i=1,data_nfre
        dat_fre(i)=dat_om(i)/pi2
      enddo
c 
c scale green values to units km and g/ccm
      do i=1,data_nslo
        do j=1,data_nfre
          green(i,j,di_read)=green(i,j,di_read)*1.e-6
        enddo
      enddo
      if (verb_io) print *,'NOTICE (dat_rgreen): green file read and closed'
c 
c set error estimates (data weight) to 1.
      do i=1,data_nslo
        do j=1,data_nfre
          rgweight(i,j)=1.
        enddo
      enddo
c 
      if (verb_subaction) print *,'LEAVE dat_rgreen'
c
      return
   99 stop 'ERROR (dat_rgreen): opening green file'
   98 stop 'ERROR (dat_rgreen): reading green file'
   97 stop 'ERROR (dat_rgreen): reading green file - unexpected end'
   96 stop 'ERROR (dat_rgreen): closing green file'
      end
c
c ----- END OF dat_rgreen.f -----
