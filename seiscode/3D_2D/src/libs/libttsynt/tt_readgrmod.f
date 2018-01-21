c this is <tt_readgrmod.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Coypright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c read model format defined by Gunther Reimann
c
c REVISIONS and CHANGES
c    16/09/99   V1.0   Thomas Forbriger
c    21/03/02   V1.1   get it compiled
c
c==============================================================================
c
      subroutine tt_readgrmod(filename,x,phitop,verbose)
c 
c x:        offset between both shotpoints
c phitop:   dip angle of the free surface against the horizontal
c verbose:  be verbose
c
c declare parameters
      character filename*(*)
      real x, phitop
      logical verbose
      include 'tt_dim.inc'
      include 'tt_model.inc'
c
cE
c declare local variables
      integer i, lu, n
      parameter(lu=10)
c
c------------------------------------------------------------------------------
c go
c
      nlay=0
      do n=1,maxlay
        d(n)=0.
        phi(n)=0.
        v(n)=0.
      enddo
c 
      backoff=x
      phi(1)=phitop
      d(1)=0.
c 
      if (verbose) print *,'going to read ''',
     &  filename(1:index(filename,' ')-1),''''
      open(lu,file=filename,err=99,status='old')
      read(lu,'(/i20//)',err=98) nlay
      if (nlay.gt.(maxlay-1))  
     &   stop 'ERROR (tt_readgrmodel): too many layers'
      do i=1,nlay
        read(lu,*,err=98) d(i+1),v(i),phi(i+1)
      enddo
      read(lu,'(10x,f20.10)',err=98) v(nlay+1)
      close(lu,err=97)
      if (verbose) print *,'file read and closed'
c 
c be verbose
      if (verbose) then
        print 50,'i','d [m]','v [km/s]','phi [°]'
        do i=1,nlay+1
          print 51,i,d(i),v(i),phi(i)
        enddo
      endif
c 
      return
   50 format(/'read model:'/a5,3(a10,2x))
   51 format(i5,3(f10.5,2x))
   99 stop 'ERROR (tt_readgrmodel): opening file'
   98 stop 'ERROR (tt_readgrmodel): reading file'
   97 stop 'ERROR (tt_readgrmodel): closing file'
      end
c
c ----- END OF tt_readgrmod.f -----
