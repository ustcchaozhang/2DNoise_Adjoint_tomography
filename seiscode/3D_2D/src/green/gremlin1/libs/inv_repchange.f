c this is <inv_repchange.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c report parameter changes
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
c    24/01/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine inv_repchange
c
      include 'glq_dim.inc'      
      include 'glq_model.inc'      
c
cE
c declare local variables
      real relchange(glqm_mano)
      integer firstpar
      integer chainofpars(glqm_mano)
      integer i, nsec, npar, npol, rank
      character*30 parname
c
c go
      do i=1,mod_n
        relchange(i)=abs(mdelta(i)/mweight(i))
      enddo
c 
      call tf_rchain(relchange, chainofpars, mod_n, firstpar, -1)
c 
      print 50,'rank','par.no.','name','sec.','ord.',
     &         'abs.change','rel.change'
c 
      rank=1
      i=firstpar
      do while (i.gt.0)
        call mod_identify(i, nsec, npol, npar, parname)
        print 51,rank,i,parname(1:index(parname,' ')),nsec,npol-1,
     &    mdelta(i),relchange(i)
        i=chainofpars(i)
        rank=rank+1
      enddo
c
      return
   50 format('parameter changes to be applied:',/
     &       2x,a4,2x,a7,2x,a12,2x,a5,2x,a5,2x,a12,2x,a11)
   51 format(2x,i4,2x,i7,2x,a13,1x,i5,2x,i5,2x,f12.6,2x,2pf10.2,1h%)
      end
c
c ----- END OF inv_repchange.f -----
