c this is <fidase_destack.f>
c------------------------------------------------------------------------------
c
c 23/10/2001 by Thomas Forbriger (IfG Stuttgart)
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
cS
c
c destack traces
c
c REVISIONS and CHANGES
c    23/10/2001   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine destack
c 
c get common blocks
      include 'fidase_dim.inc'
      include 'fidase_data.inc'
      include 'fidase_para.inc'
c
cE
c declare local variables
      integer itrace, isample, i
      real factor
c
c------------------------------------------------------------------------------
c go
      if (verbose) print *,'destack traces'
      do itrace=1,ntraces
        if (verbose) print *,'  trace ',itrace,': ',
     &                        nstack(itrace),' stacks'
        factor=1./max(1.,float(nstack(itrace)))
        do isample=1,nsamples(itrace)
          i=firstsample(itrace)-1+isample
          data(i)=data(i)*factor
        enddo
        nstack(itrace)=1
      enddo
c
      return
      end
c
c ----- END OF fidase_destack.f -----
