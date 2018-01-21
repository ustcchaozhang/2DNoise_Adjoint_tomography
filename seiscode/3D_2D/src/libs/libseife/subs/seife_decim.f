c this is <seife_decim.f>
c------------------------------------------------------------------------------
c   ($Id$)
c
c Copyright 1984 by Erhard Wielandt
c This code was part of seife.f. A current version of seife.f can be obtained
c from http://www.software-for-seismometry.de/
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
c extracted from libseife.f
c
c REVISIONS and CHANGES
c    25/10/2000   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine seife_decim(nil,par,x,n,dt,msg)
c  decimate time series by a factor up to 100
      real*8 x(n),g(100),pih,fidez
      character par*35,msg*(*)
      logical nil
      read(par,*) idez
      if(idez.gt.100) then
		write(msg,'(a)') 'ERROR: idez > 100'
		return
	endif
      pih=2.d0*datan(1.d0)
      fidez=idez
      do 3 j=1,idez
    3 g(j)=(dcos((j-idez)/fidez*pih))**2/fidez
      n=n/idez
      dt=dt*idez
      do 5 k=1,n-1
      jk=k*idez+1
      x(k)=g(idez)*x(jk)
      do 6 l=1,idez-1
    6 x(k)=x(k)+g(l)*(x(jk-idez+l)+x(jk+idez-l))
    5 continue
      do 7 j=n,2,-1
    7 x(j)=x(j-1)
      write(msg,'("dec  ",i10)') idez
      nil=.false.
      return
      end
c
c ----- END OF seife_decim.f -----
