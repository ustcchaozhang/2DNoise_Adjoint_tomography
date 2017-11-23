c this is <pg_linx2.f>
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
cS
c----------------------------------------------------------------------
c
      subroutine pg_linX2(numin,numax,npt)
c
c plot npnt points of X2(nu) from numin to numax
c use linearized forward modeling
c 
      real numin, numax
      integer npt
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_pgpara.inc'
      include 'glq_verbose.inc'
c 
cE
      integer msamp, nsamp
      parameter(msamp=50)
      real X2(msamp), param(msamp)
      real para, nu, pamax, pamin
      real X2min, X2max, fnu, fpa
      logical hot, inv_mat, inv_linX2
      integer i
      character*80 title
c
      fnu(para)=10**(-0.1*para)
      fpa(nu)=-10.*log10(nu)
c 
      if (verb_subaction) print *,'ENTER pg_linX2(',numin,',',numax,',',npt,')'
c 
c      call pg_selvp(0)
c 
      pamax=fpa(numin)
      pamin=fpa(numax)
      npt=max(5,min(msamp,npt))
c 
      if (verb_medstrategy) print *,'NOTICE (pg_linX2): ',
     &  'calculate partial derivatives'
      hot=inv_mat()
c 
      i=0
      do while ((hot).and.(i<npt))
        i=i+1
        para=pamin+(i-1)*(pamax-pamin)/(npt-1)
        nu=fnu(para)
        param(i)=para
        hot=inv_linX2(nu, X2(i))
        if (verb_medstrategy) print *,'NOTICE (pg_linX2): ',
     &    'i: ',i,'   para: ',para,'   nu: ',nu,'   X2: ',X2(i)
      enddo
c 
      nsamp=i
      if (nsamp.gt.1) then
        X2min=X2(1)
        X2max=X2(1)
        do i=1,nsamp
          X2min=min(X2min,X2(i))
          X2max=max(X2max,X2(i))
        enddo
        X2min=X2min-0.1*(X2max-X2min)
        X2max=X2max+0.1*(X2max-X2min)
c 
        call pgslw(pg_lw)
        call pgsci(pg_colind)
c 
        write (title,50) pvar_pdev
        call pgenv(pamin, pamax, X2min, X2max, 0, 2)
        call pglab('-10*log\d10\u(\gn)', '\gx\u2\d(\gn)', title)
        call pgline(nsamp, param, X2)
        call pgupdt
c 
      endif
c 
      if (verb_subaction) print *,'LEAVE pg_linX2'
c 
      return
   50 format('difference quotient over ',g10.3)
      end
c
c ----- END OF pg_linx2.f -----
