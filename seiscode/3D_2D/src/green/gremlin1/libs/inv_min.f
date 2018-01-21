c this is <inv_min.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c find the minimum of X2(nu) beginning at startnu
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
c set results to common block variables
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c
      subroutine inv_min(startnu)
c
      real startnu
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_inv.inc'
      include 'glq_invres.inc'
      include 'glq_invpara.inc'
      include 'glq_verbose.inc'
c 
cE
      real thisnu, fnu, x2ref, x2this
      integer ipara, fpa, iext, istart, i
      logical hot, inv_mat, inv_X2hist, notimprove
c
c nu functions
      fnu(ipara)=10**(-0.1*ipara)
      fpa(thisnu)=-10*log10(thisnu)
c 
      if (verb_subaction) print *,'ENTER inv_min(',startnu,')'
c 
c setup work and return values
      ipara=fpa(startnu)
      istart=ipara
c 
      lq_moderror=.false.
      lq_inverror=.false.
      lq_notimprove=.false.
      found_best=.false.
c 
      lq_npts=0
c 
c set matrix
      if (verb_topstrategy) print *,'NOTICE (inv_min): ',
     &  'calculate partial derivatives'
      hot=inv_mat()
      if (hot) then
c
c look for a suitable start value
        notimprove=.true.
        if (verb_topstrategy) print *,'NOTICE (inv_min): ',
     &    'find suitable start value for nu'
        do while ((notimprove).and.(hot))
          if (verb_medstrategy) print *,'NOTICE (inv_min): ',
     &      'test: ',ipara,' and ',ipara+1
          thisnu=fnu(ipara)
          hot=inv_X2hist(thisnu, x2ref, iext)
          if ((hot).and.(verb_medstrategy)) print *,'NOTICE (inv_min): ',
     &      'ipara: ',ipara,' thisnu: ',thisnu,' X2: ',x2ref,
     &      ' iext: ',iext
          if (hot) then
            thisnu=fnu(ipara+1)
            hot=inv_X2hist(thisnu, x2this, iext)
            if ((hot).and.(verb_medstrategy)) print *,'NOTICE (inv_min): ',
     &        'ipara: ',ipara+1,' thisnu: ',thisnu,' X2: ',x2this,
     &        ' iext: ',iext
          endif
          if ((hot).and.(x2this.le.min(x2ref,lq_x2ref))) then
            x2ref=x2this
            ipara=ipara+2
            notimprove=.false.
            if (verb_medstrategy) print *,'NOTICE (inv_min): ',
     &        'found start value: ',ipara
          else
            ipara=ipara-10
            if (fnu(ipara).gt.lq_numax) then
              lq_notimprove=.true.
              hot=.false.
              if (verb_allwarn) print *,'WARNING (inv_min): ',
     &          'nu: ',fnu(ipara),' touched maximum bound: ',lq_numax
            endif
          endif
        enddo
c 
c look out for a minimum
        if (verb_topstrategy) print *,'NOTICE (inv_min): ',
     &    'find minimum'
        do while ((hot).and.(.not.(found_best)))
          thisnu=fnu(ipara)
          x2ref=x2this
          hot=inv_X2hist(thisnu, x2this, iext)
          if (hot) then
            if (verb_medstrategy) print *,'NOTICE (inv_min): ',
     &        'ipara: ',ipara,' thisnu: ',thisnu,' X2: ',x2this,
     &        ' iext: ',iext
            if (x2this.le.x2ref) then
              ipara=ipara+1
              if (fnu(ipara).lt.lq_numin) then
                hot=.false.
                if (verb_allwarn) print *,'WARNING (inv_min): ',
     &            'nu: ',fnu(ipara),' touched minimum bound: ',lq_numin
              endif
            else
              found_best=.true.
              if (verb_topstrategy) print *,'NOTICE (inv_min): ',
     &          'found minimum'
            endif
          endif
        enddo
c 
c do we want some more downgoing values?
        if ((hot).and.(iext.lt.lq_mindown)) then
          if (verb_topstrategy) print *,'NOTICE (inv_min): ',
     &      'add some decreasing values (iext: ',
     &      iext,' lq_mindown: ',lq_mindown,')'
          ipara=int(fpa(lq_parimp(1)))-1
          do i=1,(lq_mindown-iext)
            thisnu=fnu(ipara)
            hot=inv_X2hist(thisnu, x2this, iext)
            if (verb_medstrategy) print *,'NOTICE (inv_min): ',
     &        'ipara: ',ipara,' thisnu: ',thisnu,' X2: ',x2this,
     &        ' iext: ',iext
            ipara=ipara-1
          enddo
        endif
c 
        small_nu=lq_parimp(iext)
        best_x2=lq_x2imp(iext)
        best_nu=small_nu
        if (verb_topstrategy) print *,'NOTICE (inv_min): ',
     &    'smallest X2: ',lq_x2imp(iext),' for nu: ',lq_parimp(iext),
     &    ' (ipara:',fpa(small_nu),', iext: ',iext,')'
      endif
c 
      if (verb_subaction) print *,'LEAVE inv_min'
c 
c 
      return
      end
c
c ----- END OF inv_min.f -----
