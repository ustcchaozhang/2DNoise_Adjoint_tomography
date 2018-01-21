c this is <res_part.f>
c------------------------------------------------------------------------------
cS
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
c rate the partial derivatives for all parameters
c
c REVISIONS and CHANGES
c    07/04/98   V1.0   Thomas Forbriger
c    16/04/98   V1.1   has to calculate reference synthetics first
c    14/01/99   V1.2   model definition changed (see glq_model.inc)
c                      keep track of follow flag
c    25/03/99   V1.3   I weighted with data errors squared
c                      That leads to false absolute rms values
c    29/08/01   V1.4   expects lq_dssd - so call inv_dssd
c
      subroutine res_part
c
c do a linear resolution analysis by rating the partial derivatives
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_model.inc'
      include 'glq_inv.inc'
      include 'glq_data.inc'
      include 'glq_verbose.inc'
c
cE
      logical inv_part, dat_cref, result
      double precision sqerror
      integer iano, ipol, ipar, isec, id, spol
c
c prepare partial derivatives and weighted derivatives
      if (verb_medstrategy) print *,'NOTICE (res_part): ',
     &  'calculate reference synthetics'
      result=dat_cref()
      if (result) then
        if (verb_medstrategy) print *,'NOTICE (res_part): ',
     &    'calculate partial derivatives'
        result=inv_part()
      endif
      if (result) then
        if (verb_subaction) print *,'ACTION (res_part): ',
     &    'calculate weighted partial derivatives'
        call inv_ds
        if (verb_subaction) print *,'ACTION (res_part): ',
     &    'calculate system matrix'
        call inv_dssd
        if (verb_subaction) print *,'ACTION (res_part): ',
     &    'calculate rms errors'
c clear display help space
        do ipol=1,glqm_mpol
          do ipar=1,glqm_mpar
            do isec=1,glqm_msec
              para_mweights(ipol, ipar, isec)=-10.
            enddo
          enddo
        enddo
c here we go...
        iano=0
        if (verb_subresult) print *,'RESULT (res_part): depth:'
        do isec=1,glqm_nsec
          if (destim(isec)) then
            iano=iano+1
c            sqerror=0.
c            do id=1,dat_n
c              sqerror=sqerror+real(lq_dss(id, iano))**2+
c     &                        imag(lq_dss(id, iano))**2
c            enddo
            sqerror=abs(lq_dssd(iano,iano))
            para_mdweights(isec)=sqrt(sqerror)*mweight(iano)
            if (verb_subresult) print *,'RESULT (res_part): ',
     &        'isec, err ',isec,para_mdweights(isec)
          else
            para_mdweights(isec)=-1.
          endif
        enddo
c
        if (verb_subresult) print *,'RESULT (res_part): other parameters:'
        do ipar=1,glqm_mpar
          do isec=1,glqm_nsec
            spol=1
            if (glqm_follow(isec,ipar)) spol=2
            do ipol=spol,glqm_npol(isec,ipar)
              if (mestim(isec, ipar)) then
                iano=iano+1
c                sqerror=0.
c                do id=1,dat_n
c                  sqerror=sqerror+real(lq_dss(id, iano))**2+
c     &                            imag(lq_dss(id, iano))**2
c                enddo
                sqerror=abs(lq_dssd(iano,iano))
                para_mweights(ipol, ipar, isec)=sqrt(sqerror)*mweight(iano)
                if (verb_subresult) print *,'RESULT (res_part): ',
     &            'ipol, ipar, isec, err ',ipol, ipar, 
     &            isec,para_mweights(ipol, ipar, isec)
              else
                para_mweights(ipol, ipar, isec)=-1.
              endif
            enddo
          enddo
        enddo
c
c display
        print *,'LINEAR RESOLUTION ANALYSIS'
        print *,'Rating partial derivatives:'
        print *,'The following scheme will give the resulting rms errors,'
        print *,'when varying the parameter by one search range.'
        call par_pardisp
c
      else
        print *,'ERROR (res_part): calculation of partial derivatives failed'
      endif
c
      return
      end
c
c
c ----- END OF res_part.f -----
