c this is <pg_resmod.f>
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
c plot model that results from res_optrms
c
c REVISIONS and CHANGES
c    08/04/98   V1.0   Thomas Forbriger
c    16/04/98   V1.1   plot model and keep ranges
c    09/11/98   V1.2   include title
c    22/01/99   V1.3   support title switching
c    02/06/00   V1.4   updated res_opt calling convention
c    16/04/02   V1.5   now calls res_optrms
c    06/05/02   V1.6   allow for rms and sqr mode
c    08/05/02   V1.7   plot title now is appropriate for sqr mode
c
      subroutine pg_resmod(mimin, mimax, nu, finalrms, dormstest)
c 
c call res_optrms for given partial derivatives and precalculated
c matrices for model parameters mimin to mimax and plot resulting
c ensemble of models
c
c nu:       stabilization factor
c finalrms: rms error increase to find parameter changes for
c 
      integer mimin,mimax
      real nu, finalrms
      logical dormstest
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_pgpara.inc'
      include 'glq_verbose.inc'
c 
cE
      logical res_optrms, res_optsqr
      integer mi, i
      character*132 title
      real x2ref, dat_x2
      logical result
c
      if (verb_subaction) print *,'ENTER pg_resmod(',mimin,',',mimax,
     &  nu,',',finalrms,')'
c 
      if (verb_subaction) 
     &  print *,'NOTICE (pg_resmod): plot rated models for parameters',
     &  mimin,' to ',mimax,' to viewports 1,2,3'
c 
c check parameter range
      if (mimin.gt.mimax) then
        print *,'WARNING (pg_resmod): ',
     &    'mimin is greater than mimax'
        return
      endif
      if (mimin.lt.1) then
        print *,'WARNING (pg_resmod): ',
     &    'mimin is less than 1'
        return
      endif
      if (mimax.gt.mod_n) then
        print *,'WARNING (pg_resmod): ',
     &    'mimax is too large'
        return
      endif
c plot pure model
      call mod_chop(mb_ref)
      call pg_mod(mb_ref)
c plot title
      if (dormstest) then
        write(title, 50) mimin, mimax, 'rms error', finalrms, nu
      else
        write(title, 50) mimin, mimax, 'error', finalrms, nu
      endif
      call pg_selvp(12)
      if (pg_plottitle) call pglab(' ',' ',title)
c 
      x2ref=dat_x2(.true.)
      do mi=mimin,mimax
        if (verb_topstrategy) print *,'NOTICE (pg_resmod): ',
     &    'going for parameter ',mi
        if (dormstest) then
          result=res_optrms(mi, nu, finalrms,x2ref)
        else
          result=res_optsqr(mi, nu, finalrms,x2ref)
        endif
        if (result) then
          call mod_parcor
          call mod_chop(mb_work)
          call pg_mod(-mb_work)
c          print *,'DEBUG: call with ',-mb_work
c          if (mod_prep()) then
c            call pg_mod(mb_work)
c          else
c            print *,'WARNING (pg_resmod): ',
c     &        'mod_prep failed for parameter ',mi
c          endif
          do i=1,mod_n
            mdelta(i)=-mdelta(i)
          enddo
          call mod_parcor
          call mod_chop(mb_work)
c          print *,'DEBUG: call with ',-mb_work
          call pg_mod(-mb_work)
c          if (mod_prep()) then
c            call pg_mod(mb_work)
c          else
c            print *,'WARNING (pg_resmod): ',
c     &        'mod_prep failed for parameter ',mi,' (opposite sign)'
c          endif
        else
          print *,'WARNING (pg_resmod): ',
     &      'res_optrms failed for parameter ',mi
        endif
         
      enddo
c 
      if (verb_subaction) print *,'LEAVE pg_resmod'
c
      return
   50 format('linear check of parameter ',i3,' to ',i3,
     &  ' for increasing the ',a,' by ',f6.3,
     &  ' (stabilized with \gn=',f8.5,')')
      end
c
c ----- END OF pg_resmod.f -----
