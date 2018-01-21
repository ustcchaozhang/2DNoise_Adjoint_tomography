c this is <res_wresmod.f>
c------------------------------------------------------------------------------
cS
c ($Id$)
c
c Copyright 2000 by Thomas Forbriger (IfG Stuttgart)
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
c write result of resolution analysis to model files
c
c REVISIONS and CHANGES
c    24/05/2000   V1.0   Thomas Forbriger
c    02/06/2000   V1.1   updated res_opt calling convention
c    16/04/2002   V1.2   now calls res_optrms
c    03/05/2002   V1.3   check only parameters for which parselect is true
c
c==============================================================================
c
      subroutine res_wresmod(mimin, mimax, nu, finalrms, basename,
     &                       dormstest)
c 
c call res_optrms for given partial derivatives and precalculated
c matrices for model parameters mimin to mimax and write resulting
c ensemble of models - each to a seperate file
c
c nu:             stabilization factor
c finalrms:       rms error increase to find parameter changes for
c basename:       base for filenames
c dormstest:      checks for rms error if true (else finalrms gives the
c                 relative increase in the square-error)
c 
      integer mimin,mimax
      real nu, finalrms
      character*(*) basename
      logical dormstest
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
      include 'glq_reso.inc'
c 
cE
c declare local variables
      logical res_optrms
      logical res_optsqr
      logical result
      integer mi, i
      character*120 parname, title, filename
      integer mpol, mpar, msec
      character*(*) res_wresmod_id
      real x2ref,dat_x2
      parameter (res_wresmod_id=
     &  '$Id$')
c
      if (verb_subaction) print *,'ENTER res_wresmod(',mimin,',',mimax,
     &  nu,',',finalrms,',',basename,')'
c 
      if (verb_subaction) 
     & print *,'NOTICE (res_wresmod): plot rated models for parameters',
     &  mimin,' to ',mimax,' to viewports 1,2,3'
c 
c check parameter range
      if (mimin.gt.mimax) then
        print *,'WARNING (res_wresmod): ',
     &    'mimin is greater than mimax'
        return
      endif
      if (mimin.lt.1) then
        print *,'WARNING (res_wresmod): ',
     &    'mimin is less than 1'
        return
      endif
      if (mimax.gt.mod_n) then
        print *,'WARNING (res_wresmod): ',
     &    'mimax is too large'
        return
      endif
c 
c create selection array
      call res_parselect
c
      x2ref=dat_x2(.true.)
      do mi=mimin,mimax
c plot
        if (parselect(mi)) then
          call mod_identify(mi, msec, mpol, mpar, parname)
          write (title, 50) mi, mpol-1, 
     &      parname(1:index(parname,' ')), msec,finalrms,nu
c plot pure model
          call mod_chop(mb_ref)
          if (verb_topstrategy) print *,'NOTICE (res_wresmod): ',
     &      'going for parameter ',mi
          if (dormstest) then
            result=res_optrms(mi, nu, finalrms,x2ref)
          else
            result=res_optsqr(mi, nu, finalrms,x2ref)
          endif
          if (result) then
            call mod_parcor
            write (filename, 51) basename(1:index(basename,' ')-1),mi, 
     &        parname(1:index(parname,' ')-1), msec, mpol-1,'up'
            call mod_save(filename,mb_work,.true.,title)
            do i=1,mod_n
              mdelta(i)=-mdelta(i)
            enddo
            call mod_parcor
            write (filename, 51) basename(1:index(basename,' ')-1),mi, 
     &        parname(1:index(parname,' ')-1), msec, mpol-1,'down'
            call mod_save(filename,mb_work,.true.,title)
          else
            print *,'WARNING (res_wresmod): ',
     &        'res_optrms/res_optsqr failed for parameter ',mi
          endif
        endif
         
      enddo
c 
      if (verb_subaction) print *,'LEAVE res_wresmod'
c
      return
   50 format('rating par. ',i3,': ord. ',i3,
     &  ' of ',a,'in section ',i3,' (Xrms=',
     &  g10.2,', nu=',g10.2,')')
   51 format(a,'.',i3.3,'.',a,'.',i3.3,'.',i3.3,'.',a)
c the following line prevents the linker from removing the ID string
   99 print *, res_wresmod_id
      end
c
c ----- END OF res_wresmod.f -----
