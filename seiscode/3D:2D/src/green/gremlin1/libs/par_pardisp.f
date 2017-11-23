c this is <par_pardisp.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c display named model parameter weights
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
c    07/04/98   V1.0   Thomas Forbriger
c    14/04/98   V1.1   do not display hyphen for top of model depth
c    14/01/99   V2.0   model definition has changed (see glq_model.inc)
c    22/01/99   V2.1   changed follow reporting scheme
c
      subroutine par_pardisp
c 
c display a filled array of named model parameter weights
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_model.inc'
c 
cE
      character*9 string(glqm_cpar, glqm_mpol)
      integer i, ipol, ipar, isec, iline
c 
      string(1,1)='depth'
      string(2,1)='alpha'
      string(3,1)='beta'
      string(4,1)='density'
      string(5,1)='Qalpha'
      string(6,1)='Qbeta'
c
      print 51, (string(i,1), i=1,glqm_cpar), (i, i=1,glqm_cpar)
c 
      do ipar=1,glqm_cpar
        do ipol=1,glqm_mpol
          string(ipar, ipol)=' '
        enddo
      enddo
      iline=0
c 
c DEBUG
c        do isec=1,glqm_nsec
c          print *,para_mdweights(isec)
c          do ipol=1,glqm_mpol
c            print *,(para_mweights(ipol, ipar, isec), ipar=1,glqm_mpar)
c          enddo
c        enddo
c DEBUG
      do isec=1,glqm_nsec
c 
        if (para_mdweights(isec).gt.0.) then
          write(string(1, 1), 52) para_mdweights(isec)
        else
          string(1,1)='       -'
c          if (isec.eq.1) string(1,1)='        '
        endif
c 
        do ipar=1,glqm_mpar
          do ipol=1,glqm_npol(isec,ipar)
            if (glqm_follow(isec,ipar).and.(ipol.eq.1)) then
              string(ipar+1, ipol)=' follows'
            else
              if ((para_mweights(ipol,ipar,isec).gt.0.)) then
                write(string(ipar+1,ipol), 52) para_mweights(ipol, ipar, isec)
              else
                string(ipar+1, ipol)='       -'
              endif
            endif
          enddo
        enddo
c 
        print *,' '
        do ipol=1,glqm_mpol
          iline=iline+1
          print 53,iline, (string(ipar, ipol), ipar=1,glqm_cpar)
        enddo
c 
c        string(1,1)=' '
c        do ipar=1,glqm_mpar
c          if ((glqm_follow(isec, ipar)).and.(isec.lt.glqm_nsec)) then
c          if ((glqm_follow(isec, ipar)).and.(isec.gt.1)) then
c            string(ipar+1, 1)='follow'
c          else
c            string(ipar+1, 1)=' '
c          endif
c        enddo
c        print 55, (string(ipar, 1), ipar=1,glqm_cpar)
c 
        do ipar=1,glqm_cpar
          do ipol=1,glqm_mpol
            string(ipar, ipol)=' '
          enddo
        enddo
      enddo
c 
      return
   50 format(/a)
   51 format(3x,6(2x,a9),/,3x,6(2x,i9))
   52 format(g9.2)
   53 format(i3,6(2x,a9))
   54 format(/a)
   55 format(3x,6(2x,a9))
      end
c
c ----- END OF par_pardisp.f -----
