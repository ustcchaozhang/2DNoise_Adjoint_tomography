c this is <mod_showparcor.f>
c------------------------------------------------------------------------------
cC
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
c show setting of anonymous parameters
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    14/01/99   V1.1   changed 'followed' to 'follows' in format 54
c                      and 'top' to 'bottom' in format 50
c                      as model definition changed
c    20/01/99   V1.2   changed reporting style of following sections
c    02/06/00   V1.3   had to correct follow reporting scheme (took wrong
c                      section)
c    13/01/06   V1.4   lowest polynomial oder has to be zero
c
      subroutine mod_showparcor
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
c
cE
      integer i, isec, ipar, ipol
c 
      if (verb_subaction) print *,'ENTER mod_showparcor'
c 
c
      i=0
      print 50,'no.','section','search range'
      do isec=1,glqm_nsec
        if (destim(isec)) then
          i=i+1
          print 51,i,isec,mweight(i)
        endif
      enddo
      print 52,'no.','section','parameter','pol.ord.','search range'
      do ipar=1,glqm_mpar
        do isec=1,glqm_nsec
          if (mestim(isec, ipar)) then
            do ipol=1,glqm_npol(isec,ipar)
              if ((isec.gt.1).and.(ipol.eq.1).and.
     &          glqm_follow(isec, ipar)) then
                  print 54,isec,ipar,ipol-1
              else
                i=i+1
                print 53,i,isec,ipar,ipol-1,mweight(i)
              endif
            enddo
          endif
        enddo
      enddo
      if (i.ne.mod_n) 
     &  call mod_panic('ERROR (mod_showparcor): wrong number of anonymous parameters')
c 
      if (verb_subaction) print *,'LEAVE mod_parcor'
c
      return
   50 format('bottom of section depth parameter:',/,
     &       2x,a6,a15,30x,a15)
   51 format(2x,i6,i15,30x,2x,g13.2)
   52 format('section parameters:',/,
     &       2x,a6,a15,a15,a15,a15)
   53 format(2x,i6,i15,i15,i15,2x,g13.2)
   54 format(2x,6x,i15,i15,i15,2x,'follows')
      end
c
c ----- END OF mod_showparcor.f -----
