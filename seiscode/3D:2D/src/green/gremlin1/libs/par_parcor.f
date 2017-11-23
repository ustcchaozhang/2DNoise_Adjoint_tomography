c this is <par_parcor.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c this subroutines controls the free model parameters
c it is a user interface
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
c    25/03/98   V1.1   now check maximum number of free parameters
c    07/04/98   V1.2   now use par_wenc, par_wdec and par_wcheck
c    14/04/98   V1.3   do not change top of model depth
c    14/01/99   V1.4   model definition changed (see glq_model.inc)
c                      consider now follow flag and allow changes of depth
c                      of bottom of first section
c
c==============================================================================
cS
c
      logical function par_parcor(doedit)
c 
c doedit should be .true. if you want to change values
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c 
      logical doedit
c 
cE
      integer ipol, ipar, isec
      logical hot, edit, par_wcheck
      character*80 arg
      real value
      integer row, col
c 
c 
      edit=doedit
c
      call par_wdec
c 
      hot=.true.
      do while (hot)
c 
        print 50,'PARCOR: (search ranges in units m, km/s, g/cm**3)'
c 
        call par_pardisp
c 
        hot=edit
c 
c edit values
        if ((hot).and.(edit)) then
          print 54, 'PARCOR - your command:'
          read(5, '(a80)'), arg
          if (arg(1:5).eq.'help ') then
            print *,'cl c r   clear parameter in row r, column c'
            print *,'cc c     clear parameters in column c'
            print *,'cr r     clear parameters in row r'
            print *,'se c r v set parameter in row r, column c to value v'
            print *,'sc c v   set parameters in column c to value v'
            print *,'sr r v   set parameters in row r to value v'
            print *,'exit     write new values and exit'
            print *,'quit     do not write new values and exit'
            print 54, 'press ENTER'
            read(5, '(a80)'), arg
          elseif (arg(1:5).eq.'quit ') then
            edit=.false.
            hot=.false.
          elseif (arg(1:5).eq.'exit ') then
            hot=.false.
          elseif (arg(1:3).eq.'cc ') then
            read(arg(4:80), *, err=98, end=97) col
            col=min(glqm_cpar,max(1,col))
            if (col.eq.1) then
              do isec=1,glqm_nsec
                para_mdweights(isec)=-1.
              enddo
            else
              ipar=col-1
              do isec=1,glqm_nsec
                do ipol=1,glqm_npol(isec, ipar)
                  para_mweights(ipol, ipar, isec)=-1.
                enddo
              enddo
            endif
          elseif (arg(1:3).eq.'sc ') then
            read(arg(4:80), *, err=98, end=97) col, value
            col=min(glqm_cpar,max(1,col))
            if (col.eq.1) then
              do isec=1,glqm_nsec
                para_mdweights(isec)=value
              enddo
            else
              ipar=col-1
              do isec=1,glqm_nsec
                do ipol=1,glqm_npol(isec, ipar)
                  if (.not.(glqm_follow(isec,ipar).and.(ipol.eq.1)))
     &              para_mweights(ipol, ipar, isec)=value
                enddo
              enddo
            endif
          elseif (arg(1:3).eq.'sr ') then
            read(arg(4:80), *, err=98, end=97) row, value
            row=min(glqm_msec*glqm_mpol, max(1,row))
            isec=int((row+2)/3)
            if (row.eq.((3*isec)-2)) then
c              if (isec.eq.1) then
c                print *,'WARNING (par_parcor): refuse to use top of model',
c     &            'depth as free parameter'
c              else
                para_mdweights(isec)=value
c              endif
            endif
            ipol=row+3-isec*3
            do ipar=1,glqm_mpar
              if (.not.(glqm_follow(isec,ipar).and.(ipol.eq.1)))
     &          para_mweights(ipol, ipar, isec)=value
            enddo
          elseif (arg(1:3).eq.'cr ') then
            read(arg(4:80), *, err=98, end=97) row
            row=min(glqm_msec*glqm_mpol, max(1,row))
            isec=int((row+2)/3)
            if (row.eq.((3*isec)-2)) then
              para_mdweights(isec)=-1.
            endif
            ipol=row+3-isec*3
            do ipar=1,glqm_mpar
              para_mweights(ipol, ipar, isec)=-1.
            enddo
          elseif (arg(1:3).eq.'se ') then
            read(arg(4:80), *, err=98, end=97) col, row, value
            col=min(glqm_cpar,max(1,col))
            row=min(glqm_msec*glqm_mpol, max(1,row))
            isec=int((row+2)/3)
            ipol=row+3-isec*3
            if (col.eq.1) then
              if (row.ne.((3*isec)-2)) then
                print *,'WARNING (par_parcor): senseless row...'
              else
c                if (isec.eq.1) then
c                  print *,'WARNING (par_parcor): refuse to use top of model',
c     &              'depth as free parameter'
c                else
                  para_mdweights(isec)=value
c                endif
              endif
            else
              ipar=col-1
              if (.not.(glqm_follow(isec,ipar).and.(ipol.eq.1)))
     &          para_mweights(ipol, ipar, isec)=value
            endif
          elseif (arg(1:3).eq.'cl ') then
            read(arg(4:80), *, err=98, end=97) col, row
            col=min(glqm_cpar,max(1,col))
            row=min(glqm_msec*glqm_mpol, max(1,row))
            isec=int((row+2)/3)
            ipol=row+3-isec*3
            if (col.eq.1) then
              if (row.ne.((3*isec)-2)) then
                print *,'WARNING: senseless row...'
              else
                para_mdweights(isec)=-1.
              endif
            else
              ipar=col-1
              para_mweights(ipol, ipar, isec)=-1.
            endif
          else
            print *,'WARNING: unknown command ',arg(1:index(arg,' ')),
     &              ' (try ''help'')'
          endif
          goto 99
   97     print *,'ERROR (par_parcor): unexpected end of line'
   98     print *,'ERROR (par_parcor): illegal parameter string'
   99     continue
c 
          if (.not.(par_wcheck())) then
            hot=.true.
            edit=.true.
          endif
c
        endif
c endif input
      enddo
c enddo input loop
c 
c write back values (if changed)
      if (edit) then
        call par_wenc
      endif
c 
      return
   50 format(/a)
   51 format(3x,6(2x,a9),/,3x,6(2x,i9))
   52 format(g9.1)
   53 format(i3,6(2x,a9))
   54 format(/a)
   55 format(3x,6(2x,a9))
      end
c
c ----- END OF par_parcor.f -----
