c this is <mod_read.f>
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
c read a polynomial model from file
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    13/01/99   V1.1   now reads new model format (version 2) and converts
c                      old format if found in file (see comments in
c                      glq_model.inc)
c    14/01/99   V2.0   follow still set in section that has to be changed
c                      thus is now one section below
c    03/03/99   V2.1   did not check maximum number of available sections
c    11/04/00   V2.2   call mod_track
c    09/02/10   V2.3   correction in format for reading empty line
c    05/02/11   V2.4   check consistency with search ranges and
c                      clear search ranges if inconsistent after read
c
      subroutine mod_read(filename, i)
c 
c read a named model from file
c
c filename:  name of file to use
c i:         model index within array
c
      character filename*(*)
      integer i
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_para.inc'
      include 'glq_strings.inc'
      include 'glq_verbose.inc'
c
cE
      character*79 comment
      logical oldversion
      integer lu, j, k, l
      double precision oldcoeff(glqm_mpol), dh, sectop, secbot
      parameter(lu=18)
      logical par_wcheck
c for debug:
c      verb_substrategy=.true.
c      verb_subaction=.true.
c 
      if (verb_subaction) print *,'ENTER mod_read(',filename,',',i,')'
c 
      if (verb_io) print *,'NOTICE (mod_read): reading model from ',
     &  filename(1:index(filename, ' ')),' to ',i
c 
      open(lu, file=filename, status='old', err=99)
c 
      read(lu, '(a79)', err=98, end=97) comment
      if (verb_io) print *,comment
c 
c check version
      oldversion=.false.
      read(lu, '(/a79)', err=98, end=97) comment
      if (comment(1:len(mod_version2)).ne.mod_version2) then
        oldversion=.true.
        read(comment, '(i15)', err=98, end=97) glqm_nsec
        read(lu, '(/a79)', err=98, end=97) comment
        if (verb_io) print *,
     &   'NOTICE (mod_read): found model of old version'
      else
        read(lu, '(i15//)', err=98, end=97) glqm_nsec
      endif
c 
      if (verb_io) print *,
     &   'NOTICE (mod_read): reading ',glqm_nsec,' model sections...'
      if (glqm_nsec.gt.glqm_msec) stop 'ERROR (mod_read): too many sections'
      do j=1,glqm_nsec
        read(lu, '(/f15.7)', err=95, end=97) mdepth(j, i)
c        print *,mdepth(j,i)
        read(lu, fmt=*, err=98, end=97) (glqm_npol(j, k), k=1,glqm_mpar)
c        print *,j,(glqm_npol(j, k), k=1,glqm_mpar)
        do k=1,glqm_mpar
          if (glqm_npol(j,k).lt.0) then
            glqm_npol(j,k)=-glqm_npol(j,k)
            glqm_follow(j,k)=.true.
c            print *,'isnega ',j,k
          else
            glqm_follow(j,k)=.false.
c            print *,'isposi ',j,k
          endif
          if (glqm_npol(j, k).lt.1) 
     &      stop 'ERROR (mod_read): need at least constant value'
          if (glqm_npol(j, k).gt.glqm_mpol) 
     &      stop 'ERROR (mod_read): too many coefficients!'
        enddo
        read(lu, '(a1)', err=98, end=97) comment
        do k=1,glqm_mpol
          read(lu, fmt=*, err=98, end=97) (model(k, j, l, i), l=1,glqm_mpar)
c          print *, k,(model(k, j, l, i), l=1,glqm_mpar)
        enddo
      enddo
c 
c for debug: tell me what is read
c      print *,'DEBUG: nsec:',glqm_nsec
c      print *,'DEBUG: oldversion: ',oldversion
c      do j=1,glqm_nsec
c        print *,'DEBUG: sec, depth: ',j, mdepth(j,i)
c        do k=1,glqm_mpar
c          print *,'DEBUG:   par, npol, follow ',
c     &      k,glqm_npol(j,k),glqm_follow(j,k)
c          do l=1,glqm_npol(j,k)
c            print *,'DEBUG:     pol, val ',l,model(l, j, k, i)
c          enddo
c        enddo
c      enddo
c 
      do j=1,glqm_nsec
        do k=1,glqm_mpar
          if (glqm_npol(j,k).lt.glqm_mpol) then
            do l=glqm_npol(j, k)+1,glqm_mpol
              model(l, j, k, i)=0.d0
            enddo
          endif
        enddo
      enddo
c 
c transform old model
      if (oldversion) then
c        print *,'DEBUG: TRANSFORM!'
        if (verb_io) print *,
     &   'NOTICE (mod_read): transform coefficients to new definition'
        call mod_follow1(i)
c 
c for debug: tell me what is read
c      print *,'DEBUG: after calling follow1'
c      print *,'DEBUG: nsec:',glqm_nsec
c      print *,'DEBUG: oldversion: ',oldversion
c      do j=1,glqm_nsec
c        print *,'DEBUG: sec, depth: ',j, mdepth(j,i)
c        do k=1,glqm_mpar
c          print *,'DEBUG:   par, npol, follow ',
c     &      k,glqm_npol(j,k),glqm_follow(j,k)
c          do l=1,glqm_npol(j,k)
c            print *,'DEBUG:     pol, val ',l,model(l, j, k, i)
c          enddo
c        enddo
c      enddo
c
        do j=1,glqm_nsec
c 
c transform sections depths
          sectop=mdepth(j,i)
          if (j.lt.glqm_nsec) then
            secbot=mdepth(j+1,i)
          else
            secbot=1.1*sectop
            if (verb_io) print *,
     &        'NOTICE (mod_read): depth of halfspace set to ',secbot,'m'
          endif
          mdepth(j,i)=secbot
          dh=0.5d0*(secbot-sectop)
c 
c adjust number of coefficients
          do k=1,glqm_mpar
            if (glqm_follow(j,k))
     &        glqm_npol(j,k)=min(glqm_mpol,glqm_npol(j,k)+1)
          enddo
c 
c transform coefficients
          do k=1,glqm_mpar
c 
c move follow flag to next section
            if (j.lt.glqm_nsec) then
              glqm_follow(glqm_nsec-j+1,k)=glqm_follow(glqm_nsec-j,k)
            else
              glqm_follow(glqm_nsec-j+1,k)=.false.
            endif
c 
c transform values
            do l=1,glqm_mpol
              oldcoeff(l)=model(l, j, k, i)
            enddo
c we expect here unused coefficients to be set zero (see above)
            model(1, j, k, i)=oldcoeff(1)+oldcoeff(2)*dh+oldcoeff(3)*dh*dh
            model(2, j, k, i)=oldcoeff(2)+2.d0*oldcoeff(3)*dh
          enddo
        enddo
      endif
c 
      close(lu, err=96)
      if (verb_io) print *,'NOTICE (mod_read): file read and closed.'
c 
c make section parameters follow
      call mod_follow(i)
c
c let vp track vs
      if (vptrackfactor.gt.0.d0) call mod_track(i)
c 
c for debug: tell me what is read
c      print *,'DEBUG: nsec:',glqm_nsec
c      print *,'DEBUG: oldversion: ',oldversion
c      do j=1,glqm_nsec
c        print *,'DEBUG: sec, depth: ',j, mdepth(j,i)
c        do k=1,glqm_mpar
c          print *,'DEBUG:   par, npol, follow ',
c     &      k,glqm_npol(j,k),glqm_follow(j,k)
c          do l=1,glqm_npol(j,k)
c            print *,'DEBUG:     pol, val ',l,model(l, j, k, i)
c          enddo
c        enddo
c      enddo
c 
      if (.not.(par_wcheck())) then
        print *,'WARNING (mod_read): weights failed check!'
        print *,'                    ',
     &    'search ranges and model parameters are inconsistent'
        print *,'                    ',
     &    'clearing search ranges to avoid fatal error conditions'
        j=0
        call par_sano(j, mb_ref)
      endif

      if (verb_subaction) print *,'LEAVE mod_read'
c for debug:
c      verb_substrategy=.false.
c      verb_subaction=.false.
c 
      return
   99 stop 'ERROR (mod_read): opening file'
   98 stop 'ERROR (mod_read): reading file'
   97 stop 'ERROR (mod_read): reading file - unexpected end'
   96 stop 'ERROR (mod_read): closing file'
   95 stop 'ERROR (mod_read): reading file: depth'
      end
c
c ----- END OF mod_read.f -----
