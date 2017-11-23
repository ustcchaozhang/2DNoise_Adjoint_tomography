c this is <par_pgset.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c display/edit pgplot parameters
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
c    20/01/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine par_pgset
c 
      include 'glq_pgpara.inc'
cE
      integer ival
      real rval,gval,bval
      character*80 arg
      logical hot
c 
      call par_showpg
c 
      hot=.true.
      do while (hot)
        print *,' '
        print *, 'PGPLOT - your command:'
        read(5, '(a80)'), arg
        if (arg(1:5).eq.'help ') then
          print *,'quit exit show'
          print *,'(and any bracket-command to set values)'
        elseif (arg(1:5).eq.'quit ') then
          hot=.false.
        elseif (arg(1:5).eq.'exit ') then
          hot=.false.
        elseif (arg(1:5).eq.'show ') then
          call par_showpg
c 
        elseif (arg(1:5).eq.'bord ') then
          read(arg(6:80), *, err=98, end=98) pg_border
        elseif (arg(1:5).eq.'vsep ') then
          read(arg(6:80), *, err=98, end=98) pg_vbreak
        elseif (arg(1:5).eq.'hsep ') then
          read(arg(6:80), *, err=98, end=98) pg_vphsep
        elseif (arg(1:5).eq.'vsew ') then
          read(arg(6:80), *, err=98, end=98) pg_vpsep
        elseif (arg(1:5).eq.'vbor ') then
          read(arg(6:80), *, err=98, end=98) pg_vbord
c 
        elseif (arg(1:5).eq.'sdlw ') then
          read(arg(6:80), *, err=98, end=98) pg_lw
        elseif (arg(1:5).eq.'xmlw ') then
          read(arg(6:80), *, err=98, end=98) pg_bestlw
        elseif (arg(1:5).eq.'culw ') then
          read(arg(6:80), *, err=98, end=98) pg_clw
c 
        elseif (arg(1:5).eq.'sdch ') then
          read(arg(6:80), *, err=98, end=98) pg_ch
        elseif (arg(1:5).eq.'abch ') then
          read(arg(6:80), *, err=98, end=98) pg_bch
        elseif (arg(1:5).eq.'vpch ') then
          read(arg(6:80), *, err=98, end=98) pg_vpch
        elseif (arg(1:5).eq.'wech ') then
          read(arg(6:80), *, err=98, end=98) pg_wch
        elseif (arg(1:5).eq.'lach ') then
          read(arg(6:80), *, err=98, end=98) pg_lch
        elseif (arg(1:5).eq.'anch ') then
          read(arg(6:80), *, err=98, end=98) pg_ach
c 
        elseif (arg(1:5).eq.'sdci ') then
          read(arg(6:80), *, err=98, end=98) pg_colind
        elseif (arg(1:5).eq.'rsci ') then
          read(arg(6:80), *, err=98, end=98) pg_rcolind
        elseif (arg(1:5).eq.'alci ') then
          read(arg(6:80), *, err=98, end=98) pg_alphacol
        elseif (arg(1:5).eq.'raci ') then
          read(arg(6:80), *, err=98, end=98) pg_alpharcol
        elseif (arg(1:5).eq.'beci ') then
          read(arg(6:80), *, err=98, end=98) pg_betacol
        elseif (arg(1:5).eq.'rbci ') then
          read(arg(6:80), *, err=98, end=98) pg_betarcol
c 
        elseif (arg(1:5).eq.'sdls ') then
          read(arg(6:80), *, err=98, end=98) pg_linestyle
        elseif (arg(1:5).eq.'alls ') then
          read(arg(6:80), *, err=98, end=98) pg_alphals
        elseif (arg(1:5).eq.'bels ') then
          read(arg(6:80), *, err=98, end=98) pg_betals
c 
        elseif (arg(1:5).eq.'shot ') then
          read(arg(6:80), *, err=98, end=98) pg_shorti
        elseif (arg(1:5).eq.'plot ') then
          read(arg(6:80), *, err=98, end=98) pg_plottitle
        elseif (arg(1:5).eq.'htch ') then
          read(arg(6:80), *, err=98, end=98) pg_dohatch
c 
        elseif (arg(1:4).eq.'rgb ') then
          read(arg(5:80), *, err=98, end=98) ival, rval, gval, bval
          ival=max(0,min(ival,pg_maxrgb))
          pg_rgbtable(1,ival)=rval
          pg_rgbtable(2,ival)=gval
          pg_rgbtable(3,ival)=bval
c 
        else
          print *,'WARNING (par_set): unknown command ',arg(1:index(arg, ' ')),
     &            ' (try ''help'')'
        endif
        goto 99
   98   print *,'ERROR (par_set): illegal value string'
   99   continue
      enddo
c
      call par_pgapply
c 
      return
      end
c
c ----- END OF par_pgset.f -----
