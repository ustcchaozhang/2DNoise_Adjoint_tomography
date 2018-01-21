c this is <par_pgmon.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c switch pg-monitor devices on and off
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
c    01/07/99   V1.0   Thomas Forbriger
c
      subroutine par_pgmon
c
c 
      include 'glq_pgmon.inc'
c 
cE
      character arg*(80)
      logical hot
c 
      hot=.true.
      do while (hot)
        print *,' '
        print *,'PG-MONITOR DEVICES'
        call pars_verb('tap orig green','tog',(pgmon_tapogreen.gt.0))
        call pars_verb('tap modif green','tmg',(pgmon_tapmgreen.gt.0))
        call pars_verb('tap ref green','trg',(pgmon_taprgreen.gt.0))
        print *,'PG-MONITOR DEVICES - your command:'
        read(5, '(a80)') arg
c 
        if (arg(1:6).eq.'help ') then
          print *,'quit exit tog tmg trg'
        elseif (arg(1:5).eq.'exit ') then
          hot=.false.
        elseif (arg(1:5).eq.'quit ') then
          hot=.false.
        elseif (arg(1:4).eq.'tog ') then
          call pgmon_switchdevice(pgmon_tapogreen)
        elseif (arg(1:4).eq.'tmg ') then
          call pgmon_switchdevice(pgmon_tapmgreen)
        elseif (arg(1:4).eq.'trg ') then
          call pgmon_switchdevice(pgmon_taprgreen)
        else
          print *,'NOTICE: unknown read command (try ''help'')'
        endif
c 
      enddo
c 
      return
      end
c
c----------------------------------------------------------------------
c
cS
      subroutine pgmon_switchdevice(mydevice)

c pgplot device identifiers
      integer maindevice, mydevice, pgopen
      character*10 mydevname
      parameter(mydevname='/XSERVE')
cE
      call pgqid(maindevice)
      if (mydevice.gt.0) then
        call pgslct(mydevice)
        call pgclos
        mydevice=-1
      else
        mydevice=-1
c open subdevice if desired
        mydevice=pgopen(mydevname)
        call par_pgapply
        call pgask(.false.)
        if (mydevice.lt.1) print *,
     &   'ERROR (par_pgmon/pgmon_newdevice): could not open graphics subdevice'
      endif
      call pgslct(maindevice)
      return
      end
c 
c ----- END OF par_pgmon.f -----
