c this is <pg_selvp.f>
c------------------------------------------------------------------------------
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
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c
c==============================================================================
cS
c
      subroutine pg_selvp(ivp)
c
c select viewport with index ivp
c
c 0: select the full window
c
c      +---+---+---+
c      | 1 | 2 | 3 |
c      +---+-+-+---+
c      |  4  |  5  |
c      +-----+-----+
c 
c      +-----------+
c      |     6     |  without title space
c      +-----------+
c      |     7     |
c      +-----------+
c
c      +-----+-----+
c      |  8  |  9  |
c      +-----+-----+
c      | 10  | 11  |
c      +-----+-----+
c
c      +-----------+
c      |    12     |  with title space
c      +-----------+
c      |    13     |
c      +-----------+
c
      integer ivp
c
      include 'glq_pgpara.inc'
      include 'glq_verbose.inc'
c
cE
      real vpb, vpt, vpw
      real vpl, vpr
c 
      if (verb_subaction) print *,'ENTER pg_selvp(',ivp,')'
c
      if (ivp.eq.0) then
        vpt=1.-pg_vpch*pg_ch
        vpb=pg_vpch*pg_ch
        vpl=vpb
        vpr=vpt
      elseif (ivp.eq.1) then
        vpt=1.-pg_vbord
        vpb=1.-pg_vbreak+pg_vpch*pg_ch
        vpw=(1.-pg_vpch*pg_ch-2*pg_vpsep-pg_vbord)/3.
        vpl=pg_vpch*pg_ch
        vpr=vpl+vpw
      elseif (ivp.eq.2) then
        vpt=1.-pg_vbord
        vpb=1.-pg_vbreak+pg_vpch*pg_ch
        vpw=(1.-pg_vpch*pg_ch-2*pg_vpsep-pg_vbord)/3.
        vpl=pg_vpsep+pg_vpch*pg_ch+vpw
        vpr=vpl+vpw
      elseif (ivp.eq.3) then
        vpt=1.-pg_vbord
        vpb=1.-pg_vbreak+pg_vpch*pg_ch
        vpw=(1.-pg_vpch*pg_ch-2*pg_vpsep-pg_vbord)/3.
        vpl=pg_vpch*pg_ch+2.*(vpw+pg_vpsep)
        vpr=vpl+vpw
      elseif (ivp.eq.4) then
        vpt=1.-pg_vbreak-pg_vpsep-pg_vpch*pg_ch
        vpb=pg_vpch*pg_ch
        vpl=vpb
        vpr=pg_vphsep-pg_vpsep
      elseif (ivp.eq.5) then
        vpt=1.-pg_vbreak-pg_vpsep-pg_vpch*pg_ch
        vpb=pg_vpch*pg_ch
        vpl=pg_vphsep+pg_vpsep+vpb
        vpr=1.-pg_vbord
      elseif (ivp.eq.6) then
        vpt=1.-pg_vbord
        vpb=1.-pg_vbreak+pg_vpch*pg_ch
        vpl=pg_vpch*pg_ch
        vpr=1.-pg_vbord
      elseif (ivp.eq.7) then
        vpt=1.-pg_vbreak-pg_vbord
        vpb=pg_vpch*pg_ch
        vpl=pg_vpch*pg_ch
        vpr=1.-pg_vbord
      elseif (ivp.eq.8) then
        vpt=1.-pg_vpch*pg_ch
        vpb=0.5+pg_vpch*pg_ch
        vpl=pg_vpch*pg_ch
        vpr=.5-pg_vbord
      elseif (ivp.eq.9) then
        vpt=1.-pg_vpch*pg_ch
        vpb=0.5+pg_vpch*pg_ch
        vpl=0.5+pg_vpch*pg_ch
        vpr=1.-pg_vbord
      elseif (ivp.eq.10) then
        vpt=.5-pg_vpch*pg_ch
        vpb=pg_vpch*pg_ch
        vpl=pg_vpch*pg_ch
        vpr=.5-pg_vbord
      elseif (ivp.eq.11) then
        vpt=.5-pg_vpch*pg_ch
        vpb=pg_vpch*pg_ch
        vpl=0.5+pg_vpch*pg_ch
        vpr=1.-pg_vbord
      elseif (ivp.eq.12) then
        vpt=1.-pg_vpch*pg_ch
        vpb=0.5+pg_vpch*pg_ch+pg_vbord
        vpl=pg_vpch*pg_ch
        vpr=1.-pg_vbord
      elseif (ivp.eq.13) then
        vpt=0.5-pg_vpch*pg_ch
        vpb=pg_vpch*pg_ch+pg_vbord
        vpl=pg_vpch*pg_ch
        vpr=1.-pg_vbord
      else
        call mod_panic('ERROR (pg_selvp): unknown vp index')
      endif
c 
      call pgsvp(vpl, vpr, vpb, vpt)
c 
      if (verb_subaction) print *,'LEAVE pg_selvp'
c      
      return
      end
c
c ----- END OF pg_selvp.f -----
