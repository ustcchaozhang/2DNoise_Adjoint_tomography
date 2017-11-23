c this is <grepg_message.f>
c------------------------------------------------------------------------------
c
c 25/11/98 by Thomas Forbriger (IfG Stuttgart)
c
c display or remove messages
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
c    25/11/98   V1.0   took this from refract_message.f
c
c==============================================================================
c
      subroutine grepg_message(display, message, position)
c
c display:  true  - plot message
c           false - remove message
c message:  message text
c position: 'b'   - position is bottom left
c           't'   - position is top left
c           'h'   - position is hint text at top left
c 
      logical display
      character message*(*)
      character*1 position
c
      real xpos, ypos, xbox(4), ybox(4) 
      real vp1, vp2, vp3, vp4
      real win1, win2, win3, win4
c 
      if (position.eq.'t') then
        xpos=0.02
        ypos=0.95
      elseif (position.eq.'h') then
        xpos=0.02
        ypos=0.98
      elseif (position.eq.'b') then
        xpos=0.02
        ypos=0.03
      else
        stop 'ERROR (grepg_message): called for unknown position'
      endif
c 
      call pgsave
      call pgqvp(0, vp1, vp2, vp3, vp4)
      call pgqwin(win1, win2, win3, win4)
      call pgsvp(0., 1., 0., 1.)
      call pgswin(0., 1., 0., 1.)
      if (display) then
        call pgptxt(xpos, ypos, 0., 0., message) 
      else
        call pgqtxt(xpos, ypos, 0., 0., message, xbox, ybox)
        call pgsci(0)
        call pgsfs(1)
        call pgpoly(4, xbox, ybox)
      endif
      call pgsvp(vp1, vp2, vp3, vp4)
      call pgswin(win1, win2, win3, win4)
      call pgunsa
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine grepg_warning(message)
c
c display warning text
c
      character message*(*)
c 
      include 'grepg_para.inc'
c 
      plstring_lastwarn=message
c 
      if (message.ne.'  ') then
        print *,message
c 
        call pgsave
        call pgsvp(0., 1., 0., 1.)
        call pgswin(0., 1., 0., 1.)
        call pgsci(0)
        call pgrect(0.,1.,0.,0.015)
        call pgsci(1)
        call pgsch(0.7)
        call pgptxt(0.02, 0.002, 0., 0., message) 
        call pgunsa
      endif
c 
      return
      end
c 
c ----- END OF grepg_message.f -----
