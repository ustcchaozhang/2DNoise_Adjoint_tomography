c this is <refract_setflags.f>
c------------------------------------------------------------------------------
c
c 03/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c change flag settings
c
c ----
c refract is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c refract is distributed in the hope that it will be useful,
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
c    03/07/98   V1.0   Thomas Forbriger
c
c==============================================================================
cS
c 
      subroutine refract_setflags
c 
c change settings (logical flags here)
c 
      include 'refract_para.inc'
c 
cE
      logical hot
      character*10 selection
c
      hot=.true.
      do while (hot)
        write(6, 50) 
        write(6, 51) 'remove trace average', 'rta', plpar_remav
        write(6, 51) 'use colors', 'ucl', plflag_color
        write(6, 51) 'use different line styles', 'uls', plflag_linestyle
        write(6, 51) 'plot variable area', 'var', plflag_vara
        write(6, 51) 'invert all signs', 'inv', plflag_invers
        write(6, 51) 'plot nice bubbles', 'bub', plflag_bubbles
        write(6, 51) 'plot subscales', 'sus', plflag_subscale
        write(6, 51) 'traveltime reduction', 'red', plflag_reduce
        write(6, 52)
        read(5, '(a10)') selection
        if (selection(1:4).eq.'quit') hot=.false.
        if (selection(1:3).eq.'rta') plpar_remav=.not.(plpar_remav)
        if (selection(1:3).eq.'ucl') plflag_color=.not.(plflag_color)
        if (selection(1:3).eq.'uls') plflag_linestyle=.not.(plflag_linestyle)
        if (selection(1:3).eq.'var') plflag_vara=.not.(plflag_vara)
        if (selection(1:3).eq.'inv') plflag_invers=.not.(plflag_invers)
        if (selection(1:3).eq.'bub') plflag_bubbles=.not.(plflag_bubbles)
        if (selection(1:3).eq.'sus') plflag_subscale=.not.(plflag_subscale)
        if (selection(1:3).eq.'red') plflag_reduce=.not.(plflag_reduce)
      enddo
      return
   50 format(/'FLAG settings'/
     &        '  T:true   flag is set'/
     &        '  F:false  flag is not set'/)
   51 format(2x,a40,2x,'[',a3,']',2x,L5)
   52 format(/'set/clear flags - [quit] to exit')
      end
c
c ----- END OF refract_setflags.f -----
