c this is <refract_setpara.f>
c------------------------------------------------------------------------------
c
c 03/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c control parameter settings
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
c    11/11/2009 V1.1   use free format
c
c==============================================================================
cS
c 
      subroutine refract_setpara
c 
c change settings (numerical parameters here)
c 
      include 'refract_para.inc'
c 
cE
      logical hot
      character*40 selection
c
      hot=.true.
      do while (hot)
        write(6, 50) 
        write(6, 51) 'clipping in meters', 'cim', plpar_clip
        write(6, 51) 'offset scaling exponent', 'ofe', plpar_expo
        write(6, 54) 'scaling mode', 'scm', plpar_mode
        write(6, 53) '1: scale  traces individually'
        write(6, 53) '2: scale all traces to first trace as reference'
        write(6, 53) '3: scale all traces to first trace of input dataset as reference'
        write(6, 51) 'amplitude scale in meters', 'asm', plpar_amp
        write(6, 51) 'minimum offset step', 'mos', plpar_minoff
        write(6, 54) 'color cycly', 'ccy', plpar_colcyc
        write(6, 54) 'line style cycle', 'lcy', plpar_lscyc
        write(6, 51) 'traveltime reduction velocity', 'red', plpar_vred
        write(6, 52)
        read(5, '(a40)') selection
        if (selection(1:4).eq.'quit') hot=.false.
        if (selection(1:3).eq.'cim') read(selection(4:40), *) plpar_clip
        if (selection(1:3).eq.'ofe') read(selection(4:40), *) plpar_expo
        if (selection(1:3).eq.'scm') read(selection(4:40), *) plpar_mode
        if (selection(1:3).eq.'asm') read(selection(4:40), *) plpar_amp
        if (selection(1:3).eq.'mos') read(selection(4:40), *) plpar_minoff
        if (selection(1:3).eq.'ccy') read(selection(4:40), *)
     &    plpar_colcyc
        if (selection(1:3).eq.'lcy') read(selection(4:40), *)
     &    plpar_lscyc
        if (selection(1:3).eq.'red') read(selection(4:40), *)
     &    plpar_vred
      enddo
      return
   50 format(/'PARAMETER settings'/)
   51 format(2x,a40,2x,'[',a3,']',2x,g15.5)
   54 format(2x,a40,2x,'[',a3,']',2x,i10)
   52 format(/'change parameters - [quit] to exit')
   53 format(4x,a)
      end
c
c ----- END OF refract_setpara.f -----
