c this is <tf_bytesex.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c checks the processor bytesex and compares with read magic number
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
c    25/06/97   V1.0   Thomas Forbriger
c    17/12/07   V1.1   removed unused variable
c
c==============================================================================
cS
c
      subroutine tf_bytesex(cmagic, inmagic, cpu, match)
c
c what you should give me:
c   magic:      a magic number string created by your main program
c   inmagic:    a corresponding magic number read from file
c what I will give you:
c   cpu:    1 - you are running on an Intel processor
c           2 - you are running on a Motorola processor
c           3 - unknown processor type
c   match:  1 - your data does match your processor
c           2 - you will have to swap your data
c           3 - I have got no idea what to do with your data
c   
c magic number to find processor byte sex
c is '1234' on Intel or '4321' on Motorola
c     integer magic, swapmagic
c     parameter(magic=((52*256+51)*256+50)*256+49)
c
      character*4 cmagic
      integer inmagic, cpu, match
c
cE
      integer magic, swapmagic, i, vgl
      character*4 swapcmagic, cvgl
      equivalence(vgl, cvgl)
c
      call tf_magic(cmagic, magic)
      do i=1,4
        swapcmagic(i:i)=cmagic(5-i:5-i)
      enddo
      call tf_magic(swapcmagic, swapmagic)
      vgl=magic
      cpu=3
      if (cvgl.eq.cmagic) cpu=2
      if (cvgl.eq.swapcmagic) cpu=1
      match=3
      if (inmagic.eq.magic) match=1
      if (inmagic.eq.swapmagic) match=2
      return
      end
c 
c ----- END OF tf_bytesex.f -----
