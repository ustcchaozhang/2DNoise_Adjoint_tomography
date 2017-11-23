c this is <grepg_prepcol.f>
c------------------------------------------------------------------------------
cS
c
c 23/05/2000 by Thomas Forbriger (IfG Stuttgart)
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
c prepare colors
c
c REVISIONS and CHANGES
c    23/05/2000   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine grepg_prepcol(pswitch,pshift,grayfac,hlssystem)
c
c declare parameters
c pswtich:    create a phase plotting scale
c pshift:     phaseshift value
c grayfac:    grayify value
c hlssystem:  use hls system for color mapping
c
      logical pswitch,hlssystem
      real pshift, grayfac
c
cE
c declare local variables
      integer mincol, maxcol, i
      real value, r,g,b,hm,lm,sm,hd,ld,sd,h,l,s,pi
      parameter(pi=3.141592653589793115997963468)
c
c------------------------------------------------------------------------------
c go
      call pgqcir(mincol,maxcol)
      if (maxcol.lt.(mincol+5)) stop 'ERROR: too few colors available'
      if (hlssystem) then
        call pgqcr(0,r,g,b)
        call grxhls(r,g,b,hm,lm,sm)
        call pgqcr(1,r,g,b)
        call grxhls(r,g,b,hd,ld,sd)
      else
        call pgqcr(0,hm,lm,sm)
        call pgqcr(1,hd,ld,sd)
      endif
      hd=hd-hm
      ld=ld-lm
      sd=sd-sm
      do i=mincol,maxcol
        if (pswitch) then
          value=2.*pi*float(i-mincol)/float(maxcol-mincol)+pshift*pi/180.
          value=0.5*(1.+cos(value))*grayfac
        else
          value=float(i-mincol)/float(maxcol-mincol)
        endif
        h=value*hd+hm
        l=value*ld+lm
        s=value*sd+sm
        if (hlssystem) then
          call pgshls(i,h,l,s)
        else
          call pgscr(i,h,l,s)
        endif
      enddo
c
      return
      end
c
c ----- END OF grepg_prepcol.f -----
