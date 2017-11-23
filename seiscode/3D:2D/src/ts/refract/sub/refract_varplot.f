c this is <refract_varplot.f> 
c----------------------------------------------------------------------
c
c 17/02/98 by Thomas Forbriger (IfG Stuttgart)
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
c "wiggle plot" or "variable area plot" (BISON jargon)
c
c REVISIONS and CHANGES
c    17/02/98   V1.0   Thomas Forbriger
c    22/01/01   V1.1   some improvements :-)
c
c ============================================================================
c
      subroutine varplot(x, y, my, n, s)
c
c plot in variable area mode
c
c x,y: dataset
c my: baseline
c n: number of data points
c s: .false.: fill positive wiggles (>=my)
c    .true.:  fill negative wiggles (<=my)
c
      real x(n), y(n)
      real my
      integer n
      logical s
c 
      integer mwiggle
      parameter(mwiggle=200)
      real xw(mwiggle), yw(mwiggle)
      real frac,dx
      integer i,poly
c 
c poly counts the number os samples we have already picked up into our
c polygon-array
      poly=0
c
c go through all samples
      do i=1,n
c
c is the polygon array full? if yes, plot and clear it
        if (poly.eq.(mwiggle-1)) then
          poly=poly+1
c   do we have to continue this wiggle?
          if (i.lt.n) then
            if (((y(i+1).ge.my).and.(.not.s)).or.((y(i+1).le.my).and.s)) then
              xw(poly)=xw(poly-1)
            else
              xw(poly)=x(i)+(y(i)-y(i+1))/(y(i)-my)
            endif
          else
            xw(poly)=xw(poly-1)
          endif
          yw(poly)=my
          call pgpoly(poly, xw, yw)
c          xw(1)=xw(poly-1)
c          yw(1)=yw(poly-1)
c          xw(2)=xw(poly)
c          yw(2)=yw(poly)
c          poly=2
          poly=0
        endif
c
c keep collecting as long as we are above my
c ------------------------------------------
        if (((y(i).ge.my).and.(.not.s)).or.((y(i).le.my).and.s)) then
c 
c if this is the first sample in the wiggle, find correct base position
          if (poly.eq.0) then
c   it's easy if this is the first sample in the dataset to pick up
            poly=1
            if (i.eq.1) then
              xw(poly)=x(i)
c   is this just part of a large wiggle?
            elseif 
     &        (((y(i-1).ge.my).and.(.not.s)).or.((y(i-1).le.my).and.s)) then
              xw(poly)=x(i)
c   well... we'll have to find the baseline intersection
            else
              dx=x(i)-x(i-1)
              frac=(my-y(i))/(y(i-1)-y(i))
              xw(poly)=x(i)-dx*frac
            endif
            yw(poly)=my
          endif
          poly=poly+1
          xw(poly)=x(i)
          yw(poly)=y(i)
        else
c
c maybe we just dropped below my - then plot last wiggle...
c ---------------------------------------------------------
          if (poly.ne.0) then
            poly=poly+1
            dx=x(i)-x(i-1)
            frac=(my-y(i))/(y(i-1)-y(i))
            xw(poly)=x(i)-dx*frac
            yw(poly)=my
            call pgpoly(poly, xw, yw)
            poly=0
          endif
        endif
      enddo
c
c we are through - are there some sample left? plot them...
      if (poly.ne.0) then
         poly=poly+1
         xw(poly)=xw(poly-1)
         yw(poly)=my
         call pgpoly(poly, xw, yw)
         poly=0
      endif
      return
      end
c
c ----- END OF <refract_varplot.f> -----
