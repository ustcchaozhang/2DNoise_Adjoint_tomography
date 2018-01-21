c this is <inv_addtolist.f>
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
c
cS
c----------------------------------------------------------------------
c
      subroutine inv_addtolist(x, y, n, m, iext, xn, yn, f, u)
c 
c add element to a history list
c
c x(m):   x-values (in/out)
c y(m):   y-values (in/out)
c n:      number of elements in list (in/out)
c m:      dimension of list (in)
c iext:   index of extreme value (out)
c xn:     new x-value (in)
c yn:     new y-value (in)
c f:      .true.: keep minimum (in)
c         .false.: keep maximum (in)
c u:      .true.: sort in increasing order (in)
c         .false.: sort in decreasing order (in)
c
      include 'glq_verbose.inc'
c 
      integer n, m, iext
      real x(m), y(m), xn, yn
      logical f, u
c
cE
      integer i, ni
      real exval
c 
      if (verb_subaction) print *,'ENTER inv_addtolist(x,y,',n,',',m,
     &  ',',iext,',',xn,',',yn,',',f,',',u,')'
c 
      if (n.gt.0) then
        exval=y(1)
        iext=1
        do i=1,n
          if ((f).and.(y(i).lt.exval)) then
            exval=y(i)
            iext=i
          elseif ((.not.(f)).and.(y(i).gt.exval)) then
            exval=y(i)
            iext=i
          endif
        enddo
c 
        if (n.eq.m) then
          if (iext.gt.(m/2)) then
            n=m-1
            do i=1,m-1
              x(i)=x(i+1)
              y(i)=y(i+1)
            enddo
          else
            n=m-1
          endif
        endif
c 
        ni=1
        if (u) then
          do while ((ni.le.n).and.(x(ni).gt.xn))
             ni=ni+1
          enddo
        else
          do while ((ni.le.n).and.(x(ni).lt.xn))
             ni=ni+1
          enddo
        endif
c 
        do i=n,ni,-1
          x(i+1)=x(i)
          y(i+1)=y(i)
        enddo
        n=n+1
c 
        x(ni)=xn
        y(ni)=yn
c 
        exval=y(1)
        iext=1
        do i=1,n
          if ((f).and.(y(i).lt.exval)) then
            exval=y(i)
            iext=i
          elseif ((.not.(f)).and.(y(i).gt.exval)) then
            exval=y(i)
            iext=i
          endif
        enddo
c 
      else
        n=1
        x(1)=xn
        y(1)=yn
        iext=1
      endif
c 
      if (verb_subaction) print *,'LEAVE inv_addtolist'
c 
      return
      end
c
c ----- END OF inv_addtolist.f -----
