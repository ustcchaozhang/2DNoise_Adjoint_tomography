c this is <tf_listselect.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c evaluate selection list
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
c    24/06/97   V1.0   Thomas Forbriger
c    24/08/2012 V1.1   support stepsize
c
c==============================================================================
cS
c 
c tf_listselect
c
c intepretes a string to a selection list
c string must be of the form: 4-7,9,13-26
c
c if you like to specify a stepsize, use '+' like in 5+3-12
c which take every third staring at 5 and ending at 12
c 
c parameters:
c    maxindex     maximum selectable index
c    selection    logical array from 1 to maxindex indicating selections
c    first        first character to be interpreted
c    list         string containing selection to be evaluated
c                 terminated by a blank
c    ierr         =0 no problem 
c                 =1 index out of range 
c                 =2 empty list
c
      subroutine tf_listselect(maxindex, selection, 
     &                            first, list, ierr)
c 
c parameters
c
      integer first, maxindex, ierr
      logical selection(maxindex)
      character list*(*)
c
cE
c local variables
c
      integer i, cf, cl, ifrom, ito, ce, stepsize
      logical range
c
c go
c     
      stepsize=1
      ifrom=0
      ierr=0
      do i=1,maxindex
        selection(i)=.false.
      enddo
      range=.false.
      cf=first
    3 cl=cf-1
    1 cl=cl+1
      if ((list(cl:cl).eq.'0').or.
     &    ((list(cl:cl).ge.'1').and.(list(cl:cl).le.'9'))) goto 1
      ce=cl-1
      if (cl.eq.cf) then
        ierr=2
        return
      endif
      read(list(cf:ce), *) ito
      if (ito.gt.maxindex) then
        ierr=1
        ito=maxindex
      endif
      if (range) then
        if (stepsize .ge. 1) then
          do i=min(ifrom,ito),max(ifrom,ito),stepsize
            selection(i)=.true.
          enddo
          range=.false.
          stepsize=1
        endif
      endif
      if (list(cl:cl).eq.'-') then
        if (stepsize.lt.0) then
          stepsize=ito
        else
          ifrom=ito
        endif
        range=.true.
      elseif (list(cl:cl).eq.'+') then
        ifrom=ito
        stepsize=-1
        range=.true.
      else
        selection(ito)=.true.
      endif
      cf=cl-1
    2 cf=cf+1
      if (.not.((list(cf:cf).eq.'0').or.(list(cf:cf).eq.' ').or.
     &    ((list(cf:cf).ge.'1').and.(list(cf:cf).le.'9')))) goto 2
      if (list(cf:cf).ne.' ') goto 3
      return
      end
c
c
c ----- END OF tf_listselect.f -----
