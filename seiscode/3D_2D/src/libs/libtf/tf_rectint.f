c this is <tf_rectint.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c do linear interpolation within a rectangular area
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
c
c==============================================================================
cS
c
c  tf_rectint
c
c  rectangluar interpolation
c 
c  interpolate linearly between the corner values of a rectangular
c  the function returns the value of the interpolation
c  what we nedd is:
c    X1,X2,Y1,Y2:  coordinates defining the rectangular
c    A1: value of point 1 (X1,Y1)
c    A2: value of point 2 (X2,Y1)
c    A3: value of point 3 (X2,Y2)
c    A4: value of point 4 (X1,Y2)
c    XP,YP: coordinates of desired point
c
      real function tf_rectint(X1, X2, Y1, Y2, 
     &                         A1, A2, A3, A4, XP, YP)
c
c declare parameters
c
      real X1, X2, Y1, Y2, A1, A2, A3, A4, XP, YP
c
cE
c declare variables
c
      real XR, YR, RY1, RY2
      real a,b,p,f,relcor,relval
c
c declare functions
c
c relative coordinates between edges
      RELCOR(A,B,P)=(P-A)/(B-A)
c relative value
      RELVAL(A,B,F)=A*(1-F)+B*F
c go
      if (X1.eq.X2) then
        XR=0.5
      else
        XR=RELCOR(X1,X2,XP)
      endif
      if (Y1.eq.Y2) then
        YR=0.5
      else
        YR=RELCOR(Y1,Y2,YP)
      endif
      RY1=RELVAL(A1,A2,XR)
      RY2=RELVAL(A4,A3,XR)
      tf_rectint=RELVAL(RY1,RY2,YR)
      return
      
      end
c
c ----- END OF tf_rectint.f -----
