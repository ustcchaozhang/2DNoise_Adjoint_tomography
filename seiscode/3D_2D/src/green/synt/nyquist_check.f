c this is <nyquist_check.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
c
c Check Nyquist criterion for trapezoid rule
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
c    05/07/2007   V1.0   Thomas Forbriger
c    22/10/2007   V1.1   use correct units
c
c ============================================================================
c
      subroutine checkslownesssampling(r,p,o)
c
      real r, p, o
c
c     r:    receiver offset in m
c     p:    slowness interval in s/m
c     o:    maximum angular frequency in 1/s
c
c     This function will check whether the slowness sampling is adequate for
c     the oscillating Bessel functions in the slowness expansion integral when
c     applying a trapezoid rule with slowness interval p.
c
c     The subroutine will emit a warning, when the step is larger then pi/2.
c     It will abort, when the step size is larger than 1.8*pi.
c
      real d,pi2
      parameter(pi2=2.*3.14159265358979)
      d=o*r*p/pi2
      if (d.gt.0.25) then
        print *,'WARNING: The trapezoid stepsize is ',d,'*2*pi = ',
     &    2*d,'*pi.'
        print *,'         Receiver offset:   ',1.e-3*r,' km'
        print *,'         Maximum frequency: ',o/pi2,' Hz'
        print *,'         Slowness stepsize: ',p*1.e3,' s/km'
        if (d.gt.0.9) then
          print *,'!! The stepsize for the argument of the Bessel'
          print *,'!! function is larger than 1.8*pi. The' 
          print *,'!! trapezoid rule approximation is very likely'
          print *,'!! to fail. The program aborts for this reason.'
          stop 'aborted...'
        else
          print *,'!! The stepsize for the argument of the Bessel'
          print *,'!! function is larger than pi/2. The' 
          print *,'!! trapezoid rule approximation is very likely'
          print *,'!! to be inaccurate!'
          print *,''
        endif
      endif
      return
      end
c
c ----- END OF nyquist_check.f ----- 
