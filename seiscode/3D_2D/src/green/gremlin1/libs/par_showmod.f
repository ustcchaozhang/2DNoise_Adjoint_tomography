c this is <par_showmod.f>
c------------------------------------------------------------------------------
cS
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
c display model related parameter set
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    13/01/98   V1.1   there is no chop_hs anymore (see glq_model.inc)
c    11/04/00   V1.2   introduced vptrackfactor
c 
      subroutine par_showmod
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_model.inc'
c 
cE
      integer i
c
c      print 50, chop_hs, chop_step, chop_master, chop_finett
      print 50, chop_step, chop_master, chop_finett
      if (vptrackfactor.gt.0.d0) then
        print 53, vptrackfactor, 'vp will track vs'
      else
        print 53, vptrackfactor, 'no tracking'
      endif
      print 51, (rng_mmin(i), i=1,glqm_mpar)
      print 52, (rng_mmax(i), i=1,glqm_mpar)
c 
      return
   50 format(/'model parameters',/
c     &       '  top of halfspace',t30,'[tohs]: ',f10.3,' m',/
     &       '  chopping stepsize',t30,'[chst]: ',f10.7,/
     &       '  chopping master parameter',t30,'[cmas]: ',i10,' (0=off)'/
     &       '  fine travel times with',t30,'[fitt]: ',i10,
     &       ' layers (0=use standard chop)')
   51 format('  minimum values:',/
     &    t6,'alpha',t18,'beta',t30,'density',t42,'Qalpha',t54,'Qbeta',/
     &    5(f12.3))
   52 format('  maximum values:',/
     &    t6,'alpha',t18,'beta',t30,'density',t42,'Qalpha',t54,'Qbeta',/
     &    5(f12.3))
   53 format('  vp track factor',t30,'[vptr]: ',f10.3,1x,a)
      end
c
c ----- END OF par_showmod.f -----
