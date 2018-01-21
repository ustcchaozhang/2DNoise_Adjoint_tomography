c this is <pgs_par1.f>
c this was <pgs_par.f>
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
c plot polynomial parameter par from model mod
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    13/01/99          just keep the old version of the routine 
c                      as model definition changed (see glq_model.inc)
c 
      subroutine pgs_par1(mod, par)
c
c 20/01/98: plot reciprocal values for velocities and Q
c V1.16: return to velocity and Q
c
      integer mod, par
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_pgpara.inc'
      include 'glq_verbose.inc'
c 
cE
      integer nplot
      parameter(nplot=100)
      real z(nplot), y(nplot), dz, dep, x
      integer sec, nstep, i, k, npol
c 
      if (verb_subaction) print *,'ENTER pgs_par1(',mod,',',par,')'
c 
      nstep=nplot-2*glqm_nsec
      dz=1.e3*(dmodel(glqm_nlay, mi_depth)-dmodel(2, mi_depth))/(nstep-1)
c 
      k=0
      dep=0.
      sec=1
c consider follow mode
      npol=glqm_npol(sec, par)
      if ((glqm_follow(sec, par)).and.(sec.lt.glqm_nsec)) npol=npol+1
      npol=min(3,npol)
      do i=1,nstep
        dep=dz*(i-1)
c plot interface
        if (sec.lt.glqm_nsec) then
          if (dep.gt.mdepth(sec+1, mod)) then
            k=k+1
            z(k)=mdepth(sec+1, mod)
            x=z(k)-mdepth(sec, mod)
            y(k)=model(1, sec, par, mod)
            if (npol.gt.1)
     &        y(k)=y(k)+x*model(2, sec, par, mod)
            if (npol.gt.2)
     &        y(k)=y(k)+x*x*model(3, sec, par, mod)
            k=k+1
            sec=sec+1
c consider follow mode
            npol=glqm_npol(sec, par)
            if ((glqm_follow(sec, par)).and.(sec.lt.glqm_nsec)) npol=npol+1
            npol=min(3,npol)
            z(k)=z(k-1)
            y(k)=model(1, sec, par, mod)
          endif
        endif
c plot value
        k=k+1
        z(k)=dep
        x=z(k)-mdepth(sec, mod)
        y(k)=model(1, sec, par, mod)
        if (npol.gt.1)
     &    y(k)=y(k)+x*model(2, sec, par, mod)
        if (npol.gt.2)
     &    y(k)=y(k)+x*x*model(3, sec, par, mod)
      enddo
c set correct depth (in km)
      do i=1,k
        z(i)=z(i)*1.e-3
      enddo
c and use reciprocal values for velocities and Q values
c V1.16: return to velocity and Q
c      if (par.ne.mi_density) then
c        do i=1,k
c          y(i)=1./y(i)
c        enddo
c      endif
c plot it
      call pgline(k, y, z)
c 
      if (verb_subaction) print *,'LEAVE pgs_par1'
c 
      return
      end
c
c ----- END OF pgs_par.f -----
c
c ----- END OF pgs_par1.f -----
