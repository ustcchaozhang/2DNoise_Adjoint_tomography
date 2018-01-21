c this is <pgs_par.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c plot polynomial parameter par from model mod
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
c    13/01/99   V2.0   model definition changed (see glq_model.inc)
c    14/01/99   V2.1   follow flags may be irgnored
c    20/01/99   V2.2   - new plot options (reduced colour index and pg_clw)
c                      - changed calling convention
c    22/01/99   V2.3   - changed calling convention - maxdepth gives the
c                        bottom depth of the plot (in km)
c                      - now tracks an extra coli
c    23/01/99   V2.4   - changed calculation of depth stepsize to match
c                        maxdepth
c
      subroutine pgs_par(mod, par, hatch, maxdepth)
c
c 20/01/98: plot reciprocal values for velocities and Q
c V1.16: return to velocity and Q
c 
c if hatch is switched on a hatched filled polygon will be plottet
c
      integer mod, par
      real maxdepth
      logical hatch
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_pgpara.inc'
      include 'glq_verbose.inc'
c 
cE
      integer nplot
      parameter(nplot=100)
      real z(2*nplot), y(2*nplot), dz, dep, x, secbot, sectop, zref
      real zp(nplot,glqm_mpar), yp(nplot,glqm_mpar)
      integer ci(nplot), coli,rcoli
      integer npoly
      save npoly,zp,yp
      integer sec, nstep, i, k, npol
      logical flag, ciflag
c 
      if (verb_subaction) print *,'ENTER pgs_par(',mod,',',par,',',hatch,')'
c 
      call pg_colis(par,coli,rcoli)
c 
      if (coli.eq.rcoli) then
        ciflag=.false.
        flag=.true.
      else
        ciflag=.true.
      endif
c 
      nstep=nplot-2*glqm_nsec
      dz=1.e3*(maxdepth)/(nstep-2)
c 
      k=0
      dep=0.
      sec=1
      sectop=0.
      secbot=mdepth(sec, mod)
      zref=0.5*(sectop+secbot)
c consider follow mode
      npol=glqm_npol(sec, par)
      do i=1,nstep
        dep=dz*(i-1)
c plot interface
        if (sec.le.glqm_nsec) then
          if (dep.gt.mdepth(sec, mod)) then
            k=k+1
            z(k)=secbot
            if (ciflag) call pg_zcoli(par,dble(z(k)),dble(z(k-1)),mod,flag)
            if (flag) then
              ci(k)=coli
            else
              ci(k)=rcoli
            endif
c get value above interface
            x=z(k)-zref
            y(k)=model(1, sec, par, mod)
            if (npol.gt.1)
     &        y(k)=y(k)+x*model(2, sec, par, mod)
            if (npol.gt.2)
     &        y(k)=y(k)+x*x*model(3, sec, par, mod)
c cross interface
            k=k+1
            sec=sec+1
            if (sec.le.glqm_nsec) then
c enter next section 
              sectop=secbot
              secbot=mdepth(sec, mod)
              zref=0.5*(sectop+secbot)
              z(k)=z(k-1)
              if (destim(sec-1)) then
                ci(k)=coli
              else
                ci(k)=rcoli
              endif
c get next plot value (below interface)
              x=z(k)-zref
              npol=glqm_npol(sec, par)
              y(k)=model(1, sec, par, mod)
              if (npol.gt.1)
     &          y(k)=y(k)+x*model(2, sec, par, mod)
              if (npol.gt.2)
     &          y(k)=y(k)+x*x*model(3, sec, par, mod)
            else
c enter halfspace 
c (multiply depth with 1000 as it will later be divided by this value)
              z(k)=maxdepth*1.e3
              ci(k)=ci(k-1)
              y(k)=y(k-1)
            endif
          else
c plotvalue
            k=k+1
            z(k)=dep
            if (ciflag) call pg_zcoli(par,dble(z(k)),dble(z(k-1)),mod,flag)
            if (flag) then
              ci(k)=coli
            else
              ci(k)=rcoli
            endif
            x=z(k)-zref
            y(k)=model(1, sec, par, mod)
            if (npol.gt.1)
     &        y(k)=y(k)+x*model(2, sec, par, mod)
            if (npol.gt.2)
     &        y(k)=y(k)+x*x*model(3, sec, par, mod)
          endif
        else
          z(k)=dep
          ci(k)=ci(k-1)
          y(k)=y(k-1)
        endif
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
      if (pg_dohatch) then
        if (hatch) then
          do i=1,npoly
            z(k+i)=zp(npoly+1-i,par)
            y(k+i)=yp(npoly+1-i,par)
          enddo
          call pgslw(pg_lw)
          call pgshs(45.,1.,0.)
          if ((par.eq.mi_alpha).or.(par.eq.mi_Qalpha)) then
            call pgsci(pg_alpharcol)
          elseif ((par.eq.mi_beta).or.(par.eq.mi_Qbeta)) then
            call pgsci(pg_betarcol)
            call pgshs(135.,1.,0.)
          else
            call pgsci(pg_rcolind)
          endif
          call pgsfs(3)
          call pgslw(pg_lw)
          call pgpoly(k+npoly,y,z)
          call pgsfs(1)
c          call pgline(k+npoly,y,z)
        else
          npoly=k
          do i=1,npoly
            zp(i,par)=z(i)
            yp(i,par)=y(i)
          enddo
        endif
      endif
c 
      call pgslw(pg_clw)
      call pgmove(y(1),z(1))
      call pgsci(coli)
      do i=2,k
        if (ciflag) call pgsci(ci(i))
        call pgdraw(y(i),z(i))
      enddo
c
      call pgslw(pg_lw)
c 
      if (verb_subaction) print *,'LEAVE pgs_par'
c 
      return
      end
c
c ----- END OF pgs_par.f -----
