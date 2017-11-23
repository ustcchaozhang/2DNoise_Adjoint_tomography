c this is <pg_inv.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c proceeding inversion be decreasing nu
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
c----------------------------------------------------------------------
c 
      subroutine pg_inv(numax, nsteps, ivp)
c
c proceeding inversion be decreasing nu
c
c numax:     where to start with nu
c nsteps:    number of steps decreasing nu (but will be limited to maxsteps)
c ivp:       viewport index to be used
c
      real numax
      integer nsteps, ivp
c 
      include 'glq_dim.inc'
      include 'glq_pgpara.inc'
      include 'glq_para.inc'
      include 'glq_invres.inc'
      include 'glq_verbose.inc'
c
cE
c notice: nu(i) will be 1/nu in fact
c 
      integer i, maxsteps, nst, maxn
      parameter(maxsteps=50)
      real nu(maxsteps), X2(maxsteps), X2ref, X2max, X2min
      real dat_X2, thisnu
      logical inv_model, hot, mod_prep
      real oonumin, oonumax
c 
      if (verb_subaction) print *,'ENTER pg_inv(',numax,',',nsteps,',',ivp,')'
c
      nst=min(nsteps,maxsteps)
c select viewport
      call pg_selvp(ivp)
c 
      call pgsci(pg_colind)
      call pgslw(pg_lw)
c
c calculate X2 of reference data
      X2ref=dat_X2(.true.)
c 
c set expected world coordinates
      oonumax=1./(numax*(1.-float(nst-1)/nst))
      oonumin=1./numax
      call pgswin(.95*oonumin,1.05*oonumax,0.,(1.05*X2ref))
      call pgbox('BC',0.0,0,'BC',0.0,0)
      call pglab('1/\gn','\gx\u2','find best \gn')
c 
      do i=1,nst
        X2(i)=0.
      enddo
c 
      call pgsci(pg_alphacol)
      call pgslw(pg_bestlw)
      maxn=1
      found_best=.false.
      hot=.true.
      do i=1,nst
        if (hot) then
          nu(i)=oonumin+(oonumax-oonumin)*float(i-1)/float(nst-1)
          thisnu=1./nu(i)
          if (verb_substrategy) print *,
     &      'NOTICE (pg_inv): find better model for nu ',thisnu
          if (inv_model(thisnu)) then
            if (mod_prep()) then
              call dat_synt(.false.)
              maxn=i
              small_nu=thisnu
              X2(i)=dat_X2(.false.)
              if (verb_substrategy) print *,'NOTICE (pg_inv): X2 is ',X2(i)
              if (i.gt.1) then
                call pgdraw(nu(i),X2(i))
                if (X2(i).gt.X2(i-1)) then
                  hot=.false.
                  found_best=.true.
                  best_nu=1./nu(i-1)
                endif
              else
                call pgmove(nu(i),X2(i))
              endif
              call pgupdt
            else
              hot=.false.
            endif
          endif
        endif
      enddo
      call pgsci(pg_colind)
      call pgslw(pg_lw)
c
      if (verb_subresult) then
        if (found_best) then
          print *,'NOTICE (inv_pg): best nu is ',best_nu
        else
          print *,'NOTICE (inv_pg): smallest nu is ',small_nu
        endif
      endif
c 
      X2max=X2(1)
      X2min=X2(1)
      do i=1,maxn
        X2max=max(X2max,X2(i))
        X2min=min(X2min,X2(i))
      enddo
c 
      call pgswin(0.,1.,0.,1.)
      call pgsci(0)
      call pgrect(0.,1.,0.,1.)
      call pgsci(pg_colind)
c 
      call pgswin(nu(1),nu(maxn),X2min,X2max)
      call pgbox('BCGNTS',0.0,0,'BCGNTS',0.0,0)
      call pgsci(pg_alphacol)
      call pgslw(pg_bestlw)
      call pgline(maxn, nu, X2)
      call pgsci(pg_colind)
      call pgslw(pg_lw)
      call pgupdt
c 
      if (verb_subaction) print *,'LEAVE pg_inv'
c 
      return
      end
c
c ----- END OF pg_inv.f -----
