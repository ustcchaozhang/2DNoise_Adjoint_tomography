c this is <earthmod.f>
c------------------------------------------------------------------------------
c
c 23/06/99 by Thomas Forbriger (IfG Stuttgart)
c
c psexpl earth model reading
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
c    23/06/99   V1.0   Thomas Forbriger
c    06/12/07   V1.1   replace o by 0 in line 64 (apparently a typo)
c
c==============================================================================
c
      subroutine earthmod(funit, maxlay, z, d, a, b, rho, qa ,qb, nlay)
c 
c declare parameters
c
      integer nlay, maxlay, funit
      real z(maxlay),d(maxlay),a(maxlay),b(maxlay),rho(maxlay)
      real qa(maxlay), qb(maxlay)
c 
cE
c declare variables
c
      integer maxinlay, ninlay
      parameter (maxinlay=20)
      real zi(maxinlay), ai(maxinlay), bi(maxinlay), rhoi(maxinlay)
      real qai(maxinlay), qbi(maxinlay)
      integer nhs(maxinlay)
      real anhs, dz, da, db ,drho, dqa, dqb
      real sz, sa, sb, srho, sqa, sqb
      integer i, lay
      real fac
      logical debug
      debug=.FALSE.
c 
c read information from file
c
      ninlay=0
    2 continue
      ninlay=ninlay+1
      if (ninlay.gt.maxinlay)
     &  stop 'ERROR: too many layers - increase dimension maxinlay'
      read(funit, 1, err=99, end=98) 
     &               zi(ninlay), ai(ninlay), bi(ninlay), 
     &               rhoi(ninlay), qai(ninlay), qbi(ninlay), 
     &               nhs(ninlay)
    1 format(6f10.3,i5)
      if (debug) print *,i,
     &               zi(ninlay), ai(ninlay), bi(ninlay), 
     &               rhoi(ninlay), qai(ninlay), qbi(ninlay), 
     &               nhs(ninlay)
      if (rhoi(ninlay).eq.0.) goto 3
      if (qai(ninlay).eq.0.) qai(ninlay)=10.**6
      if (qbi(ninlay).eq.0.) qbi(ninlay)=10.**6
      if (qbi(ninlay).eq.1.) qbi(ninlay)=4.*qai(ninlay)/9.
      goto 2
    3 continue
      ninlay=ninlay-1
c 
c evaluate multilayer model
c
      nlay=0
      do 4 lay=2,ninlay
        if (nhs(lay).eq.0) goto 5
        anhs=float(nhs(lay))
c stepwidth
        da=(ai(lay)-ai(lay-1))/anhs
        db=(bi(lay)-bi(lay-1))/anhs
        dqa=(qai(lay)-qai(lay-1))/anhs
        dqb=(qbi(lay)-qbi(lay-1))/anhs
        drho=(rhoi(lay)-rhoi(lay-1))/anhs
        dz=(zi(lay)-zi(lay-1))/anhs
c start value
        sa=ai(lay-1)-da/2.
        sb=bi(lay-1)-db/2.
        sqb=qbi(lay-1)-dqb/2.
        sqa=qai(lay-1)-dqa/2.
        srho=rhoi(lay-1)-drho/2.
        sz=zi(lay-1)-dz/2.
c go
        do 6 i=1,nhs(lay)
          nlay=nlay+1
          if (nlay.gt.maxlay)
     &      stop 'ERROR: too many layers - increase maxlay'
          fac=float(i)
          a(nlay)=sa+fac*da
          b(nlay)=sb+fac*db
          rho(nlay)=srho+fac*drho
          qa(nlay)=sqa+fac*dqa
          qb(nlay)=sqb+fac*dqb
          z(nlay)=sz+fac*dz
          d(nlay)=dz
    6   continue
    5 continue
    4 continue
c deepest layer
      nlay=nlay+1
      if (nlay.gt.maxlay)
     &  stop 'ERROR: too many layers - increase maxlay'
      a(nlay)=ai(ninlay)
      b(nlay)=bi(ninlay)
      qa(nlay)=qai(ninlay)
      qb(nlay)=qbi(ninlay)
      rho(nlay)=rhoi(ninlay)
      z(nlay)=zi(ninlay)
      d(nlay)=10.**6
      return
   99 stop 'ERROR: reading model file'
   98 stop 'ERROR: reading model file - unexpected end of file'
      end
c
c ----- END OF earthmod.f -----
