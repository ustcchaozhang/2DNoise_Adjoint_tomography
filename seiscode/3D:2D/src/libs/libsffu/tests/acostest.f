c this is <acostest.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
c
c test numerical resolution for calculation of epicentral distance with
c spherical coordinates.
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
c along with this program. If not, see <http://www.gnu.org/licenses/>.
c ----
c
c REVISIONS and CHANGES
c    01/12/2016   V1.0   Thomas Forbriger
c
c ============================================================================
c
      program acostest
c
        integer i, imax, k
        parameter(imax=10)
        real phi,phimax,cphi, rphi, r, rmax, rr, raphi, rra
        double precision dphi, dcphi, drphi, draphi, drr, drra, cf
        parameter(rmax=4.5)
        parameter(cf=2.d0*3.14159265358979/6371.)
        parameter(phimax=rmax*cf)

      print *,'Test numerical resolution for calculation of epicentral'
      print *,'distance with spherical coordinates.'
      print *,' '
      print *,'The epicentral distance usually is represented by a'
      print *,'cosine of the distance angle as computed with the'
      print *,'cosine rule of spherical geometry. We do not test'
      print *,'the numerical precision of the cosine rule. We just'
      print *,'test the inverse to the cosine function and the'
      print *,'representation of the angle by a cosine values.'
      print *,'The computation of the cosine rule in single'
      print *,'precision most likely will add additional inaccuracy.'
      print *,' '
      print *,'first line:'
      print *,'  r: epicentral distance on Earth'' surface in km'
      print *,'  phi: corresponding distance angle'
      print *,'  r/km: distance computed from cosine of distance angle'
      print *,'        four values are presented'
      print *,'   1st: single precision computation with acos-function'
      print *,'   2nd: single precision computation with atan2-function'
      print *,'   3rd: double precision computation with acos-function'
      print *,'   4th: double precision computation with atan2-function'
      print *,'  residual: residual between single and double precision'
      print *,'            results'
      print *,'second line:'
      print *,'  cos(phi): cosine of distance angle'
      print *,'   1st: represented in single precision'
      print *,'   2nd: represented in double precision'
      print *,'  residual: the distance information is contained in the'
      print *,'            difference of the cosine value to 1. the'
      print *,'            residual specifies the residual between this'
      print *,'            difference in single precision and the'
      print *,'            difference represented in double precision'
      print *,' '

      do k=1,10,9
      do i=1,imax
        phi=k*phimax*i/imax
        r=k*rmax*i/imax
        dphi=phi
        cphi=cos(phi)
        dcphi=cos(dphi)
        rphi=acos(cphi)
        rr=rphi/cf
        raphi=atan2(sqrt(1.-cphi**2),cphi)
        rra=raphi/cf
        drphi=acos(dcphi)
        drr=drphi/cf
        draphi=atan2(sqrt(1.d0-dcphi**2),dcphi)
        drra=draphi/cf
        print 50,r, phi*180./3.14159265358979, rr, rra, drr, drra,
     &    (100.*(1.d0-(rr/drr)))
        print 51,cphi,dcphi,100.*(1.d0-((1.d0-cphi)/(1.d0-dcphi)))
      enddo
      enddo

      stop
   50 format('r=',f6.3,'km phi/°=',f5.3,' r/km=',4(f6.3,1x),
     &      ' residual=',f7.2,' %')
   51 format(10x,' cos(phi)=',2(f17.12,1x),' residual=',f7.2,' %')
      end
c
c
c ----- END OF acostest.f ----- 
