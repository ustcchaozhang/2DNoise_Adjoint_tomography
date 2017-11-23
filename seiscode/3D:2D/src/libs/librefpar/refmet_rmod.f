c this is <sub/refmet_rmod.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
c
c read and reconfigure earth model from file
c
c ============================================================================
c
c this is part of the REFMET reflectivity program
c (for comments and revisions see also the main source refmet.f)
c 
c REVISIONS and CHANGES
c 09/02/97   changed to fit new model format including reference frequency
c 16/07/98   give correct model layer input format in help section (f10,1x)
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
c======================================================================
c 
c read and reconfigure earth model from file
c
      subroutine refmet_rmod(modelfile, modeltext, MS, nuref,
     &  radius, n, z, alpha, beta, rho, qa, qb, cl_vlevel, cl_debug, lev2)
c 
      integer MS, N
      character modelfile*(*), modeltext*(*)
      real*8 radius, z(0:MS), alpha(0:MS), beta(0:MS), rho(0:MS), nuref
      real qa(0:MS), qb(0:MS)
      integer cl_vlevel, lev2
      logical cl_debug
c 
      character*80 line
      integer i, modlu, modellen
      parameter(modlu=11)
c 
      modellen=index(modelfile, ' ')-1
      if (cl_vlevel.gt.lev2) print 52,'opening ',modelfile(1:modellen)
      open(modlu, file=modelfile, status='old', err=99)

      read(modlu, '(a72)', err=98, end=97) modeltext
      if (cl_debug) print *,'DEBUG: read modeltext',modeltext

      read(modlu, '(//30x,f10.3,30x,f10.3)', err=98, end=97) radius, nuref

      read(modlu, '(//)', err=98, end=97)

      n=0
  100 continue
        read(modlu, '(a80)', err=98, end=97) line
        if (cl_debug) print *,'DEBUG: n,line ',n,line
        if (line(1:10).ne.'halfspace:') then
          read(line, '(f10.3,5(1x,f10.6))', err=98, end=97)
     &      z(n),alpha(n),beta(n),rho(n),qa(n),qb(n)
          if (n.eq.MS) then
            print *,'WARNING: too many layers'
            print *,'WARNING: stopped model reading at depth ',z(n)
            print *,'WARNING: layer ',n,' will be used as lower halfspace'
          else
            n=n+1
            goto 100
          endif
        endif
      if (n.lt.MS) then
        read(line, '(10x,5(1x,f10.6))', err=98, end=97)
     &    alpha(n),beta(n),rho(n),qa(n),qb(n)
c depth with halfspace index will be set below
      endif
      if (n.lt.1) stop 'ERROR we need at least a top and a bottom halfspace'
        
      if (cl_vlevel.gt.lev2) print 52,'closing ',modelfile(1:modellen)
      close(modlu, err=96)

c z-values must be top of layer
      do i=n,1,-1
        z(i)=z(i-1)
      enddo
c fix special case values for reflectivity use
      do i=0,n
        if (qb(i).lt.0.) qb(i)=1.d2
        if (beta(i).lt.1.d-5) beta(i)=1.d-5
      enddo
      return
   52 format(/a,1x,a)
   99 stop 'ERROR opening model file'
   98 stop 'ERROR reading model file'
   97 stop 'ERROR reading model file - unexpected end of file'
   96 stop 'ERROR closing model file'
      end

c----------------------------------------------------------------------
c
c Information on model file structure
c
      subroutine refmet_modinf
c 
      print 50,' '
      print 50,'How to build an earth model file'
      print 50,'================================'
      print 50,' '
      print 51,'line','contents'

      print 52,1,'text'
      print 53,'text','a70','any comment on earth model'

      print 52,4,'(30x,f10,30x,f10) radius, fref'
      print 53,'radius','f10','earth radius (in km) to which flattened model'
      print 55,'corresponds. A negative earth radius indicates that the'
      print 55,'model was not transformed from spherical to flat geometry.'
      print 55,'In this case there will be no amplitude correction applied.'
      print 54,'fref','f10','reference frequency of model parameters to be'
      print 55,'used when calculating velocity dispersion.'

      print 52,8,'6(f10,1x) Zb, alpha, beta, rho, Qa, Qb'
      print 53,'Zb','f10','depth of bottom of layer in km. The first depth'
      print 55,'must be 0.km being the bottom of the top halfspace.'
      print 54,'alpha','f10','compression wave velocity of layer in km/s'
      print 54,'beta','f10','shear wave velocity of layer in km/s'
      print 54,'rho','f10','density of layer in g/cm**3'
      print 54,'Qa','f10','Quality factor for alpha'
      print 54,'Qb','f10','Quality factor for beta'
      print 50,'There must be one line for each layer down to the'
      print 50,'bottom half space. The bottom halfspace has no bottom depth'
      print 50,'and must be indicated by the keyword ''halfspace:'' in'
      print 50,'columns 1-10.'
c 
      print 50,'NOTICE: The current version (18/4/2005) of refmet'
      print 50,'uses two different definitions of complex velocities.'
      print 50,'In case you do not select dispersion of seismic'
      print 50,'velocities (constant Q and constant velocity), the'
      print 50,'recipe I proposed be Forbriger and Friederich (2005)'
      print 50,'is used. This takes the tabulated velocity to be the'
      print 50,'real part of the complex velocity. In the case of'
      print 50,'dispersion, the program uses recipe III, which is'
      print 50,'also used in the original codes of Ungerer and Mueller'
      print 50,'(1985).'
c 
      return
   50 format(1x,a)
   51 format(1x,a4,1x,a)
   52 format(/1x,i4,1x,a)
   53 format(/6x,a8,1x,1h(,a3,1h),1x,a)
   54 format(6x,a8,1x,1h(,a3,1h),1x,a)
   55 format(21x,a)
      end
c
c ----- END OF sub/refmet_rmod.f ----- 
