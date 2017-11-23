c this is <matls.f> by Thomas Forbriger
c
c Copyright (c) 1997 by Thomas Forbriger
c
c show configuration of refmat matrix
c
c----------------------------------------------------------------------
      program matls
c 
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
      character*70 version
      parameter(version='MATLS   V1.0   look into reflectivity matrix file')
c 
      integer iargc, slen, lu
      character*70 matversion, mattext
      character*72 matmodel, matsrc
      character*80 matrixfile
      parameter(lu=20)
      real qah, qbh, qatop, qbtop
      real*8 alphah, betah, rhoh, zh
      real*8 alphatop, betatop, rhotop
      real*8 fwil, fwir, uwil, uwir, TL, df, fny
      real*8 nuref, zq, radius, dt, umin, umax, fmin, fmax
      integer Nu, SL, fmi, fma, nf
c
c----------------------------------------------------------------------
c go  
      print *,version
      if (iargc().ne.1) stop 'ERROR: give me the name of the matrix file'
      call getarg(1, matrixfile)

      open(lu, file=matrixfile, form='unformatted', status='old', err=99)

      read(lu, err=98, end=97) matversion
      read(lu, err=98, end=97) fmi, fma
      read(lu, err=98, end=97) dt, SL, fmin, fwil, fwir, fmax
      read(lu, err=98, end=97) Nu, umin, uwil, uwir, umax
      read(lu, err=98, end=97) zq, radius
      read(lu, err=98, end=97) qah, qbh, alphah, betah, rhoh, zh
      read(lu, err=98, end=97) qatop, qbtop, alphatop, betatop, rhotop
      read(lu, err=98, end=97) mattext, matmodel, matsrc
      read(lu, err=98, end=97) nuref

      close(lu, err=96)

      TL=dt*SL
      df=1.d0/TL
      fny=0.5d0/dt
      nf=fma-fmi+1

      call sff_TrimLen(matversion, slen)
      print 50,matversion(1:slen)

  50  format(/'This reflectivity matrix was created by:'/a)

      call sff_TrimLen(mattext, slen)
      print 51,mattext(1:slen)

  51  format(/'The numerical parameter setting:'/a)

      print 52,dt,SL,TL,df,fny
      print 53,fmin,fwil,fwir,fmax
      print 54,umin,uwil,uwir,umax
      print 55,nu,nf

  52  format(/'                     dt: ',f10.4,' sec',
     &      /'      number of samples: ',i10,
     &      /'  length of seismograms: ',f10.2,' sec'
     &     //'resulting from that:',
     &      /'   frequency resolution: ',f10.6,' Hz',
     &      /'      nyquist frequency: ',f10.4,' Hz')
  53  format(/'Frequency and slowness range (no tapering was applied):',
     &      /'   fmin: ',f10.6,' Hz',
     &      /'   fwil: ',f10.6,' Hz',
     &      /'   fwir: ',f10.6,' Hz',
     &      /'   fmax: ',f10.6,' Hz')
  54  format('   umin: ',f10.6,' s/km',
     &      /'   uwil: ',f10.6,' s/km',
     &      /'   uwir: ',f10.6,' s/km',
     &      /'   umax: ',f10.6,' s/km')
  55  format(/'matrix dimensions:',
     &      /'   number of slowness values: ',i6
     &      /'  number of frequency values: ',i6)

      call sff_TrimLen(matmodel, slen)
      print 56,matmodel(1:slen)
  56  format(/'The earth model:'/a)

      print 57,radius,nuref
      print 58,alphatop, betatop, rhotop, qatop, qbtop
      print 59,zh,alphah, betah, rhoh, qah, qbh
  57  format(/'                            earth radius: ',f10.1,' km',
     &      /' velocity dispersion reference frequency: ',f10.5,' Hz')
  58  format(/'receiver and source layer properties:'
     &      /'  depth [km]   Vp [km/s]   Vs [km/s]  rho [g/cm**3]',
     &       '       Qp          Qs',
     &      /'      0.0     ',3(f10.4,2x),2(f10.2,2x))
  59  format(2x,4(f10.4,2x),2(f10.2,2x))

      call sff_TrimLen(matsrc, slen)
      print 60,matsrc(1:slen)
  60  format(/'The source model:'/a)

      print 61,zq
  61  format('  source depth: ',f10.3,' km')

c end of action
      stop

   99 stop 'ERROR: opening matrix file'
   98 stop 'ERROR: reading matrix file'
   97 stop 'ERROR: reading matrix file - unexpected end of file'
   96 stop 'ERROR: closing matrix file'

      end
