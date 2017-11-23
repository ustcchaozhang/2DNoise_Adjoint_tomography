c this is <main/matbau.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c combine to matrix files to a complete set
c
c ============================================================================
c
c combine to matrix files to a complete set
c
c expect: arg1: infile psv
c         arg2: infile sh
c         arg3: outfile
c         arg4: extras
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
c    30/07/1997   V1.0   Thomas Forbriger
c
c
      program matbau
      character*70 version
      parameter(version='MATBAU   V1.0   combine psv and sh matrix')
c extra
      real qah, qbh, qatop, qbtop
      real*8 alphah, betah, rhoh
      real*8 alphatop, betatop, rhotop
      real*8 nuref
c psv
      real*8 uminpsv, uwilpsv, uwirpsv, umaxpsv
      real*8 zhpsv, fminpsv, fwilpsv, fwirpsv, fmaxpsv, dtpsv
      integer SLpsv, Nupsv
      real*8 zqpsv, radiuspsv
      character textpsv*70, mtpsv*72, stpsv*72, vpsv*70
c sh
      real*8 uminsh, uwilsh, uwirsh, umaxsh
      real*8 zhsh, fminsh, fwilsh, fwirsh, fmaxsh, dtsh
      integer SLsh, Nush
      real*8 zqsh, radiussh
      character textsh*70, mtsh*72, stsh*72, vsh*70
c filename
      character*80 inpsv, insh, outfile, extrafile
      integer iargc, inpsvlu, inshlu, outlu, extralu
      parameter(inpsvlu=10, inshlu=11, outlu=12, extralu=13)
c 
      integer fmi, fma, f, msl, u
      real*8 tl, df
      parameter(msl=4100)
      complex c1(msl), c2(msl), c3(msl), c4(msl),
     &  c5(msl), c6(msl), c7(msl), c8(msl), c9(msl), c10(msl)
      
      if (iargc().ne.4) stop 'ERROR: arguments'
      call getarg(1, inpsv)
      call getarg(2, insh)
      call getarg(3, outfile)
      call getarg(4, extrafile)
      print *,'INPSV: ',inpsv
      print *,'INSH: ',insh
      print *,'OUTFILE: ',outfile
      print *,'EXTRAS: ',extrafile

      open(extralu, file=extrafile, status='old', err=80)
      read(extralu, *, err=81, end=82) alphah, betah, rhoh, qah, qbh
      read(extralu, *, err=81, end=82) alphatop, betatop, rhotop, qatop, qbtop
      read(extralu, *, err=81, end=82) nuref
      close(extralu, err=83)

      print *,'qah, qbh, alphah, betah, rhoh',
     &  qah, qbh, alphah, betah, rhoh
      print *,'qatop, qbtop, alphatop, betatop, rhotop',
     &  qatop, qbtop, alphatop, betatop, rhotop
      print *,'nuref ',nuref

      pause

      open(outlu, file=outfile, status='new', form='unformatted', err=99)
      open(inpsvlu, file=inpsv, status='old', form='unformatted', err=98)
      open(inshlu, file=insh, status='old', form='unformatted', err=97)

      read(inpsvlu, err=93, end=92) vpsv
      print *,'vpsv: ',vpsv
      read(inpsvlu, err=93, end=92) 
     &  dtpsv, slpsv, fminpsv, fwilpsv, fwirpsv, fmaxpsv
      print *,'PSV: dt, sl, fmin, fwil, fwir, fmax ',
     &  dtpsv, slpsv, fminpsv, fwilpsv, fwirpsv, fmaxpsv
      read(inpsvlu, err=93, end=92) 
     &  Nupsv, uminpsv, uwilpsv, uwirpsv, umaxpsv
      print *,'PSV: nu, umin, uwil, uwir, umax ',
     &  Nupsv, uminpsv, uwilpsv, uwirpsv, umaxpsv
      read(inpsvlu, err=93, end=92) Zqpsv, radiuspsv
      print *,'PSV: zq, radius ',zqpsv, radiuspsv
      read(inpsvlu, err=93, end=92) textpsv, mtpsv, stpsv
      print *,'PSV: text ',textpsv
      print *,'PSV: modeltext ',mtpsv
      print *,'PSV: source ',stpsv
      read(inpsvlu, err=93, end=92) zhpsv
      print *,'PSV: zh ',zhpsv

      read(inshlu, err=91, end=90) vsh
      print *,'vsh: ',vpsv
      read(inshlu, err=91, end=90) 
     &  dtsh, slsh, fminsh, fwilsh, fwirsh, fmaxsh
      print *,'PSH: dt, sl, fmin, fwil, fwir, fmax ',
     &  dtsh, slsh, fminsh, fwilsh, fwirsh, fmaxsh
      read(inshlu, err=91, end=90) 
     &  Nush, uminsh, uwilsh, uwirsh, umaxsh
      print *,'PSH: nu, umin, uwil, uwir, umax ',
     &  Nush, uminsh, uwilsh, uwirsh, umaxsh
      read(inshlu, err=91, end=90) Zqsh, radiussh
      print *,'PSH: zq, radius ',zqsh, radiussh
      read(inshlu, err=91, end=90) textsh, mtsh, stsh
      print *,'PSH: text ',textsh
      print *,'PSH: modeltext ',mtsh
      print *,'PSH: source ',stsh
      read(inshlu, err=91, end=90) zhsh
      print *,'PSH: zh ',zhsh

      if (dtpsv.ne.dtsh) stop 'ERROR: dt does not match!'
      if (slpsv.ne.slsh) stop 'ERROR: sl does not match!'
      if (fminpsv.ne.fminsh) stop 'ERROR: fmin does not match!'
      if (fwilpsv.ne.fwilsh) stop 'ERROR: fwil does not match!'
      if (fwirpsv.ne.fwirsh) stop 'ERROR: fwir does not match!'
      if (fmaxpsv.ne.fmaxsh) stop 'ERROR: fmax does not match!'
      if (nupsv.ne.nush) stop 'ERROR: nu does not match!'
      if (uminpsv.ne.uminsh) stop 'ERROR: umin does not match!'
      if (uwilpsv.ne.uwilsh) stop 'ERROR: uwil does not match!'
      if (uwirpsv.ne.uwirsh) stop 'ERROR: uwir does not match!'
      if (umaxpsv.ne.umaxsh) stop 'ERROR: umax does not match!'
      if (zqpsv.ne.zqsh) stop 'ERROR: zq does not match!'
      if (zhpsv.ne.zhsh) stop 'ERROR: zh does not match!'
      if (radiuspsv.ne.radiussh) stop 'ERROR: radius does not match!'
      if (textpsv.ne.textsh) stop 'ERROR: text does not match!'
      if (mtpsv.ne.mtsh) stop 'ERROR: mt does not match!'
      if (stpsv.ne.stsh) stop 'ERROR: st does not match!'
      
      tl=slpsv*dtpsv
      df=1.d0/tl
      fmi=IDINT(fminpsv/Df)+1
      fma=IDINT(fmaxpsv/Df)+1
      if (fmi.eq.1) fmi=2
      print *,'fmi,fma,nf ',fmi,fma,fma-fmi+1

      if (fma.gt.msl) stop 'ERROR: array too small'

      write(outlu, err=89) version
      write(outlu, err=89) fmi, fma
      write(outlu, err=89) 
     &  dtsh, slsh, fminsh, fwilsh, fwirsh, fmaxsh
      write(outlu, err=89) 
     &  Nush, uminsh, uwilsh, uwirsh, umaxsh
      write(outlu, err=89) Zqsh, radiussh
      write(outlu, err=89) qah, qbh, alphah, betah, rhoh,zhsh
      write(outlu, err=89) qatop, qbtop, alphatop, betatop, rhotop
      write(outlu, err=89) textsh, mtsh, stsh
      write(outlu, err=89) nuref

      do u=1,nupsv
        print *,'no u ',u
        read(inpsvlu, err=93, end=92) 
     &    (c1(f),c2(f),c3(f),c4(f),c5(f),c6(f),c7(f),c8(f),f=fmi,fma)
        read(inshlu, err=91, end=90) 
     &    (c9(f),c10(f),f=fmi,fma)
        write(outlu, err=89) 
     &    (c1(f),c2(f),c3(f),c4(f),c5(f),c6(f),c7(f),c8(f),c9(f),c10(f),
     &     f=fmi,fma)
      enddo

      close(inshlu, err=96)
      close(inpsvlu, err=95)
      close(outlu, err=94)

      stop
   99 stop 'ERROR: opening outfile'
   98 stop 'ERROR: opening inpsv'
   97 stop 'ERROR: opening insh'
   96 stop 'ERROR: closing outfile'
   95 stop 'ERROR: closing inpsv'
   94 stop 'ERROR: closing insh'
   93 stop 'ERROR: reading inpsv'
   92 stop 'ERROR: reading inpsv - unexpected end of file'
   91 stop 'ERROR: reading insh'
   90 stop 'ERROR: reading insh - unexpected end of file'
   89 stop 'ERROR: writing outfile'
   80 stop 'ERROR: opening extrafile'
   81 stop 'ERROR: reading extrafile'
   82 stop 'ERROR: reading extrafile - unexpected end'
   83 stop 'ERROR: closing extrafile'
      end
c
c ----- END OF main/matbau.f ----- 
