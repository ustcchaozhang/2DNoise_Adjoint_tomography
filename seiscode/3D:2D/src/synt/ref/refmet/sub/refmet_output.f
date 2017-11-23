c this is <sub/refmet_output.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
c
c seismogram output to file
c
c ============================================================================
c
c this is part of the REFMET reflectivity program
c (for comments and revisions see also the main source refmet.f)
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
c
c 30/01/97   variable length of output file numbering
c 31/01/97   added second output sorting
c 10/02/97   added output component selection
c 27/06/97   changed coordinate units (psource) to meters (as defined by sff)
c 25/02/99   introduced spherical coordinates
c 26/02/99   sensible date setting
c 09/06/11   corrected sign for sff_sc3 in refmet_psource
c
c======================================================================
c 
c write seimograms to disk (Stuttgart File Format - SFF)
c
c write one file per receiver
c
      subroutine refmet_outputr(basename, fdata, idata, MSL, SL, ME, NE,
     &  outunits, selstring,
     &  sff_free, sff_maxfree, sff_nfree, VFz, VFr, VFphi, VNz, VNr, VNphi,
     &  ZQ, The, Thd, typ, r, phi, radius,
     &  dt, tred, cl_vlevel, cl_debug, lev2)
c 
      integer MSL, SL, ME, NE, sff_maxfree, sff_nfree
      character*(*) sff_free(sff_maxfree), basename
      character outunits*(*), selstring*(*)
      complex*16 VFz(MSL, ME), VFr(MSL, ME), VFphi(MSL, ME)
      complex*16 VNz(MSL, ME), VNr(MSL, ME), VNphi(MSL, ME)
      real fdata(MSL)
      integer idata(MSL), srcdate(3)
      real*8 ZQ, The, Thd, r(ME), phi(ME), dt, tred(MSL, ME), radius
      integer typ
      integer cl_vlevel, lev2
      logical cl_debug
c
c source info
      character sff_scs*1, sff_sdate*6, sff_stime*10, sff_stype*40
      real sff_sc1, sff_sc2, sff_sc3
c component selection
      logical TZ,TR,TT,FZ,FR,FT,NZ,NR,NT
c building filename
      integer logne
      character*8 form
c free
      integer sff_freebase
c receivers
      integer e
      character rcvloc*42
c general
      character*80 outfile, line
      integer ierr, outlu
      parameter(outlu=12)
      logical last
c 
c prepare component selection
      call refmet_csel(selstring,TZ,TR,TT,FZ,FR,FT,NZ,NR,NT)
c prepare source info
      call refmet_psource(ZQ, The, typ, sff_scs, sff_sdate, srcdate,
     &  sff_stime, sff_stype, sff_sc1, sff_sc2, sff_sc3, radius)
c 
c write one file per receiver
c
      sff_freebase=sff_nfree
c prepare filename format
      logne=int(log10(float(ne+1))+1.)
      write(form, '(2h(i,i1.1,1h.,i1.1,1h))') logne,logne
      if (cl_debug) print *,'DEBUG: form ',form
      do e=1,ne
c build filename
        write(line, form) e
        if (cl_debug) print *,'DEBUG: line ',line(1:logne)
        outfile=basename(1:index(basename, ' ')-1)//'.'//line(1:logne)
c prepare file FREE block
        sff_nfree=sff_freebase
        sff_nfree=min(sff_nfree+1, sff_maxfree)
        sff_free(sff_nfree)=' '
        sff_nfree=min(sff_nfree+1, sff_maxfree)
        if (cl_debug) print *,'DEBUG: sff_nfree/sff_maxfree',
     &    sff_nfree,sff_maxfree
        write(sff_free(sff_nfree), '(a,1x,i4,1x,a,1x,f12.6,1x,a,1x,f8.3)')
     &    'receiver',e,'at distance [km]',r(e),'and azimuth [degrees]',phi(e)
c prepare location string
        write(rcvloc, '(f10.3,"km  ",f6.1,"°  (units:",a8,")")')
     &    r(e), phi(e), outunits
c open file
        if (cl_vlevel.gt.lev2) print 52, 
     &    'clearing ',outfile(1:index(outfile, ' ')-1)
        call sff_New(outlu, outfile, ierr)
        if (ierr.ne.0) stop 'ERROR: clearing output file'
        if (cl_vlevel.gt.lev2) print 52, 
     &    'opening ',outfile(1:index(outfile, ' ')-1)
        call sff_WOpenFS(outlu, outfile, sff_free, sff_nfree, 
     &    sff_stype, sff_scs, sff_sc1, sff_sc2, sff_sc3, sff_sdate, 
     &    sff_stime, ierr)
c go through components

c   total field
        if (TZ) then
          last=(.not.(TR.or.TT.or.FZ.or.FR.or.FT.or.NZ.or.NR.or.NT))
          call addtraces(VFz, VNz, fdata, SL, E, MSL, ME)
          call writetrace(rcvloc//'total field, vertical component', outlu, SL,
     &      cl_debug, srcdate,
     &      dt, E, tred(1,E), r(E), phi(E), radius,
     &      'TZ', idata, fdata, MSL, last)
        endif

        if (TR) then
          last=(.not.(TT.or.FZ.or.FR.or.FT.or.NZ.or.NR.or.NT))
          call addtraces(VFr, VNr, fdata, SL, E, MSL, ME)
          call writetrace(rcvloc//'total field, radial component', outlu, SL,
     &      cl_debug, srcdate,
     &      dt, E, tred(1,E), r(E), phi(E), radius,
     &      'TR', idata, fdata, MSL, last)
        endif

        if (TT) then
          last=(.not.(FZ.or.FR.or.FT.or.NZ.or.NR.or.NT))
          call addtraces(VFphi, VNphi, fdata, SL, E, MSL, ME)
          call writetrace(rcvloc//'total field, transverse component', outlu, 
     &      SL, cl_debug, srcdate,
     &      dt, E, tred(1,E), r(E), phi(E), radius,
     &      'TT', idata, fdata, MSL, last)
        endif

c   far field
        if (FZ) then
          last=(.not.(FR.or.FT.or.NZ.or.NR.or.NT))
          call copytrace(VFz, fdata, SL, E, MSL, ME)
          call writetrace(rcvloc//'far field, vertical component', outlu, SL,
     &      cl_debug, srcdate,
     &      dt, E, tred(1,E), r(E), phi(E), radius,
     &      'FZ', idata, fdata, MSL, last)
        endif

        if (FR) then
          last=(.not.(FT.or.NZ.or.NR.or.NT))
          call copytrace(VFr, fdata, SL, E, MSL, ME)
          call writetrace(rcvloc//'far field, radial component', outlu, SL,
     &      cl_debug, srcdate,
     &      dt, E, tred(1,E), r(E), phi(E), radius,
     &      'FR', idata, fdata, MSL, last)
        endif

        if (FT) then
          last=(.not.(NZ.or.NR.or.NT))
          call copytrace(VFphi, fdata, SL, E, MSL, ME)
          call writetrace(rcvloc//'far field, transverse component', outlu, SL,
     &      cl_debug, srcdate,
     &      dt, E, tred(1,E), r(E), phi(E), radius,
     &      'FT', idata, fdata, MSL, last)
        endif

c   near field
        if (NZ) then
          last=(.not.(NR.or.NT))
          call copytrace(VNz, fdata, SL, E, MSL, ME)
          call writetrace(rcvloc//'near field, vertical component', outlu, SL,
     &      cl_debug, srcdate,
     &      dt, E, tred(1,E), r(E), phi(E), radius,
     &      'NZ', idata, fdata, MSL, last)
        endif

        if (NR) then
          last=(.not.NT)
          call copytrace(VNr, fdata, SL, E, MSL, ME)
          call writetrace(rcvloc//'near field, radial component', outlu, SL,
     &      cl_debug, srcdate,
     &      dt, E, tred(1,E), r(E), phi(E), radius,
     &      'NR', idata, fdata, MSL, last)
        endif

        if (NZ) then
          call copytrace(VNphi, fdata, SL, E, MSL, ME)
          call writetrace(rcvloc//'near field, transverse component', outlu, SL,
     &      cl_debug, srcdate,
     &      dt, E, tred(1,E), r(E), phi(E), radius,
     &      'NT', idata, fdata, MSL, .true.)
        endif
        if (cl_vlevel.gt.lev2) print 52, 
     &    'closed ',outfile(1:index(outfile, ' ')-1)
      enddo
      return
   52 format(/a,1x,a)
      end

c======================================================================
c 
c write one file per component
c
      subroutine refmet_outputc(basename, fdata, idata, MSL, SL, ME, NE,
     &  outunits, selstring,
     &  sff_free, sff_maxfree, sff_nfree, VFz, VFr, VFphi, VNz, VNr, VNphi,
     &  ZQ, The, Thd, typ, r, phi, radius,
     &  dt, tred, cl_vlevel, cl_debug, lev2)
c 
      integer MSL, SL, ME, NE, sff_maxfree, sff_nfree
      character*(*) sff_free(sff_maxfree), basename
      character outunits*(*), selstring*(*)
      complex*16 VFz(MSL, ME), VFr(MSL, ME), VFphi(MSL, ME)
      complex*16 VNz(MSL, ME), VNr(MSL, ME), VNphi(MSL, ME)
      real fdata(MSL)
      integer idata(MSL), srcdate(3)
      real*8 ZQ, The, Thd, r(ME), phi(ME), dt, tred(MSL, ME), radius
      integer typ
      integer cl_vlevel, lev2
      logical cl_debug
c
c source info
      character sff_scs*1, sff_sdate*6, sff_stime*10, sff_stype*40
      real sff_sc1, sff_sc2, sff_sc3
c component selection
      logical TZ,TR,TT,FZ,FR,FT,NZ,NR,NT
c free
      integer sff_freebase
c 
c prepare component selection
      call refmet_csel(selstring,TZ,TR,TT,FZ,FR,FT,NZ,NR,NT)
c prepare source info
      call refmet_psource(ZQ, The, typ, sff_scs, sff_sdate, srcdate,
     &  sff_stime, sff_stype, sff_sc1, sff_sc2, sff_sc3, radius)
c 
c write one file per component
c
      sff_freebase=sff_nfree
      if (cl_debug) print *,'DEBUG: typ ',typ
c vertical total field
      if (TZ) call refmet_wrec(MSL, SL, ME, NE, sff_maxfree, sff_nfree,
     &  sff_free, basename, outunits, fdata, idata, ZQ, The, r, radius,
     &  phi, dt, tred, cl_vlevel, lev2, cl_debug, sff_scs, sff_sdate,
     &  sff_sc1, sff_sc2, sff_sc3, sff_stime, sff_stype, srcdate,
     &  VFz, VNz, .true., 'TZ', 'total field, vertical component')
      sff_nfree=sff_freebase
c radial total field
      if (TR) call refmet_wrec(MSL, SL, ME, NE, sff_maxfree, sff_nfree,
     &  sff_free, basename, outunits, fdata, idata, ZQ, The, r, radius,
     &  phi, dt, tred, cl_vlevel, lev2, cl_debug, sff_scs, sff_sdate,
     &  sff_sc1, sff_sc2, sff_sc3, sff_stime, sff_stype, srcdate,
     &  VFr, VNr, .true., 'TR', 'total field, radial component')
      sff_nfree=sff_freebase
c transversal total field
      if (TT) call refmet_wrec(MSL, SL, ME, NE, sff_maxfree, sff_nfree,
     &  sff_free, basename, outunits, fdata, idata, ZQ, The, r, radius,
     &  phi, dt, tred, cl_vlevel, lev2, cl_debug, sff_scs, sff_sdate,
     &  sff_sc1, sff_sc2, sff_sc3, sff_stime, sff_stype, srcdate,
     &  VFphi, VNphi, .true., 'TT', 'total field, transversal component')
      sff_nfree=sff_freebase
c vertical far field
      if (FZ) call refmet_wrec(MSL, SL, ME, NE, sff_maxfree, sff_nfree,
     &  sff_free, basename, outunits, fdata, idata, ZQ, The, r, radius,
     &  phi, dt, tred, cl_vlevel, lev2, cl_debug, sff_scs, sff_sdate,
     &  sff_sc1, sff_sc2, sff_sc3, sff_stime, sff_stype, srcdate,
     &  VFz, VFz, .false., 'FZ', 'far field, vertical component')
      sff_nfree=sff_freebase
c radial far field
      if (FR) call refmet_wrec(MSL, SL, ME, NE, sff_maxfree, sff_nfree,
     &  sff_free, basename, outunits, fdata, idata, ZQ, The, r, radius,
     &  phi, dt, tred, cl_vlevel, lev2, cl_debug, sff_scs, sff_sdate,
     &  sff_sc1, sff_sc2, sff_sc3, sff_stime, sff_stype, srcdate,
     &  VFr, VFr, .false., 'FR', 'far field, radial component')
      sff_nfree=sff_freebase
c transversal far field
      if (FT) call refmet_wrec(MSL, SL, ME, NE, sff_maxfree, sff_nfree,
     &  sff_free, basename, outunits, fdata, idata, ZQ, The, r, radius,
     &  phi, dt, tred, cl_vlevel, lev2, cl_debug, sff_scs, sff_sdate,
     &  sff_sc1, sff_sc2, sff_sc3, sff_stime, sff_stype, srcdate,
     &  VFphi, VFphi, .false., 'FT', 'far field, transversal component')
      sff_nfree=sff_freebase
c vertical near field
      if (NZ) call refmet_wrec(MSL, SL, ME, NE, sff_maxfree, sff_nfree,
     &  sff_free, basename, outunits, fdata, idata, ZQ, The, r, radius,
     &  phi, dt, tred, cl_vlevel, lev2, cl_debug, sff_scs, sff_sdate,
     &  sff_sc1, sff_sc2, sff_sc3, sff_stime, sff_stype, srcdate,
     &  VNz, VNz, .false., 'NZ', 'near field, vertical component')
      sff_nfree=sff_freebase
c radial near field
      if (NR) call refmet_wrec(MSL, SL, ME, NE, sff_maxfree, sff_nfree,
     &  sff_free, basename, outunits, fdata, idata, ZQ, The, r, radius,
     &  phi, dt, tred, cl_vlevel, lev2, cl_debug, sff_scs, sff_sdate,
     &  sff_sc1, sff_sc2, sff_sc3, sff_stime, sff_stype, srcdate,
     &  VNr, VNr, .false., 'NR', 'near field, radial component')
      sff_nfree=sff_freebase
c transversal near field
      if (NT) call refmet_wrec(MSL, SL, ME, NE, sff_maxfree, sff_nfree,
     &  sff_free, basename, outunits, fdata, idata, ZQ, The, r, radius,
     &  phi, dt, tred, cl_vlevel, lev2, cl_debug, sff_scs, sff_sdate,
     &  sff_sc1, sff_sc2, sff_sc3, sff_stime, sff_stype, srcdate,
     &  VNphi, VNphi, .false., 'NT', 'near field, transversal component')
      sff_nfree=sff_freebase
      return
      end
c----------------------------------------------------------------------
c
c write a complete receiver set to file
c
      subroutine refmet_wrec(MSL, SL, ME, NE, sff_maxfree, sff_nfree,
     &  sff_free, basename, outunits, fdata, idata, ZQ, The, r, radius,
     &  phi, dt, tred, cl_vlevel, lev2, cl_debug, sff_scs, sff_sdate,
     &  sff_sc1, sff_sc2, sff_sc3, sff_stime, sff_stype, srcdate,
     &  V1, V2, doadd, exten, compo)
c 
      integer MSL, SL, ME, NE, sff_maxfree, sff_nfree
      character*(*) sff_free(sff_maxfree), basename
      character outunits*(*)
      complex*16 V1(MSL, ME), V2(MSL, ME)
      real fdata(MSL)
      integer idata(MSL), srcdate(3)
      real*8 ZQ, The, r(ME), phi(ME), dt, tred(MSL, ME), radius
      integer cl_vlevel, lev2
      logical cl_debug, doadd
c
c source info
      character sff_scs*1, sff_sdate*6, sff_stime*10, sff_stype*40
      real sff_sc1, sff_sc2, sff_sc3
c building filename
      character exten*(*), compo*(*)
c receivers
      integer e
      character rcvloc*80
c general
      character*80 outfile
      integer ierr, outlu
      parameter(outlu=12)
      logical last
c 
c build filename
      outfile=basename(1:index(basename, ' ')-1)//'.'//exten
c prepare file FREE block
      sff_nfree=min(sff_nfree+1, sff_maxfree)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_nfree+1, sff_maxfree)
      if (cl_debug) print *,'DEBUG: sff_nfree/sff_maxfree',
     &  sff_nfree,sff_maxfree
      sff_free(sff_nfree)=compo
c open file
      if (cl_vlevel.gt.lev2) print 52, 
     &  'clearing ',outfile(1:index(outfile, ' ')-1)
      call sff_New(outlu, outfile, ierr)
      if (ierr.ne.0) stop 'ERROR: clearing output file'
      if (cl_vlevel.gt.lev2) print 52, 
     &  'opening ',outfile(1:index(outfile, ' ')-1)
      call sff_WOpenFS(outlu, outfile, sff_free, sff_nfree, 
     &  sff_stype, sff_scs, sff_sc1, sff_sc2, sff_sc3, sff_sdate, 
     &  sff_stime, ierr)
      if (ierr.ne.0) stop 'ERROR: opening file'
c go thorough receivers
      do e=1,ne
        if (cl_debug) print *,'DEBUG: write receiver no. ',e
c prepare location string
        write(rcvloc, '(f10.3,"km  ",f6.1,"°  (units:",a8,")   ",a36)')
     &    r(e), phi(e), outunits,compo
        if (doadd) then
          call addtraces(V1, V2, fdata, SL, E, MSL, ME)
        else
          call copytrace(V1, fdata, SL, E, MSL, ME)
        endif
        last=.false.
        if (e.eq.ne) last=.true.
        if (cl_debug) print *,'DEBUG: call writetrace'
        call writetrace(rcvloc, outlu, SL, cl_debug, srcdate,
     &    dt, E, tred(1,E), r(E), phi(E), radius,
     &    exten, idata, fdata, MSL, last)
      enddo
      if (cl_vlevel.gt.lev2) print 52, 
     &  'closed ',outfile(1:index(outfile, ' ')-1)
      return
   52 format(/a,1x,a)
      end

c----------------------------------------------------------------------
c
c prepare source info
c 
c 27/06/97   changed coordinate units (psource) to meters (as defined by sff)
c 28/04/00   set y2k save nowdate
c 29/04/05   add 1900 not 1999 to year value
c 09/06/11   ZQ means depth below surface, sff_sc3 means height above
c            surface; wrong sign was passed; corrected
c
      subroutine refmet_psource(ZQ, The, typ, sff_scs, sff_sdate, nowdate,
     &  sff_stime, sff_stype, sff_sc1, sff_sc2, sff_sc3, radius)
c 
      real*8 ZQ, The, radius
      integer typ
      character sff_scs*1, sff_sdate*6, sff_stime*10, sff_stype*40
      real sff_sc1, sff_sc2, sff_sc3
      integer sff_hour, sff_minute
      real sff_second
      integer nowdate(3)
c 
c prepare sff source info
c
      if (radius.gt.0.d0) then
        sff_scs='S'
        sff_sc1=90.
        sff_sc2=0.
        sff_sc3=-sngl(ZQ)
      else
        sff_scs='C'
        sff_sc1=0.
        sff_sc2=0.
        sff_sc3=-sngl(ZQ)*1000.
      endif
      call idate(nowdate)
      nowdate(3)=nowdate(3)-100*int(nowdate(3)/100)
c      nowdate(3)=99
c      nowdate(2)=4
c      nowdate(1)=28
      write(sff_sdate, '(3i2)') nowdate(3),nowdate(2),nowdate(1)
      if (nowdate(3).lt.70) then
        nowdate(3)=nowdate(3)+2000
      else
        nowdate(3)=nowdate(3)+1900
      endif
c      nowdate(3)=1999
c      call idate(nowdate)
      sff_hour=int(The/3600) 
      sff_minute=int((The-3600.d0*sff_hour)/60)
      sff_second=sngl(The-3600.d0*sff_hour-60.d0*sff_minute)
      write(sff_stime,'(i2.2,i2.2,f6.3)') sff_hour, sff_minute, sff_second
      if (typ.eq.1) then
        sff_stype='moment tensor'
      elseif (typ.eq.2) then
        sff_stype='vertical force'
      elseif (typ.eq.3) then
        sff_stype='single force'
      endif
      return
      end

c----------------------------------------------------------------------
c
c evaluate component selection string
c 
      subroutine refmet_csel(selstring,TZ,TR,TT,FZ,FR,FT,NZ,NR,NT)
c 
      character selstring*(*)
      logical TZ,TR,TT,FZ,FR,FT,NZ,NR,NT
c 
      character*2 part
      integer i
c 
      TZ=.true.
      TR=.true.
      TT=.true.
      FZ=.true.
      FR=.true.
      FT=.true.
      NZ=.true.
      NR=.true.
      NT=.true.
c 
      i=1
    1 continue
      part=selstring(i:(i+1))
      if (part.ne.'  ') then
        if (part.eq.'TZ') TZ=.false.
        if (part.eq.'TR') TR=.false.
        if (part.eq.'TT') TT=.false.
        if (part.eq.'FZ') FZ=.false.
        if (part.eq.'FR') FR=.false.
        if (part.eq.'FT') FT=.false.
        if (part.eq.'NZ') NZ=.false.
        if (part.eq.'NR') NR=.false.
        if (part.eq.'NT') NT=.false.
        i=i+2
        goto 1
      endif
      return
      end
c
c ----- END OF sub/refmet_output.f ----- 
