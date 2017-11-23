c this is <pmotra.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2011 by Thomas Forbriger (IfG Stuttgart)
c
c plot P-wave MOdel and TRAvel times
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
c    30/01/98   V1.0   Thomas Forbriger
c    02/02/98   V1.1   add some plot options
c    14/01/99   V1.2   model definition changed (see glq_model.inc)
c                      there is no depth-of-halfspace parameter anymore
c    20/01/99   V1.3   changed calling convention for pgs_par
c    22/01/99   V1.3   changed calling convention for pgs_par
c    28/01/99   V1.4   change all character height variables due to
c                      commandline option
c    14/04/00   V1.5   added asphalt support
c    26/05/00   V1.6   extra command line options
c
c==============================================================================
c
      program pmotra
c
      character*79 version
      parameter(version='PMOTRA   V1.6   plot P-wave MOdel and TRAvel times')
c 
      include '../libs/glq_dim.inc'
      include '../libs/glq_para.inc'
      include '../libs/glq_pgpara.inc'
      include '../libs/glq_data.inc'
      include '../libs/glq_model.inc'
      include '../libs/glq_verbose.inc'
c
      integer lu, i
      parameter(lu=20)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument, title
      parameter(maxopt=17)
      character*2 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug
c options
      character*80 device, belowtt, outfile
      real step, xmin,xmax,ymin,ymax
      integer finelay, ifile, curvelw, nfiles
      logical usesecondtt, argbset, brgbset, writevalues
      real argb_r, argb_g, argb_b
      real brgb_r, brgb_g, brgb_b
c here are the keys to our commandline options
      data optid/2h-D,2h-d,2h-h,2h-s,2h-v,2h-f,2h-m,2h-e,2h-c,2h-l,2h-A,2h-B,
     &           2h-t,2h-L,2h-a,2h-b,2h-F/
      data opthasarg/.FALSE.,3*.TRUE.,.FALSE.,12*.TRUE./
      data optarg/1h-,3hx11,3h40.,3h.10,1h-,2h60,1h3,2h2.,3h1.8,1h1,1h2,1h5,
     &            1h-,1h1,2*8h0.,0.,0.,1h-/
c
c------------------------------------------------------------------------------
c basic information
c
      print *,version
      print *,
c     &  'Usage: pmotra [-d dev] [-h depth] [-v] [-s step] [-f n] [-m n]'
     &  'Usage: pmotra [-d dev] [-v] [-s step] [-f n] [-m n]'
      print *,'               [-e err] [-c height] [-l width] [-A ci] [-B ci]'
      print *,'               [-t file] [-L width] [-a r,g,b] [-b r,g,b]'
      print *,'               [-F file]'
      print *,'               ttfile file [n:title] ...'
      print *,'   or: pmotra -help'
c
      if (iargc().lt.1) stop 'ERROR: missing arguments'
      call getarg(1, argument)
      if (argument(1:5).eq.'-help') then
        print *,' '
        print *,'plot P-wave MOdel and TRAvel times'
        print *,' '
        print *,'filename       file to read model from'
        print *,'-v             be verbose'
        print *,'-d dev         plot device (default: x11)'
        print *,'               default=',optarg(2)(1:4)
c        print *,'-h depth       depth of top of halfspace'
c        print *,'               default=',optarg(3)(1:4)
        print *,'-s step        maximum relative step for parameter in'
        print *,'               discrete model'
        print *,'               default=',optarg(4)(1:4)
        print *,'-f n           number of layers for fine travel times'
        print *,'               n=0: use conventional method'
        print *,'               default=',optarg(6)(1:3)
        print *,'-m n           do prefit by mean travel time over n samples'
        print *,'               n=0: no prefit'
        print *,'               default=',optarg(7)(1:3)
        print *,'-e err         travel time error in ms'
        print *,'               default=',optarg(8)(1:3)
        print *,'-c height      character height'
        print *,'               default=',optarg(9)(1:3)
        print *,'-l width       set standard linewidth'
        print *,'               default=',optarg(10)(1:3)
        print *,'-A ci          set alpha color index'
        print *,'               default=',optarg(11)(1:3)
        print *,'-B ci          set beta color index'
        print *,'               default=',optarg(12)(1:3)
        print *,'-t file        load second travel time data for sections'
        print *,'               below asphalt'
        print *,'-L width       set curves line width'
        print *,'-a r,g,b       set alpha color RGB value'
        print *,'-b r,g,b       set beta color RGB value'
        print *,'-F file        write travel time values to ''file'' '
        print *,' '
        print *,'File specific titles may be set through option ''n:'' '
        print *,'(see refract).'
        print *,' '
        call pgp_showdevices
        print *,' '
        stop
      endif
c 
      call par_setdefpg
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      device=optarg(2)
c      read(optarg(3), *) depth
      read(optarg(4), *) step
      verb_subaction=optset(5)
      read(optarg(6), *) finelay
      chop_finett=finelay
      read(optarg(7), *) dottprefit
      read(optarg(8), *) tterror
      tterror=tterror*1.e-3
      read(optarg(9), *) pg_ch
      read(optarg(9), *) pg_bch
      read(optarg(9), *) pg_lch
      read(optarg(9), *) pg_ach
      read(optarg(10), *) pg_lw
      read(optarg(11), *) pg_alphacol
      read(optarg(12), *) pg_betacol
      usesecondtt=optset(13)
      if (usesecondtt) belowtt=optarg(13)
      read(optarg(14), *) curvelw
      argbset=optset(15)
      read(optarg(15), *) argb_r, argb_g, argb_b
      brgbset=optset(16)
      read(optarg(16), *) brgb_r, brgb_g, brgb_b
      writevalues=optset(17)
      outfile=optarg(17)
c 
      pg_clw=curvelw
c 
c        print *,'halfspace depth: ',depth,'m'
        print *,'relative parameter step: ',step
        print *,'fine layering steps: ',finelay
        print *,'prefit: ',dottprefit
        print *,'travel time error: ',tterror
c
c------------------------------------------------------------------------------
c go
c 
      lastarg=lastarg+1
      if (iargc().lt.(lastarg+1)) stop 'ERROR: no filenames'
c 
c prepare dummy with green
      rng_smin=1
      rng_smax=1
      rng_fmin=1
      rng_fmax=1
      green(1, 1, 1)=(0.d0,0.d0)
      green(1, 1, 2)=(0.d0,0.d0)
      green(1, 1, 3)=(0.d0,0.d0)
      green(1, 1, 4)=(0.d0,0.d0)
      gweight(1,1)=1.
      balance=0.
c read travel times
      call getarg(lastarg, argument)
      call dat_rtt(argument)
      if (usesecondtt) call dat_mtt(belowtt)
c prepare additonal info
      rng_xmax=data_ntts
      do i=1,rng_xmax
        tweight(i)=1/(tterror**2*float(rng_xmax))
        travt(i, di_mread)=travt(i, di_read)
      enddo
c 
      lastarg=lastarg+1
      pg_bestlw=6
c
c do filecount
      nfiles=0
      do ifile=lastarg,iargc()
        call getarg(ifile, argument)
        if (argument(1:2).ne.'n:') nfiles=nfiles+1
      enddo
      if (nfiles.eq.0) stop 'ERROR: missing model files'
c 
      call pgp_setdevice(device, (-nfiles), 2)
      if (argbset) call pgscr(pg_alphacol, argb_r, argb_g, argb_b)
      if (brgbset) call pgscr(pg_betacol, brgb_r, brgb_g, brgb_b)
      ifile=lastarg
      do while (ifile.le.iargc())
        call getarg(ifile, argument)
c read model file
        call mod_read(argument, 1)
c set title
        if (ifile.lt.iargc()) then
          call getarg(ifile+1, belowtt)
          if (belowtt(1:2).eq.'n:') then
            title=belowtt(3:)
            ifile=ifile+1
          else
            title=argument
          endif
        else
          title=argument
        endif
c chop it
c        call par_chop(.true., step, depth)
        call par_chop(.true., step)
        call mod_chop(1)
        if (chop_finett.gt.0) then
          call dat_dctt(.true.)
        else
          call dat_ctt(.true.)
        endif
        if (dottprefit.gt.0) call dat_ftt(.true.)
c 
c model ranges
        xmin=0.
        xmax=0.
        ymin=0.
        ymax=dmodel(glqm_nlay, mi_depth)*1.1
        do i=2,glqm_nlay
          xmin=min(xmin,dmodel(i, mi_alpha))
          xmax=max(xmax,dmodel(i, mi_alpha))
        enddo
        xmin=xmin*0.9
        xmax=xmax*1.1
c plot model
        pg_clw=curvelw
        call pgpage
        call pg_selvp(0)
        call pgsci(pg_colind)
        call pgsch(pg_ch)
        call pgslw(pg_lw)
        call pgswin(xmin, xmax, ymax*1.e3, ymin*1.e3)
        call pgbox('BCNTS', 0.,0,'BCNTS',0.,0)
        call pgsls(4)
        call pgslw(1)
        call pgbox('BCGNTS', 0.,0,'BCGNTS',0.,0)
        call pgsls(1)
        call pgslw(pg_lw)
        call pgswin(xmin, xmax, ymax, ymin)
        call pglab('p-velocity [km/s]','depth [m]',title)
        call pgslw(6)
        call pgsci(pg_alphacol)
        call pgs_par(1, mi_alpha, .false., ymax)
c plot travel times
        pg_clw=pg_lw
        call pgpage
        call pgsci(pg_colind)
        call pgsch(pg_bch)
        call pgslw(pg_lw)
        call pg_tt(0, .true.)
c write values
        if (writevalues) then
          open(lu, file=outfile, err=99)
          if (data_ttsplit.gt.0) then
            write(lu, 50, err=98) (1.e3*travx(i), 1.e3*travt(i, di_mref), 
     &                             i=1,data_ttsplit)
            write(lu, *, err=98) ' '
          endif
          write(lu, 50, err=98) (1.e3*travx(i), 1.e3*travt(i, di_mref),
     &                           i=data_ttsplit+1,data_ntts)
          close(lu)
        endif
        ifile=ifile+1
      enddo
      call pgend
c
      stop
   50 format(f13.6,2xf13.6)
   99 stop 'ERROR: opening output file'
   98 stop 'ERROR: writing output file'
      end
c
c ----- END OF pmotra.f -----
