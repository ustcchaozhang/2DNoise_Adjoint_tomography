c this is <refract.f>
c------------------------------------------------------------------------------
c
c Copyright 1998, 2010, 2016 by Thomas Forbriger (IfG Stuttgart)
c
c REFRACTion seismics - data interpretation
c
c ----
c refract is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c refract is distributed in the hope that it will be useful,
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
c    09/01/98   V3.0   new version based on include-file data
c    03/07/98   V3.1   worked on scaling modes these days
c    05/07/98   V3.2   introduced menu system and element switching
c    08/07/98   V3.3   finished a lot of minor addon with this version
c                      things like model and pick file writing are implemented
c                      now
c    14/08/98   V3.4   added portrait/landscape postscript output
c    18/08/98   V3.5   pick trace arrivals
c    18/11/98   V3.6   check air-coupled sound wave
c    25/11/98   V3.7   had to change read format for arrival times
c                      to read old refra-created picks files
c    18/02/99   V3.8   allow offset scale in degree on a sphere
c    24/05/00   V3.9   - increased size of option identifier to 3 characters
c                      - introduced new options
c    24/05/00   V4.0   allowing almost full parameter control from the command
c                      line justifies the step to the next major revision 
c    25/05/00   V4.1   usage quick info was missing
c    29/07/00   V4.2   introduced option -Ta
c    22/01/01   V4.3   improved wiggle-plot
c    19/03/02   V4.4   increased polygon breaks visibility
c    16/07/2003 V4.5   new option to label traces
c    09/09/2004 V4.6   new option to label traces with station names
c    16/06/2005 V4.7   - set defaults in doplot subroutine
c                      - prefer blue
c    26/11/2010 V4.8   provide additional input formats
c    15/11/2011 V4.8a  this version does safe amplitude scaling even for
c                      unusual cases (see refract_setscale.f)
c    20/11/2012 V4.9   several new plot style options are implemented
c    24/10/2013 V4.10  added alternative definitions of ordinate scale
c    21/03/2014 V4.12  optionally reverse order of legend strings (-TR)
c    07/12/2015 V4.13  add option -SN; make amplitude scaling with
c                      respect to offset range the standard (without
c                      alternative)
c    08/02/2016 V4.14  introduce upper limit for trace displays world
c                      coordinate range in counts; this makes the
c                      program robust, when displaying a zero offset
c                      trace with offset dependend scaling
c    01/12/2016 V4.15  fix trace label in reverse mode
c
c==============================================================================
c
      program refract
c
      character*79 version
      parameter(version=
     &  'REFRACT   V4.15  REFRACTion seismics - data interpretation')
c
c get common blocks
      include 'refract_dim.inc'
      include 'refract_data.inc'
      include 'refract_para.inc'
      include 'refract_pgpara.inc'
      include 'refract_seipar.inc'
      include 'refract_opt.inc'
c 
      integer iargc, lastarg, i
      character*80 argument
c pgplot
      integer pgp_open
      character*80 device
c
c------------------------------------------------------------------------------
c basic information
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        call refract_usage(version)
      elseif (argument(1:6).eq.'-xhelp') then
        call refract_usage_formats(version)
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call refract_cmdopt(version,device,lastarg)

      if (debug) then
        do i=1,iargc()
          call getarg(i, argument)
          print '(a,i3,a)','DEBUG: arg# ',i,': '
          print '(a79)',argument(1:79)
        enddo
      endif
c
c------------------------------------------------------------------------------
c read input data files
      if (debug) then
        print *,'DEBUG: lastarg=',lastarg
        print *,'DEBUG: iargc=',iargc()
        print *,'DEBUG: expecting to read ',
     &    iargc()-lastarg,' files'
      endif
      call readdata(lastarg)
      if (debug) then
        print *,'DEBUG: ntraces ',ntraces
        print *,'DEBUG: nfiles ',nfiles
      endif
      call setscale
      call refract_setrefrange
c
      call mpcfactors
      call setfullrange
c 
c------------------------------------------------------------------------------
c read files
c
      call refract_preread
c 
c------------------------------------------------------------------------------
c plot and loop
c 
      pg_maindevice=pgp_open(device)
      if (pg_maindevice.le.0) stop 'ERROR: opening pgplot device'
c 
c set window to command line settings
      if (opt_Sxrange) then
        tov_rmin=opt_Sxmin
        tov_rmax=opt_Sxmax
      endif
      if (opt_Strange) then
        tov_tmin=opt_Stmin
        tov_tmax=opt_Stmax
      endif
c 
      call doplot(pg_maindevice)
      do while (flag_pick)
        call loopaction
        if (flag_replot) then
          call pgask(.false.)
          call pgpage
          call doplot(pg_maindevice)
        endif
      enddo
      call pgclos
c
      stop
      end
c
c ----- END OF refract.f -----
