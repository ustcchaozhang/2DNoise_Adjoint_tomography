c this is <motab.f>
c------------------------------------------------------------------------------
c   ($Id$)
c
c Copyright 2000, 2011 by Thomas Forbriger (IfG Stuttgart)
c
c tabulate model parameters
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
c    12/04/2000   V1.0   Thomas Forbriger
c
c==============================================================================
c
      program motab
c
      character*(*) version
      parameter(version='MOTAB   V1.0   tabulate model parameters')
      character*(*) MOTAB_CVS_ID
      parameter(MOTAB_CVS_ID=
     &  '$Id$')
c use subroutines from gremlin1
      include '../libs/glq_dim.inc'
      include '../libs/glq_model.inc'
c
      integer lu
      parameter(lu=20)
      double precision z, zbot, ztop, zref, dz
      integer isec, ipar
      double precision p(glqm_mpar)
      logical hot
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=4)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c options
      integer nvalues
      logical overwrite
      character*100 infile, outfile
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-n, 2h-o/
      data opthasarg/2*.FALSE.,.TRUE.,.FALSE./
      data optarg/2*1h-,3h100,1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.2)) then
        print *,version
        print *,'Usage: motab input output [-n n] [-o]'
        print *,'   or: motab -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'tabulate model parameters'
        print *,' '
        print *,'input        polynomial input model (see gremlin)'
        print *,'output       tabulated model values suitable to be raed'
        print *,'             by generic plot program'
        print *,' '
        print *,'-n n         output n values per parameter'
        print *,'-o           overwrite output file'
        print *,' '
        print *,MOTAB_CVS_ID
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
      call getarg(1, infile)
      call getarg(2, outfile)
c
      call tf_cmdline(3, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      read(optarg(3), *) nvalues
      overwrite=optset(4)
c
c------------------------------------------------------------------------------
c go
      call mod_read(infile, 1)
      dz=mdepth(glqm_nsec, 1)/float(nvalues-2*glqm_nsec-1)
      z=0.
      ztop=0.
      hot=.true.
      isec=1
      zbot=mdepth(isec, 1)
      zref=(ztop+zbot)/2.d0
      if (overwrite) then
        open(lu, file=outfile, status='old', err=99)
      else
        open(lu, file=outfile, status='new', err=99)
      endif
      do while (hot)
        do ipar=1,glqm_mpar
          p(ipar)=model(1, isec, ipar, 1)
     &          +model(2, isec, ipar, 1)*(z-zref)
     &          +model(3, isec, ipar, 1)*(z-zref)*(z-zref)
        enddo
        write(lu, '(6(f10.5,2x))', err=98) z, (p(ipar), ipar=1,glqm_mpar)
        z=z+dz
        if (z.gt.zbot) then
          z=zbot
          do ipar=1,glqm_mpar
            p(ipar)=model(1, isec, ipar, 1)
     &            +model(2, isec, ipar, 1)*(z-zref)
     &            +model(3, isec, ipar, 1)*(z-zref)*(z-zref)
          enddo
          write(lu, '(6(f10.5,2x))', err=98) z, (p(ipar), ipar=1,glqm_mpar)
          if (isec.eq.glqm_nsec) then
            z=1.1*zbot
            write(lu, '(6(f10.5,2x))', err=98) z, (p(ipar), ipar=1,glqm_mpar)
            hot=.false.
          else
            ztop=zbot
            isec=isec+1
            zbot=mdepth(isec, 1)
            zref=(ztop+zbot)/2.d0
            z=ztop
          endif
        endif
      enddo
      close(lu, err=97)
c
      stop
   99 stop 'ERROR: opening output file'
   98 stop 'ERROR: writing output file'
   97 stop 'ERROR: closing output file'
      end
c
c ----- END OF motab.f -----
