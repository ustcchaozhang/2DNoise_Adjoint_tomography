c this is <moshift.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
c
c shift a gremlin subsurface model
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
c    01/06/2006   V1.0   Thomas Forbriger
c    17/11/2010   V1.1   use correct include path
c
c ============================================================================
c
      program moshift
c
      character*(*) version
      parameter(version=
     &  'MOSHIFT   V1.1   shift a gremlin subsurface model')
      character*(*) MOSHIFT_CVS_ID
      parameter(MOSHIFT_CVS_ID=
     &  '$Id$')
c
c use subroutines from gremlin1
      include '../libs/glq_dim.inc'
      include '../libs/glq_model.inc'
c
      integer lu, i, j, k
      parameter(lu=20)
      logical overwrite
      double precision v
      character*80 infile, outfile,shiftfile
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=3)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-o/
      data opthasarg/3*.FALSE./
      data optarg/3*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.4)) then
        print *,version
        print *,'Usage: moshift [-v] [-o] infile outfile shiftfile v'
        print *,'   or: moshift -help'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'infile       input model file'
        print *,'outfile      output model file'
        print *,'shiftfile    model to be used for shifting'
        print *,'v            factor to shift model'
        print *,' '
        print *,'-v           be verbose'
        print *,'-o           overwrite output file'
        print *,' '
        print *,'The output will be'
        print *,'outfile = infile + v * shiftfile'
        print *,' '
        print *,MOSHIFT_CVS_ID
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      overwrite=optset(3)
      call getarg(lastarg+4, infile)
      read (infile, *) v
      call getarg(lastarg+1, infile)
      call getarg(lastarg+2, outfile)
      call getarg(lastarg+3, shiftfile)
      if (verbose) then
        print *,'     input model: ',infile(1:index(infile,' ')-1)
        print *,'  write model to: ',outfile(1:index(outfile,' ')-1)
        print *,'  shift by model: ',shiftfile(1:index(shiftfile,' ')-1)
        print *,'    shift factor: ',v
      endif
c 
      if (verbose) print *,'read ',infile(1:index(shiftfile,' ')-1)
      call mod_read(shiftfile, mb_work)
      if (verbose) print *,'read ',infile(1:index(infile,' ')-1)
      call mod_read(infile, mb_ref)
      if (verbose) print *,'apply changes' 
      do i=1,glqm_nsec
        mdepth(i, mb_ref)=v*mdepth(i, mb_work)+mdepth(i, mb_ref)
        do j=1,glqm_mpar
          do k=1,glqm_mpol
            model(k, i, j, mb_ref)=
     &        v*model(k, i, j, mb_work)+model(k, i, j, mb_ref)
          enddo
        enddo
      enddo
      call mod_follow(mb_ref)
      if (verbose) print *,'write ',infile(1:index(outfile,' ')-1)
      call mod_save(outfile, mb_ref, overwrite, version)
c
c------------------------------------------------------------------------------
c go
c
      stop
      end
c
c
c ----- END OF moshift.f ----- 
