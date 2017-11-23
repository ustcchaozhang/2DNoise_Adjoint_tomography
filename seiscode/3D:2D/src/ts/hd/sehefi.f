c this is <sehefi.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
c
c set header field
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
c    01/09/2005   V1.0   Thomas Forbriger
c
c ============================================================================
c
      program sehefi
c
      character*(*) version
      parameter(version='SEHEFI   V1.0   set header field')
c
      integer luin,luout
      parameter(luin=10,luout=11)
      character*200 line
      logical infree
      integer tfstr_trimlen
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=7)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c 
      logical setchannel, setstation, setinstrument, setauxid
      logical overwrite
      character*10 newchannel, newstation, newinstrument, newauxid
      character*80 infile,outfile
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v,'-c','-s','-i','-a','-o'/
      data opthasarg/2*.FALSE.,4*.true.,.false./
      data optarg/7*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: sehefi infile outfile'
        print *,'              [-v] [-o] [-c s] [-s s] [-i s] [-a s]'
        print *,'   or: sehefi -help'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'infile       name of input file'
        print *,'outfile      name of output file'
        print *,' '
        print *,'-v           be verbose'
        print *,'-o           overwrite output'
        print *,'-c s         set channel ID to ''s'' '
        print *,'-s s         set station ID to ''s'' '
        print *,'-i s         set instrument ID to ''s'' '
        print *,'-a s         set auxid ID to ''s'' '
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(2, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      setchannel=optset(3)
      newchannel=optarg(3)
      setstation=optset(4)
      newstation=optarg(4)
      setinstrument=optset(5)
      newinstrument=optarg(5)
      setauxid=optset(6)
      newauxid=optarg(6)
      overwrite=optset(7)
c 
      if (setchannel) then
        print *,'set channel ID to: ',newchannel(1:3)
      endif
      if (setstation) then
        print *,'set station ID to: ',newstation(1:5)
      endif
      if (setinstrument) then
        print *,'set instrument ID to: ',newinstrument(1:6)
      endif
      if (setauxid) then
        print *,'set auxid ID to: ',newauxid(1:4)
      endif
c
c------------------------------------------------------------------------------
c go
      call getarg(1, infile)
      call getarg(2, outfile)
c
c------------------------------------------------------------------------------
c go
      if (verbose) then 
        print *,'open input file ',infile(1:index(infile,' '))
      endif
      open(luin, file=infile, status='old', err=99)
      if (verbose) then 
        print *,'open output file ',outfile(1:index(outfile,' '))
      endif
      if (overwrite) then
        if (verbose) print *,'overwrite, if output exists'
        open(luout, file=outfile, err=98)
      else
        open(luout, file=outfile, status='new', err=93)
      endif
c 
      infree=.false.
    2 continue
        read(luin, '(a200)', err=95, end=1) line
        if (infree) then
          if (line(1:5).eq.'FREE ') infree=.false.
        else
          if (line(1:5).eq.'WID2 ') then
            if (setchannel) then
              line(36:38)=newchannel(1:3)
            endif
            if (setstation) then
              line(30:34)=newstation(1:5)
            endif
            if (setinstrument) then
              line(89:94)=newinstrument(1:6)
            endif
            if (setauxid) then
              line(40:43)=newauxid(1:4)
            endif
          elseif (line(1:5).eq.'FREE ') then
            infree=.true.
          endif
        endif
        write(luout, '(a)', err=94) line(1:tfstr_trimlen(line))
      goto 2
c 
    1 close(luout, err=97)
      close(luin, err=96)
      if (verbose) print *,'finished...'
c
      stop
   99 stop 'ERROR: opening input file'
   98 stop 'ERROR: opening output file'
   97 stop 'ERROR: closing output file'
   96 stop 'ERROR: closing input file'
   95 stop 'ERROR: reading input file'
   94 stop 'ERROR: writing input file'
   93 stop 'ERROR: opening output file - exists'
c
      end
c
c
c ----- END OF sehefi.f ----- 
