c this is <greto.f>
c------------------------------------------------------------------------------
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c GREen matrix Thin Out
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
c    22/01/98   V1.0   Thomas Forbriger
c    25/03/98   V1.1   the first frequency is zero - start writing with that
c
c==============================================================================
c
      program greto
c
      character*79 version
      parameter(version='GRETO   V1.1   GREen matrix Thin Out')
c
c data
      integer maxslo, maxfreq
      parameter (maxslo=150, maxfreq=600)
      complex green(maxfreq, maxslo)
      real om(maxfreq), slo(maxslo)
      integer nom, nslo
      character*80 infile, outfile
c magic number for binary file identification
      integer inmagic, cpu, match
      character*4 cmagic, incmagic
      parameter(cmagic='1234')
      equivalence(incmagic, inmagic)
      integer magic
c file
      integer lu
      parameter(lu=20)
c 
      integer i, j
c step
      integer n
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=2)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug
c here are the keys to our commandline options
      data optid/'-d','-n'/
      data opthasarg/.FALSE.,.TRUE./
      data optarg/'-','2'/
c
c------------------------------------------------------------------------------
c basic information
c
      print *,version
      print *,'Usage: greto infile outfile [-n n]'
      print *,'   or: greto -help'
c
      if (iargc().lt.1) stop 'ERROR: missing arguments'
      call getarg(1, argument)
      if (argument(1:5).eq.'-help') then
        print *,' '
        print *,'GREen matrix Thin Out'
        print *,' '
        print *,'thin out frequency samples'
        print *,' '
        print *,'infile       green input (correct bytesex!)'
        print *,'outfile      thin green output'
        print *,'-n n         thinning factor (default: ',optarg(2)(1:2),')'
        stop
      endif
      if (iargc().lt.2) stop 'ERROR: missing arguments'
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(3, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      read(optarg(2), '(i10)') n
c 
      call getarg(1, infile)
      call getarg(2, outfile)
c
c------------------------------------------------------------------------------
c go
      print *,'read green file ',infile(1:index(infile, ' '))
      open(lu, file=infile, form='unformatted', status='old', err=89)
c check byte sex
      read(lu, err=88, end=87) inmagic
      call tf_bytesex(cmagic, inmagic, cpu, match)
      if (cpu.eq.1) then
        print *,'running on Intel...'
      elseif (cpu.eq.2) then
        print *,'running on Motorola...'
      else
        stop 'ERROR: unknown processor type...'
      endif
      if (match.eq.1) then
        print *,'matching bytesex - good...'
      elseif (match.eq.2) then 
        print *,'bytesex not matching - we will have to swap!'
        stop 'ERROR: do not know how to do that...'
      else
        close(lu, err=86)
        print *,'bytesex read is ',incmagic
        stop 'ERROR: bytesex is unkown - oh oh...'
      endif
      read(lu, err=88, end=87) nom, nslo
      if ((nom.gt.maxfreq).or.(nslo.gt.maxslo)) then
        close(lu, err=86)
        stop 'ERROR: data exceeds array bounds'
      endif
      read(lu, err=88, end=87) (om(i), i=1,nom), (slo(i), i=1,nslo)
      read(lu, err=88, end=87) ((green(i,j), i=1,nom), j=1,nslo)
      close(lu, err=86)
      print *,'green file read and closed'
c
c write green code (easy to use)
c 
      print *,' '
      print *,'opening green file ',outfile(1:index(outfile,' ')),
     &    ' - overwrite mode'
      open(lu, file=outfile, form='unformatted', err=98)
      call tf_magic(cmagic, magic)
      write(lu, err=97) magic
      write(lu, err=97) int(float(nom)/float(n)+0.6), nslo
      write(lu, err=97) (om(i), i=1,nom,n), (slo(i), i=1,nslo)
      write(lu, err=97) ((green(i,j), i=1,nom,n), j=1,nslo)
      close(lu, err=96)
      print *,'green file written and closed'
      print *,'file came with ',nom,' frequencies'
      print *,'file left with ',int(float(nom)/float(n)+0.6),' frequencies'
c 
      stop
   98 stop 'ERROR: opening green file'
   97 stop 'ERROR: writing green file'
   96 stop 'ERROR: closing green file'
   89 stop 'ERROR: opening green file'
   88 stop 'ERROR: reading green file'
   87 stop 'ERROR: reading green file - unexpected end'
   86 stop 'ERROR: closing green file'
      end
c
c ----- END OF greto.f -----
