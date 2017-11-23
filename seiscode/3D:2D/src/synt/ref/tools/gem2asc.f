c this is <gem2asc.f>
c
c Copyright (c) 1995 by Jörg Dalkolmo
c Copyright (c) 1997 by Thomas Forbriger
c
c plot GEMINI earth model 
c
c Some parts of the source code are copied from gemini by Jörg Dalkolmo.
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
c V1.0   10/01/97   Thomas Forbriger (from gemmodpg.)
c V1.1   12/09/03   now uses tf_cmdline (not tflib_cmdline)
c
c----------------------------------------------------------------------
      program gem2asc
      character*70 version
      parameter(version='GEM2ASC   V1.1   create ascii file from gemini')
c gemini
      integer maxlayer, nlayer
      parameter(maxlayer=30)
      double precision alpha(maxlayer, 4), beta(maxlayer, 4)
      double precision rho(maxlayer, 4), qm(maxlayer), qk(maxlayer)
      double precision rb(0:maxlayer)
      integer iflso(maxlayer), nco(maxlayer)
      character text*72
c 
      character*80 filename, device, title, outfile
      logical debug
      integer fin, iargc, lu, nsteps, i, nl, gemini_layer
      parameter(lu=10, nsteps=400)
      real rmin, rmax, tqm, tqk, rad
      real talpha, tbeta, trho
      double precision gemini_par
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=3)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c here are the keys to our commandline options
      data optid/2h-d,2h-D,2h-r/
      data opthasarg/.TRUE.,.FALSE.,.TRUE./
      data optarg/3hx11,1h-,7h0.,1.e4/
c----------------------------------------------------------------------
      print *,version
      print *,'Usage: gem2asc [-r rmin,rmax] modelfile output'
      print *,'   or: gem2asc -help'
      if (iargc().lt.2) stop 'ERROR: missing parameters'
      call getarg(1, filename)
      if (filename.eq.'-help') then
        print *,'writes a gemini model to an ascii table'
        print *,' '
        print *,'-r rmin,rmax plot range from radius rmin to radius rmax'
        print *,' '
        stop
      endif
c----------------------------------------------------------------------
c set options
      call tf_cmdline(1, lastarg,
     &     maxopt, optid, optarg, optset, opthasarg)
      device=optarg(1)
      debug=optset(2)
      read(optarg(3), *) rmin,rmax
      if (iargc().eq.(lastarg+1)) stop 'ERROR: no files'
      call getarg(iargc()-1, filename)
      call getarg(iargc(), outfile)
      fin=index(filename,' ')
c----------------------------------------------------------------------
c read file
      if (debug) print *,'DEBUG: filename ',filename(1:fin)
      call gemini_getmod(filename, lu, maxlayer, 
     &  rb, qm, qk, rho, alpha, beta, nlayer, iflso, nco, text)
c----------------------------------------------------------------------
c check ranges
      rmin=max(rmin,sngl(rb(0)))
      rmax=min(rmax,sngl(rb(nlayer)))
c----------------------------------------------------------------------
      open(lu, outfile, err=99)
      do i=1,nsteps
        rad=rmin+(rmax-rmin)*float(i-1)/float(nsteps-1)
        talpha=gemini_par(alpha, rb, dble(rad), maxlayer, nlayer)
        tbeta=gemini_par(beta, rb, dble(rad), maxlayer, nlayer)
        trho=gemini_par(rho, rb, dble(rad), maxlayer, nlayer)
        nl=gemini_layer(rb, dble(rad), maxlayer, nlayer)
        tqm=sngl(qm(nl))
        tqk=sngl(qk(nl))
        write(lu, '(6(f10.3,1x))', err=98)
     &    rad,talpha,tbeta,trho,tqm,tqk
      enddo
      close(lu, err=97)
      stop
   99 stop 'ERROR: opening output file'
   98 stop 'ERROR: writing output file'
   97 stop 'ERROR: closing output file'
      end
