c this is <tf_cmdline.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1997 by Thomas Forbriger (IfG Stuttgart)
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
c evaluate commandline arguments
c
c REVISIONS and CHANGES
c    24/06/97   V1.0   Thomas Forbriger
c    25/06/97   V1.1   now also gets correct last option on command line
c    07/07/99   V1.2   whooo - found major bug! optset was not cleared
c                      correctly - just optset(maxopt) was set to .false.
c    24/05/00   V1.3   allow ident strings of arbitrary length
c
c==============================================================================
cS
c
c  tf_cmdline
c
c  evaluate commandline input
c
c  optstart          first argument to be used
c  lastarg           last command-line argument used
c  ident(maxopt)     option identifiers
c  argu(maxopt)      returnvalues
c  optset(maxopt)    option is set
c  hasarg(maxopt)    are there arguments to read
c
      subroutine tf_cmdline(optstart, lastarg,
     &    maxopt, ident, argu, optset, hasarg)
c
c  declare parameters
c
      integer maxopt, optstart, lastarg
      character*(*) ident(maxopt)
      character*(*) argu(maxopt)
      logical optset(maxopt), hasarg(maxopt)
c
cE
c  declare variables
c
      integer opt,arg, iargc,arglen
      character*100 argument
c
      arglen=len(ident(1))
      do 3 opt=1,maxopt
        optset(opt)=.FALSE.
    3 continue
      arg=optstart
      lastarg=optstart-1
    1 continue
        call getarg(arg, argument)
        opt=1
    2   continue
          if (argument(1:arglen).eq.ident(opt)) then
            optset(opt)=.TRUE.
            if (hasarg(opt)) then
              if ((arg+1).gt.iargc()) then
                print *,'WARNING (tf_cmdline): missing argument for option ',
     &                  ident(opt),' (defaults to ''none'')'
                argu(opt)='none'
              else
                call getarg(arg+1, argu(opt))
              endif
              arg=arg+1
            endif
            lastarg=arg
            opt=maxopt
          endif
          opt=opt+1
        if (opt.le.maxopt) goto 2
        arg=arg+1
      if (arg.le.iargc()) goto 1
      return
      end
c
c ----- END OF tf_cmdline.f -----
