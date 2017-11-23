c this is <par_read.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c read various parameters from file
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
c    24/03/98   V1.0   Thomas Forbriger
c    07/04/98   V1.1   introduced green amplitude error
c    13/01/99   V1.2   there is no chop_hs parameter anymore - but keep it
c                      within file for now (see glq_model.inc)
c 
      subroutine par_read(filename)
c 
c read parameter file
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
c
      character filename*(*)
c 
cE
      integer lu, i
      parameter(lu=17)
      real junk
c 
      if (verb_io) 
     &  print *,'NOTICE: read parameter file ',filename(1:index(filename, ' '))
      open(lu, file=filename, status='old', err=99)
      read(lu, 50, err=98, end=97)
      read(lu, *, err=98, end=97) val_fmin, val_fmax, val_smin, 
     &                            val_smax, val_xmax
      read(lu, 51, err=98, end=97)
      read(lu, *, err=98, end=97) balance, tterror, gerror, datamode, logstretch
      read(lu, 51, err=98, end=97)
      read(lu, *, err=98, end=97) prefit_mode, dottprefit
      dottprefit=max(0, dottprefit)
      read(lu, 51, err=98, end=97)
c      read(lu, *, err=98, end=97) chop_hs, chop_step, chop_master, chop_finett
      read(lu, *, err=98, end=97) junk, chop_step, chop_master, chop_finett
      chop_master=min(glqm_mpar,max(0,chop_master))
      chop_finett=max(0,min(chop_finett, glqm_mlay-glqm_nsec))
      read(lu, 51, err=98, end=97)
      read(lu, *, err=98, end=97) src_depth, src_amp, src_type
      read(lu, 51, err=98, end=97)
      read(lu, *, err=98, end=97) (rng_mmin(i), i=1,glqm_mpar)
      read(lu, *, err=98, end=97) (rng_mmax(i), i=1,glqm_mpar)
      read(lu, 51, err=98, end=97)
      read(lu, *, err=98, end=97) pvar_pdev
      close(lu, err=96)
      if (verb_io) print *,'NOTICE: file read and closed'
c 
      return
   50 format(//)
   51 format(/)
   99 stop 'ERROR (par_read): opening file'
   98 stop 'ERROR (par_read): reading file'
   97 stop 'ERROR (par_read): reading file - unexpected end'
   96 stop 'ERROR (par_read): closing file'
      end
c
c ----- END OF par_read.f -----
