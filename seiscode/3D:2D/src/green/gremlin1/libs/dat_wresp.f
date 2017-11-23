c this is <dat_wresp.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c write complex response to disk file
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
c    25/03/98   V1.0   Thomas Forbriger
c    29/06/99   V1.1   call dat_dmode
c
c==============================================================================
cS
c
      subroutine dat_wresp(filename)
c
c write complex response function to disk file
c
      character filename*(*)
c 
c get common blocks and more
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_verbose.inc'
      include 'glq_para.inc'
c
cE
      integer ifre, lu
      integer ifremin, ifremax, premode
      real realpart, imagpart
      parameter(lu=13)
c 
      if (verb_subaction) print *,'NOTICE (dat_wresp): ',
     &  'write response to file ',filename(1:index(filename, ' '))
c
c save parameters
      ifremin=rng_fmin
      ifremax=rng_fmax
      premode=prefit_mode
c 
c set intermediate parameters
      if (verb_subaction) print *,'NOTICE (dat_wresp): ',
     &  'extending frequency range, switching to complex prefit'
      rng_fmin=1
      rng_fmax=data_nfre
      val_fmin=dat_fre(rng_fmin)
      val_fmax=dat_fre(rng_fmax)
      prefit_mode=0
c 
      call dat_dmode
c 
c recalculate the whole thing
      call dat_synt(.false.)
c 
c restore parameters
      if (verb_subaction) print *,'NOTICE (dat_wresp): ',
     &  'restoring parameters'
      rng_fmin=ifremin
      rng_fmax=ifremax
      val_fmin=dat_fre(rng_fmin)
      val_fmax=dat_fre(rng_fmax)
      prefit_mode=premode
c 
      call dat_dmode
c 
c write to disk now
      if (verb_io) print *,'NOTICE (dat_wresp): ',
     &  'opening file'
      open(lu, file=filename, status='new', err=99)
      write(lu, '(a)', err=98) 'GREENS INVERSION complex transfer function'
      write(lu, '(a)', err=98) 'columns below row number 7:'
      write(lu, '(a)', err=98) '  1: angular frequency (1/s)'
      write(lu, '(a)', err=98) '  2: real part'
      write(lu, '(a)', err=98) '  3: imaginary part'
      write(lu, '(f10.3,a)', err=98) read_dat_om(2)-read_dat_om(1),
     &  ' angular frequency step width'
      write(lu, '(i5,a)', err=98) data_nfre, ' number of frequency samples'
      do ifre=1,data_nfre
        realpart=real(dat_response(ifre))
        imagpart=imag(dat_response(ifre))
        write(lu, '(3g15.7)', err=98) read_dat_om(ifre), realpart, imagpart
      enddo
      close(lu, err=97)
      if (verb_io) print *,'NOTICE (dat_wresp): ',
     &  'file written and closed'
c 
      return
   99 print *,'WARNING (dat_wresp): could not open file - aborting'
      return
   98 call mod_panic('ERROR (dat_wresp): writing to file')
   97 call mod_panic('ERROR (dat_wresp): clsoing file')
      end
c
c ----- END OF dat_wresp.f -----
