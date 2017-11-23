c this is <res_wgpart.f>
c------------------------------------------------------------------------------
cS
c ($Id$)
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c write partial derivative of green
c
c REVISIONS and CHANGES
c    24/05/2000   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine res_wgpart(basename, ipar)
c
c declare parameters
c basename: filename base
c ipar:     partial derivative for parameter
c     
      character*(*) basename
      integer ipar
c
cE
c declare local variables
      character*(*) res_wgpart_id
      parameter (res_wgpart_id='$Id$')
c
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_model.inc'
      include 'glq_inv.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c
cE
      integer ipara
      character*80 title, filename
      integer islo, ifre, lu,i,j
      parameter(lu=20)
      integer mpar, msec, mpol, iano
      character*20 parname
      real pi2
      parameter(pi2=2.*3.14159265358979311599)
c
c magic number for binary file identification
      integer magic
      character*4 cmagic
      parameter(cmagic='1234')
c
c------------------------------------------------------------------------------
c go
c 
      if (verb_subaction) print *,'ENTER res_wgpart(',basename,',',ipar,')'
c 
      if (verb_subaction) print *,
     &  'NOTICE (res_wgpart): write green partial derivatives for parameter ',
     &  ipar,' to file'
c 
c set model index
      ipara=ipar
      if ((ipara.lt.1).or.(ipara.gt.mod_n)) then
        print *,'WARNING (res_wgpart): invalid model parameter index ',ipara
        return
      endif
c 
c copy values
      iano=0
      do islo=rng_smin,rng_smax
        do ifre=rng_fmin,rng_fmax
          iano=iano+1
          readgreen(islo, ifre)=lq_dss(iano, ipara)
        enddo
      enddo
c 
      do islo=rng_smin,rng_smax
        read_dat_slo(islo)=dat_slo(islo)*1.e-3
      enddo
c 
      do ifre=rng_fmin,rng_fmax
        read_dat_om(ifre)=dat_fre(ifre)*pi2
      enddo
c 
c plot
      call mod_identify(ipara, msec, mpol, mpar, parname)
      write (title, 50) ipara, mpol-1, 
     &  parname(1:index(parname,' ')), msec
      write (filename, 51) basename(1:index(basename,' ')-1),ipara, 
     &  parname(1:index(parname,' ')-1), msec, mpol-1
c 
c write green code (easy to use)
c 
      if (verb_io) print *,'NOTICE: opening green file ',
     &    filename(1:index(filename,' ')),
     &    ' - overwrite mode'
      open(lu, file=filename, form='unformatted', err=98)
      call tf_magic(cmagic, magic)
      write(lu, err=97) magic
      write(lu, err=97) (rng_fmax-rng_fmin)+1, (rng_smax-rng_smin)+1
      write(lu, err=97) (read_dat_om(i), i=rng_fmin,rng_fmax),
     &                  (read_dat_slo(i), i=rng_smin,rng_smax)
      write(lu, err=97) ((readgreen(j,i), i=rng_fmin,rng_fmax),
     &                                    j=rng_smin,rng_smax)
      close(lu, err=96)
      if (verb_io) print *,'NOTICE: closed file'
c 
      if (verb_subaction) print *,'LEAVE res_wgpart'
c 
      return
   50 format('partial derivatives for parameter ',i3,': ord. ',i3,
     &  ' of ',a,'in section ',i3)
   51 format(a,'.',i3.3,'.',a,'.',i3.3,'.',i3.3)
   99 stop 'ERROR: reading command line argument'
   98 stop 'ERROR: opening green file'
   97 stop 'ERROR: writing green file'
   96 stop 'ERROR: closing green file'
c the following line prevents the linker from removing the ID string
   95 print *, res_wgpart_id
      end
c
c ----- END OF res_wgpart.f -----
