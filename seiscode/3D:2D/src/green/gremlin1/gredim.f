c this is <gredim.f>
c------------------------------------------------------------------------------
c
c Copyright 1997, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c give me an idea on how large my program may become
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
c    12/12/97   V1.0   Thomas Forbriger
c    19/12/97   V1.1   double complex version
c    25/03/98   V1.2   take dat_response into account
c    04/07/02   V1.3   added msc_green array
c    08/11/12   V1.4   comments on output
c
c==============================================================================
c
      program gredim
c
      character*79 version
      parameter(version='GREDIM   V1.4   GREmlin DIMensions')
c 
      include 'libs/glq_dim.inc'
c 
      character*80 line
      integer summe, dcsumme
      common /forall/ summe, dcsumme
c
c
c------------------------------------------------------------------------------
c basic information
c
      print *,version
      print *,' '
      print *,'This program reports the dimension of the fixed size'
      print *,'Fortran 77 arrays used in gremlin. The actual dimensions'
      print *,'for the arrays are taken from'
      print *,' '
      print *,'libs/glq_dim.inc'
      print *,' '
      print *,'and can be adjusted there. Since this include file is a'
      print *,'copy generated from'
      print *,' '
      print *,'libs/glq_dimstd.inc'
      print *,' '
      print *,'it will be used as local settings and modifications will'
      print *,'not be transferred to the source code repository.'
      print *,' '
c
c
c------------------------------------------------------------------------------
c go
      include 'gredim_comments.xxx'
      print *,' '
c 
      summe=0
      dcsumme=0
c 
      print 50,'Dimensions set in libs/glq_dim.inc:'
c 
      print 51,'glqm_msec', glqm_msec
      print 51,'glqm_mpol', glqm_mpol
      print 51,'glqm_mpar', glqm_mpar
      print 51,'glqm_mmod', glqm_mmod
      print 51,'glqm_cpar', glqm_cpar
      print 51,'glqm_mlay', glqm_mlay
      print 51,'glqm_mano', glqm_mano
      print 51,'glqd_mslo', glqd_mslo
      print 51,'glqd_mfre', glqd_mfre
      print 51,'glqd_mtts', glqd_mtts
      print 51,'glqd_mdat', glqd_mdat
      print 51,'glqd_mano', glqd_mano
      print *,' '
c 
      print 50,'Individual arrays, their dimensions and byte size:'
      print 50,'  left column: number of bytes for single precision'
      print 50,'  right column: number of bytes for double precision'
      print *,' '
c 
      print 50,'data:'
      call sar('dat_slo',glqd_mslo)
      call sar('dat_fre',glqd_mfre)
      call sar('dat_om',glqd_mfre)
      call dctar('green',glqd_mslo,glqd_mfre,glqd_mdat)
      call cdar('readgreen',glqd_mslo,glqd_mfre)
      call dcdar('msc_green',glqd_mslo,glqd_mfre)
      call sar('travx',glqd_mtts)
      call dar('travt',glqd_mtts,glqd_mdat)
      call dar('rgweight',glqd_mslo,glqd_mfre)
      call dar('gweight',glqd_mslo,glqd_mfre)
      call sar('rtweight',glqd_mtts)
      call sar('tweight',glqd_mtts)
      call car('dat_response',glqd_mfre)
      print *,' '
      print 50,'models:'
      call dar('glqm_npol',glqm_msec,glqm_mpar)
      call qar('model',glqm_mpol,glqm_msec,glqm_mpar,glqm_mmod)
      call dar('mdepth',glqm_msec,glqm_mmod)
      call sar('destim',glqm_msec)
      call dar('mestim',glqm_msec,glqm_mpar)
      call sar('mdelta',glqm_mano)
      call sar('mweight',glqm_mano)
      call dar('dmodel',glqm_mlay,glqm_cpar)
      print *,' '
      print 50,'inversion workspace:'
      call dcdar('lq_d',glqd_mano,glqm_mano)
      call dcdar('lq_dss',glqd_mano,glqm_mano)
      call dcdar('lq_dssd',glqm_mano,glqm_mano)
      call dcsar('lq_dssdelta',glqm_mano)
      call dcsar('lq_mestim',glqm_mano)
      call dcdar('lq_dssd_nuww',glqm_mano,glqm_mano)
      print *,' '
      print 50,'total memory required:'
      print 52,'sum:',summe,dcsumme
c
      stop
   50 format(a)
   51 format(a15,':',i10)
   52 format(a15,t45,i11,' byte',t61,i11,' byte')
   99 stop 'ERROR: opening glq_dim.inc'
   98 stop 'ERROR: reading glq_dim.inc'
   97 stop 'ERROR: closing glq_dim.inc'
      end
c 
c----------------------------------------------------------------------
c 
      subroutine car(c,d1)
      integer summe, dcsumme
      common /forall/ summe, dcsumme
      character c*(*)
      integer d1
      print 50, c, d1, d1*8
      summe=summe+d1*8
      return
   50 format(a15,'(',i7,')',t45,':',i10,' byte')
      end
c 
c----------------------------------------------------------------------
c 
      subroutine sar(c,d1)
      integer summe, dcsumme
      common /forall/ summe, dcsumme
      character c*(*)
      integer d1
      print 50, c, d1, d1*4
      summe=summe+d1*4
      return
   50 format(a15,'(',i7,')',t45,':',i10,' byte')
      end
c 
c----------------------------------------------------------------------
c 
      subroutine dar(c,d1,d2)
      integer summe, dcsumme
      common /forall/ summe, dcsumme
      character c*(*)
      integer d1,d2
      print 50, c, d1, d2, d2*d1*4
      summe=summe+d1*4*d2
      return
   50 format(a15,'(',i7,',',i5,')',t45,':',i10,' byte')
      end
c 
c----------------------------------------------------------------------
c 
      subroutine qar(c,d1,d2,d3,d4)
      integer summe, dcsumme
      common /forall/ summe, dcsumme
      character c*(*)
      integer d1,d2,d3,d4
      print 50, c, d1, d2, d3, d4, d3*d2*d1*4*d4
      summe=summe+d1*4*d2*d3*d4
      return
   50 format(a15,'(',i7,3(',',i5),')',t45,':',i10,' byte')
      end
c 
c----------------------------------------------------------------------
c 
      subroutine csar(c,d1)
      integer summe, dcsumme
      common /forall/ summe, dcsumme
      character c*(*)
      integer d1
      print 50, c, d1, d1*8
      summe=summe+d1*8
      return
   50 format(a15,'(',i7,')',t45,':',i10,' byte')
      end
c 
c----------------------------------------------------------------------
c 
      subroutine cdar(c,d1,d2)
      integer summe, dcsumme
      common /forall/ summe, dcsumme
      character c*(*)
      integer d1,d2
      print 50, c, d1, d2, d2*d1*8
      summe=summe+d1*8*d2
      return
   50 format(a15,'(',i7,',',i5,')',t45,':',i10,' byte')
      end
c 
c----------------------------------------------------------------------
c 
      subroutine ctar(c,d1,d2,d3)
      integer summe, dcsumme
      common /forall/ summe, dcsumme
      character c*(*)
      integer d1,d2,d3
      print 50, c, d1, d2, d3, d2*d1*8*d3
      summe=summe+d1*8*d2*d3
      return
   50 format(a15,'(',i7,2(',',i5),')',t45,':',i10,' byte')
      end
c
c----------------------------------------------------------------------
c 
      subroutine dcsar(c,d1)
      integer summe, dcsumme
      common /forall/ summe, dcsumme
      character c*(*)
      integer d1
      print 50, c, d1, d1*8, d1*16
      summe=summe+d1*8
      dcsumme=dcsumme+d1*16
      return
   50 format(a15,'(',i7,')',t45,':',i10,' byte',t62,i10,' byte')
      end
c 
c----------------------------------------------------------------------
c 
      subroutine dcdar(c,d1,d2)
      integer summe, dcsumme
      common /forall/ summe, dcsumme
      character c*(*)
      integer d1,d2
      print 50, c, d1, d2, d2*d1*8, d2*d1*16
      summe=summe+d1*8*d2
      dcsumme=dcsumme+d1*16*d2
      return
   50 format(a15,'(',i7,',',i5,')',t45,':',i10,' byte',t62,i10,' byte')
      end
c 
c----------------------------------------------------------------------
c 
      subroutine dctar(c,d1,d2,d3)
      integer summe, dcsumme
      common /forall/ summe, dcsumme
      character c*(*)
      integer d1,d2,d3
      print 50, c, d1, d2, d3, d2*d1*8*d3, d2*d1*16*d3
      summe=summe+d1*8*d2*d3
      dcsumme=dcsumme+d1*16*d2*d3
      return
   50 format(a15,'(',i7,2(',',i5),')',t45,':',i10,' byte',t62,i10,' byte')
      end
c 
c----------------------------------------------------------------------
c 
      subroutine repline(s)
      character s*(*)
      character*80 line
      line=s
      print 50, line(2:80)
      return
   50 format(a)
      end
c
c ----- END OF gredim.f -----
