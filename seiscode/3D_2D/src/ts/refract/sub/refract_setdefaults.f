c this is <refract_setdefaults.f>
c------------------------------------------------------------------------------
c
c 30/04/98 by Thomas Forbriger (IfG Stuttgart)
c
c set default values for plot options and scaling
c
c ----
c refract is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c refract is distributed in the hope that it will be useful,
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
c    30/04/98   V1.0   Thomas Forbriger
c    24/05/00   V1.1   some of these settings may now be overridden by
c                      defaults set in refract_cmdopt.f
c
c==============================================================================
cS
c
      subroutine setdefaults
c
c set default values
c
      include 'refract_dim.inc'
      include 'refract_para.inc'
      include 'refract_pgpara.inc'
      include 'refract_strings.inc'
      include 'refract_seipar.inc'
      include 'refract_picks.inc'
      include 'refract_model.inc'
c
cE
      integer i
c
c set plot scaling parameters
      plpar_amp=2.
      plpar_clip=plpar_amp
      plpar_mode=1
      plpar_minoff=0.1
c 
      plflag_color=.false.
      plflag_picol=.true.
      plflag_linestyle=.false.
c 
      plflag_subscale=.false.
      elem_version=.true.
      elem_scales=.true.
c 
      plpar_pmmode=1
      plpar_pickmode=1
      pg_pmmode=string_pmamp
      pg_pickmode=string_picktt
c 
      mod_valid=.false.
c 
      plstring_lastwarn=' '
c 
      do i=1,pick_ntypes
        pick_n(i)=0
      enddo
c 
c set global plot values
      tov_vpbot=0.1
      tov_vptop=0.9
      tov_vpleft=0.1
      tov_vpright=0.9
c
      return
      end
c
c ----- END OF refract_setdefaults.f -----
