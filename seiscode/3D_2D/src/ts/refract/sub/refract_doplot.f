c this is <refract_doplot.f>
c------------------------------------------------------------------------------
c
c 30/04/98 by Thomas Forbriger (IfG Stuttgart)
c
c perform a complete plot
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
c    16/06/2005 V1.1   set defaults here
c    13/11/2012 V1.2   plot frame at viewport bounds
c
c==============================================================================
cS
c
      subroutine doplot(idev)
c
c preform a complete plot to device mit index idev
c 
      integer idev
c
      include 'refract_dim.inc'
      include 'refract_data.inc'
      include 'refract_para.inc'
c 
cE
      integer itrace
      character*80 pgq_item, pgq_value
      integer pgq_length

c 
      call pgslct(idev)
c set defaults
      call refract_pgdefaults
      call refract_pgnamscal
      if (verbose) then
        pgq_item='DEV/TYPE'
        call pgqinf(pgq_item, pgq_value, pgq_length)
        print *,'NOTICE (doplot): plot to ',pgq_value(1:pgq_length)
      endif
c 
      call pgframe
      if (elem_filenames) call refract_pgfilenames
      if (elem_annot) call refract_pgparameters
      if (elem_vpframe) call refract_vpframe
c 
      if (elem_data) then
        itrace=firstinrevchain
        do while(itrace.gt.0)
          call pgtrace(itrace)
          call pgupdt
          itrace=revchain(itrace)
        enddo
      endif
      call pgframeact
c
      if (elem_picks) call refract_plotallpicks
      if (elem_syntt) call pgtraveltime
      if (elem_modbox) call modelbox
c 
      call refract_warning(plstring_lastwarn)
c 
      return
      end
c
c ----- END OF refract_doplot.f -----
