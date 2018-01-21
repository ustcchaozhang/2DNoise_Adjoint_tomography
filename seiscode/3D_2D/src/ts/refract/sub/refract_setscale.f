c this is <refract_setscale.f>
c------------------------------------------------------------------------------
c
c 30/04/98 by Thomas Forbriger (IfG Stuttgart)
c
c set default scaling
c
c This subroutine sets the initial amplitude, clip and minimum offset
c interval values.
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
c    24/05/00   V1.1   respect command line options
c    15/11/11   V1.2   handle unusual cases of a single trace, all
c                      traces at the same location or profiles shorter
c                      than 10cm
c
c==============================================================================
cS
c
      subroutine setscale
c
c set default scaling
c
      include 'refract_dim.inc'
      include 'refract_data.inc'
      include 'refract_para.inc'
      include 'refract_opt.inc'
c
cE
      integer i
      real dr, ndr, avgdr
c average receiver interval
      avgdr=(maxoffset-minoffset)/ntraces
c 
c do full calculation only in case there are more than one traces and
c average receiver interval is larger than 1.e-4 of the minimum offset
      if ((ntraces.gt.1).and.(avgdr.gt.(1.e-4*minoffset))) then
c check for reasonable minimum offset
        if (.not.plpar_forceminoff) then
          plpar_minoff=min(plpar_minoff,avgdr)
        endif
c now work on scaling
        dr=-1.
        i=firstinchain
        do while (chain(i).gt.0)
          ndr=roffset(chain(i))-roffset(i)
          if (ndr.gt.plpar_minoff) then
            if (dr.lt.0.) then
              dr=ndr
            else
              dr=min(dr,ndr)
            endif
          endif
          i=chain(i)
        enddo
        if (dr.lt.0.) dr=avgdr
        plpar_clip=dr
        plpar_amp=dr
      else
        plpar_clip=minoffset
        plpar_amp=minoffset
      endif
c last check - just in case
      if (plpar_amp.lt.1.e-20) then
        if (minoffset.gt.1.e-20) then
          plpar_amp=minoffset
        else
          plpar_amp=1.
        endif
        plpar_clip=plpar_amp
      endif
      if (opt_Samp.gt.0.) plpar_amp=opt_Samp
      if (opt_Sclip.gt.0.) plpar_clip=opt_Sclip
c
      return
      end
c
c ----- END OF refract_setscale.f -----
