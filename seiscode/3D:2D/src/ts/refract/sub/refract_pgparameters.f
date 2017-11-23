c this is <refract_pgparameters.f>
c------------------------------------------------------------------------------
c
c 04/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c plot parameter settings
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
c    04/07/98   V1.0   Thomas Forbriger
c    29/07/00   V1.1   introduced opt_Tannotate
c
c==============================================================================
c
      subroutine refract_pgparameters
c
      include 'refract_para.inc'
      include 'refract_opt.inc'
c
      character*200 parastring
      character*20 numstring
      integer nchar, mm, pp, nc
      real oldch, newch
      integer tfstr_trimlen
c 
      if (debug) print *,'DEBUG: entered pgparameters'
c
      if (opt_Tannotate.eq.'NSP') then
c 
        nchar=0
        call refract_sub_pgpara(parastring, nchar, 'mode: ',6)
        call pgnumb(plpar_mode, 0, 0, numstring, nc)
        call refract_sub_pgpara(parastring, nchar, numstring, nc)
c 
        call refract_sub_pgpara(parastring, nchar, '   ',3)
        call refract_sub_pgpara(parastring, nchar, 'exp: ',5)
        mm=int(plpar_expo*1.e3)
        pp=-3
        call pgnumb(mm, pp, 0, numstring, nc)
        call refract_sub_pgpara(parastring, nchar, numstring, nc)
c 
        call refract_sub_pgpara(parastring, nchar, '   ',3)
        call refract_sub_pgpara(parastring, nchar, 'clip: ',6)
        mm=int(plpar_clip*1.e3)
        pp=-3
        call pgnumb(mm, pp, 0, numstring, nc)
        call refract_sub_pgpara(parastring, nchar, numstring, nc)
        call refract_sub_pgpara(parastring, nchar, 'm',1)
c 
        call refract_sub_pgpara(parastring, nchar, '   ',3)
        call refract_sub_pgpara(parastring, nchar, 'amp: ',5)
        mm=int(plpar_amp*1.e3)
        pp=-3
        call pgnumb(mm, pp, 0, numstring, nc)
        call refract_sub_pgpara(parastring, nchar, numstring, nc)
        call refract_sub_pgpara(parastring, nchar, 'm',1)
c 
        if (plflag_reduce) then
          call refract_sub_pgpara(parastring, nchar, '   ',3)
          call refract_sub_pgpara(parastring, nchar, 'v\dred\u: ',10)
          mm=int(plpar_vred*1.e3)
          pp=-3
          call pgnumb(mm, pp, 0, numstring, nc)
          call refract_sub_pgpara(parastring, nchar, numstring, nc)
          call refract_sub_pgpara(parastring, nchar, 'km/s',4)
        endif
c 
        if (plpar_remav)
     &    call refract_sub_pgpara(parastring, nchar, '   avg',6)
        if (plflag_invers)
     &    call refract_sub_pgpara(parastring, nchar, '   inv',6)
c
      else
        parastring=opt_Tannotate
        nchar=tfstr_trimlen(parastring)
      endif
c 
      call pgqch(oldch)
      newch=oldch*0.8
      call pgsch(newch)
      call pgframeact
      call pgmtxt('T', 1., 0.5, 0.5, parastring(1:nchar))
      call pgsch(oldch)
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine refract_sub_pgpara(ps, pc, is, ic)
c
      character ps*(200), is*(*)
      integer pc, ic
c 
      integer e1,e2,en
c
      e1=pc+1
      e2=pc+ic
      e1=min(200,e1)
      e2=min(200,e2)
      en=e2-e1+1
      ps(e1:e2)=is(1:en)
      pc=e2
c
      return
      end
c
c ----- END OF refract_pgparameters.f -----
