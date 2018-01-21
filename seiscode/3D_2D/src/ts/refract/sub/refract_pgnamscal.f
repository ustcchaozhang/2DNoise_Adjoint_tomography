c this is <refract_pgnamscal.f>
c------------------------------------------------------------------------------
c
c 03/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c scale filenames
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
c    03/07/98   V1.0   Thomas Forbriger (thof)
c    13/11/2012 V1.1   do positioning by full trial and error
c    30/04/2013 V1.2   the algorithm resulted in an infinite loop, if
c                      the last filename would not fit into one line
c    21/03/2014 thof:  provide reverse order of labels
c
c==============================================================================
c
      subroutine refract_pgnamscal
c
      include 'refract_dim.inc'
      include 'refract_opt.inc'
      include 'refract_data.inc'
      include 'refract_pgpara.inc'
      include 'refract_para.inc'
      include 'refract_seipar.inc'
c 
      integer i,j
      real xbox(4), ybox(4)
      real xsep, ysep, xpos, ypos
      real purexsep, pureysep, maxheight
      real scalfac
      integer scalstep, scalmaxstep, nline
      logical gotit
      parameter (purexsep=0.1, pureysep=0.02, scalmaxstep=60)
c 
c set viewport
      call pgsave
      call pgsvp(tov_vpright, 1., tov_vpbot, tov_vptop)
      call pgswin(0., 1.0, 0., 1.)
c
c go and find best scale by trial and error
      scalstep=-1
      gotit=.false.
      do while ((.not.(gotit)).and.(scalstep.le.scalmaxstep))
c set parameters for this run
        scalstep=scalstep+1
        scalfac=0.98**scalstep 
        xsep=purexsep*scalfac
        ysep=pureysep*scalfac
        xpos=xsep
        ypos=0.
        pg_nam_ch=scalfac*pg_std_ch
        pg_nam_linesep=xsep
        nline=1
        gotit=.true.
c go
        call pgsch(pg_nam_ch)
        maxheight=0.
        call pgqtxt(xpos, ypos, 90., 0., 'Fyg', xbox, ybox)
        maxheight=max(maxheight,xbox(1)-xbox(2))
        xpos=xpos+maxheight
        j=1
        do while (j.le.nfiles)
c file index
        if (opt_Treverselegend) then
          i=nfiles+1-j
        else
          i=j
        endif
c place filename label at next available position and test bounding box
          pg_nam_xpos(i)=xpos
          pg_nam_ypos(i)=ypos
          call pgqtxt(xpos, ypos, 90., 0., filename(i), xbox, ybox)
          pg_nam_yend(i)=ybox(4)
          if (pg_nam_yend(i).gt.1.0) then
c label exceeds upper bound
c create a carriage return and line-feed
            ypos=0.
            nline=nline+1
            xpos=maxheight+xpos+xsep
            if (xpos.gt.1.) then
              gotit=.false.
              j=nfiles+1
            endif
          else
c label fits, shift to next position
            ypos=pg_nam_yend(i)+ysep
            j=j+1
          endif
        enddo
        if (nline.gt.pg_nam_maxlines) gotit=.false.
      enddo
c 
      call pgunsa
c 
      if (scalstep.gt.scalmaxstep)
     &  stop 'ERROR (pgnamscal): could not scale filenames'
c 
      if (verbose) print *,'NOTICE (pgnamscal): use ',nline,' lines',
     &  ' with scaling factor ',pg_nam_scale
c 
      return
      end
c
c ----- END OF refract_pgnamscal.f -----
