c this is <greda_phasor.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c subroutines to handle phasor walkout requests
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
c    16/09/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
c this is a dummy function; it became obsolete when source code was move
c to git
      subroutine pwo_cvsid
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine pwo_init(verbose)
c
c call this once at the beginning of program execution
c 
      logical verbose
c 
      include 'greda_dim.inc'
      include 'greda_pwo.inc'
c 
      if (verbose) print *,'initialize phasor walkout handling'
      pwo_nsel=0
      pwo_nlu=0
c 
      return
      end
c 
c----------------------------------------------------------------------
c
      subroutine pwo_selpair(om,p,filename,verbose)
c
c select one omega,p-pair
c
      logical verbose
      character*(*) filename
      real om,p
c 
      include 'greda_dim.inc'
      include 'greda_pwo.inc'
c 
      if (pwo_nsel.lt.pwo_maxsel) then
        if (verbose) then
          print *,'select phasor walkout at ',
     &      om/pwo_pi2,' Hz ',p,' s/km'
        endif
        pwo_nsel=pwo_nsel+1
        pwo_om(pwo_nsel)=om
        pwo_p(pwo_nsel)=p*1.e-3
        pwo_file(pwo_nsel)=filename
        pwo_process(pwo_nsel)=.true.
      else
        if (verbose) then
          print *,'skipping phasor walkout selection for ',
     &      om/pwo_pi2,' Hz ',p,' s/km ',
     &      ' - exceeds data space'
        endif
      endif
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine pwo_readsel(filename, automatic, verbose)
c
c read selection from file
c
c generate automatic filenames if automatic is set to true
c 
      character*(*) filename
      logical automatic, verbose
c 
      include 'greda_dim.inc'
      include 'greda_pwo.inc'
c 
      integer i, ci, lu
      parameter(lu=20)
      real fre,slo
      character*(80) outfile
c 
      ci=index(filename,' ')-1
      i=1
      if (verbose) then
        print *,' '
        print *,'phasor walkout:'
        print *,'opening selection file ',filename(1:ci)
      endif
      open(lu, file=filename, err=99)
    1 continue
      if (automatic) then
        read(lu, *, err=98, end=2) fre,slo
        outfile=filename(1:ci)
        outfile(ci+1:ci+1)='.'
        write (outfile(ci+2:ci+4), '(i3.3)') i
        outfile(ci+5:ci+8)='.dat'
      else
        read(lu, *, err=98, end=2) fre,slo,outfile
      endif
      call pwo_selpair(fre*pwo_pi2,slo,outfile,verbose)
      i=i+1
      goto 1
    2 continue
      close(lu, err=97)
      if (verbose) print *,'read selection, file is closed...'
c 
      return 
   99 stop 'ERROR (pwo_readsel): opening selection file'
   98 stop 'ERROR (pwo_readsel): reading selection file'
   97 stop 'ERROR (pwo_readsel): closing selection file'
      end
c
c----------------------------------------------------------------------
c
      subroutine pwo_initable(om,p,nom,np,iom,verbose)
c 
c setup lookup table
c call this at the beginning of each frequency
c
c om,p: frequency and slowness array from main program
c iom: frequency index which is going to be processed
c
      real om(nom), p(np)
      integer nom, np, iom
      logical verbose
c
      include 'greda_dim.inc'
      include 'greda_pwo.inc'
c 
      integer i,j
c 
      pwo_nlu=0  
c
      if (pwo_nsel.gt.0) then
        do i=1,pwo_nsel
          if (pwo_process(i)) then
            j=0
c is frequency closer than previous
            if (iom.gt.1) then
              if (abs(om(iom)-pwo_om(i)).lt.
     &            abs(om(iom-1)-pwo_om(i))) then
                j=j+1
              endif
            else
              j=j+1
            endif
c is frequency closer than next
            if (iom.lt.nom) then
              if (abs(om(iom)-pwo_om(i)).lt.
     &            abs(om(iom+1)-pwo_om(i))) then
                j=j+1
              endif
            else
              j=j+1
            endif
c both conditions hold
            if (j.eq.2) then
              if (pwo_nlu.lt.pwo_maxlu) then
                pwo_nlu=pwo_nlu+1
                pwo_lu(pwo_nlu)=i
                pwo_process(i)=.false.
                if (verbose) then
                  print 50,'phasor walkout for ',
     &              pwo_om(i)/pwo_pi2,'Hz, ',
     &              pwo_p(i)*1.e3,'s/km setup at',
     &              om(iom)/pwo_pi2,'Hz (',
     &              100.*abs(1.-om(iom)/pwo_om(i)),'% wrong)'
                endif
              else
                print *,'NOTICE (pwo_initable): too many entries'
              endif
            endif
          endif
        enddo
      endif
c
      return
   50 format(a,f7.3,a,f7.3,a,f7.3,a,f4.1,a)
      end
c
c----------------------------------------------------------------------
c
      subroutine pwo_write(overwrite, verbose, method, r, ntr)
c
c write phasor walkout to file
c
      logical overwrite, verbose
      character*(*) method
      integer ntr
      real r(ntr)
c 
      include 'greda_dim.inc'
      include 'greda_pwo.inc'
c 
      integer i,ci,lu,j
      parameter(lu=20)
      character*(80) filename
      real maxamp
c 
      if (pwo_nlu.gt.0) then
        if (verbose) print *,'writing phasor walkout:'
        do i=1,pwo_nlu
          filename=pwo_file(pwo_lu(i))
          ci=index(filename,' ')-1
          if (verbose) then
            if (overwrite) then
              print *,'opening phasor file ',filename(1:ci),
     &      ' - overwrite mode'
            else
              print *,'opening phasor file ',filename(1:ci)
            endif
          endif
          if (overwrite) then
            open(lu, file=filename, err=99)
          else
            open(lu, file=filename, status='new', err=99)
          endif
          write(lu, '(a)', err=98) '# phasor walkout'
          write(lu, '(a,a)', err=98) '# ',method
          write(lu, '(a,f10.3,a)', err=98) '# ',
     &      pwo_om(pwo_lu(i))/pwo_pi2,' Hz'
          write(lu, '(a,f10.3,a)', err=98) '# ',
     &      pwo_p(pwo_lu(i))*1.e3,' s/km'
          write(lu, '(a,i5,a)', err=98) '# ',ntr,' offsets'
          write(lu, '(a)', err=98) 
     &      '# offset (m), real, imag, norm real, norm imag'
          maxamp=1.e-30
          do j=1,ntr
            maxamp=max(maxamp,abs(pwo_phasor(i,j)))
          enddo
          write(lu, '(f10.3,4(2x,g10.4))', err=98) 0.,0.,0.,0.,0.
          do j=1,ntr
            write(lu, '(f10.3,4(2x,g10.4))', err=98)
     &        r(j), real(pwo_phasor(i,j)), aimag(pwo_phasor(i,j)),
     &        real(pwo_phasor(i,j))/maxamp, 
     &        aimag(pwo_phasor(i,j))/maxamp
          enddo
          close(lu, err=97)
        enddo
      endif
c 
      return
   99 stop 'ERROR: opening phasor walkout file'
   98 stop 'ERROR: writing phasor walkout file'
   97 stop 'ERROR: closing phasor walkout file'
      end
c
c ----- END OF greda_phasor.f ----- 
