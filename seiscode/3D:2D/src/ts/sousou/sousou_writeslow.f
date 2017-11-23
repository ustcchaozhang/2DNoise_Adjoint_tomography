c this is <sousou_writeslow.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c write slowness analysis as seismic traces
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
c    18/11/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine writeslow(outfile, comment)
c 
c write full dataset to filename
c overwrite all dataset if true
c 
      include 'sousou_dim.inc'
      include 'sousou_data.inc'
      include 'sousou_para.inc'
      include 'sousou_options.inc'
      include 'sousou_workspace.inc'
c 
      character outfile*(*)
      character comment*(*)
c 
      integer lu, ierr
      parameter(lu=11)
c 
      integer itrace, isamp
c 
c source
      character stype*20, sdate*6, stime*10
c 
      complex fact,ime
      real pi,freq
      parameter(pi=3.1415927,ime=(0.,1.))
c data
      complex outspec(maxspec)
      real outdata(maxspec)
      integer idata(maxspec)
      equivalence(outdata, idata)
c trace
      logical last
c FREE block
      integer maxfree, nfree
      parameter(maxfree=10)
      character*80 free(maxfree)
c WID2 info
      character*132 wid2line
      integer refdate(7), shiftdate(7), startdate(7)
c 
      timeshift=dt(1)*nspecsamp/2
c dummy reference time and SRCE information
      sdate='980101' 
      stime='000000.000'
      stype='unknown'
c create file FREE block
      free(1)='file was created by'
      free(2)=para_version
      free(3)=comment
      nfree=3
c 
      if (opt_verbose.gt.0) then
        print *,' '
        print *,'writing to file ',outfile(1:index(outfile, ' '))
        print *,'  shift by ',timeshift,' sec'
      endif
      call sff_WOpenFS(lu, outfile, free, nfree, stype, 'C',
     &  0., 0., 0., sdate, stime, ierr)
      if (ierr.ne.0) stop 'ERROR (writedata): could not open file'
c 
      do itrace=1,maxslow
        if (opt_verbose.gt.0) print 50,itrace,ntraces,slowvalue(itrace)
        last=.false.
        if (itrace.eq.maxslow) last=.true.
c prepare WID2 line
        call time_clear(refdate)
        call time_clear(shiftdate)
        call time_clear(startdate)
        refdate(1)=1998
        refdate(2)=1
        call sffu_dttotime(timeshift,shiftdate)
        call time_finish(refdate)
        call time_sub(refdate,shiftdate,startdate)
        call sff_PrepWid2(nspecsamp, 1./dt(1), 'NSP   ',
     &    0, 0, 0, 0, 0, 
     &    'NSP    ', 'NSP    ', 'NSP     ', 
     &    0., -1., -1., -1., -1., 
     &    wid2line, ierr)
        if (ierr.ne.0) stop 'ERROR (writedata): preparing WID2 line'
        call sffu_setwid2time(wid2line, startdate)
c create FREE block
        free(1)='This is a slowness analysis result'
        free(2)='read time as tau'
        free(3)='read offset as velocity'
        nfree=3
c 
c        print *,'copy'
        do isamp=1,nspecsamp
          freq=df*(isamp-1)
          fact=cexp(-ime*freq*2.*pi*timeshift)
          if (isamp.gt.nspecsamp/2) fact=conjg(fact)
          outspec(isamp)=slowspec((itrace-1)*nspecsamp+isamp)*fact
        enddo
c        print *,'FT'
        call tf_fork(nspecsamp, outspec, 1.)
c        print *,'copy back'
        do isamp=1,nspecsamp
          outdata(isamp)=real(outspec(isamp))
        enddo
c 
c        print *,'write'
        call sff_WTraceFI(lu, wid2line, nspecsamp, 
     &    outdata, idata, last,
     &    nfree, free,
     &    'C', 1./slowvalue(itrace), 0., 0., 0, ierr)
        if (ierr.ne.0) stop 'ERROR (writedata): writing trace'
      enddo
      if (opt_verbose.gt.0) print *,'file written and closed'
c 
      return
   50 format(3x,'writing trace ',i3,'/',i3,' at ',f10.6,'s/m')
      end
c
c ----- END OF sousou_writeslow.f -----
