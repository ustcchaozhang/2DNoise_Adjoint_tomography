c--------------------------------------------------------------
c  Test a full write
c
c Copyright (c) 1996 by Wolfgang Friederich
c Copyright (c) 1996, 2010 by Thomas Forbriger 
c
c ----
c This file is part of libsff.
c
c libsff is free software; you can redistribute it and/or modify
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
c--------------------------------------------------------------
	character code*10, timestamp*13,lines(50)*80,cs*1,typ*20
	character date*6, time*10, wid2line*132
	integer msamp
	parameter(msamp=30000)
c
	integer idata(msamp)
        character infile*80, outfile*80
c
        print *,'input file name?'
        read(5, '(a80)') infile
        print *,'output file name?'
        read(5, '(a80)') outfile
c
	open(1,file=infile)
	call sff_RStatus(1,version,timestamp,code,ierr)
c
c  optional FS blocks
c
	i=1
 11	if( ichar(code(i:i)).ne.32 .and. ichar(code(i:i)).ne.0) then
		if(code(i:i).eq.'F') then
			call sff_RFree(1,nline,lines,lenmax,50,ierr)
		endif
		if(code(i:i).eq.'S') then
			call sff_RSource(1,typ,cs,c1,c2,c3,date,time,ierr)
		endif
		i=i+1
		goto 11
	endif
c
c  read data
c
	nsamp=msamp
	call sff_RData(1,wid2line,nsamp,tanf,dt,idata,ampfac,code,ierr)
c
c  optional blocks
c
	i=1
 12	if( ichar(code(i:i)).ne.32 .and. ichar(code(i:i)).ne.0) then
		if(code(i:i).eq.'F') then
			call sff_RFree(1,nline,lines,lenmax,50,ierr)
		endif
		if(code(i:i).eq.'I') then
			call sff_RInfo(1,cs,c1,c2,c3,nstack,ierr)
		endif
		i=i+1
		goto 12
	endif
c
	close(1)
c
c  now write back into SFF-file
c
	open(2,file=outfile)
	call sff_WStatus(2,'FS')
	call sff_WFree(2,nline,lines)
	call sff_WSource(2,typ,cs,c1,c2,c3,date,time)
	call sff_WData(2,wid2line,nsamp,idata,ampfac,'FI')
	call sff_WFree(2,nline,lines)
	call sff_WInfo(2,cs,c1,c2,c3,nstack)
	close(2)
c
	stop
	end
