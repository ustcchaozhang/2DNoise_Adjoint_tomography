c--------------------------------------------------------------
c Test a full read
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
	character infile*80, outfile*80
	integer msamp
c
	parameter(msamp=30000)
	integer idata(msamp)
      real sff_libversion
c 
	print *,'input file name?'
	read(5, '(a80)') infile
	print *,'output file name?'
	read(5, '(a80)') outfile
      write(6,'(a,f6.2)')'TESTING LIBRARY: ',sff_libversion()
	open(1,file=infile)
	call sff_RStatus(1,version,timestamp,code,ierr)
	write(6,'(a,f6.2,1x,a10,1x,a5,1x,i2)') 'RStatus: ',
     &  version,timestamp,code,ierr
c
c  optional FS blocks
c
	i=1
	write(6,'(a)') 'Optional blocks:'
 11	if( ichar(code(i:i)).ne.32 .and. ichar(code(i:i)).ne.0) then
		if(code(i:i).eq.'F') then
			call sff_RFree(1,nline,lines,lenmax,50,ierr)
			do 10 j=1,nline
 10			write(6,'(a)') lines(j)(1:lenmax)
		endif
		if(code(i:i).eq.'S') then
			call sff_RSource(1,typ,cs,c1,c2,c3,date,time,ierr)
			write(6,'(a,a,3f15.3,1x,a6,1x,a10)') typ,cs,c1,c2,c3,date,time
		endif
		i=i+1
		goto 11
	endif
c
c  read data
c
	nsamp=msamp
      call sff_rdata(1,wid2line,nsamp,tanf,dt,idata,ampfac,code,ierr)
c
c  write data to pgy file
c
	open(2,file=outfile)
	call sff_TrimLen(wid2line,ntrim)
	write(2,'(a)') wid2line(1:ntrim)
	write(2,'(i8)') nsamp
	write(2,'(f15.3)') tanf
	write(2,'(f15.3)') dt
	do 20 i=1,nsamp
 20	write(2,'(i8)') idata(i)
	close(2)
c
	i=1
 12	if( ichar(code(i:i)).ne.32 .and. ichar(code(i:i)).ne.0) then
		if(code(i:i).eq.'F') then
			call sff_RFree(1,nline,lines,lenmax,50,ierr)
			do 30 j=1,nline
 30			write(6,'(a)') lines(j)(1:lenmax)
		endif
		if(code(i:i).eq.'I') then
			call sff_RInfo(1,cs,c1,c2,c3,nstack,ierr)
			write(6,'(a,1x,3f15.3,1x,i4)') cs,c1,c2,c3,nstack
		endif
		i=i+1
		goto 12
	endif
c
	stop
	end


