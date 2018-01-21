c--------------------------------------------------------------
c  Test skipping data
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
	character date*6, time*10
	logical last
        character infile*80
c
        print *,'input file name?'
        read(5, '(a80)') infile
c
	open(1,file=infile)
	call sff_RStatus(1,version,timestamp,code,ierr)
	write(6,'(a,f6.2,1x,a10,1x,a5)') 'RStatus: ',version,timestamp,code
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
c  skip data
c
	call sff_SkipData(1,code,last,ierr)
	if (last) then
	  print *,'was last trace (code:',code,')'
        else
	  print *,'was NOT last trace (code:',code,')'
        endif
c
	stop
	end



