c--------------------------------------------
c  Test sff_PrepWid2
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
c-------------------------------------------
	character wid2line*132,sta*5,cdum(3)*3,cmp*3,auxid*4,instyp*6
	character date*10, time*12
	real rdum(5)
	integer idum(5)
c
	integer sff_GetN
	real sff_GetDt
c
c  Test the defaults
c
	nsamp=1000
	sampr=1.
	sta='STU  '
	call sff_PrepWid2(nsamp,sampr,sta,-1,-1,-1,-1,-1,'NSP','NSP','NSP',
     1		-1.,-1.,-1.,-1.,-1.,wid2line,ierr)
	write(6,'(a)') wid2line
c
	idum(1)=1996
	idum(2)=10
	idum(3)=23
	idum(4)=23
	idum(5)=10
	cmp='LHZ'
	auxid='auxi'
	instyp=' STS-1'
	rdum(1)=56.234
	rdum(2)=1.59
	rdum(3)=6.284
	rdum(4)=-1.
	rdum(5)=0.
	call sff_PrepWid2(nsamp,sampr,sta,1996,10,23,23,10,'LHZ',
     1			auxid,' STS-1',56.234,1.59,6.284,-1.,0.,wid2line,ierr)
	write(6,'(a)') wid2line
c
c  Test the other service routines
c
	call sff_GetDate(wid2line,date)
	write(6,'(a,a)') 'Date: ',date
	call sff_GetTime(wid2line,time)
	write(6,'(a,a)') 'Time: ',time
	call sff_GetChannel(wid2line,cmp)
	write(6,'(a,a)') 'Channel: ',cmp
	call sff_GetStation(wid2line,sta)
	write(6,'(a,a)') 'Station: ',sta
	nsamp=sff_GetN(wid2line)
	write(6,'(a,i4)') 'N = ',nsamp
	dt=sff_GetDt(wid2line)
	write(6,'(a,f8.4)') 'dt = ',dt
	stop
	end





