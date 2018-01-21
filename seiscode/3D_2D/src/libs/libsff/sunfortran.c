/* these are mimicking functions to implement sunfortran routines */

/*
 * Copyright (c) 1996 by Wolfgang Friederich
 * Copyright (c) 1996 by Thomas Forbriger 
 *
 * ----
 * This file is part of libsff.
 *
 * libsff is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version. 
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 * ----
 *
 */

#include <stddef.h>
#include <time.h>

void idate_(int *iarray);
void itime_(int *iarray);
/*
 *--------------------------------------
 *  function mimicking fortran idate
 *--------------------------------------
*/
void idate_(iarray)
	int *iarray;
{
	struct tm *datime;
	time_t zeit;
	zeit = time((time_t *) NULL);
	datime = localtime(&zeit);
	iarray[0] = datime->tm_mday;
	iarray[1] = datime->tm_mon + 1;
	iarray[2] = datime->tm_year;
}
/*
 *--------------------------------------
 *  function mimicking fortran itime
 *--------------------------------------
*/
void itime_(iarray)
	int *iarray;
{
	struct tm *datime;
	time_t zeit;
	zeit = time((time_t *) NULL);
	datime = localtime(&zeit);
	iarray[0] = datime->tm_hour;
	iarray[1] = datime->tm_min;
	iarray[2] = datime->tm_sec;
}
