/* this is <timeutil.h>
 *
 * this file contains definitions for timeutil.c routines
 *
 * Copyright 1997 Thomas Forbriger (IfG Stuttgart)
 *
 * This is a pure C pre-version of libtime.f
 * By now (5/8/2000) it is still included in libtime.a but provides no extra
 * functionality. It may be removed in the future.
 *
 *
 * ----
 * libtime is free software; you can redistribute it and/or modify
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
 *  15/11/2010    make timeutil deprecated, its functionality duplicates
 *                the functionality presented in libtime.h
 *
 */

#ifndef _TF_TIMEUTIL_H
#define _TF_TIMEUTIL_H

#warning "timeutil is deprecated! It may vanish in the near future"

/* 
 * define macros
 * =============
 */

/* number of long ints in structure */
#define TIMEUTIL_N_ELEMENTS 9

/* first linear element in structure */
#define TIMEUTIL_FIRST_LINEAR 5

/*S*/

/*
 * define data structures
 * ======================
 */

/* time data structure */
typedef struct {
  long int year, month, day, doy;
  long int hour, min, sec, msec, usec;
} timeutil_Ttime;

/*
 * prototypes
 * ==========
 */

/* clear a time record */
void timeutil_clear(timeutil_Ttime *);

/* type out a time record */
char *timeutil_print(timeutil_Ttime);

/* finish a preset time record */
void timeutil_finish(timeutil_Ttime *);

/* calculate doy */
long int timeutil_doy(timeutil_Ttime);

/* check for leap year */
int timeutil_is_leap(long int);

/* set date from doy */
void timeutil_date(timeutil_Ttime *, long int);

/* set elements to correct value range */
void timeutil_norm(timeutil_Ttime *);

/* add two time records */
void timeutil_add(timeutil_Ttime *, timeutil_Ttime, timeutil_Ttime);

/* compare two time records */
int timeutil_compare(timeutil_Ttime, timeutil_Ttime);

/*E*/

#endif /* _TF_TIMEUTIL_H */

/***** END OF timeutil.h *****/
