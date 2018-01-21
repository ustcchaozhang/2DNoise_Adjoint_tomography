/* this is <timeutil.c>
 *
 * for manipulating absolute data times
 *
 * Copyright 1997 by Thomas Forbriger
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
 * 25/04/97   Thomas Forbriger (IfG Stuttgart)
 * 27/05/97   timeutil_norm did ignore carry to doy
 * 15/11/10   avoid tfmacros.h
 *
 * This is a pure C pre-version of libtime.f
 * By now (5/8/2000) it is still included in libtime.a but provides no extra
 * functionality. It may be removed in the future.
 *
 */

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <timeutil.h>

#define TU_EXIT_FAILURE 1
#define TU_CHECKERROR( EXPR , SUB, STR )\
    if ( EXPR ) { fprintf(stderr, "ERROR (%s):\n   %s\n", SUB, STR );\
        exit(TU_EXIT_FAILURE); }

/*
 * some constants we need internally
 * =================================
 */

/* days in each month */
static int timeutil_DaysInYear[13] \
    ={0,31,28,31,30,31,30,31,31,30,31,30,31};
static int timeutil_DaysInLeapYear[13] \
    ={0,31,29,31,30,31,30,31,31,30,31,30,31};

/* value limits */
static long int timeutil_limits[TIMEUTIL_N_ELEMENTS] \
  = {-1L, -1L, -1L, -1L, 24L, 60L, 60L, 1000L, 1000L};

/*
 * clear time structure
 * ====================
 */
void timeutil_clear(td)
timeutil_Ttime *td;
{
  long int *ptr;
  int i;

  ptr=(long int *)td;
  for (i=0; i<TIMEUTIL_N_ELEMENTS; i++) {
    ptr[i]=0;
  }
} /* timeutil_clear */

/*
 * write time to string
 * ====================
 */
char *timeutil_print(td)
timeutil_Ttime td;
{
  static char string[35];
  timeutil_finish(&td);
  sprintf(string, "%3d %2.2d/%2.2d/%4.4d %2.2d:%2.2d:%2.2d.%3.3d%3.3d",
    td.doy, td.day, td.month, td.year,
    td.hour, td.min, td.sec, td.msec, td.usec);
  return string;
} /* timeutil_print */

/*
 * finish preset time structure
 * ============================
 *
 * this is used to set the doy-value and the full year
 */
void timeutil_finish(td)
timeutil_Ttime *td;
{
  if (td->year<70) td->year=td->year+2000;
  if (td->year<100) td->year=td->year+1900;
  td->doy=timeutil_doy(*td);
  timeutil_norm(td);
} /* timeutil_finish */

/* 
 * calculate day of year (doy)
 * ===========================
 */
long int timeutil_doy(td)
timeutil_Ttime td;
{
  int *days;
  long int doy;
  int i;
  
  days=timeutil_DaysInYear;
  if (timeutil_is_leap(td.year)==1) { days=timeutil_DaysInLeapYear; }

  doy=0;
  i=1;
  while (i<td.month) {
    doy=doy+days[i];
    i=i+1;
    TU_CHECKERROR((i>13),"timeutil_doy","month value out of range")
  }
  doy=doy+td.day;
  return doy;
} /* timeutil_doy */

/*
 * check wheter year in data is leap-year or not
 * =============================================
 * 
 * return code
 * 0:  no leap-year
 * 1:  is leap-year
 */
int timeutil_is_leap(year)
long int year;
{
  int res1, res2, res3;

  if (year<70) { year = year + 2000; }
  if (year<100) { year = year + 1900; }

  res1=(int)(year-(int)(year/4)*4);
  res2=(int)(year-(int)(year/100)*100);
  res3=(int)(year-(int)(year/400)*400);

  if (((res1==0) && (res2!=0)) || (res3==0))
  {
    return 1;
  } else {
    return 0;
  }
} /* timeutil_is_leap */

/* 
 * calculate date from day of year (doy)
 * =====================================
 */
void timeutil_date(td, doy)
timeutil_Ttime *td;
long int doy;
{
  int *days;

  td->doy=doy;

  days=timeutil_DaysInYear;
  if (timeutil_is_leap(td->year)==1) { days=timeutil_DaysInLeapYear; }

  td->day=doy;
  td->month=1;
  while(td->day>days[td->month]) {
    td->day=td->day-days[td->month];
    td->month=td->month+1;
    TU_CHECKERROR((td->month>13), "timeutil_date", "doy value out of range")
  }
} /* timeutil_date */

/*
 * normalize values in time structure to allowed ranges
 * ====================================================
 *
 * the date is taken from the doy value in this case
 */
void timeutil_norm(td)
timeutil_Ttime *td;
{
  int i;
  long int carry, diy;
  long int *ptr;

  ptr=(long int *)td;

  /* set correct values up to hours            */
  /* carry to doy values is worked out         */
  /* but doy value may still be incorrect      */
  for (i=TIMEUTIL_N_ELEMENTS-1; i>=TIMEUTIL_FIRST_LINEAR-1; i--) {
    carry=(long int)(ptr[i]/timeutil_limits[i]);
    if (ptr[i]<0) carry=carry-1;
    ptr[i]=ptr[i]-carry*timeutil_limits[i];
    ptr[i-1]=ptr[i-1]+carry;
  }

  /* catch day carry for doy */
  td->doy=td->doy+carry;

  /* work on doy and year                      */
  if (td->year<70) td->year=td->year+2000;
  if (td->year<100) td->year=td->year+1900;
  if (td->doy>0) {
    diy=365;
    if (timeutil_is_leap(td->year)==1) diy=366;
    while (td->doy>diy) {
      td->doy=td->doy-diy;
      td->year=td->year+1;
      diy=365;
      if (timeutil_is_leap(td->year)==1) diy=366;
    }
  } else {
    while (td->doy<1) {
      td->year=td->year-1;
      diy=365;
      if (timeutil_is_leap(td->year)==1) diy=366;
      td->doy=td->doy+diy;
    }
  }
  timeutil_date(td, td->doy);
} /* timeutil_norm */

/*
 * add to time records
 * ===================
 */
void timeutil_add(sum, td1, td2)
timeutil_Ttime *sum;
timeutil_Ttime td1;
timeutil_Ttime td2;
{
  sum->usec  =td1.usec   +td2.usec;
  sum->msec  =td1.msec   +td2.msec;
  sum->sec   =td1.sec    +td2.sec;
  sum->min   =td1.min    +td2.min;
  sum->hour  =td1.hour   +td2.hour;
  sum->doy   =td1.doy    +td2.doy;
  sum->year  =td1.year   +td2.year;
  timeutil_norm(sum);
} /* timeutil_add */

/*
 * compare two data times
 * ======================
 *
 * returnvalue:
 *   0:  both are equal
 *   1:  time1>time2
 *  -1:  time1<time2
 */
int timeutil_compare(time1, time2)
timeutil_Ttime time1;
timeutil_Ttime time2;
{
  if (time1.year  >time2.year)   return 1;
  if (time1.year  <time2.year)   return -1;
  if (time1.month >time2.month)  return 1;
  if (time1.month <time2.month)  return -1;
  if (time1.day   >time2.day)    return 1;
  if (time1.day   <time2.day)    return -1;
  if (time1.hour  >time2.hour)   return 1;
  if (time1.hour  <time2.hour)   return -1;
  if (time1.min   >time2.min)    return 1;
  if (time1.min   <time2.min)    return -1;
  if (time1.sec   >time2.sec)    return 1;
  if (time1.sec   <time2.sec)    return -1;
  if (time1.msec  >time2.msec)   return 1;
  if (time1.msec  <time2.msec)   return -1;
  if (time1.usec  >time2.usec)   return 1;
  if (time1.usec  <time2.usec)   return -1;
  return 0;
} /* timeutil_compare */

/***** END OF timeutil.c *****/

