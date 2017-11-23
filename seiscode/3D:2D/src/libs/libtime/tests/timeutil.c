/* this is <timeutil.c>
 *
 * Copytight 1997 Thomas Forbriger
 *
 * a program to test libtimeutil.a
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
 *
 */

#include <timeutil.h>
#include <stdio.h>

main ()
{
  timeutil_Ttime time1, time2;
  char *cptr;

  timeutil_clear(&time1);
  timeutil_clear(&time2);

  time1.year=97;
  time1.month=4;
  time1.day=29;
  time1.hour=12;
  time1.min=10;
  time1.sec=9;
  time1.msec=200;
  time1.usec=300;
  printf("time: %s\n", timeutil_print(time1));
  timeutil_finish(&time1);
  printf("time: %s\n", timeutil_print(time1));

  timeutil_date(&time1, 35L);
  printf("time: %s\n", timeutil_print(time1));

  printf("leapyear: 1996: %d   1997: %d   2000: %d   1900: %d\n",
    timeutil_is_leap(1996),
    timeutil_is_leap(1997),
    timeutil_is_leap(2000),
    timeutil_is_leap(1900));

  timeutil_clear(&time1);
  printf("time: %s\n", timeutil_print(time1));

  timeutil_add(&time2, time1, time2); 
  printf("time: %s\n", timeutil_print(time2));

  timeutil_clear(&time2);
  time2.hour=12;
  timeutil_add(&time2, time1, time2); 
  printf("time: %s\n", timeutil_print(time2));

  timeutil_clear(&time2);
  time2.hour=34;
  timeutil_add(&time2, time1, time2); 
  printf("time: %s\n", timeutil_print(time2));

  timeutil_clear(&time2);
  time2.hour=1;
  time2.min=2;
  time2.sec=3;
  time2.msec=4;
  time2.usec=5;
  timeutil_add(&time2, time1, time2); 
  printf("time: %s\n", timeutil_print(time2));

  timeutil_clear(&time2);
  time2.hour=24;
  time2.min=60;
  time2.sec=60;
  time2.msec=1000;
  time2.usec=1000;
  timeutil_add(&time2, time1, time2); 
  printf("time: %s\n", timeutil_print(time2));

  timeutil_clear(&time2);
  time2.hour=24;
  time2.min=241;
  time2.sec=60;
  time2.msec=1000;
  time2.usec=1000;
  timeutil_add(&time2, time1, time2); 
  printf("time: %s\n", timeutil_print(time2));

  time1.day=29;
  time1.month=2;
  time1.year=1997;
  printf("time: %s\n", timeutil_print(time1));

  time1.day=29;
  time1.month=2;
  time1.year=1996;
  printf("time: %s\n", timeutil_print(time1));

  time1.day=30;
  time1.month=2;
  time1.year=1996;
  printf("time: %s\n", timeutil_print(time1));

  cptr=timeutil_print(time1);
  cptr[27]='\0';
  printf("nicetime: %s\n", cptr+4);
}
