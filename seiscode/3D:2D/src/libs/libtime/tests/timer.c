/* this program checks time functions 
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
 */
#include <sys/types.h>
#include <sys/times.h>
#include <stdio.h>

struct timeb {
  time_t   time;
  unsigned short millitm;
  short    timezone;
  short    dstflag;
};

extern int      ftime(struct timeb *__tp);

main()
{
  struct timeb zeit;
  int resulter, i,j, k;
  printf("hi there\n");
  for (i=0;i<20;i++)
  {
    resulter=ftime(&zeit);
    printf("%d %d %huh %hdh %hdh\n",resulter, zeit.time, zeit.millitm,
      zeit.timezone, zeit.dstflag);
    for (j=1;j<5000;j++)
      k=j*50+4/j*j;
  }
}
