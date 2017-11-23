/* this is <ctime_read.c>
 * ----------------------------------------------------------------------------
 *
 * Copyright 2000 by Thomas Forbriger (IfG Stuttgart)
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
 * fill a time_Ts structure from a character string
 *
 * REVISIONS and CHANGES
 *    06/08/2000   V1.0   Thomas Forbriger
 *    09/08/2000   V1.1   correct handling of relative dates
 *    14/12/2007   V1.2   use the Fortran integer type defined in libtime.h
 *                        see there
 *
 * ============================================================================
 */

#include <libtime.h>
#include <string.h>
#include <stdio.h>

int time_read(time_Ts *Date, const char *String)
{
  double sec;
  const char *ptr;
  char cmilmicsec[7];
  integer day, month, milmicsec;
  int i;

  time_clear(Date);
  ptr=String;

  ptr=strpbrk(ptr, "0123456789");
  RETURNERROR((ptr==NULL), "time_read", \
    "year is missing", EXIT_FAILURE)
  Date->year=strtol(ptr, &ptr, 10);

  ptr=strpbrk(ptr, "0123456789");
  RETURNERROR((ptr==NULL), "time_read", \
    "month is missing", EXIT_FAILURE)
  month=strtol(ptr, &ptr, 10);

  ptr=strpbrk(ptr, "0123456789");
  RETURNERROR((ptr==NULL), "time_read", \
    "day is missing", EXIT_FAILURE)
  day=strtol(ptr, &ptr, 10);

  ptr=strpbrk(ptr, "0123456789");
  if (ptr!=NULL) {
    Date->hour=strtol(ptr, &ptr, 10);
    ptr=strpbrk(ptr, "0123456789");
  }
  if (ptr!=NULL) {
    Date->minute=strtol(ptr, &ptr, 10);
    ptr=strpbrk(ptr, "0123456789");
  }
  if (ptr!=NULL) {
    Date->second=strtol(ptr, &ptr, 10);
    ptr=strpbrk(ptr, "0123456789");
  }
  if (ptr!=NULL) {
    for(i=0; i<6; i++) {
      if (ptr!=NULL) {
        if (isdigit(ptr[i])) {
          cmilmicsec[i]=ptr[i];
        } else {
          ptr=NULL;
          cmilmicsec[i]='0';
        }
      } else {
        cmilmicsec[i]='0';
      }
      cmilmicsec[6]='\0';
    }
    milmicsec=strtol(cmilmicsec, &ptr, 10);
    Date->milsec=(integer)(milmicsec/1000);
    Date->micsec=milmicsec-(Date->milsec*1000);
  }

  if ((month>0 && Date->year>0) || month>0) 
  { 
    time_fullyear(&Date->year); 
    time_setdoy(day, month, Date);
  }
  else
  {
    Date->doy=day;
  }

  time_norm(Date);
  return(EXIT_SUCCESS);
} /* time_read */
 
/* ----- END OF ctime_read.c ----- */
