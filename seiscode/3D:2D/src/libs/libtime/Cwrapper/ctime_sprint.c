/* this is <ctime_sprint.c>
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
 * libtime C kernel function
 * 
 * NOTICE: This routine returns a pointer to a static character array. The
 * next call to time_sprint will destroy the contents of this string.
 *
 * REVISIONS and CHANGES
 *    06/08/2000   V1.0   Thomas Forbriger
 *    17/12/2007   V1.1   use integer type
 *    12/12/2008   V1.2   use dots not slashes in the date string
 *    19/07/2010	 V1.3   Daniel Armbruster: cast of variables to (int)
 *
 * ============================================================================
 */

#include <libtime.h>
#include <stdio.h>

char *time_sprint(time_Ts Date)
{
  integer day;
  integer month;
  static char sdate[TIME_SLEN];

  time_norm(&Date);

  if (Date.year == 0L) {
    sprintf(sdate, "%03dd %02dh %02dm %02d.%03d%03ds", (int)Date.doy,
      (int)Date.hour, (int)Date.minute, (int)Date.second, (int)Date.milsec, (int)Date.micsec);
  } else {
    time_getdate(&day, &month, Date);
    sprintf(sdate, "%03d %02d.%02d.%04d %02d:%02d:%02d.%03d%03d",
      (int)Date.doy, (int)day, (int)month, (int)Date.year, 
      (int)Date.hour, (int)Date.minute, (int)Date.second, (int)Date.milsec, (int)Date.micsec);
  }
  return(sdate);
} /* time_setdoy */
 
/* ----- END OF ctime_sprint.c ----- */
