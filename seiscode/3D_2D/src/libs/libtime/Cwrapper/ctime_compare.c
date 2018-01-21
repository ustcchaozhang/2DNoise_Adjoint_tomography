/* this is <ctime_compare.c>
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
 * C-wrapper for FORTRAN time_compare libtime kernel function
 *
 * REVISIONS and CHANGES
 *    06/08/2000   V1.0   Thomas Forbriger
 *    14/12/2007   V1.1   use the Fortran integer type defined in libtime.h
 *                        see there
 *    17/12/2007   V1.2   discarded time_Tu::farray
 *
 * ============================================================================
 */

#include <libtime.h>

integer time_compare(time_Ts Date1, time_Ts Date2)
{
  time_Tu *u1;
  time_Tu *u2;
  extern int time_compare__();
  u1=(time_Tu *)&Date1;
  u2=(time_Tu *)&Date2;
  return((integer)time_compare__(&u1->array, &u2->array));
} /* time_compare */
 
/* ----- END OF ctime_compare.c ----- */
