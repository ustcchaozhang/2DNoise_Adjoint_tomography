/* this is <ctime_copy.c>
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
 * C-wrapper for FORTRAN time_copy libtime kernel function
 *
 * REVISIONS and CHANGES
 *    06/08/2000   V1.0   Thomas Forbriger
 *    17/12/2007   V1.1   discarded time_Tu::farray
 *
 * ============================================================================
 */

#include <libtime.h>

void time_copy(time_Ts Date1, time_Ts *Pdate2)
{
  time_Tu *u1;
  time_Tu *u2;
  extern int time_copy__();
  u1=(time_Tu *)&Date1;
  u2=(time_Tu *)Pdate2;
  time_copy__(&u1->array, &u2->array);
} /* time_copy */
 
/* ----- END OF ctime_copy.c ----- */
