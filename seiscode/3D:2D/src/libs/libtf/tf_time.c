/* this is <tf_time.c>
 * ----------------------------------------------------------------------------
 *
 * Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
 *
 * provide C-time function to be used in f2c fortran code
 *
 * ----
 * This program is free software; you can redistribute it and/or modify
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
 * REVISIONS and CHANGES
 *    18/11/97   V1.0   Thomas Forbriger
 *
 * ============================================================================
 */

#include "f2c.h"
#include "time.h"
#include "stddef.h"

/*
 * returns the time since 00:00:00 GMT, January 1, 1970, 
 * measured in seconds
 */

integer tf_time__()
{
  integer retval;
  retval=time(NULL);
  return retval;
}
 
/* ----- END OF tf_time.c ----- */
