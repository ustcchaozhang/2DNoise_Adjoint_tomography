/* this is <tf_cswap.c>
 * ----------------------------------------------------------------------------
 *
 * Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
 *
 * swap 2 bytes and 4 bytes to convert from big to small endian
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
 *    13/08/97   V1.0   Thomas Forbriger
 *
 * ============================================================================
 */

#include <libtf.h>

void tf_swap2(ptr)
char *ptr;
{
  char help;
  help=ptr[1];
  ptr[1]=ptr[0];
  ptr[0]=help;
} /* tf_swap2 */

void tf_swap4(ptr)
char *ptr;
{
  char help;
  help=ptr[1];
  ptr[1]=ptr[2];
  ptr[2]=help;
  help=ptr[0];
  ptr[0]=ptr[3];
  ptr[3]=help;
} /* tf_swap4 */

/* ----- END OF tf_cswap.c ----- */
