/* this is <tf_cbytesex.c>
 * ----------------------------------------------------------------------------
 *
 * Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
 *
 * C written bytesex routine
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
 * routine to check bytesex of processor
 *
 * return code:
 *   68:   Motorola type (SPARC, 68000, etc.)
 *   80:   INTEL type (Pentium, 80386, etc.)
 *   -1:   unknown
 *
 * REVISIONS and CHANGES
 *    11/08/97   V1.0   Thomas Forbriger
 *    13/11/10   V1.1   include libtf.h
 *
 * ============================================================================
 */

#include <libtf.h>

int tf_bytesex()
{
  short int i = 0x1234;
  char *ptr;
  ptr=(char *) &i;
  if (ptr[0]==0x12) {
    return 68;
  } else {
    if (ptr[0] == 0x34) {
      return 80;
    } else {
      return -1;
    }
  }
}

/* ----- END OF tf_cbytesex.c ----- */
