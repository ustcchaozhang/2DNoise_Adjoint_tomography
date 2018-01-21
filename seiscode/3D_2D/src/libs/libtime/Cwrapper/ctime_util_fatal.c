/* this is <ctime_util_fatal.c>
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
 * C language specific fatal error handler
 *
 * REVISIONS and CHANGES
 *    06/08/2000   V1.0   Thomas Forbriger
 *
 * ============================================================================
 */

#include <libtime.h>
#include <stdio.h>
#include <stdlib.h>

/* 
 * Fortran calling convention:
 */
int time_util_fatal__(char *caller, char *text, 
                      ftnlen caller_len, ftnlen text_len)
{
    int i;
    fputs("ERROR (", stderr);
    for (i=0; i<caller_len; i++) { fputc((int)*(caller++), stderr); }
    fputs("): ",stderr);
    for (i=0; i<text_len; i++) { fputc((int)*(text++), stderr); }
    fputc('\n', stderr);
    abort();
} /* time_util_fatal__ */

 
/* ----- END OF ctime_util_fatal.c ----- */
