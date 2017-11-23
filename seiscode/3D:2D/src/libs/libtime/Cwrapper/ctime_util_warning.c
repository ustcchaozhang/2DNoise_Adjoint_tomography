/* this is <ctime_util_warning.c>
 * ----------------------------------------------------------------------------
 *
 * Copyright 06/08/2000 by Thomas Forbriger (IfG Stuttgart)
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
 * C language specific warning handler
 *
 * REVISIONS and CHANGES
 *    06/08/2000   V1.0   Thomas Forbriger
 *    09/08/2000   V1.1   both are WARNINGs not ERRORs
 *    14/12/2007   V1.2   use the Fortran integer type defined in libtime.h
 *                        see there
 *    17/12/2007   V1.3   introduced typedef timeint
 *
 * ============================================================================
 */

#include <libtime.h>
#include <stdio.h>

/* 
 * Fortran calling convention:
 */
int time_util_warning__(char *caller, char *text, 
                        ftnlen caller_len, ftnlen text_len)
{
    int i;
    fputs("WARNING (", stderr);
    for (i=0; i<caller_len; i++) { fputc((int)*(caller++), stderr); }
    fputs("): ",stderr);
    for (i=0; i<text_len; i++) { fputc((int)*(text++), stderr); }
    fputc('\n', stderr);
    return 0;
} /* time_util_warning__ */

int time_util_warning_n__(char *caller, char *text, timeint *n, 
                          ftnlen caller_len, ftnlen text_len)
{
    integer i;
    fputs("WARNING (", stderr);
    for (i=0; i<caller_len; i++) { fputc((int)*(caller++), stderr); }
    fputs("): ",stderr);
    for (i=0; i<text_len; i++) { fputc((int)*(text++), stderr); }
    i=(int)*n;
    fprintf(stderr," %d\n",i);
    return 0;
} /* time_util_warning_n__ */
 
/* ----- END OF ctime_util_warning.c ----- */
