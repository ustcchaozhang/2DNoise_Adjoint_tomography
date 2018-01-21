/* this is <tf_makegrd.c>
 * ----------------------------------------------------------------------------
 *
 * Copyright (c) 1999 by Thomas Forbriger (IfG Stuttgart)
 *
 * create GMT grid-file from fortran array
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
 * provided with fortran interface
 *
 * REVISIONS and CHANGES
 *    15/01/99   V1.0   Thomas Forbriger
 *
 * ============================================================================
 */

#include "errno.h"
#include "stdio.h"
#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;
static integer c__1 = 1;

/*<    >*/
/* Subroutine */ int makegrd_(arra, nx, ny, xmin, ymin, dx, dy, mx, my, 
	filename, grdfile, filename_len, grdfile_len)
real *arra;
integer *nx, *ny;
real *xmin, *ymin, *dx, *dy;
integer *mx, *my;
char *filename;
char *grdfile;
ftnlen filename_len;
ftnlen grdfile_len;
{
    /* System generated locals */
    integer arra_dim1, arra_offset, i__1, i__2;
    int i,j;
    real value;

    FILE *fptr;

    char commandline[200];

/*
 * arra:      data array
 * nx,mx:     filled part of arra
 * mx,my:     dimensions of arra
 * xmin,ymin: axis values of first element
 * dx,dy:     axis step from element to element
 *
 * filename:  name of GMT grid file
 */

/*<       character filename*(*) >*/
/*<       integer nx,ny,mx,my >*/
/*<       real xmin,ymin,dx,dy >*/
/*<       real arra(mx,my) >*/
/*<       integer i,j >*/

/*<       do i=1,nx >*/
    /* Parameter adjustments */
    arra_dim1 = *mx;
    arra_offset = arra_dim1 + 1;
    arra -= arra_offset;

    filename[filename_len]='\0';
    printf("called makegrd with\n");
    printf("filename1: %s\n", filename);
    printf("filename2: %s\n", grdfile);

    printf("nx,ny: %d,%d\n",*nx,*ny);

/*
 * open, write and close output file
 */
    fptr=fopen(filename, "w");
    if (fptr == NULL) {
      fprintf(stderr,
      "ERROR %s (tf_makegrd): opening %s for output\n",
      strerror(errno), filename);
      exit(2);
    }

    /* Function Body */
    i__1 = *nx;
    for (i = 1; i <= i__1; ++i) {
/*<         do j=1,ny >*/
	i__2 = *ny;
        
        printf("i,j %d %d\n",i,j);
        value=arra[i+j*arra_dim1];
        fwrite((char *)&value, 
          sizeof(real), 1, fptr);
        
	for (j = 1; j <= i__2; ++j) {
/*<         enddo >*/
	}
/*<       enddo >*/
    }

    fclose(fptr);

/*
 * create grid file
 */

    printf("closed\n");

    sprintf(commandline, 
      "xyt2grd %s -G%s -R%f/%f/%f/%f -I%f/%f -Z -V -b -N0.",
      filename, grdfile, 
      xmin, (*xmin+(*nx-1) * *dx), *ymin, (*ymin+(*ny-1) * *dy), *dx, *dy);

    printf("NOTICE (tf_makegrd): command to execute:\n%s\n", commandline);

    system(commandline);

/*<       return >*/
    return 0;
/*<       end >*/
} /* makegrd_ */
 
/* ----- END OF tf_makegrd.c ----- */
