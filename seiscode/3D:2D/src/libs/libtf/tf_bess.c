/* this is <tf_bess.c>
 * ----------------------------------------------------------------------------
 *
 * Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
 *
 * provide f2c Fortran code with libmath intrinsic bessel function
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
 *    25/06/97   V1.0   Thomas Forbriger
 *
 * ============================================================================
 */

#include "f2c.h"
#include "math.h"

/*
 * J0, J1 and Jn
 */

doublereal tf_dj0__(x)
doublereal *x;
{
  doublereal retval;
  retval=(doublereal)j0((double)*x);
  return retval;
}

doublereal tf_dj1__(x)
doublereal *x;
{
  doublereal retval;
  retval=(doublereal)j1((double)*x);
  return retval;
}

doublereal tf_djn__(n, x)
integer *n;
doublereal *x;
{
  doublereal retval;
  retval=(doublereal)jn((int)*n,(double)*x);
  return retval;
}

/*
 * single precision
 */

real tf_j0__(x)
real *x;
{
  doublereal retval;
  retval=(real)j0((real)*x);
  return retval;
}

real tf_j1__(x)
real *x;
{
  doublereal retval;
  retval=(real)j1((real)*x);
  return retval;
}

real tf_jn__(n, x)
integer *n;
real *x;
{
  doublereal retval;
  retval=(real)jn((int)*n,(real)*x);
  return retval;
}

/*
 * Y0, Y1 and Yn
 */

doublereal tf_dy0__(x)
doublereal *x;
{
  doublereal retval;
  retval=(doublereal)y0((double)*x);
  return retval;
}

doublereal tf_dy1__(x)
doublereal *x;
{
  doublereal retval;
  retval=(doublereal)y1((double)*x);
  return retval;
}

doublereal tf_dyn__(n, x)
integer *n;
doublereal *x;
{
  doublereal retval;
  retval=(doublereal)yn((int)*n,(double)*x);
  return retval;
}

/*
 * single precision
 */

real tf_y0__(x)
real *x;
{
  doublereal retval;
  retval=(real)y0((real)*x);
  return retval;
}

real tf_y1__(x)
real *x;
{
  doublereal retval;
  retval=(real)y1((real)*x);
  return retval;
}

real tf_yn__(n, x)
integer *n;
real *x;
{
  doublereal retval;
  retval=(real)yn((int)*n,(real)*x);
  return retval;
}

/* ----- END OF tf_bess.c ----- */
