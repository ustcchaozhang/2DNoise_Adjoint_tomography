/*! \file lmath.c
 * \brief this file is used to provide access to libm.a (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
 * 
 * $Id$
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
 * \author Thomas Forbriger
 * \date 23/02/2009
 * 
 * this file is used to serve some mathematical routines from C (implementation)
 * 
 * REVISIONS and CHANGES 
 *  -       1997   V1.0   Thomas Forbriger
 *  - 29/04/2000   V1.1   provide Neumann functions too
 *  - 23/02/2009   V1.2   to be used on 64 bit system
 * 
 * ============================================================================
 */
#define TF_LMATH_C_VERSION \
  "TF_LMATH_C   V1.0   "
#define TF_LMATH_C_CVSID \
  "$Id$"

#include <math.h>

/*
 * all f2c stuff that is needed here
 * =================================
 */

#ifndef F2C_INCLUDE
/* FORTRAN (f2c) types needed by the wrapper functions */

/* g77 on 64bit system apparently doesn't use long int */
#ifdef __x86_64
typedef int integer;
typedef double doublereal;
typedef int logical;
typedef int ftnlen;
#else
typedef long int integer;
typedef double doublereal;
typedef long int logical;
typedef long int ftnlen;
#endif
#else
#warning f2c.h is read from somewhere else!
#endif

doublereal d_j0__(x)
doublereal *x;
{
  doublereal retval;
  retval=(doublereal)j0((double)*x);
  return retval;
}

doublereal d_j1__(x)
doublereal *x;
{
  doublereal retval;
  retval=(doublereal)j1((double)*x);
  return retval;
}

doublereal d_jn__(n, x)
integer *n;
doublereal *x;
{
  doublereal retval;
  retval=(doublereal)jn((int)*n,(double)*x);
  return retval;
}

doublereal d_y0__(x)
doublereal *x;
{
  doublereal retval;
  retval=(doublereal)y0((double)*x);
  return retval;
}

doublereal d_y1__(x)
doublereal *x;
{
  doublereal retval;
  retval=(doublereal)y1((double)*x);
  return retval;
}

doublereal d_yn__(n, x)
integer *n;
doublereal *x;
{
  doublereal retval;
  retval=(doublereal)yn((int)*n,(double)*x);
  return retval;
}

/* ----- END OF lmath.c ----- */
