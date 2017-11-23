/* this is <tf_random.c>
 * ----------------------------------------------------------------------------
 *
 * Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
 *
 * implement C-stdlib randomize functions into f2c fortran
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
 *    11/11/99   V1.1   include stdlib.h and added function tsrand_()
 *    03/06/12   V1.2   interface appears broken; issue warning
 *    20/06/12   V1.3   with -ff2c gfortran uses doublereal return values for
 *                      real functions
 *
 * ============================================================================
 */

#include <stdio.h> 
#include <stdlib.h> 
#ifndef RAND_MAX
#include <math.h>
#define RAND_MAX (pow(2.,31.)-1.)
#endif
#include <f2c.h>
#include <time.h>

/*
 * get a random number
 */


doublereal tf_rand__()
{
  double randval, randmax;
  doublereal retval;
  randmax=(double)RAND_MAX;
  randval=(double)rand();
  retval=randval/randmax;
  return retval;
}

/*
 * initialize a sequence of random numbers
 */

int tf_srand__(seed)
integer *seed;
{
  unsigned int cseed;
  cseed=*seed;
  srand(cseed);
  return 0;
}

/*
 * initialize a sequence of random numbers with the ctime value
 */
int tf_tsrand__()
{
  unsigned int cseed;
  time_t mytime;
  time(&mytime);
  cseed=(unsigned int)mytime;
  srand(cseed);
  return 0;
}

/* ----- END OF tf_random.c ----- */
