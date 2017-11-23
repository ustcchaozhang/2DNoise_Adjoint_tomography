/*! \file hankel.cc
 * \brief Hankel function (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/04/2013
 * 
 * Hankel function (implementation)
 * 
 * Copyright (c) 2013 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * lisousi is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version. 
 * 
 * lisousi is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 * ----
 *
 * 
 * REVISIONS and CHANGES 
 *  - 17/04/2013   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_HANKEL_CC_VERSION \
  "TF_HANKEL_CC   V1.0"

#include "lisousi.h"
#include "functions.h"

TFourier::Tcoeff hankel(const double& arg)
{
  TFourier::Tcoeff retval;
  if (arg < 1.e-30)
  {
    retval=0.;
  }
  else
  {
    retval=gsl_sf_bessel_J0(arg)-IME*gsl_sf_bessel_Y0(arg);
  }
  return(retval);
}

/* ----- END OF hankel.cc ----- */
