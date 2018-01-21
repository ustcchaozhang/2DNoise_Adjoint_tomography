/*! \file f77interface.cc
 * \brief interface functions (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 23/12/2002
 * 
 * interface functions (implementation)
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
 * \sa \ref page_fortran
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 23/12/2002   V1.0   Thomas Forbriger
 *  - 29/12/2002   V1.1   now uses aff::subarray
 *  - 03/01/2003   V1.2   (thof)
 *                        - use aff::util::SizeCheckedCast
 *                        - FortranArray now takes container type as template
 *                          argument
 * 
 * ============================================================================
 */
#define AFF_F77INTERFACE_CC_VERSION \
  "AFF_F77INTERFACE_CC   V1.2"

// include assertions
#include<aff/lib/error.h>
// include FortranArray stuff
#include<aff/fortranshape.h>
#include<aff/subarray.h>
#include<aff/shaper.h>
#include<aff/lib/checkedcast.h>

/*----------------------------------------------------------------------*/

// include interface prototypes
#include"f77proto.h"

/*----------------------------------------------------------------------*/

// get common block
#include"f77common_com.P"

/*----------------------------------------------------------------------*/

// f2c declarations
#include "f2c.h"

#ifdef __cplusplus
  extern "C" {
#endif

// include prototypes of Fortran subroutines
#include"f77procs.P"

//! essential definitions to satisfy linker
int MAIN__() 
{
  AFF_abort("should never be called!");
}

#ifdef __cplusplus
  }
#endif

/*======================================================================*/

namespace f77interface {

//! interface function to Fortran77 subroutine fill
int fill(const aff::Array<int>& a)
{
  aff::FortranArray<aff::Array<int> > fa(a);
  integer* pa=fa.castedpointer<integer>();
  integer n1=fa.last(0);
  integer n2=fa.last(1);
  integer n3=fa.last(2);
  integer l1=fa.dimlast(0);
  integer l2=fa.dimlast(1);
  integer l3=fa.dimlast(2);
  return(fill_(pa, &l1, &n1, &l2, &n2, &l3, &n3));
}

/*----------------------------------------------------------------------*/

//! fill common block through Fortran subroutine
int fillarray(const aff::Array<float>& v1,
              const aff::Array<float>& v2)
{
  aff::FortranArray<aff::Array<float> > fv1(v1),fv2(v2);
  real* p1=fv1.castedpointer<real>();
  real* p2=fv2.castedpointer<real>();
  integer n1=fv1.last(0);
  integer n2=fv2.last(0);
  return(fillarray_(p1, p2, &n1, &n2));
}

/*----------------------------------------------------------------------*/

//! read from common block through Fortran subroutine
Tcarray sums()
{
  typedef Tcarray::Tvalue Tcvalue;
  // prepare array that is large enough
  integer maxa,maxb;
  comdim_(&maxa, &maxb);
  Tcarray result(maxa);
  // prepare Fortran view
  aff::FortranArray<Tcarray> fa(result);
  complex* p=fa.castedpointer<complex>();
  integer size;
  sums_(p, &maxa, &size);
  return(aff::subarray(result)(size));
}

/*----------------------------------------------------------------------*/

//! create view from common
Tzarray viewcommon()
{
  typedef Tzarray::Tvalue Tzvalue;
  typedef aff::util::SizeCheckedCast<doublecomplex, Tzvalue> Tcast;
  integer maxa,maxb;
  comdim_(&maxa, &maxb);
  Tzvalue* p=Tcast::cast(f77common_.array);
  // create a shape
  aff::Strided shape(aff::Shaper(1,f77common_.na,maxa)(1,f77common_.nb,maxb));
  // create a representation
  aff::SharedHeap<Tzvalue> repr(p, shape.memory_size());
  // create array and return
  return Tzarray(shape, repr);
}

} // namespace f77interface

/* ----- END OF f77interface.cc ----- */
