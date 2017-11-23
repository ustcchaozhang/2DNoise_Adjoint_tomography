/*! \file f77test.cc
 * \brief test interfacing Fortran code (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 22/12/2002
 * 
 * test interfacing Fortran code (implementation)
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
 * Activate sections for testing illegal constructs by passing macros
 * ILLEGAL1, ILLEGAL2, ILLEGAL3, ILLEGAL4, ILLEGAL5 to the preprocessor. Use
 * option -DILLEGAL1 etc.
 *
 * \sa \ref page_fortran
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 22/12/2002   V1.0   Thomas Forbriger
 *  - 29/12/2002   V1.1   (thof)
 *                        - new uses new function aff::subarray
 *                        - new uses new function aff::slice
 *  - 03/01/2003   V1.2   (thof)
 *                        - test aff::util::SizeCheckedCast
 * 
 * ============================================================================
 */

/*! \example tests/f77test.cc
 *
 * Passing arrays to Fortran 77 code and retrieving array structures
 * from Fortran 77 common blocks.
 *
 * This test program gives an example of the usage of the following classes,
 * functions, and preprocessor macros:
 *   - aff::FortranArray
 *   - aff::FortranShape
 *   - aff::Array
 *   - aff::util::SizeCheckedCast
 *   - #DUMP
 *   - #CODE
 *
 * \sa tests/f77test.cc
 * \sa \ref page_fortran
 */

#define AFF_F77TEST_CC_VERSION \
  "AFF_F77TEST_CC   V1.2"

#include <aff/array.h>
#include <aff/fortranshape.h>
#include <aff/dump.h>
#include <aff/shaper.h>
#include <aff/subarray.h>
#include <aff/slice.h>
#include <aff/lib/checkedcast.h>
#include "f77proto.h"

using std::cout;
using std::endl;
using namespace aff;

/*----------------------------------------------------------------------*/

//! print headline
void section(const char* s, const char l='-')
{
  cout << endl << s << endl;
  const char* p=s;
  while (*p) { cout << l; ++p; }
  cout << endl;
}

/*======================================================================*/

/*! \brief test array interface to Fortran 77
 *
 * \sa \ref page_fortran
 */
int main()
{
  cout << AFF_F77TEST_CC_VERSION << endl;

  // First we test the shape class that should support passing arrays to
  // Fortran 77 functions
  section("FortranShape:", '=');
  {
    section("Full Layout");
    // we create a test array with known Fortran layout
    CODE(Strided strided(Shaper(1,10,20)(1,5,10)(1,30)(20)));
    // dump this shape
    DUMP(strided);
    // create Fortran shape from this (should be identical to known)
    CODE(aff::util::FortranShape fs1(strided));
    // and dump this
    DUMP(fs1.first());
    DUMP(fs1.last());
    DUMP(fs1.dimlast());
    cout << "fs1.offset(): " << fs1.offset() << endl;

    section("Sliced Subshape");
    // now take shape of a subarray
    CODE(Strided subshape(strided));
    CODE(subshape.shrink(0,2).shrink(1,3,5).shrink(3,10,20));
    CODE(subshape.collapse(2,15));
    // create Fortran shape from this
    CODE(aff::util::FortranShape fs2(subshape));
    // and dump this
    DUMP(fs2.first());
    DUMP(fs2.last());
    DUMP(fs2.dimlast());
    cout << "fs2.offset(): " << fs2.offset() << endl;
  }

  /*----------------------------------------------------------------------*/

  section("Pass array to Fortran via subroutine arguments:", '=');
  {
    // create an array and fill it
    CODE(Array<int> A(Shaper(-3,3)(9)(-1,1)));
    CODE(A=-55);
    // create a subarray view and fill this through Fortran
    CODE(Array<int> B=subarray(A)(-2,2)(3,7)(0));
    CODE(f77interface::fill(B));
    // dump the result
    CODE(dump_array(A));
    // do it again for a slice
    CODE(f77interface::fill(slice(A)()(2)));
    CODE(dump_array(A));
  }

  /*----------------------------------------------------------------------*/

  section("Access to common block:", '=');
  {
    // prepare to vectors to pass to fillarray
    CODE(Array<float> v1(5));
    CODE(Array<float> v2(3));
    CODE(for(int i=v1.f(0); i<=v1.l(0); i++) { v1(i)=2.*i; });
    CODE(for(int i=v2.f(0); i<=v2.l(0); i++) { v2(i)=.5*i; });
    // fill common block through Fortran 77 subroutine
    CODE(f77interface::fillarray(v1, v2));
    // get a view on the common block and dump it
    CODE(f77interface::Tzarray Z(f77interface::viewcommon()));
    CODE(dump_array(Z));
    // call Fortran subroutine sum and dump result
    CODE(dump_array(f77interface::sums()));
    CODE(typedef f77interface::Tzarray::Tvalue Tzvalue);
    // write directly to common block through a subarray
    CODE(subarray(Z)(2,4)=Tzvalue(-10.));
    // and dump the effect
    CODE(dump_array(Z));
    CODE(dump_array(f77interface::sums()));
  }

  /*----------------------------------------------------------------------*/

  section("Size-checked casts:", '=');
  {
    CODE(typedef std::complex<int> Ticvalue);
    CODE(typedef std::complex<float> Tcvalue);
    CODE(Array<Ticvalue> v1(1));
    CODE(ConstArray<Ticvalue> v2(v1));
    CODE(FortranArray<Array<Ticvalue> > fv1(v1));
    CODE(FortranArray<ConstArray<Ticvalue> > fv2(v2));
    CODE(v1(1)=Ticvalue(3,7));
    CODE(cout << v1(1) << ", " << v2(1) << endl);
    CODE(Ticvalue *icp=fv1.castedpointer<Ticvalue>());
    CODE(*icp=Ticvalue(35,60));
    CODE(cout << v1(1) << ", " << v2(1) << endl);
    CODE(const Ticvalue *cicp1=fv1.castedpointer<const Ticvalue>());
    CODE(const Ticvalue *cicp2=fv2.castedpointer<const Ticvalue>());
    CODE(cout << *cicp1 << ", " << *cicp2 << endl);
    section("That's dangerous:",' ');
    CODE(Tcvalue *cp=fv1.castedpointer<Tcvalue>());
    CODE(*cp=Ticvalue(35,60));
    CODE(cout << v1(1) << ", " << v2(1) << endl);
    CODE(double *dp=fv1.castedpointer<double>());
    CODE(*dp=35.e12);
    CODE(cout << v1(1) << ", " << v2(1) << endl);

    section("Test illegal usage (only if activated through macro-definition):",
            ' ');
    CODE(Array<int> iv1(1));
    CODE(ConstArray<int> iv2(iv1));
    CODE(FortranArray<Array<int> > fiv1(iv1));
    CODE(FortranArray<ConstArray<int> > fiv2(iv2));
    CODE(iv1(1)=50);
    CODE(cout << iv1(1) << ", " << iv2(1) << endl);
    CODE(int *iv1p=fiv1.pointer());
    CODE(const int *iv2p=fiv2.pointer());
    CODE(cout << *iv1p << ", " << *iv2p << endl);
#ifdef ILLEGAL1
#warning intentionally compiling illegal code:
#warning direct discard of const qualifier in conversion from non-const
    CODE(int *ip1=fiv1.castedpointer<const int>());
#endif
#ifdef ILLEGAL2
#warning intentionally compiling illegal code:
#warning direct discard of const qualifier in conversion from const array
    CODE(int *ip2=fiv2.castedpointer<const int>());
#endif
#ifdef ILLEGAL3
#warning intentionally compiling illegal code:
#warning discards const in conversion (reinterpret_cast)
    CODE(int *ip3=fiv2.castedpointer<int>());
#endif
#ifdef ILLEGAL4
#warning intentionally compiling illegal code:
#warning direct type mismatch
    CODE(float *ip4=fiv1.castedpointer<int>());
#endif
#ifdef ILLEGAL5
#warning intentionally compiling illegal code:
#warning wrong type size in conversion through reinterpret_cast
    CODE(double *ip5=fiv1.castedpointer<double>());
#endif
  }

} // main

/* ----- END OF f77test.cc ----- */
