/*! \file shapetest.cc
 * \brief test shape classes (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 25/05/2002
 * 
 * test shape classes (implementation)
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
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 25/05/2002   V1.0   Thomas Forbriger
 *  - 18/12/2002   V1.1   (thof)
 *                        - started test of Strided (aff)
 *                        - test works
 *  - 19/12/2002   V1.2   (thof)
 *                        - print valid() of stepper too
 *  - 09/01/2003   V1.3   (thof)
 *                        - using too many bracket operators now triggers 
 *                          an exception in aff::Shaper
 * 
 * ============================================================================
 */
#define AFF_SHAPETEST_CC_VERSION \
  "AFF_SHAPETEST_CC   V1.3"

#include<iostream>
#include<aff/dump.h>
#include<aff/shaper.h>
#include<aff/lib/stridedstepper.h>

using std::cout;
using std::endl;
using namespace aff;

/*! \example tests/shapetest.cc
 *
 * Test shape aff::Strided, the shaper class aff::Shaper,
 * and the helper class aff::util::StridedStepper
 *
 * This test program gives an example of the usage of the following classes,
 * functions, and preprocessor macros:
 *   - aff::Strided
 *   - aff::StridedStepper
 *   - aff::Shaper
 *   - aff::dump
 *   - aff::dump_map
 *   - #DUMP
 *   - #CODE
 *
 * \sa tests/shapetest.cc
 */

/*----------------------------------------------------------------------*/

//! print headline
void section(const char* s, const char l='-')
{
  cout << endl << s << endl;
  const char* p=s;
  while (*p) { cout << l; ++p; }
  cout << endl;
}

/*----------------------------------------------------------------------*/

//! print value of a bool
const char* bvalue(const bool& b)
{ if (b) return("true"); return("false"); }

/*----------------------------------------------------------------------*/

//! print value of a bool
#define BOOLIS( S ) cout << " *** " << #S << ": " << bvalue(S);

/*----------------------------------------------------------------------*/

//! cycle steppers
void steppers(const Strided& shape, const int& nmax=30)
{
  StridedStepper s1(shape), s2(shape);
  s2.tolast();
  for (int i=0; i<nmax; i++)
  {
    cout.width(3); cout << i+1;
    cout << "    ";
    cout.width(5); cout << s1.current() << "=[";
    for (int j=0; j<Strided::Mmax_dimen; j++)
    {
      if (j) cout << ", ";
      cout.width(2); cout << s1.index(j);
    }
    cout << "] "; 
    cout.width(5); cout << bvalue(s1.more());
    cout.width(6); cout << bvalue(s1.valid());
    cout << "   ";
    cout.width(5); cout << s2.current() << "=[";
    for (int j=0; j<Strided::Mmax_dimen; j++)
    {
      if (j) cout << ", ";
      cout.width(2); cout << s2.index(j);
    }
    cout << "] "; 
    cout.width(5); cout << bvalue(s2.less());
    cout.width(6); cout << bvalue(s2.valid());
    cout << endl;
    s1.incr();
    s2.decr();
  }
}

/*======================================================================*/

//! test shape class
int main()
{
  cout << AFF_SHAPETEST_CC_VERSION << endl;
  
  section("Test shape class Strided", '=');

  section("Test shaper class Shaper");

  cout << endl << "Basic usage:" << endl;
  DUMP( Shaper(10)(12) );

#ifdef ILLEGAL1
#warning intentionally compiling illegal code
  cout << endl << "Illegal with limited dimensionality:" << endl;
  DUMP( Shaper(2,10)(-12,12)(9)(2,7)(3,4) );
#endif

  cout << endl << "Usage for an external Fortran shape:" << endl;
  DUMP( Shaper(1,6,10)(1,12,30)(1,10) );

  section("Test subarrays and slicing");

  DUMP( Shaper(10)(10)(10) );

  cout << endl;
  DUMP( Strided(Shaper(10)(10)(10)).shrink(0,2,5) );

  cout << endl;
  DUMP( Strided(Shaper(10)(10)(10)).shrink(2,5) );

  cout << endl;
  DUMP( Strided(Shaper(10)(15)(20)).collapse(1,5) );

  cout << endl;
  DUMP( Strided(Shaper(11,20)(11,20)(11,20)).shift(1,5) );

  cout << endl;
  DUMP( Strided(Shaper(11,20)(11,20)(11,20)).setfirst(1,5) );

  section("Test stepper");

  {
    cout << endl << "Basic functionality" << endl;
    CODE( Strided shape(Shaper(3)(4)(2)(3)); )
    DUMP( shape );
    CODE( steppers(shape, shape.size()+10); )
  }

  {
    cout << endl << "Shape with gaps" << endl;
    CODE( Strided shape(Shaper(0,9)(0,9)(0,9)(0,9)); )
    DUMP( shape.shrink(0,3,5).shrink(1,4,6).shrink(2,5,7).shrink(3,6,8) );
    CODE( steppers(shape, shape.size()+10); )
  }

  {
    cout << endl << "Small array" << endl;
    CODE( Strided shape(Shaper(3,5,10)(3)); )
    DUMP( shape );
    CODE( steppers(shape, shape.size()+10); )
  }

  section("Test offset functions");
  {
    CODE( Strided shape(Shaper(5)(10)(10)(10)); )
    CODE( shape.shrink(0,2,4).shrink(1,3,7).shrink(2,5,6).shrink(3,2,3); )
    CODE( dump_map(shape, 3); )
    CODE( dump_map(shape, 2); )
    CODE( dump_map(shape, 1); )
    CODE( dump_map(shape, 0); )
    CODE( steppers(shape, shape.size()); )
    CODE( Strided copy(shape); )
    CODE( copy.collapse(2,6).collapse(2,3).shift(0,1000).setfirst(1,50); )
    CODE( dump_map(copy); )
    CODE( copy=shape; )
    CODE( copy.collapse(1,7).collapse(2,3).shift(0,1000).setfirst(1,50); )
    CODE( dump_map(copy); )
  }
}

/* ----- END OF shapetest.cc ----- */
