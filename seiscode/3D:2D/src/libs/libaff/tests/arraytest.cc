/*! \file arraytest.cc
 * \brief test array class template (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 09/06/2002
 * 
 * test array class template (implementation)
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
 *  - 09/06/2002   V1.0   Thomas Forbriger
 *  - 25/11/2002   V1.1   whole array assign
 *  - 19/12/2002   V1.2   now works for libaff
 *  - 28/12/2002   V1.3   (thof)
 *                        - new term for containers of const elements
 *  - 03/01/2003   V1.4   (thof)
 *                        - now tests copyout function
 * 
 * ============================================================================
 */

/*! \example tests/arraytest.cc
 *
 * Here you can learn about how to use the aff::Array class.
 *
 * This test program gives an example of the usage of the following classes,
 * functions, and preprocessor macros:
 *   - aff::Array
 *   - aff::ConstArray
 *   - aff::Shaper
 *   - aff::dump
 *   - aff::dump_array
 *   - #DUMP
 *   - #CODE
 *
 * \sa tests/arraytest.cc
 */

#define AFF_ARRAYTEST_CC_VERSION \
  "AFF_ARRAYTEST_CC   V1.3"

#define CONT_INDEXCHECK

#include <aff/dump.h>
#include <aff/array.h>
#include <aff/shaper.h>
#include <aff/subarray.h>

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

//! test array functionality
int main()
{
  cout << AFF_ARRAYTEST_CC_VERSION << endl;

  section("Test constructors", '=');
  section("size-constructor");
  {
    CODE( Array<float> A(3,4); )
    DUMP( A );
  }
  section("shape-constructor");
  {
    CODE( Array<float> A(Shaper(4,14)(12)(-5,5)); )
    DUMP( A );
  }

  /*----------------------------------------------------------------------*/

  section("Test assignment", '=');
  {
    CODE( Array<float> A(3,4); );
    CODE( A=15.5; ); 
    CODE( dump_array(A); );
    CODE( A(2,3)=-2.; ); 
    CODE( A(3,1)=-5.; ); 
    CODE( dump_array(A); );
  }

  /*----------------------------------------------------------------------*/

  section("Test access operators", '=');
  {
    CODE( Array<int> A(Shaper(-2,2)(10,16)(2)(-1,0)); );
    section("fill array");
    for(int i=A.f(0); i<=A.l(0); i++)
    {
      for(int j=A.f(1); j<=A.l(1); j++)
      {
        for(int k=A.f(2); k<=A.l(2); k++)
        {
          for(int l=A.f(3); l<=A.l(3); l++)
          {
            A(i,j,k,l)=(i-A.f(0)+1)+(j-A.f(1)+1)*10
              +(k-A.f(2)+1)*100+(l-A.f(3)+1)*1000;
          }
        }
      }
    }
    CODE( dump_array(A,3); );
    CODE( dump_array(A,2); );
    CODE( dump_array(A,1); );
    CODE( dump_array(A,0); );
    DUMP( A.representation() );
  }

  /*----------------------------------------------------------------------*/

  section("Test copy", '=');
  section("non-const copy");
  {
    CODE( Array<float> A(3,4); );
    CODE( Array<float> B(A); );
    CODE( Array<float> C; );
    CODE( C=B; );
    CODE( A=15.5; ); 
    CODE( A(2,3)=-2.; ); 
    CODE( A(3,1)=-5.; ); 
    CODE( dump_array(C); );
  }
  section("const copy");
  {
    CODE( Array<float> A(3,4); );
    CODE( A=15.5; ); 
    CODE( ConstArray<float> B(A); );
    CODE( dump_array(B); );
    CODE( A(2,3)=-2.; ); 
    CODE( A(3,1)=-5.; ); 
    CODE( Array<float>::Trepresentation H; );
    CODE( H=A.representation(); );
    CODE( H[5]=-12.; );
    CODE( dump_array(B); );
    CODE( ConstArray<float>::Trepresentation H2; );
    CODE( H2=B.representation(); );
    DUMP( H2 );

#ifdef ILLEGAL1
#warning compiling supposedly illegal code
    CODE( Array<float> C(B); );
#endif

#ifdef ILLEGAL2
#warning compiling supposedly illegal code
    CODE( Array<float> C; );
    CODE( C=B; );
#endif

#ifdef ILLEGAL3
#warning compiling supposedly illegal code
    CODE( B(1,2)=12.; );
#endif

#ifdef ILLEGAL4
#warning compiling supposedly illegal code
    CODE( B=12.; );
#endif

#ifdef ILLEGAL5
#warning compiling supposedly illegal code
    CODE( H=B; );
#endif

#ifdef ILLEGAL6
#warning compiling supposedly illegal code
    CODE( H2[5]=-12.; );
#endif
  }

  section("read access to ConstArray and const Array");
  {
    CODE( Array<int> A(3,3) );
    CODE( ConstArray<int> B(A) );
    CODE( const Array<int> C(A) );
    CODE( A=123 );
    CODE( cout << A(2,2) << ", " << B(2,2) << ", " << C(2,2) << endl );

#ifdef ILLEGAL7
#warning compiling supposedly illegal code
    CODE( C(1,1)=12; );
#endif
  }

  section("test copyout function");
  {
    CODE( Array<int> A(8,8));
    section("fill array");
    for(int i=A.f(0); i<=A.l(0); i++)
    {
      for(int j=A.f(1); j<=A.l(1); j++)
      {
            A(i,j)=(i-A.f(0)+1)+(j-A.f(1)+1)*10;
      }
    }
    CODE( dump_array(A); );
    CODE( ConstArray<int> CA(subarray(A)(4,6)(3,5)));
    CODE( dump_array(CA); );
    CODE( Array<int> B=CA.copyout());
    CODE( dump_array(B); );
  }
}

/* ----- END OF arraytest.cc ----- */
