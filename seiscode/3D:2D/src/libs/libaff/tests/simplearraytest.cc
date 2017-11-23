/*! \file simplearraytest.cc
 * \brief test simple array class (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 25/05/2002
 * 
 * test simple array class (implementation)
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
 *  - 24/11/2002   V1.1   add rawarfun tests
 *  - 25/11/2002   V1.2   new inline tests
 *  - 13/12/2002   V1.3   reworked for libaff
 *  - 10/12/2007   V1.4   compiles and is tested on Fenoglio (64bit)
 * 
 * ============================================================================
 */
#define AFF_SIMPLEARRAYTEST_CC_VERSION \
  "AFF_SIMPLEARRAYTEST_CC   V1.4"

/*! \example tests/simplearraytest.cc
 *
 * Tests for aff::SimpleRidigArray and aff::util::Inline.
 *
 * This test program gives an example of the usage of the following classes,
 * functions, and preprocessor macros:
 *   - aff::SimpleRigidArray
 *   - aff::util::Inline
 *   - aff::dump
 *   - #DUMP
 *   - #CODE
 *
 * \sa tests/simplearraytest.cc
 */

#include<iostream>
#include<aff/array.h>
#include<aff/dump.h>

using std::cout;
using std::endl;

using namespace aff;

//! print result of function
#define FUNC( func ) cout << #func << "=" << func << endl

//! return string for bool value
const char* boolchar(const bool& v)
{
  char *s;
  if (v) { s="true"; } else { s="false"; }
  return(s);
}

//! print headline
void section(const char* s)
{
  cout << endl
       << s << endl;
  const char* p=s;
  while (*p) {
    cout << "-";
    ++p;
  }
  cout << endl;
}

//! Test for the SimpleRigidArray module and associated functions
int main()
{
  cout << AFF_SIMPLEARRAYTEST_CC_VERSION << endl;

  // test for basic functionality of SimpleRigidArray
  {
    cout << "Test SimpleRigidArray" << endl
         << "=====================" << endl;

    section("constructors");

    SimpleRigidArray<double, 6> A,B(8.);
    SimpleRigidArray<int, 4> C,D(100);

    cout << "SimpleRigidArray<double, 6> A,B(8.);" << endl;
    cout << "SimpleRigidArray<int, 4> C,D(100);" << endl;

    DUMP( A );
    DUMP( B );
    DUMP( C );
    DUMP( D );

    const SimpleRigidArray<int, 4> E(D);
    cout << "const SimpleRigidArray<int, 4> E(D);" << endl;

    SimpleRigidArray<float, 4> F(D);
    cout << "SimpleRigidArray<float, 4> F(D);" << endl;

    DUMP( E );
    DUMP( F );

    section("assignment");

    CODE( A=B; )
    CODE( C=D; )

    CODE( B[3]=78.5; )
    CODE( D[2]=7883; )
    CODE( C[2]=int(-13.2); )

    CODE( F=C; )
    CODE( F[1]=-13.2; )
   
    DUMP( A );
    DUMP( B );
    DUMP( C );
    DUMP( D );
    DUMP( F );

    CODE( D=777; )

    DUMP( D );

// the following is illegal, since E is declared with a const qualifier
#ifdef ILLEGAL1
    CODE( E[4]=334 );
#endif

// the following is illegal, since E is declared with a const qualifier
#ifdef ILLEGAL2
    CODE( E=A );
#endif

  }

  cout << endl;

  {
    cout << "Test raw array functions" << endl
         << "========================" << endl << endl;

    SimpleRigidArray<int, 4> A;
    cout << "SimpleRigidArray<int, 4> A;" << endl;
    SimpleRigidArray<int, 4> B;
    cout << "SimpleRigidArray<int, 4> B;" << endl;
    SimpleRigidArray<float, 4> C;
    cout << "SimpleRigidArray<float, 4> C;" << endl;

    CODE( A[0]=2; A[1]=3; A[2]=4; A[3]=5; )
    CODE( B[0]=1; B[1]=3; B[2]=5; B[3]=7; )
    CODE( C[0]=.1; C[1]=.3; C[2]=.5; C[3]=.7; )

    DUMP(A);
    DUMP(B);
    DUMP(C);

    section("reduction to scalar");
    {
      FUNC( inline_sum(A) );
      FUNC( inline_product(A) );
      FUNC( inline_innerproduct(B,A) );
      FUNC( inline_innerproduct(C,A) );
      FUNC( inline_strideproduct(A,B) );
    }

    section("comparison");
    { 
      CODE( B=A; )
      DUMP(B);
      cout << "anysmaller A<B: " 
        << boolchar(inline_anysmaller(A, B));
      cout << "  anylarger A>B: " 
        << boolchar(inline_anylarger(A, B));
      cout << endl;
      B[2]++; DUMP(B);
      cout << "anysmaller A<B: " 
        << boolchar(inline_anysmaller(A, B));
      cout << "  anylarger A>B: " 
        << boolchar(inline_anylarger(A, B));
      cout << endl;
      B[1]--; DUMP(B);
      cout << "anysmaller A<B: " 
        << boolchar(inline_anysmaller(A, B));
      cout << "  anylarger A>B: " 
        << boolchar(inline_anylarger(A, B));
      cout << endl;
      B[2]--; DUMP(B);
      cout << "anysmaller A<B: " 
        << boolchar(inline_anysmaller(A, B));
      cout << "  anylarger A>B: " 
        << boolchar(inline_anylarger(A, B));
      cout << endl;
    }
  }
}

/* ----- END OF simplearraytest.cc ----- */
