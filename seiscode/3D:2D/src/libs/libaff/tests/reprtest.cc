/*! \file reprtest.cc
 * \brief test representation classes (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 10/05/2002
 * 
 * test representation classes (implementation)
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
 * \todo
 * Test ALL constructors etc intensively!
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 10/05/2002   V1.0   Thomas Forbriger
 *  - 16/12/2002   V1.1   (thof)
 *                        - compiles and works
 *  - 28/12/2002   V1.2   (thof)
 *                        - new term for containers of const elements
 *  - 31/12/2002   V1.3   (thof)
 *                        - initializing an aff::ConstSharedHeap to a defined
 *                          size is a useless exercise
 * 
 * ============================================================================
 */
#define AFF_REPRTEST_CC_VERSION \
  "AFF_REPRTEST_CC   V1.3"

#include <aff/lib/sharedheap.h>
#include <aff/dump.h>

using std::cout;
using std::endl;

/*! \example tests/reprtest.cc
 *
 * Test representation class aff::SharedHeap.
 *
 * This test program gives an example of the usage of the following classes
 * and preprocessor macros:
 *   - aff::SharedHeap
 *   - aff::ConstSharedHeap
 *   - #DUMP
 *   - #CODE
 *
 * \sa tests/reprtest.cc
 */

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

//! testing SharedHeap
int main()
{
  cout << AFF_REPRTEST_CC_VERSION << endl;

  section("array dimensions");
  CODE( const int N=10; )
  CODE( const int N2=23; )

  section("instantiate arrays");
  CODE( aff::SharedHeap<int> A(N); )
  CODE( aff::SharedHeap<int> B(N2); )
#ifdef SENSELESS
#warning intentionally compiling senseless code
  CODE( aff::ConstSharedHeap<int> Cuseless(N2); )
#endif
  CODE( aff::ConstSharedHeap<int> C; )
  CODE( aff::SharedHeap<float> D(N2); )

  section("create a const reference to D");
  CODE( aff::SharedHeap<float>::Tcoc E(D); )

  section("const int array should share memory with int array");
  CODE( C=B; )

  section("fill arrays");
  CODE( for (int i=0; i<N; i++) { A[i]=(i+1)*10; } )
  CODE( for (int i=0; i<N2; i++) { B[i]=(i+4)*3; } )
  CODE( for (int i=0; i<N2; i++) { D[i]=(i+2.45)*2; } )

  section("dump contents - C is filled through B and E is filled through D");
  DUMP( aff::SharedHeap<int>::Tcontainer_of_const(A) );
  DUMP( B );
  DUMP( C );
  DUMP( E );

  section("modify C through B");
  CODE( B[8]=632; )
  DUMP( C );

#ifdef ILLEGAL1
  // you may activate this section by passing -DILLEGAL1 to the compiler
  section("modify C directly - is illegal");
  C[9]=354;
  DUMP( C );
#endif

  section("make B a reference to A");
  CODE( B=A; )
  section("modification should be visible through B");
  CODE( A[5]=453; )
  DUMP( B );
  section("but C holds the original values");
  DUMP( C );
}

/* ----- END OF reprtest.cc ----- */
