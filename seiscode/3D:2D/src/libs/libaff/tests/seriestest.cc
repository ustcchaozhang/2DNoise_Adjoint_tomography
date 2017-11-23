/*! \file seriestest.cc
 * \brief test aff::Series class (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/12/2002
 * 
 * test aff::Series class (implementation)
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
 *  - 17/12/2002   V1.0   Thomas Forbriger
 *  - 28/12/2002   V1.1   (thof)
 *                        - new term for containers of const elements
 *  - 19/12/2003   V1.2   (thof)
 *                        - working on series container
 * 
 * ============================================================================
 */
#define AFF_SERIESTEST_CC_VERSION \
  "AFF_SERIESTEST_CC   V1.2"

#include <aff/series.h>
#include <aff/dump.h>

using std::cout;
using std::endl;

/*! \example tests/seriestest.cc
 *
 * Test series class aff::Series.
 *
 * \sa tests/seriestest.cc
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
  cout << AFF_SERIESTEST_CC_VERSION << endl;

  section("array dimensions");
  CODE( const int N=5 );
  CODE( const int N2=-5 );

  section("create a series container");
  CODE( aff::Series<int> A );
  CODE( aff::Series<int> B(N) );
  CODE( aff::Series<int> C(N2,N) );
  DUMP( A );
  DUMP( B );
  DUMP( C );
  CODE( A=B );
  DUMP( A );
  CODE( aff::ConstSeries<int> D(C) );
  CODE( for (int i=C.first(); i<=C.last(); i++) { C(i)=i; } );
  DUMP( D );
  CODE( C.setlastindex(N-2); );
  DUMP( C );
#ifdef ILLEGAL1
  CODE( D(6)=10 );
#endif
  CODE( D.shift(300) );
  DUMP( D );
}

/* ----- END OF seriestest.cc ----- */
