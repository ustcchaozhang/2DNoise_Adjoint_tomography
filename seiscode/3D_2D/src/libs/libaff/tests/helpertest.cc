/*! \file helpertest.cc
 * \brief test helpers like iterator, subarray and slice (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 23/12/2002
 * 
 * test helpers like iterator, subarray and slice (implementation)
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
 *  - 23/12/2002   V1.0   Thomas Forbriger
 *  - 28/12/2002   V1.1   (thof)
 *                        - new term for containers of const elements
 *  - 29/12/2002   V1.2   (thof)
 *                        - new subarray function
 *                        - test subarray of ConstArray
 *                        - new slice function
 *                        - test slice of ConstArray
 *                        - now no resolution conflicts with const containers
 *                          are experienced anymore
 *  - 10/12/2007   V1.3   (thof)
 *                        - scanning a const array requires a Browser
 *  - 15/05/2011   V1.4   (thof)
 *                        - test aff::CArray
 *                        - test helpers in converters.h
 * 
 * ============================================================================
 */
#define AFF_HELPERTEST_CC_VERSION \
  "AFF_HELPERTEST_CC   V1.4"

/*! \example tests/helpertest.cc
 *
 * Test helper classes.
 *
 * This test program gives an example of the usage of the following classes,
 * functions, and preprocessor macros:
 *   - \ref group_array_extensions
 *   - \ref group_series_extensions
 *   - aff::CArray
 *   - aff::Iterator
 *   - aff::util::Subarray
 *   - aff::subarray
 *   - aff::utl::Slice
 *   - aff::slice
 *   - aff::Array
 *   - aff::ConstArray
 *   - aff::Shaper
 *   - aff::dump
 *   - aff::dump_array
 *   - #DUMP
 *   - #CODE
 *
 * \sa tests/helpertest.cc
 */

#include <aff/array.h>
#include <aff/dump.h>
#include <aff/shaper.h>
#include <aff/iterator.h>
#include <aff/subarray.h>
#include <aff/slice.h>
#include <aff/Carray.h>
#include <aff/converters.h>

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

/*----------------------------------------------------------------------*/

//! test for passing subarrays and slices
template<class T>
void printarray(const ConstArray<T>& array)
{
  cout << endl;
  cout << "Received array in function:" << endl;
  dump_array(array);
}

/*======================================================================*/

//! test helper classes
int main()
{
  cout << AFF_HELPERTEST_CC_VERSION << endl;

  section("Preparing test array");
  CODE( Array<int> A(Shaper(-2,2)(-3,3)) );
  CODE( dump_array(A) );

  section("Tests for class Iterator", '=');
  {
    CODE(int i=0);
    CODE(for( Iterator<Array<int> > I(A); I.valid(); ++I) 
         { *I=i; i++; });
    CODE( dump_array(A) );
    CODE( ConstArray<int> CA(A.copyout()) );
    CODE( i=1);
    // use a browser here
    CODE(for( Browser<ConstArray<int> > I(CA); I.valid(); ++I) 
         { cout << i << ", " << *I << endl; i++; });
  }

  CODE( ConstArray<int> OrigA(A.copyout()) );
  
  /*----------------------------------------------------------------------*/

  section("Tests for class Subarray", '=');
  {
    section("First mode: Use a Subarray object just for initialization",'.');
    CODE(Array<int> B=aff::util::Subarray<Array<int> >(A)(-1,1)(-2,2));
    CODE( dump_array(B) );
    CODE(B=-5);
    CODE( dump_array(A) );
    // you must explicitely create an array through member function array()
    CODE(printarray(aff::util::Subarray<Array<int> >(A)(-2,0)(-3,0).array()));
  }
  
  section("Tests of subarray function", '-');
  {
    CODE( A.copyin(OrigA) );
    section("First mode: Use a Subarray object just for initialization",'.');
    CODE(Array<int> B=subarray(A)(-1,1)(-2,2));
    CODE( dump_array(B) );
    CODE(B=-5);
    CODE( dump_array(A) );
    section("Second mode: Create a Subarray object to which you can assign",
            '.');
    CODE(subarray(A)()(1,2)=100);
    CODE(subarray(A)(1,1)=-200);
    CODE( dump_array(A) );
    // you must explicitely create an array through member function array()
    CODE(printarray(subarray(A)(-2,0)(-3,0).array()));
  }

  section("Test for subarray of ConstArray",'-');
  {
    CODE( dump_array(OrigA) );
    CODE(ConstArray<int> B=subarray(OrigA)(-1,1)(-2,2));
    CODE( dump_array(B) );
    // you must explicitely create an array through member function array()
    CODE(printarray(subarray(OrigA)(-2,0)(-3,0).array()));
  }
  
  /*----------------------------------------------------------------------*/

  section("Tests for class Slice", '=');
  {
    CODE( A.copyin(OrigA) );
    section("First mode: Use a Slice object just for initialization",'.');
    CODE(Array<int> B=aff::util::Slice<Array<int> >(A)(1,-1));
    CODE( dump_array(B) );
    CODE(B=555);
    CODE( dump_array(A) );
    // you must explicitely create an array through member function array()
    CODE(printarray(aff::util::Slice<Array<int> >(A)(-2).array()));
  }

  section("Test slice function", '-');
  {
    CODE( A.copyin(OrigA) );
    section("First mode: Use a Slice object just for initialization",'.');
    CODE(Array<int> B=slice(A)(1,-1));
    CODE( dump_array(B) );
    CODE(B=555);
    CODE( dump_array(A) );
    section("Second mode: Create a Slice object to which you can assign",
            '.');
    CODE(slice(A)()(1)=666);
    CODE(slice(A)(0)=-777);
    CODE( dump_array(A) );
    // you must explicitely create an array through member function array()
    CODE(printarray(slice(A)(-2).array()));
  }

  section("Test slice of ConstArray",'-');
  {
    CODE( dump_array(OrigA) );
    CODE(ConstArray<int> B=slice(OrigA)(1,-1));
    CODE( dump_array(B) );
    // you must explicitely create an array through member function array()
    CODE(printarray(slice(OrigA)(-2).array()));
  }

  /*----------------------------------------------------------------------*/

  section("Mixed tests", '=');

  section("Iterator on a Subarray");
  {
    CODE(int i=501);
    CODE(Array<int> B=subarray(A)(-1,1)(-2,2));
    CODE(for( Iterator<Array<int> > I(B); I.valid(); ++I) 
         { *I=i; i++; });
    CODE( dump_array(A) );
  }

  section("Iterator on a Slice");
  {
    CODE(int i=-801);
    CODE(Array<int> B=slice(A)(1));
    CODE(for( Iterator<Array<int> > I(B); I.valid(); ++I) 
         { *I=i; i++; });
    CODE( dump_array(A) );
  }

  section("Extract copy of subarray");
  {
    CODE(Array<int> B=subarray(A)(-1,1)(-1,1));
    CODE(dump_array(B));
    CODE(Array<int> C=B.copyout());
    CODE(dump_array(C));
  }

  /*----------------------------------------------------------------------*/

  section("Test external access interfaces", '=');

  section("Test interface to raw memory array: aff::CArray");
  {
    CODE( Array<int> A(Shaper(2)(10,16)(-2,2)(-1,0)); );
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
    CODE( Array<int> B=aff::slice(A)(0,2)(2,-1); )
    CODE( printarray(B) );
    CODE( CArray<int> C(B); )
    section("fill C array");
    CODE( int* p=C.pointer(); )
    CODE( 
    for (unsigned i=0; i<C.size(0); ++i)
    {
      for (unsigned j=0; j<C.size(1); ++j)
      {
        p[i*C.stride(0)+j*C.stride(1)]=i+10*j;
      }
    }
    )
    CODE( printarray(B) );
    CODE( printarray(A) );
  }

  /*----------------------------------------------------------------------*/

  section("Test interface to raw memory array: aff::CSeries");
  {
    CODE( aff::Series<int> A(-3,4); )
    CODE( for (int i=A.f(); i<=A.l(); ++i) { A(i)=i; })
    CODE( dump(A); )
    CODE( aff::CSeries<int> CS(A); )
    CODE( int* p=CS.pointer(); )
    CODE( for (int i=0; i<CS.size(); ++i) { p[i]=i; })
    CODE( dump(A); )
    CODE( Series<int> S=series_from_raw_memory(p, CS.size()); )
    CODE( dump(S); )
  }

  /*----------------------------------------------------------------------*/

  section("Test converters", '=');

  section("Test conversion from Array to Series");
  {
    CODE( Array<int> A(Shaper(2,9)(10,12)(0,2)(2,4)); );
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
    CODE( Array<int> B=aff::slice(A)(1,11)(1,1)(1,3); )
    CODE( printarray(B) );
    CODE( Series<int> C=series_from_array(B); )
    CODE( dump(C); )
    CODE( ConstArray<int> D=B; )
    CODE( ConstSeries<int> E=series_from_array(D); )
    CODE( dump(E); )
  }
}

/* ----- END OF helpertest.cc ----- */
