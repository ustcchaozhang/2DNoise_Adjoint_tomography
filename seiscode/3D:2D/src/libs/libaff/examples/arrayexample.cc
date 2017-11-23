/*! \file arrayexample.cc
 * \brief Demonstrate array usage
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 21/10/2016
 * 
 * Demonstrate array usage
 * 
 * Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ----
 *
 * REVISIONS and CHANGES 
 *  - 21/10/2016   V1.0   Thomas Forbriger
 *  - 24/10/2016   V1.1   demonstrate:
 *                        - usage of array shape query functions
 *                        - shallow and deep copy
 *                        - function Shaper
 * 
 * ============================================================================
 */

/*! \example examples/arrayexample.cc
 *
 * This program demonstrates basic properties of the array class template in
 * libaff.
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
 * \sa examples/arrayexample.cc
 */

#define ARRAYEXAMPLE_VERSION \
  "ARRAYEXAMPLE   V1.1   Demonstrate array usage"

// include iostream for output to terminal
#include <iostream>
// provide function sqrt()
#include <cmath>

// provide aff::Array class template
#include <aff/array.h>
// provide aff::Shaper class template
#include <aff/shaper.h>
// provide function template aff::subarray
#include <aff/subarray.h>
// provide function template to produce a dump of array contents
#include <aff/dump.h>

using std::cout;
using std::endl;

/* ---------------------------------------------------------------------- */

/*! print headline
 *
 * This function prints a string to cout and underlines the string.
 */
void section(const char* s, const char l='-')
{
  cout << endl << s << endl;
  const char* p=s;
  while (*p) { cout << l; ++p; }
  cout << endl;
}

/* ---------------------------------------------------------------------- */

/*! print values of an NxM array
 *
 * This operator function can be used as an output operator for aff::Array and
 * aff:ConstArray objects. It silently assumes that the passed array has only
 * two dimensions. It then uses formatted stream output to display the
 * contents of the array.
 *
 * The function in particular demonstrates the use of array member functions
 * size, first, and last in order to query the index value ranges for the two
 * dimensions in use.
 */
template<class T>
  std::ostream& operator<<(std::ostream& os, const aff::ConstArray<T>& a)
{
  // size of array dimensions
  int N=a.size(0);
  int M=a.size(1);

  // report size of array
  cout << "Elements (i,j) of " << N << "x" << M << " array:" << endl;

  // width of first column, where value of i will be printed
  const int firstwidth=6;
  // width of columns:
  int columnwidth=(78-firstwidth)/M;

  // print column headings
  cout.width(firstwidth);
  cout.setf(std::ios_base::left, std::ios_base::adjustfield);
  cout << "";
  for (int j=a.first(1); j<=a.last(1); j++)
  {
    cout << "j=";
    cout.width(columnwidth-2);
    cout.setf(std::ios_base::left, std::ios_base::adjustfield);
    cout << j;
  }
  cout << endl;

  // print contents of array
  // outer loop: first index
  for (int i=a.first(0); i<=a.last(0); i++)
  {
    cout << "i=";
    cout.width(firstwidth-2);
    cout.setf(std::ios_base::left, std::ios_base::adjustfield);
    cout << i;
    // inner loop: second index
    for (int j=a.first(1); j<=a.last(1); j++)
    {
      cout.width(columnwidth);
      cout << a(i,j);
    }
    cout << endl;
  }
  return os;
} // std::ostream& operator<<(std::ostream& os, const aff::ConstArray<T>& a)

/*======================================================================*/

//! test array functionality
int main()
{
  cout << ARRAYEXAMPLE_VERSION << endl;

  // create a 3x4 array of type float
  aff::Array<float> A(3,4);

  // print array (layout and contents) to terminal
  section("Array A after being created:");
  cout << A;

  // set all elements in array to 5.2
  A=5.2;
  section("Array A after all elements being set to 5.2:");
  cout << A;

  /* ---------------------------------------------------------------------- */

  section("Demonstrate effect of shallow copy", '=');
  // create a second array
  aff::Array<float> B;

  // let B be a copy of A
  B=A;

  // B just references the same memory; thus changing values in A and B will
  // effectively change the same array values
  A(2,3)=10.7;
  B(3,1)=-19.3;

  // both modifications are present in A
  section("Array A after assignment to B and to A:");
  cout << A;

  /* ---------------------------------------------------------------------- */

  section("Demonstrate effect of deep copy", '=');
  // create a copy by actually copying all elements
  B=A.copyout();

  // modify B
  B(3,3)=102.3;
  section("Array B after assignment to B:");
  cout << B;
  section("Array A after assignment to B:");
  cout << A;

  /* ---------------------------------------------------------------------- */

  section("Demonstrate array usage with custom index range", '=');

  /* To create an array which does not use index range beginning with 1, use
   * the function Shaper, which is available through aff/shaper.h
   */
  A=aff::Array<float>(aff::Shaper(-2,6)(12,16));
  A=sqrt(2.);
  section("Array A after being assigned to array of different shape:");
  cout << A;
}

/* ----- END OF arrayexample.cc ----- */
