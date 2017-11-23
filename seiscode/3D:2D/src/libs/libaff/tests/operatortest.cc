/*! \file operatortest.cc
 * \brief test operator functions
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 10/02/2004
 * 
 * test operator functions
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 10/02/2004   V1.0   Thomas Forbriger
 *  - 05/07/2005   V1.1   check operators on containers declared const
 * 
 * ============================================================================
 */
#define OPERATORTEST_VERSION \
  "OPERATORTEST   V1.1   test operator functions"

#include <iostream>
#include <complex>
#include <aff/array.h>
#include <aff/series.h>
#include <aff/dump.h>
#include <aff/arrayoperators.h>
#include <aff/seriesoperators.h>
#include <aff/subarray.h>

using std::cout;
using std::cerr;
using std::endl;

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

/*! \example tests/operatortest.cc
 *
 * Here you can learn how to use operators with aff classes.
 *
 * This test program gives an example of the usage of the following classes,
 * functions, and preprocessor macros:
 *   - aff::Array
 *   - aff::ConstArray
 *   - aff::dump
 *   - aff::dump_array
 *   - #DUMP
 *   - #CODE
 *
 * \sa tests/operatortest.cc
 */

int main(int iargc, char* argv[])
{
  cout << OPERATORTEST_VERSION << endl;

  section("Test array operators", '=');
  section("Normal array with unary operator");
  CODE(aff::Array<double> A(4,4));
  CODE(A=14.);
  CODE(dump_array(A));
  CODE(A+=500.);
  CODE(dump_array(A));

  section("Access through subarray");
  CODE(aff::Array<double> Asub=aff::subarray(A)(2,3)(2,3));
  CODE(Asub/=200);
  CODE(dump_array(A));

  section("binary operator");
  CODE(aff::Array<double> B(A));
  CODE(B=A*5);
  CODE(dump_array(B));

  section("test implicit type conversion");
  CODE(B=10.);
  CODE(dump_array(B));
  CODE(B*=0.2);
  CODE(dump_array(B));
  CODE(B*=4L);
  CODE(dump_array(B));
  CODE(B*=0xa0);
  CODE(dump_array(B));

  section("test advanced type conversion");
  CODE(aff::Array<std::complex<double> > C(3,3);)
  CODE(C=std::complex<double>(14.,2.);)
  CODE(dump_array(C));
  CODE(C*=0.2);
  CODE(dump_array(C));

  section("test binary operator with constant input");
  CODE(aff::ConstArray<double> D(A));
  CODE(aff::Array<double> E(D*2.));
  CODE(dump_array(E));

  section("mixed implicit operations");
  CODE(dump_array((D*5.)-12L));

  section("Test series operators", '=');
  section("test scalar operators");
  CODE(aff::Series<double> F(-3,3));
  CODE(F=5.);
  DUMP(F);
  CODE(F/=25.);
  DUMP(F);

  section("test vector operators");
  CODE(aff::Series<double> G(5));
  CODE(G=5.);
  DUMP(G);
  DUMP(F+G);
  CODE(aff::Series<double> H);
  CODE(H=F+G);
  DUMP(H);
  CODE(H -= F);
  DUMP(H);
  DUMP((H *= G + 0.2) + 0.2);
  DUMP(H);
  DUMP(100+H);
  DUMP(H+100);
  CODE(const aff::Series<double> CH(H));
  DUMP(CH+100);
  DUMP(CH -= 10);
  DUMP(CH);
}

/* ----- END OF operatortest.cc ----- */
