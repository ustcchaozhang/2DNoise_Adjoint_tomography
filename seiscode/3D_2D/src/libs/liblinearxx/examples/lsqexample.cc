/*! \file lsqexample.cc
 * \brief least squares example (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 27/10/2016
 * 
 * least squares example (implementation)
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
 *  - 27/10/2016   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_LSQEXAMPLE_CC_VERSION \
  "TF_LSQEXAMPLE_CC   V1.0"

#include <iostream>

// provide interface to LAPACK
#include <linearxx/lapackxx.h>
// provide matrix and vector operators
#include <linearxx/operators.h>

// provide iterators for matrix and vector containers
#include <aff/iterator.h>
// provide elementwise operators for matrix and vector containers
#include <aff/arrayoperators.h>

// provide interface to random number generator
#include <tfxx/rng.h>

using std::cout;
using std::cerr;
using std::endl;

// define type of matrix container to be used
typedef linear::TDmatrix Tmatrix;

// print matrix to cout
void printmatrix(const Tmatrix::Tcoc& m)
{
  const int width=15;
  const int precision=5;
  cout.width(width);
  cout << " ";
  for (int i=m.first(1); i<=m.last(1); ++i)
  {
    cout.width(width);
    cout << i;
  }
  cout << endl;
  for (int j=m.first(0); j<=m.last(0); ++j)
  {
    cout.width(width);
    cout << j;
    for (int i=m.first(1); i<=m.last(1); ++i)
    {
      cout.width(width);
      cout.precision(precision);
      cout << m(j,i);
    }
    cout << endl;
  }
} // void printmatrix(const Tmatrix::Tcoc& m)

/* ====================================================================== */

int main(int iargc, char* argv[])
{

  // size of matrix for test
  const int n=3;
  const int m=4;

  // set up random number generator
  tfxx::numeric::RNGgaussian rng;

  // create matrix of partial derivatives
  Tmatrix G(m,n);
  // create data vector
  Tmatrix d(m,1);
  // create vector for given model parameters
  Tmatrix mknown(n,1);

  // fill matrix with random numbers
  // enclose code in block to make declarations local
  {
    aff::Iterator<Tmatrix> I(G);
    while (I.valid())
    {
      *I = rng();
      ++I;
    }
  }

  // fill vector with random numbers
  // enclose code in block to make declarations local
  {
    aff::Iterator<Tmatrix> I(mknown);
    while (I.valid())
    {
      *I = rng();
      ++I;
    }
  }

  cout << TF_LSQEXAMPLE_CC_VERSION "\n" << endl;
  cout << "Solve least squares problem" << endl;
  cout << "===========================" << endl;

  cout << "\n"
          "Residuals:\n"
          "r = G * m - d\n"
          "Find solution such that |r|^2 becomes minimal" << endl;

  // setup data vector
  d=linear::op::dotNxM(G,mknown);

  // report system
  cout << "\nMatrix of partial derivatives (G):\n";
  printmatrix(G);
  cout << "\nData vector (d):\n";
  printmatrix(d);
 
  // setup system of linear equations
  // --------------------------------
  // system matrix
  Tmatrix M=linear::op::dotNxM(linear::op::transposeNxM(G),G);
  // right hand side
  Tmatrix rhs=linear::op::dotNxM(linear::op::transposeNxM(G),d);

  cout << "\nSystem of linear equations to be solved:\n"
          "M * m = rhs\n" << endl;

  cout << "With matrix M=G^T*G:\n";
  printmatrix(M);
  cout << "and vector rhs=G^T*d:\n";
  printmatrix(rhs);

  // solve system
  // ------------
  // solution vetcor (optimal model parameters)
  Tmatrix mopt=linear::lapack::dposv(M,rhs,'U',false);

  cout << "\nsolution vector mopt:\n";
  printmatrix(mopt);
  cout << "\nexpected solution (mknown):\n";
  printmatrix(mknown);
  cout << "\nresidual (mopt-mknown):\n";
  printmatrix(mopt-mknown);
}

/* ----- END OF lsqexample.cc ----- */
