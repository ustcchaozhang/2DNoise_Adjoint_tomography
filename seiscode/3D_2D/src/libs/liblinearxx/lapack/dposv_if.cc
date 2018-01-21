/*! \file dposv_if.cc
 * \brief interface to dposv LAPACK subroutine (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/02/2004
 * 
 * interface to dposv LAPACK subroutine (implementation)
 * 
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
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
 * REVISIONS and CHANGES 
 *  - 08/02/2004   V1.0   Thomas Forbriger
 *  - 18/03/2011   V1.1   avoid tfxx header file misc.h
 * 
 * ============================================================================
 */
#define LINEAR_DPOSV_IF_CC_VERSION \
  "TF_DPOSV_IF_CC   V1.1"

#include <iostream>
#include <aff/fortranshape.h>
#include <linearxx/f77lapack.h>
#include <linearxx/lapackxx.h>
#include <linearxx/error.h>

namespace linear {

namespace lapack {

  /*! Compute  the solution to a real system of linear
   *        equations  A * X = B.
   *
   * \param UPLO (input) CHARACTER*1
   *             = 'U':  Upper triangle of A is stored;
   *             = 'L':  Lower triangle of A is stored.
   * \param
   *     A       (input/output) DOUBLE PRECISION  array,  dimension
   *             (LDA,N)
   *             On entry, the symmetric matrix A.  If UPLO =  'U',
   *             the leading N-by-N upper triangular part of A con­
   *             tains the upper triangular part of the  matrix  A,
   *             and the strictly lower triangular part of A is not
   *             referenced.  If UPLO =  'L',  the  leading  N-by-N
   *             lower triangular part of A contains the lower tri­
   *             angular part of the matrix  A,  and  the  strictly
   *             upper triangular part of A is not referenced.
   *             On  exit,  if INFO = 0, the factor U or L from the
   *             Cholesky factorization A = U**T*U or A = L*L**T.
   * \param
   *     B       (input/output)  DOUBLE PRECISION array, dimension
   *             (LDB,NRHS)
   *             On  entry, the N-by-NRHS right hand side matrix B.
   *             On exit, if  INFO  =  0,  the  N-by-NRHS  solution
   *             matrix X.
   * \param
   *     INFO    (output) INTEGER
   *             = 0:  successful exit
   *             < 0:  if INFO = -i, the i-th argument had an ille­
   *             gal value
   *             > 0:  if INFO = i, the leading minor of order i of
   *             A is not positive definite, so  the  factorization
   *             could  not  be completed, and the solution has not
   *             been computed.
   * \param
   *     debug   (input) bool
   *             switch debug messages on
   *
   */
  void dposv(char UPLO, TDmatrix& A, TDmatrix& B, int& INFO, const bool& debug)
  {
    LINEAR_debug(debug, "dposv", "solve system of linear equations");
    // convert to Fortran interfacing array
    typedef aff::FortranArray<TDmatrix> Tfortranarray;
    Tfortranarray FA(A);
    Tfortranarray FB(B);
    LINEAR_assert((FA.last(0)==FA.last(1)),
                  "ERROR (dposv): Matrix A is not a square matrix");
    LINEAR_assert((FB.last(0)==FA.last(0)),
                  "ERROR (dposv): Dimension of right hand side "
                  "does not match");
    f77lapack::integer N=FA.last(0);
    f77lapack::integer NRHS=FB.last(1);
    f77lapack::integer LDA=FA.dimlast(0);
    f77lapack::integer LDB=FB.dimlast(0);
    f77lapack::integer f77INFO;
    f77lapack::doublereal* PA=FA.castedpointer<f77lapack::doublereal>();
    f77lapack::doublereal* PB=FB.castedpointer<f77lapack::doublereal>();
    LINEAR_debug(debug, "dposv", "N: " << N 
               << " NRHS: " << NRHS
               << " LDA: " << LDA
               << " LDB: " << LDB);
    LINEAR_debug(debug, "dposv", "first in A: " << *PA 
               << " first in B: " << *PB);
    f77lapack::dposv_(&UPLO, &N, &NRHS, PA, &LDA, PB, &LDB, &f77INFO);
    LINEAR_debug(debug, "dposv", "INFO: " << f77INFO);
    INFO=f77INFO;
    LINEAR_LAPACK_failure((INFO<0),"dposv",
                          "the -INFO-th argument had an illegal value",
                          INFO);
    LINEAR_LAPACK_failure((INFO>0),"dposv",
                          "the leading INFO-th order is not positive definite",
                          INFO);
  } // dposv

  //! easy to use version
  TDmatrix dposv(const TDmatrix::Tcoc& A, 
                 const TDmatrix::Tcoc& B, 
                 char UPLO, const bool& debug)
  {
    // factorized Matrix
    TDmatrix M(A.copyout());
    // return value
    TDmatrix X=B.copyout();
    int INFO;
    dposv(UPLO, M, X, INFO, debug);
    return(X);
  } // dposv
  
} // namespace lapack

} // namespace linear

/* ----- END OF dposv_if.cc ----- */
