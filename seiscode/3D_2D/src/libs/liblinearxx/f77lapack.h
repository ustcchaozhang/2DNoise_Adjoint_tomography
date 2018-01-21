/*! \file f77lapack.h
 * \brief prototypes for interfaced F77 LAPACK subroutines (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/02/2004
 * 
 * prototypes for interfaced F77 LAPACK subroutines (prototypes)
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
 *  - 18/12/2007   V1.1   modified Fortran type convention on 64bit systems
 * 
 * ============================================================================
 */

// include guard
#ifndef LINEAR_F77LAPACK_H_VERSION

#define LINEAR_F77LAPACK_H_VERSION \
  "LINEAR_F77LAPACK_H   V1.0"

#ifdef __cplusplus
//! prototypes to F77 LAPACK subroutines
namespace f77lapack {
extern "C" {
#endif

/*
 * all f2c stuff that is needed here
 * =================================
 */

#ifndef F2C_INCLUDE
/* FORTRAN (f2c) types needed by the wrapper functions */
#ifdef __x86_64
typedef int integer;
typedef double doublereal;
typedef int logical;
typedef int ftnlen;
#else
typedef long int integer;
typedef double doublereal;
typedef long int logical;
typedef long int ftnlen;
#endif
#else
#warning f2c.h is read from somewhere else!
#endif

  // solve a real system of linear equations with a positive definite matrix
  void dposv_(char *uplo, integer *m, integer *k , 
              doublereal *A, integer *lda,
              doublereal *X, integer *ldx, integer *info);

#ifdef __cplusplus
} // extern "C"
} // namespace f77lapack
#endif

#endif // LINEAR_F77LAPACK_H_VERSION (includeguard)

/* ----- END OF f77lapack.h ----- */
