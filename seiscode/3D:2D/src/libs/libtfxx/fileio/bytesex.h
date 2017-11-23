/*! \file bytesex.h
 * \brief handle different data bytesex (prototypes)
 * 
 * \ingroup bytesex_h
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/07/2005
 * 
 * handle different data bytesex (prototypes)
 *
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 19/07/2005   V1.0   Thomas Forbriger
 *                        - all contents are copied from misc.h 
 *                        - removed struct IOTsize; this is obsolete, since
 *                          I learned that the sizeof function is a
 *                          compile-time literal
 *                        - removed check_assumed_size() for the same reason
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_BYTESEX_H_VERSION

#define TF_BYTESEX_H_VERSION \
  "TF_BYTESEX_H   V1.0"

// we include fstream, because all function are closely related to file I/O
// and file_magic definitely requires fstream
#include<fstream>

namespace tfxx {

/*! \brief Interface provided through bytesex.h
 * \defgroup bytesex_h Interface provided through bytesex.h
 * \ingroup group_ioswap, group_fortranio
 */

/*! \namespace tfxx::ioswap
 * \brief All facilities for checking bytesex and swapping input binary data.
 * \ingroup bytesex_h, group_ioswap
 *
 * \since November 2002
 *
 * \sa example_fortraniotest
 */
namespace ioswap {

/*! \defgroup group_ioswap I/O byte swapping facility.
 * \brief Provides function to check bytesex and swap input bytes.
 *
 * When working in a multi-CPU-model environment, data written on one host
 * (with Intel CPU e.g.) can not easily be read on a different host (with
 * Motorola CPU e.g.) when using binary data. In this group we provide
 * facilities to check bytesex of datafiles and perform byte-swapping if
 * necessary.
 *
 * \ref anchor_fortranio_firsttest "Tests on this module" were performed
 * within tests of the \ref group_fortranio.
 *
 * \sa example_fortraniotest
 * \sa group_fortranio
 * \sa bytesex_h
 *
 * The components are collected in namespace tfxx::ioswap.
 * @{
 */

//----------------------------------------------------------------------
//
// we start with a set of useful types and enums

/*! A union to support raw I/O.
 *
 * \arg \c value   the numerical value
 * \arg \c bytes   the raw byte representation
 */
template<typename T>
union IOUnion {
  T value;                       
  char bytes[sizeof(T)]; 
};

//! Define different CPU type that are recognized
enum Ecpu_type {
  //! Intel CPU
  cpu_Intel = 1,
  //! Motorola CPU
  cpu_Motorola = 2,
  //! unknown CPU
  cpu_unknown = 3
};

//! Define bytesex indicator for magic number test
enum Emagic_type {
  //! The bytesex of the file matches this machine
  magic_match = 1,
  //! The bytesex of the file must be swapped to match this machine
  magic_swap = 2,
  //! The magic number does match the file
  magic_nomatch = 3
};

/*!
 * @} end of group_ioswap
 */

//----------------------------------------------------------------------
//
// some function templates

/*! \brief How to swap any generic type
 *
 * This function template calls ist specialization
 * \ref ioswap_swap_specialization "swap<IOUnion<T>>".
 * \ingroup group_ioswap, bytesex_h
 */
template<class T>
T swap(const T& value)
{
  IOUnion<T> in, out;
  in.value=value;
  out=swap(in);
  return(out.value);
}

/*! \brief Specialization in case we use use an IOUnion.
 * i.e. overloading the function
 *
 * \anchor ioswap_swap_specialization
 * \ingroup group_ioswap, bytesex_h
 */
template<class T>
IOUnion<T> swap(const IOUnion<T>& value)
{
  IOUnion<T> result;
  for (unsigned int i=0; i<sizeof(T); i++)
  { result.bytes[i]=value.bytes[sizeof(T)-i-1]; }
  return(result);
}

//----------------------------------------------------------------------
//
// some binary function

/*! \brief Create a magic number from a character string.
 * \ingroup group_ioswap, bytesex_h
 *
 * If \f$x_{i}=(\vec{x})_{i}\f$ represents the input character sequence \c
 * cmagic and \f$N=4\f$ is the value of \c sizeof(int), then the return
 * value will be
 * \f[
 *   \textrm{magic}(\vec{x})
 *      =\sum\limits_{i=0}^{N-1} x_{i}\; 2^{8\; (N-1-i)}
 *      =x_{3}+256\; (x_{2}+256\; (x_{1}+256\; x_{0})).
 * \f]
 *
 * \param cmagic 4-byte character sequence representing magic number (most
 *               restrictive: pass a const pointer to a const char)
 *               is pointer to character array of size sizeof(int)
 * \return The magic number representing the 4-byte character sequence on your
 *         system
 * \sa example_fortraniotest
 */
int magic(const char* const cmagic);

/*! \brief Check CPU model.
 * \ingroup group_ioswap, bytesex_h
 * \return return value indicates the CPU model found
 */
Ecpu_type cpu();

/*! \brief Check for magic number in file.
 *
 * \ingroup group_ioswap, bytesex_h
 *
 * \param is input stream to read from
 * \param cmagic 4-byte character sequence representing magic number (most
 *               restrictive: pass a const pointer to a const char)
 *               is pointer to character array of size sizeof(int)
 * \param fortranmode use Fortran binary I/O if true
 * \return The return value tells whether the file matches
 *
 * The function may have the following return values
 * \arg magic_match The file has the requested magic number and the bytesex of
 *                  the data matches that of the system
 * \arg magic_swap The file has the requested magic number but byte swapping
 *                 is required for input data
 * \arg magic_nomatch The requested magic number was not found in the file
 */
Emagic_type file_magic_test(std::istream& is, const char* const cmagic,
                            const bool& fortranmode=false);

/*! \brief Write magic number to file.
 * \ingroup group_ioswap, bytesex_h
 *
 * \param os output stream to write to
 * \param cmagic 4-byte character sequence representing magic number (most
 *               restrictive: pass a const pointer to a const char)
 *               is pointer to character array of size sizeof(int)
 * \param fortranmode use Fortran binary I/O if true
 */
void file_magic_write(std::ostream& os, const char* const cmagic,
                      const bool& fortranmode=false);

} // namespace ioswap

} // namespace tfxx

#endif // TF_BYTESEX_H_VERSION (includeguard)

/* ----- END OF bytesex.h ----- */
