/*! \file bytesex.h
 * \brief A copy of bytesex.h from libtfxx (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 29/06/2007
 * 
 * A copy of bytesex.h from libtfxx (prototypes)
 *
 * just to make libdatrw a bit more independent
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
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 29/06/2007   V1.0   Thomas Forbriger
 *  - 30/04/2010   V1.1   added magic number writing and reading
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_BYTESEX_H_VERSION

#define DATRW_BYTESEX_H_VERSION \
  "DATRW_BYTESEX_H   V1.1"

// we include fstream, because all function are closely related to file I/O
// and file_magic definitely requires fstream
#include<fstream>

namespace datrw {

  namespace util {

    /*! \defgroup group_bytesex I/O byte swapping facility.
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
     * The components are collected in namespace datrw::util.
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
     * @} end of group_bytesex
     */

    //----------------------------------------------------------------------
    //
    // some function templates

    /*! \brief How to swap any generic type
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
     * \return The magic number representing the 4-byte character sequence on
     *         your system
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

  } // namespace util

} // namespace datrw

#endif // DATRW_BYTESEX_H_VERSION (includeguard)

/* ----- END OF bytesex.h ----- */
