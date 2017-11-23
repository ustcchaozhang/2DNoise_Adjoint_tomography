/*! \file blitzfortranio.h
 * \brief provide Fortran I/O for raw Blitz++ arrays (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/11/2002
 * 
 * provide Fortran I/O for raw Blitz++ arrays (prototypes)
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 28/11/2002   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_BLITZFORTRANIO_H_VERSION

#define TF_BLITZFORTRANIO_H_VERSION \
  "TF_BLITZFORTRANIO_H   V1.0   "

#include<fstream>
#include<blitz/array.h>
#include<tfxx/fortranio.h>
#include<tfxx/misc.h>
#include<tfxx/error.h>

namespace tfxx {
namespace fortranio {

/*! \brief magic numbers for blitz array I/O
 * \ingroup group_fortranio
 *
 * Please define TF_COMPLEX_ARRAY to activate I/O for arrays of complex types.
 * 
 * \deprecated
 * This struct is a quick hack and may be replaced by another algorithm in the
 * future. Magic number character sequences will, however, remain the same.
 * \todo
 * use a more clever technique than spezialisation for all types and ranks
 */
template<class T, int N>
struct blitz_magic {
}; // blitz_magic

//! partial specializations (others are hidden to doxygen)
template<> struct blitz_magic<float, 1> { static const char magic[]="ZZf1"; };
#ifndef DOXYGEN_MUST_SKIP_THIS
template<> struct blitz_magic<float, 2> { static const char magic[]="ZZf2"; };
template<> struct blitz_magic<float, 3> { static const char magic[]="ZZf3"; };
template<> struct blitz_magic<float, 4> { static const char magic[]="ZZf4"; };
template<> struct blitz_magic<float, 5> { static const char magic[]="ZZf5"; };
template<> struct blitz_magic<double, 1> { static const char magic[]="ZZd1"; };
template<> struct blitz_magic<double, 2> { static const char magic[]="ZZd2"; };
template<> struct blitz_magic<double, 3> { static const char magic[]="ZZd3"; };
template<> struct blitz_magic<double, 4> { static const char magic[]="ZZd4"; };
template<> struct blitz_magic<double, 5> { static const char magic[]="ZZd5"; };
template<> struct blitz_magic<int, 1> { static const char magic[]="ZZi1"; };
template<> struct blitz_magic<int, 2> { static const char magic[]="ZZi2"; };
template<> struct blitz_magic<int, 3> { static const char magic[]="ZZi3"; };
template<> struct blitz_magic<int, 4> { static const char magic[]="ZZi4"; };
template<> struct blitz_magic<int, 5> { static const char magic[]="ZZi5"; };
#ifdef TF_COMPLEX_ARRAY
#include<complex>
template<> struct blitz_magic<std::complex<float>, 1> 
{ static const char magic[]="ZZc1"; };
template<> struct blitz_magic<std::complex<float>, 2> 
{ static const char magic[]="ZZc2"; };
template<> struct blitz_magic<std::complex<float>, 3> 
{ static const char magic[]="ZZc3"; };
template<> struct blitz_magic<std::complex<float>, 4> 
{ static const char magic[]="ZZc4"; };
template<> struct blitz_magic<std::complex<float>, 5> 
{ static const char magic[]="ZZc5"; };
template<> struct blitz_magic<std::complex<double>, 1> 
{ static const char magic[]="ZZz1"; };
template<> struct blitz_magic<std::complex<double>, 2> 
{ static const char magic[]="ZZz2"; };
template<> struct blitz_magic<std::complex<double>, 3> 
{ static const char magic[]="ZZz3"; };
template<> struct blitz_magic<std::complex<double>, 4> 
{ static const char magic[]="ZZz4"; };
template<> struct blitz_magic<std::complex<double>, 5> 
{ static const char magic[]="ZZz5"; };
#endif
#endif

/*----------------------------------------------------------------------*/

/*! \brief Output operator template for class FortranBinOutput and blitz
 * \ingroup group_fortranio
 *
 * \anchor anchor_blitzfortranio_output
 * The compiler will consider namespace tfxx::fortranio due to the involved
 * FortranBinOutput object.
 * \todo
 * still far from finished
 */
template<class T, int N>
  tfxx::fortranio::FortranBinOutput& 
    operator << (tfxx::fortranio::FortranBinOutput& fo,
                 const blitz::Array<T, N>& a)
{
  fo.write_magic(blitz_magic<T,N>::magic);
  fo.put(N);
  fo.end_block();
  for (int i=0; i<N; i++) fo.put(a.lbound(i));
  fo.end_block();
  for (int i=0; i<N; i++) fo.put(a.extent(i));
  fo.end_block();
  for (blitz::Array<T, N>::const_iterator i=a.begin(); i!=a.end(); ++i)
  { fo.put(*i); }
  fo.end_block();
  return(fo);
}

/*----------------------------------------------------------------------*/

/*! \brief Input operator template for class FortranBinInput and blitz
 * \ingroup group_fortranio
 *
 * \anchor anchor_blitzfortranio_input
 * The compiler will consider namespace tfxx::fortranio due to the involved
 * FortranBinInput object.
 * \todo
 * still far from finished
 */
template<class T, int N>
  tfxx::fortranio::FortranBinInput& 
    operator >> (tfxx::fortranio::FortranBinInput& fi, 
                 blitz::Array<T, N>& a)
{
  TFXX_assert((fi.match_magic(blitz_magic<T,N>::magic)),
    "ERROR (blitz array operator>>): input has no matching magic number!");
  int n;
  fi.get(n);
  TFXX_assert((N==n), "ERROR (blitz array operator>>): illegal rank!");
  blitz::TinyVector<int, N> lbound, extent;
  for (int i=0; i<N; i++) fi.get(lbound[i]);
  for (int i=0; i<N; i++) fi.get(extent[i]);
  blitz::Array<T, N> result(lbound, extent, blitz::fortranArray);
  for (blitz::Array<T, N>::iterator i=result.begin(); i!=result.end(); ++i)
  { fi.get(*i); }
  a.reference(result);
  return(fi);
}

  
} // namespace fortranio
} // namespace tfxx

#endif // TF_BLITZFORTRANIO_H_VERSION (includeguard)

/* ----- END OF blitzfortranio.h ----- */
