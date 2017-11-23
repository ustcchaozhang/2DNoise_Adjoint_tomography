/*! \file hexdump.h
 * \brief output hex dump of any structure (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 27/06/2016
 * 
 * output hex dump of any structure (prototypes)
 *
 * \ingroup group_util
 * 
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
 * Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 27/06/2016   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_HEXDUMP_H_VERSION

#define TF_HEXDUMP_H_VERSION \
  "TF_HEXDUMP_H   V1.0"

#include<iomanip>
#include<iostream>
#include<ctype.h>

namespace tfxx {

  namespace util {

    /*! \brief output hex dump of memory area.
     * \ingroup group_util
     *
     * \param[in] p     memory location to start dump at
     * \param[in] size  number of bytes to dump in sequence
     * \param[in] os    output stream to which hex dump will be sent
     * \param[in] c     character to be used for non-printable characters
     * \param[in] n     number of bytes per line
     * \return          return stream \c os
     */
    std::ostream& hexdump(const void* pp, const unsigned int& size,
                          std::ostream& os=std::cout,
                          const char& c='.', const unsigned int&n=16);

  /* ---------------------------------------------------------------------- */

    /*! \brief output hex dump of any object.
     * \ingroup group_util
     *
     * \param[in] v     structure to dump
     * \param[in] os    output stream to which hex dump will be sent
     * \param[in] c     character to be used for non-printable characters
     * \param[in] n     number of bytes per line
     * \return          return stream \c os
     */
    template<class C>
      std::ostream& hexdump(const C& v, std::ostream& os=std::cout,
                   const char& c='.', const unsigned int&n=16)
      {
        // provide pointer to object
        //const unsigned char* p=reinterpret_cast<const unsigned char *>(&v);
        // size of object
        const unsigned int size=sizeof(v);
        // call raw memory hex dump
        hexdump(&v, size, os, c, n);
        return os;
      } // std::ostream& hexdump(const C& v, std::ostream& os=std::cout,
        //                       const char& c='.', const int&n=16)

  } // namespace util

} // namespace tfxx

/*! \brief produce hexdump to stdout, preceeded by object name.
 * \ingroup group_util
 */
#define TFXX_hexdump(V) std::cout << "hex dump of object \"" \
                        << #V << "\":" << std::endl; \
                        tfxx::util::hexdump(V);

#endif // TF_HEXDUMP_H_VERSION (includeguard)

/* ----- END OF hexdump.h ----- */
