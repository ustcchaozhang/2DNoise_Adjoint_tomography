/*! \file complexio.h
 * \brief binary I/O of complex types (prototypes)
 * 
 * \ingroup complexio_h
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 20/11/2002
 * 
 * binary I/O of complex types (prototypes)
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
 *  - 20/11/2002   V1.0   Thomas Forbriger
 *  - 19/07/2005   V1.1   removed struct IOTsize; this is obsolete, since
 *                        I learned that the sizeof function is a
 *                        compile-time literal
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_COMPLEXIO_H_VERSION

#define TF_COMPLEXIO_H_VERSION \
  "TF_COMPLEXIO_H   V1.1"

#include<complex>
#include<tfxx/fortranio.h>

namespace tfxx {

  /*! \brief Interface provided through complexio.h
   *
   * \defgroup complexio_h Interface provided through complexio.h
   * \ingroup group_ioswap, group_fortranio
   */

  namespace fortranio {


    /*! \brief Output operator template for class FortranBinOutput 
     *
     * \ingroup group_fortranio, complexio_h
     *
     * It is quite save to present this operator in the global namespace. This
     * is just function overloading. The compiler will decide which operator
     * we need be the FortranBinOutput object involved. An this way using the
     * operator function is most convenient.
     *
     * \anchor fortranio_opout
     */
    template<typename T>
      tfxx::fortranio::FortranBinOutput& 
        operator << (tfxx::fortranio::FortranBinOutput& fo, 
                     const std::complex<T>& value)
    {
      fo.put(value.real());
      fo.put(value.imag());
      return(fo);
    }

    /*! \brief Input operator template for class FortranBinInput 
     *
     * \ingroup group_fortranio, complexio_h
     *
     * It is quite save to present this operator in the global namespace. This
     * is just function overloading. The compiler will decide which operator
     * we need be the FortranBinInput object involved. An this way using the
     * operator function is most convenient.
     *
     * \anchor fortranio_opin
     */
    template<typename T>
      tfxx::fortranio::FortranBinInput& 
        operator >> (tfxx::fortranio::FortranBinInput& fi, 
                     std::complex<T>& value)
    {
      T rvalue,ivalue;
      fi.get(rvalue);
      fi.get(ivalue);
      value=std::complex<T>(rvalue, ivalue);
      return(fi);
    }

  } // namespace fortranio

}

#endif // TF_COMPLEXIO_H_VERSION (includeguard)

/* ----- END OF complexio.h ----- */
