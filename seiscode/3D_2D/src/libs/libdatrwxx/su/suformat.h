/*! \file suformat.h
 * \brief subformat keys and properties (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 21/11/2011
 * 
 * subformat keys and properties (prototypes)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 * 
 * REVISIONS and CHANGES 
 *  - 21/11/2011   V1.0   Thomas Forbriger
 *  - 21/01/2012   V1.1   added option structs and strict modifier
 *  - 22/01/2012   V1.2   bestrict control parameter must be bundled with
 *                        spatial sampling parameters
 *  - 24/01/2012   V1.3   provide functions to evaluate output modifiers
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_SUFORMAT_H_VERSION

#define DATRW_SUFORMAT_H_VERSION \
  "DATRW_SUFORMAT_H   V1.3"

#include <string>

namespace datrw {

  namespace su {

    /*! \brief subformat definitions
     * \ingroup group_su
     */
    namespace subformat {

      /*! \brief key for subformat modifiers
       * \ingroup group_su
       */
      namespace key {
          
        //! strictly interpret header as defined in SeismicUnix source
        extern const char* const strict;
          
        //! understand data file as seismic data file in any case
        extern const char* const forceseismic;

        //! understand data file as ultrasonic data file in any case
        extern const char* const forceultrasonic;

        //! maximum number of significant digits to be used
        extern const char* const coodigits;

        //! set desired scalco value
        extern const char* const scalco;

      } // namespace key

      /*----------------------------------------------------------------------*/

      /*! \brief default values for subformat modifiers
       * \ingroup group_su
       */
      namespace def {

        //! default maximum number of significant digits to be used
        extern const unsigned int coodigits;

        //! default scalco value
        extern const short scalco;

      /*----------------------------------------------------------------------*/
      // not adjustable values

        /*! Values smaller than this (coordinates) are understood as being
         * zero
         */
        extern const double thresholdzero;

        /*! Comparison of integer values and floating point values requires a
         * way to handle round-off. Values which are closer to each other
         * (relative residual) than the threshold are regarded as being equal.
         */
        extern const double thresholdcmp;

      } // namespace def

    } // namespace subformat

    /*======================================================================*/

    /*! \brief options to be passed between classes definitions
     * \ingroup group_su
     */
    namespace options {

      /*! \brief options to control the way spatial sampling header variables
       * are handeled.
       * \ingroup group_su
       * \sa datrw::su::ScalCoo, datrw::su::Coordinates
       */
      struct SpatialSampling {
        /*! \brief maximum number of significant digits to be used
         *
         * Floating point number representation and conversion easily leads to
         * cases where 0.01 becomes 0.00999999977648 which is not intended.
         * In such cases we will round to the nearest value.
         */
        unsigned int coodigits;
        //! preferred scalco value
        short scalco;
        //! if true: strictly use header definition by SeismicUnix source
        bool bestrict;

        SpatialSampling():
          coodigits(datrw::su::subformat::def::coodigits),
          scalco(datrw::su::subformat::def::scalco),
          bestrict(false)
        { }
      }; // struct SpatialSampling

      /*----------------------------------------------------------------------*/

      /*! \brief options to control the way temporal sampling header variables
       * are handeled.
       * \ingroup group_su
       * \sa datrw::su::SUheader, \ref sec_su_for_TOAST 
       */
      struct TemporalSampling {
        //! force ultrasonic headers
        bool forceultrasonic;
        //! force seismic headers
        bool forceseismic;

        TemporalSampling():
          forceultrasonic(false),
          forceseismic(false) { }
        bool isconsistent() const;
      }; // struct TemporalSampling

      /*----------------------------------------------------------------------*/

      /*! \brief options to control the way header variables
       * are handeled.
       * \ingroup group_su
       * \sa datrw::su::SUheader
       */
      struct SUHeaderControl {
        SpatialSampling spatialsampling;
        TemporalSampling temporalsampling;
      }; // struct SUHeaderControl 

    } // namespace options

    /*----------------------------------------------------------------------*/

    /*! \brief evaluate input stream format modifiers
     * \ingroup group_su
     */
    options::SUHeaderControl inputmodifiers(const std::string& mf,
                                            const bool& debug=false);

    /*! \brief evaluate output stream format modifiers
     * \ingroup group_su
     */
    options::SUHeaderControl outputmodifiers(const std::string& mf,
                                             const bool& debug=false);

    /*----------------------------------------------------------------------*/

  } // namespace su

} // namespace datrw

#endif // DATRW_SUFORMAT_H_VERSION (includeguard)

/* ----- END OF suformat.h ----- */
