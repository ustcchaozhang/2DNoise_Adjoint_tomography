/*! \file mseed_keywords.h
 * \brief keywords used in mseed module (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/07/2016
 * 
 * keywords used in mseed module (prototypes)
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
 *  - 05/07/2016   V1.0   Thomas Forbriger
 *  - 12/07/2016   V1.1   add usec consistency check
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_MSEED_KEYWORDS_H_VERSION

#define DATRW_MSEED_KEYWORDS_H_VERSION \
  "DATRW_MSEED_KEYWORDS_H   V1.1"

namespace datrw {

  namespace mseed {

    /*! \brief namespace for keyword string constants.
     * \ingroup group_mseed
     */
    namespace key {

      /*! \brief keywords for format modifiers
       * \ingroup group_mseed
       * @{
       */
      extern const char* const dumpascii;
      extern const char* const ttolerance;
      extern const char* const estimateNframes;
      extern const char* const skipcheck;
      extern const char* const nonfatal;
      /**@}*/

      /*! \brief keywords for consistency checks
       * \ingroup group_mseed
       * @{
       */
      extern const char* const nframes;
      extern const char* const nsamples;
      extern const char* const data;
      extern const char* const usec;
      extern const char* const all;
      /**@}*/

    } // namespace key

  } // namespace mseed

} // namespace datrw

#endif // DATRW_MSEED_KEYWORDS_H_VERSION (includeguard)

/* ----- END OF mseed_keywords.h ----- */
