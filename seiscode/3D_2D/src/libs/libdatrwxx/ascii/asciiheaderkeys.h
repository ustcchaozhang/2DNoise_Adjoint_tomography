/*! \file asciiheaderkeys.h
 * \brief key IDs for ASCII header (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/11/2011
 * 
 * key IDs for ASCII header (prototypes)
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
 *  - 05/11/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef ASCII_ASCIIHEADERKEYS_H_VERSION

#define ASCII_ASCIIHEADERKEYS_H_VERSION \
  "ASCII_ASCIIHEADERKEYS_H   V1.0   "

namespace datrw {

  namespace ascii {

  /*! \brief key for WID2 line header field
   * \ingroup group_ascii
   * @{
   */
    extern const char* const keydate;
    extern const char* const keydt;
    extern const char* const keynsamples;
    extern const char* const keystation;
    extern const char* const keychannel;
    extern const char* const keyauxid;
    extern const char* const keyinstype;
    extern const char* const keycalib;
    extern const char* const keycalper;
    extern const char* const keyhang;
    extern const char* const keyvang;
  /**@}*/

  /*! \brief key for SRCE line header field
   * \ingroup group_ascii
   * @{
   */
    extern const char* const keySRCEdate;
    extern const char* const keySRCEtype;
    extern const char* const keySRCEX;
    extern const char* const keySRCEY;
    extern const char* const keySRCEZ;
    extern const char* const keySRCECS;
  /**@}*/

  /*! \brief key for INFO line header field
   * \ingroup group_ascii
   * @{
   */
    extern const char* const keyRECVX;
    extern const char* const keyRECVY;
    extern const char* const keyRECVZ;
    extern const char* const keyRECVCS;
    extern const char* const keynstacks;
  /**@}*/

  /*! \brief key for DATA type header field
   * \ingroup group_ascii
   * @{
   */
    extern const char* const keydata;
    extern const char* const keyint;
    extern const char* const keyfloat;
    extern const char* const keydouble;
  /**@}*/

  /*! \brief format modifier key
   * \ingroup group_ascii
   * @{
   */
    extern const char* const keynonfatal;
  /**@}*/

  } // namespace ascii

} // namespace datrw

#endif // ASCII_ASCIIHEADERKEYS_H_VERSION (includeguard)

/* ----- END OF asciiheaderkeys.h ----- */
