/*! \file properties.h
 * \brief describe data properties (prototypes)
 * 
 * \ingroup group_properties
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 14/02/2011
 * 
 * describe data properties (prototypes)
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
 * REVISIONS and CHANGES 
 *  - 14/02/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_PROPERTIES_H_VERSION

#define DATRW_PROPERTIES_H_VERSION \
  "DATRW_PROPERTIES_H   V1.0   "

namespace datrw {

  /*! \brief Handle information on file type, stream and file properties.
   *
   * \defgroup group_properties Internal utility: Handle information on file type, stream and file
   * properties
   */

  /*! \brief properties base class.
   *
   * An object of this type will be returned by the member function properties
   * in input and output stream classes. This way the interface to query
   * properties will always remain constant. Only the fields provided by the
   * properties class may grow in the future and not necessarily has to be the
   * same for all types of streams.
   */
  class Properties {
  }; // class Properties

} // namespace datrw

#endif // DATRW_PROPERTIES_H_VERSION (includeguard)

/* ----- END OF properties.h ----- */
