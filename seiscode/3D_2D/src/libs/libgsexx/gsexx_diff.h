/*! \file gsexx_diff.h
 * \brief series differencing templates (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 29/03/2002
 * 
 * series differencing templates (prototypes)
 *
 * contains documentation for namespace differences
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * libgsexx is free software; you can redistribute it and/or modify
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
 * 
 * REVISIONS and CHANGES 
 *  - 29/03/2002   V1.0   Thomas Forbriger
 *  - 30/03/2002   V1.1   had to move code to gesxx.h
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_GSEXX_DIFF_H_VERSION

#define TF_GSEXX_DIFF_H_VERSION \
  "TF_GSEXX_DIFF_H   V1.0   "
#define TF_GSEXX_DIFF_H_CVSID \
  "$Id$"

#include <gsexx.h>

namespace GSE2 {
namespace waveform {

/*! \namespace GSE2::waveform::differences
 *
 * These operators are only used within the reading and writing classes. Thus
 * we declare them here - hidden from the public.
 *
 * The Tdiff_operator template provides a means to apply or remove differences
 * of any order from an integer data stream. To use this feature: Create an
 * object of the desired operator class. Feed the input datastream sample by
 * sample into the class' operator function. It returns the samples with
 * differences applied or removed.
 *
 * \sa apply1stdiffT, apply2nddiffT, remove1stdiffT, remove2nddiffT
 */
namespace differences {

/*! \class Tapply_diff
 *
 * This class applies differences to an integer data stream. For this purpose
 * it has to remember the previous data value. You have to initialize an
 * object of this class for each data stream.
 *
 * \todo
 * This is the place, where overflow-checking should be done.
 */

/*! \class Tremove_diff
 *
 * This class removes differences from an integer data stream by calculating a
 * cumulative sum. For this purpose it has to remember the previous returned
 * data value. You have to initialize an object of this class for each data
 * stream.
 *
 * \todo
 * This is the place, where overflow-checking should be done.
 */

/*! \class Tdiff_operator
 *
 * This template expands recursively and thus provides an operator of the
 * requested order. It is a pure function class.
 *
 * Template parameters are:
 * \param n order of differences
 * \param OP operator, which could be Tapply_diff or Tremove_diff
 */

/*! \class Tdiff_operator<0, OP>
 *
 * Just copy the value. Never use this directly (it just copies numbers). This
 * specialization is necessary to stop the template expansion recursion.
 *
 * Tamplate parameter:
 * \param OP operator, which could be Tapply_diff or Tremove_diff
 */

} // namespace differences

} // namespace waveform
} // namespace GSE2

#endif // TF_GSEXX_DIFF_H_VERSION (includeguard)

/* ----- END OF gsexx_diff.h ----- */
