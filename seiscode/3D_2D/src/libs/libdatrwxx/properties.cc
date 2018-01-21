/*! \file properties.cc
 * \brief describe data properties (implementation)
 * 
 * \ingroup group_properties
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 14/02/2011
 * 
 * describe data properties (implementation)
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
#define DATRW_PROPERTIES_CC_VERSION \
  "DATRW_PROPERTIES_CC   V1.0   "

#include <datrwxx/properties.h>

namespace datrw {

} // namespace datrw

/*======================================================================*/

/*! \page page_properties Format, File, data, and stream properties
 *
 * There are properties of different category which should be presented to the
 * user:
 *
 *   -# Format properties:
 *      Are properties of a specific data format like
 *      - The file is binary or ASCII
 *      - Each file can contain multiple traces
 *   -# File properties:
 *      Are properties of a specific file and are not specific to the format.
 *      For example data can be encoded differently in PDAS data files.
 *   -# Stream properties:
 *      Are properties of a specific stream.
 *      This can be a specifc normalization mode for SFF data used upon
 *      output.
 *   -# Trace properties:
 *      Are properties which can vary from trace to trace within one file.
 *   -# Data properties:
 *      Are properties of a data representation used internally (are samples
 *      stored as integers or floats in single or double precision).
 *
 * Funtions should be provided, which return structures which present these
 * properties. 
 * Using structures makes this interface quite flexible.
 * These structures can be extended in the future without the need to change
 * the query interface in all strteams.
 * For this reason it is also necessary to define meaningful default values
 * (which could also mean "undefined") such that the structure is filled with
 * reasonable return values also in cases where the data module query
 * interface is not explicitely made aware of this parameter.
 *
 * Structures should contain each other hierarchically, such that the query
 * for trace properties also return properties of the file.
 * However, a query for format properties will not be able to return
 * information on a specific traces.
 *
 * \sa \ref page_data_types
 */

/* ----- END OF properties.cc ----- */
