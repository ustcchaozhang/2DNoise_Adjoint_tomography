/*! \file seriesoperators.h
 * \brief provide operators for series classes (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 10/02/2004
 * 
 * provide operators for series classes (prototypes)
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 10/02/2004   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_SERIESOPERATORS_H_VERSION

#define AFF_SERIESOPERATORS_H_VERSION \
  "AFF_SERIESOPERATORS_H   V1.0   "

#include<aff/series.h>

/*! \brief
 * Series operators are defined through the macro mechanism in operators.h
 * and \ref group_operators
 * \ingroup group_series
 */
//@{
#define AFF_OPERATORS_CLASS aff::Series
#define AFF_OPERATORS_CONSTCLASS aff::ConstSeries
#include <aff/lib/operators.h>
#undef AFF_OPERATORS_CLASS
#undef AFF_OPERATORS_CONSTCLASS
//@}

#endif // AFF_SERIESOPERATORS_H_VERSION (includeguard)

/* ----- END OF seriesoperators.h ----- */
