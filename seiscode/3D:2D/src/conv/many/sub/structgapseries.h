/*! \file structgapseries.h
 * \brief all structs use to produce series of gaps and series of completeness (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/02/2012
 * 
 * all structs use to produce series of gaps and series of completeness 
 * (prototypes)
 * 
 * Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * This file is part of the conv/many suite.
 *
 * The conv/many suite is free software; you can redistribute it and/or modify
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
 *  - 12/02/2012   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_STRUCTGAPSERIES_H_VERSION

#define TF_STRUCTGAPSERIES_H_VERSION \
  "TF_STRUCTGAPSERIES_H   V1.0   "

#include <aff/series.h>
#include <sffxx.h>
#include "structgapanalysis.h"
#include "completenessbins.h"

/*! \brief header data for complenetess series
 */
struct GapSeriesHeader {
  GapSeriesHeader(const CompletenessBins& cb,
                  const Gapid& id)
    :  ID(id), completenessbins(cb) { }
  ::sff::WID2 wid2() const;
  Gapid ID;
  CompletenessBins completenessbins;
}; // struct GapSeriesHeader

/*----------------------------------------------------------------------*/

/*! \brief a class to hold a gap series for one stream
 *
 * The value of counts in member gapseries provide the number of missing
 * samples in the respective bin.
 */
struct GapSeries {
  typedef ::aff::Series<int> Tgapseries;
  GapSeries(const GapSeriesHeader& h);
  GapSeriesHeader header;
  Tgapseries gapseries;
}; // struct GapSeries

/*----------------------------------------------------------------------*/

/*! \brief a class to hold a completeness series for one stream
 *
 * The values in member completeness provide the percentage of
 * completeness in the respective bin.
 */
struct CompletenessSeries {
  typedef ::aff::Series<double> Tcompletenessseries;
  CompletenessSeries(const GapSeriesHeader& h);
  GapSeriesHeader header;
  Tcompletenessseries completeness;
}; // struct CompletenessSeries

#endif // TF_STRUCTGAPSERIES_H_VERSION (includeguard)

/* ----- END OF structgapseries.h ----- */
