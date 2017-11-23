/*! \file structgapseries.cc
 * \brief all structs use to produce series of gaps and series of completeness (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/02/2012
 * 
 * all structs use to produce series of gaps and series of completeness (implementation)
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
#define TF_STRUCTGAPSERIES_CC_VERSION \
  "TF_STRUCTGAPSERIES_CC   V1.0   "

#include "structgapseries.h"

/*! \brief produce a WID2 header for file output
 */
::sff::WID2 GapSeriesHeader::wid2() const
{
  ::sff::WID2 retval;
  retval.dt=libtime::time2double(this->completenessbins.binsize());
  retval.date=this->completenessbins.firstbin();
  retval.station=this->ID.station();
  retval.channel=this->ID.channel();
  retval.auxid=this->ID.auxid();
  return(retval);
} // ::sff::WID2 GapSeriesHeader::wid2() const

/*----------------------------------------------------------------------*/

CompletenessSeries::CompletenessSeries(const GapSeriesHeader& h):
  header(h), completeness(0, h.completenessbins.nbins()-1)
{
  completeness=0.;
} // CompletenessSeries::CompletenessSeries(const GapSeriesHeader& h)

/*----------------------------------------------------------------------*/

GapSeries::GapSeries(const GapSeriesHeader& h):
  header(h), gapseries(0, h.completenessbins.nbins()-1)
{
  gapseries=0.;
} // GapSeries::GapSeries(const GapSeriesHeader& h)

/* ----- END OF structgapseries.cc ----- */
