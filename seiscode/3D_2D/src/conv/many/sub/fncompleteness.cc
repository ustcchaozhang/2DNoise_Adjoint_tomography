/*! \file fncompleteness.cc
 * \brief function completeness() (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/02/2012
 * 
 * function completeness() (implementation)
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
#define TF_FNCOMPLETENESS_CC_VERSION \
  "TF_FNCOMPLETENESS_CC   V1.0   "

#include "gapfunctions.h"
#include <tfxx/misc.h>
#include <aff/seriesoperators.h>

/*! \brief convert gaps to completeness
 */
CompletenessSeries completeness(const GapSeries& gs, 
                                const bool& debug)
{
  TFXX_debug(debug, "completeness",
             "just entered function" << "\n" << gs.header.ID);
  CompletenessSeries retseries(gs.header);
  retseries.completeness.copyin(gs.gapseries);
  retseries.header=gs.header;
  double samplesperbin
    =static_cast<double>(gs.header.completenessbins.samplesinbin(gs.header.ID.dt));
  retseries.completeness *= -100./samplesperbin;
  retseries.completeness += 100.;
  // handle first and last bin separately
  for (unsigned int j=0; j<2; ++j)
  {
    unsigned int i=0;
    if (j==1) { i=gs.gapseries.last(); }
    samplesperbin
      =static_cast<double>(gs.header.completenessbins.samplesinbin(i, gs.header.ID.dt));
    TFXX_debug(debug,  "completeness",
               TFXX_value(j) << " " << TFXX_value(samplesperbin));
    retseries.completeness(i)=
      100.*(1.-static_cast<double>(gs.gapseries(i))/samplesperbin);
  }
  if (debug)
  {
    for (unsigned int j=retseries.completeness.first();
         j<=retseries.completeness.last(); ++j)
    {
      std::cout << TFXX_value(j) << " " 
        << TFXX_value(retseries.completeness(j)) << "\n";
    }
  }
  TFXX_debug(debug, "completeness",
             "about to leave function");
  return retseries;
} // CompletenessSeries completeness(const GapSeries& gs)

/* ----- END OF fncompleteness.cc ----- */
