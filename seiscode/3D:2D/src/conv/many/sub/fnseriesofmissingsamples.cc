/*! \file fnseriesofmissingsamples.cc
 * \brief function seriesofmissingsamples() (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/02/2012
 * 
 * function seriesofmissingsamples() (implementation)
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
 *  - 25/04/2012   V1.1   do not process breaks in contiguous data
 * 
 * ============================================================================
 */
#define TF_FNSERIESOFMISSINGSAMPLES_CC_VERSION \
  "TF_FNSERIESOFMISSINGSAMPLES_CC   V1.1"

#include "gapfunctions.h"
#include "structgapanalysis.h"
#include <iostream>
#include <tfxx/misc.h>

/*! \brief Function to extract a GapSeries from given Gapsofstream
 */
GapSeries seriesofmissingsamples(const Gapsofstream& gos,
                                 const CompletenessBins& cb,
                                 const bool& debug)
{
  // fetch header data
  GapSeriesHeader header(cb, gos.ID);;
  GapSeries retseries(header);
  const libtime::TRelativeTime& dt=gos.ID.dt;
  // build series
  for (Gapsofstream::Tvecofgap::const_iterator G=gos.gap.begin();
       G!=gos.gap.end(); ++G)
  {
    TFXX_debug(debug, "seriesofmissingsamples",
               "fill in gap: " << (*G));
    const libtime::TAbsoluteTime& first=G->first;
    const libtime::TAbsoluteTime& last=G->last;
    libtime::TAbsoluteTime current=first;
    // do not process if this gaps actually is a break
    if (!G->isbreak())
    {
      for (unsigned int i=0; i<cb.nbins(); ++i)
      {
        TFXX_debug(debug, "seriesofmissingsamples",
                   "bin #" << i << ": "
                   << cb.bin(i).timestring() << " - " 
                   << cb.nextbin(i).timestring());
        TFXX_debug(debug, "seriesofmissingsamples",
                   " current: " << current.timestring());
        if (cb.isinbin(current, i))
        {
          TFXX_debug(debug, "seriesofmissingsamples",
                   "is in bin " << i << ": " << current.timestring());
          libtime::TAbsoluteTime nextbin=cb.nextbin(i);
          TFXX_debug(debug, "seriesofmissingsamples",
                   TFXX_value(nextbin.timestring()) << " " <<
                   TFXX_value(last.timestring()));
          if (nextbin<=last)
          {
            unsigned int n=(nextbin-current)/dt;
            while ((current+n*dt)<nextbin) 
            { 
              TFXX_debug(debug, "seriesofmissingsamples",
                   "n: " << n <<
                   " (current+n*dt): " << 
                   libtime::TAbsoluteTime(current+n*dt).timestring() <<
                   " nextbin: " << nextbin.timestring());
              ++n; 
            }
            TFXX_debug(debug, "seriesofmissingsamples",
                 "n: " << n <<
                 " (current+n*dt): " << 
                 libtime::TAbsoluteTime(current+n*dt).timestring() <<
                 " nextbin: " << nextbin.timestring());
            retseries.gapseries(i) += n;
            current += n*dt;
          }
          else
          {
            unsigned int n=(last-current)/dt;
            while ((current+n*dt)<=last) 
            { 
              TFXX_debug(debug, "seriesofmissingsamples",
                   "n: " << n <<
                   " (current+n*dt): " << 
                   libtime::TAbsoluteTime(current+n*dt).timestring() <<
                   " last: " << last.timestring());
              ++n; 
            }
            TFXX_debug(debug, "seriesofmissingsamples",
                 "n: " << n <<
                 " (current+n*dt): " << 
                 libtime::TAbsoluteTime(current+n*dt).timestring() <<
                 " last: " << last.timestring());
            retseries.gapseries(i) += n;
            // no need to cycle further
            i=cb.nbins();
          }
        } // if (cb.isinbin(current, i))
      } // end of loop over bins
    } // if (last >= first)
  } // end of loop over gaps in gos
  if (debug)
  {
    for (unsigned int j=retseries.gapseries.first();
         j<=retseries.gapseries.last(); ++j)
    {
      std::cout << TFXX_value(j) << " " 
        << TFXX_value(retseries.gapseries(j)) << "\n";
    }
  }
  return(retseries);
} // GapSeries seriesofmissingsamples()

/* ----- END OF fnseriesofmissingsamples.cc ----- */
