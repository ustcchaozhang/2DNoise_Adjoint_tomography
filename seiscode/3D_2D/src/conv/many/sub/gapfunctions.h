/*! \file gapfunctions.h
 * \brief function prototypes used in gap analysis (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/02/2012
 * 
 * function prototypes used in gap analysis (prototypes)
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
#ifndef TF_GAPFUNCTIONS_H_VERSION

#define TF_GAPFUNCTIONS_H_VERSION \
  "TF_GAPFUNCTION_H   V1.0   "

#include <iostream>
#include <libtime++.h>
#include "structgapanalysis.h"
#include "completenessbins.h"
#include "structgapseries.h"

Tvecofgaps gaps(const libtime::TAbsoluteTime& earliest,
                const libtime::TAbsoluteTime& latest,
                const TContiguouslist& cl,
                const bool& debug=false);

void printgaps(std::ostream& os, const Tvecofgaps& vog,
                                 const libtime::TAbsoluteTime& earliest,
                                 const libtime::TAbsoluteTime& latest,
               const unsigned int& summarizelevel=0);

CompletenessSeries completeness(const GapSeries& gs, 
                                const bool& debug=false);

GapSeries seriesofmissingsamples(const Gapsofstream& gos,
                                 const CompletenessBins& cb,
                                 const bool& debug=false);

void gnuplotplot(std::ostream& os,
                 const std::string& psname,
                 const CompletenessBins& cb,
                 const Tvecofgaps& vog);

#endif // TF_GAPFUNCTIONS_H_VERSION (includeguard)

/* ----- END OF gapfunctions.h ----- */
