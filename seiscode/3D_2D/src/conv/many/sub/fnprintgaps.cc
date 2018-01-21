/*! \file fnprintgaps.cc
 * \brief function printgaps() (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/02/2012
 * 
 * function printgaps() (implementation)
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
 * REVISIONS and CHANGES 
 *  - 12/02/2012   V1.0   Thomas Forbriger
 *  - 25/04/2012   V1.1   properly distinguish between gaps and breaks
 *  - 01/01/2012   V1.2   completeness may not be reported as 100% if at least
 *                        one gap or break is present
 *  - 08/01/2013   V1.3   report analysis period
 * 
 * ============================================================================
 */
#define TF_FNPRINTGAPS_CC_VERSION \
  "TF_FNPRINTGAPS_CC   V1.3"

#include "gapfunctions.h"
#include <tfxx/misc.h>

void printgaps(std::ostream& os, const Tvecofgaps& vog,
                                 const libtime::TAbsoluteTime& earliest,
                                 const libtime::TAbsoluteTime& latest,
               const unsigned int& lev)
{
  if (lev<3) 
  {
    os << "analysis period: "
      << earliest.timestring().substr(4)
      << " - "
      << latest.timestring().substr(4)
      << "\n"; 
    os << "gaps present in input data:\n"; 
  }
  unsigned int ntotalgaps=0;
  unsigned int ntotalbreaks=0;
  unsigned int ntotalmissing=0;
  unsigned int ntotalexpected=0;
  libtime::TRelativeTime ttotalexpected(0);
  libtime::TRelativeTime ttotalmissing(0);
  for (Tvecofgaps::const_iterator I=vog.begin(); I!=vog.end(); ++I)
  {
    if (lev<2) { os << "stream: " << I->ID << std::endl; }
    for (Gapsofstream::Tvecofgap::const_iterator J=I->gap.begin(); 
         J!=I->gap.end(); ++J)
    {
      if (lev<1) { os << *J << std::endl; }
    } // for (Gapsofstream::Tvecofgap::const_iterator J=I->gap.begin();
      //      J!=I->gap.end(); ++J)
        
    Gapsummary summary=I->summarize();
//    os << "DEBUG: I->mearliest.timestring(): " << I->Mearliest.timestring() << std::endl;
//    os << "DEBUG: I->mlatest.timestring(): " << I->Mlatest.timestring() << std::endl;
    ntotalgaps += summary.ngaps;
    ntotalbreaks += summary.nbreaks;
    ntotalmissing += summary.nmissing;
    ntotalexpected += summary.nexpected;
    ttotalexpected += summary.tspan;
    ttotalmissing += summary.tmissing;
    if (lev<2) 
    {
      os << "  number of breaks / number of gaps / number of missing samples / completeness:\n";
      os << "  " << summary.nbreaks << " / ";
      os << summary.ngaps << " / ";
      os << summary.nmissing << " / ";
      double tspan=libtime::time2double(summary.tspan);
      double tmissing=libtime::time2double(summary.tmissing);
      int completeness=nearbyint(1.e4*(1.-(tmissing/tspan)));
      if ((completeness == 10000) &&
          ((summary.nbreaks+summary.ngaps+summary.nmissing)>0))
      { completeness -= 1; }
      os << static_cast<double>(completeness)*1.e-2 << "%\n";
    }
  } // for (Tvecofgaps::const_iterator I=vog.begin(); I!=vog.end(); ++I)

  double tspan=libtime::time2double(ttotalexpected);
  double tmissing=libtime::time2double(ttotalmissing);
  int completeness=nearbyint(1.e4*(1.-(tmissing/tspan)));
  if (lev<3)
  {
    os << "summary of entire analysis:\n";
    os << "number of breaks / number of gaps / number of missing samples / completeness:\n";
    os << ntotalbreaks << " / ";
    os << ntotalgaps << " / ";
    os << ntotalmissing << " / ";
    if ((completeness == 10000) &&
        ((ntotalbreaks+ntotalgaps+ntotalmissing)>0))
    { completeness -= 1; }
    os << static_cast<double>(completeness)*1.e-2 << "%\n";
  }

  if (lev==3) { os << ntotalgaps << std::endl; }
  if (lev==4) { os << ntotalmissing << std::endl; }
} // void printgaps(std::ostream& os, const Tvecofgaps& vog)

/* ----- END OF fnprintgaps.cc ----- */
