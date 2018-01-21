/*! \file structgapanalysis.cc
 * \brief structs used in primary gap analysis (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/02/2012
 * 
 * structs used in primary gap analysis (implementation)
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
 *  - 25/04/2012   V1.1   properly distinguish between braeks and gaps
 * 
 * ============================================================================
 */
#define TF_STRUCTGAPANALYSIS_CC_VERSION \
  "TF_STRUCTGAPANALYSIS_CC   V1.1"

#include "structgapanalysis.h"

/*----------------------------------------------------------------------*/

Gapsummary Gapsofstream::summarize() const
{
  Gapsummary retval(this->Mearliest, this->Mlatest, this->ID.dt);
  Tvecofgap::const_iterator I=gap.begin();
  while (I!=gap.end())
  {
    if (I->isbreak()) { ++retval.nbreaks; } else { ++retval.ngaps; }
    retval.nmissing += I->nmissing();
    retval.tmissing += I->tmissing();
    ++I;
  }
  return(retval);
} // Gapsummary Gapsofstream::summarize() const
  
/*----------------------------------------------------------------------*/

//! print stream ID
std::ostream& operator<<(std::ostream& os, const Gapid& id)
{
  os << id.ID << " dt=" << id.dt.timestring();
  return(os);
} // std::ostream& operator<<(std:ostream& os, const Gapid& id)

/*----------------------------------------------------------------------*/

//! print contiguous chunk definition
std::ostream& operator<<(std::ostream& os, const Contiguous& chunk)
{
  Gapid id(chunk);
  os << "chunk of " << id << ":" << "\n";
  os << chunk.first.timestring() << " - " << chunk.last.timestring();
  return(os);
} // std::ostream& operator<<(std:ostream& os, const Contiguous& chunk)

/*----------------------------------------------------------------------*/

//! print gaps
std::ostream& operator<<(std::ostream& os, const Gap& gap)
{
  os << "  " << gap.first.timestring();
  if (gap.isbreak())
  {
    os << ": break in contiguous data\n";
  }
  else
  {
    os << " - " << gap.last.timestring() << "\n";
  }
  unsigned int nmissing=(gap.ID.dt+gap.last-gap.first)/gap.ID.dt;
  os << "    "
    << ": " << nmissing
    << " sample";
  if (nmissing!=1) { os << "s are"; } else { os << " is"; }
  os << " missing";
  return(os);
} // std::ostream& operator<<(std::ostream& os, const Gap& gap)^

/* ----- END OF structgapanalysis.cc ----- */
