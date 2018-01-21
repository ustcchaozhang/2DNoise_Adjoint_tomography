/*! \file structgapanalysis.h
 * \brief structs used in primary gap analysis (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/02/2012
 * 
 * structs used in primary gap analysis (prototypes)
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
 *  - 25/04/2012   V1.1   distinguish between breaks and gaps
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_STRUCTGAPANALYSIS_H_VERSION

#define TF_STRUCTGAPANALYSIS_H_VERSION \
  "TF_STRUCTGAPANALYSIS_H   V1.1"

#include <tfxx/stringfunc.h>
#include <libtime++.h>
#include <list>
#include <vector>
#include <iostream>

/*! \brief Indicate a contiguous set of data.
 */
struct Contiguous {
  libtime::TAbsoluteTime first, last;
  libtime::TRelativeTime dt;
  std::string station, channel, auxid;
}; // struct Contiguous

/*! \brief A list to store information on all sequences of contiguous data.
 */
typedef std::list<Contiguous> TContiguouslist;

/*! \brief ID to identify stream.
 *
 * Holds data uniquely indetifying a data stream or channel.
 * Supports comparison in order to check whether two chunks of data belong to
 * the same stream.
 *
 * station, channel, and auxid strings are stored separately since they are
 * used to build the header of completeness time series and therefore will be
 * passed to the customer of these functions and modules.
 */
struct Gapid {
  Gapid() { }
  Gapid(const Contiguous& c):
    ID(tfxx::string::trimws(c.channel)
       +":"+tfxx::string::trimws(c.station)
       +":"+tfxx::string::trimws(c.auxid)), 
    dt(c.dt), Mstation(c.station), Mchannel(c.channel), Mauxid(c.auxid)
  { }
  bool operator==(const Gapid& other) const
  { return((this->ID == other.ID) && (this->dt == other.dt)); }
  bool operator!=(const Gapid& other) const
  { return(!this->operator==(other)); }
  std::string ID;
  libtime::TRelativeTime dt;
  std::string station() const { return Mstation; }
  std::string channel() const { return Mchannel; }
  std::string auxid() const { return Mauxid; }
  private:
  std::string Mstation, Mchannel, Mauxid;
}; // struct Gapid

/*----------------------------------------------------------------------*/

//! struct to hold one gap
struct Gap {
  // true, if no sample is missing (just a break in contiguous data)
  bool isbreak() const { return (last<=(first+ID.dt)); }
  // time span from first missing sample to last missing sample + one sample
  // interval (represents gaps size in terms of missing sample intervals)
  libtime::TRelativeTime tmissing() const 
  { 
    libtime::TRelativeTime retval(0);
    if (!this->isbreak()) { retval=last-first+ID.dt; }
    return(retval); 
  }
  // number of missing samples, taking irregular sampling into account
  unsigned int nmissing() const 
  { 
    unsigned int retval=0;
    if (!this->isbreak()) { retval=(this->tmissing()+(ID.dt/2))/ID.dt; }
    return retval;
  }
  // data
  libtime::TAbsoluteTime first, last;
  Gapid ID;
}; // struct Gap

/*----------------------------------------------------------------------*/

//! struct to present a summary
struct Gapsummary {
  Gapsummary(const libtime::TAbsoluteTime& earliest,
             const libtime::TAbsoluteTime& latest,
             const libtime::TRelativeTime& indt)
    : ngaps(0), nbreaks(0), nmissing(0), nexpected(1+((latest-earliest)/indt)),
    tspan(latest-earliest), tmissing(0), dt(indt)
  { }
  //! total number of gaps
  unsigned int ngaps;
  //! total number of breaks
  unsigned int nbreaks;
  //! number of missing samples
  unsigned int nmissing;
  //! number of expected samples
  unsigned int nexpected;
  //! time span of analysis
  libtime::TRelativeTime tspan;
  //! time span not covered with samples
  libtime::TRelativeTime tmissing;
  //! sampling interval
  libtime::TRelativeTime dt;
}; // struct Gapsummary

/*----------------------------------------------------------------------*/

//! struct to hold gaps of one stream
struct Gapsofstream {
  typedef std::vector<Gap> Tvecofgap;
  Gapsofstream(const Gapid& id,
               const libtime::TAbsoluteTime& earliest,
               const libtime::TAbsoluteTime& latest)
    : ID(id), Mearliest(earliest), Mlatest(latest)
  {
    this->gap.clear(); 
  }
  //! \brief provide a summary for this stream
  Gapsummary summarize() const;
  Gapid ID;
  Tvecofgap gap;
  private: 
    libtime::TAbsoluteTime Mearliest;
    libtime::TAbsoluteTime Mlatest;
}; // struct Gapsofstream 

/*----------------------------------------------------------------------*/

//! vector to hold all gaps
typedef std::vector<Gapsofstream> Tvecofgaps;

/*----------------------------------------------------------------------*/
// output operators
  
//! print stream ID
std::ostream& operator<<(std::ostream& os, const Gapid& id);

//! print contiguous chunk definition
std::ostream& operator<<(std::ostream& os, const Contiguous& chunk);

//! print gaps
std::ostream& operator<<(std::ostream& os, const Gap& gap);

#endif // TF_STRUCTGAPANALYSIS_H_VERSION (includeguard)

/* ----- END OF structgapanalysis.h ----- */
