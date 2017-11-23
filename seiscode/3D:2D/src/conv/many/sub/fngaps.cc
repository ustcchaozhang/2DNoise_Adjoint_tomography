/*! \file fngaps.cc
 * \brief function gaps() (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/02/2012
 * 
 * function gaps() (implementation)
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
 *  - 25/04/2012   V1.1   collect breaks too
 * 
 * ============================================================================
 */
#define TF_FNGAPS_CC_VERSION \
  "TF_FNGAPS_CC   V1.1"

#include "gapfunctions.h"
#include <tfxx/misc.h>

/*! Take the collection of contiguous data and the earliest and latest time
 * for which data are expected and construct a list of missing samples.
 */
Tvecofgaps gaps(const libtime::TAbsoluteTime& earliest,
                const libtime::TAbsoluteTime& latest,
                const TContiguouslist& cl,
                const bool& debug)
{
  TFXX_debug(debug, "gaps()", "received values: " <<
             TFXX_value(earliest.timestring()));
  TFXX_debug(debug, "gaps()", "received values: " <<
             TFXX_value(latest.timestring()));
  /*
   * The word "stream" here refers to a "data stream" i.e. data comming from
   * the same source as indicated by parameters like channel, station, auxid,
   * and sampling interval. The first three are combined to the streams ID.
   *
   * The algorithm assumes the cl contains a sorted list:
   * 1. contiguous chunks of data for one stream appear in order of increasing
   *    sampling time
   * 2. the chunks of one stream are collected contiguously
   */
  // create return variable
  Tvecofgaps stream;
  // first index in vector to be used
  int istream=0;
  libtime::TAbsoluteTime lastexisting=earliest;
  for (TContiguouslist::const_iterator I=cl.begin(); I!=cl.end(); ++I)
  {
    Gapid currentid(*I);
    TFXX_debug(debug, "gaps()", "current gap ID: " << TFXX_value(*I));
    
    bool firstofstream=false;
    // check whether a new stream starts here
    if (stream.size()<1) 
    {
      firstofstream=true;
      TFXX_debug(debug, "gaps()", "first of all streams");
    }
    else
    {
      if (stream[istream].ID!=currentid) 
      { 
        TFXX_debug(debug, "gaps()", "first of all in this stream");
        firstofstream=true; 
      }
    }

    // initialize new stream if required and find time of last existing sample
    if (firstofstream)
    {
      Gapsofstream newgaps(currentid, earliest, latest);
      // newgaps.ID=currentid;
      stream.push_back(newgaps);
      istream=stream.size()-1;
      lastexisting=earliest-currentid.dt;
      TFXX_debug(debug, "gaps()", "initialized new stream");
    }

    TFXX_debug(debug, "gaps()", 
               TFXX_value(lastexisting.timestring()));

    // set up new gap
    Gap newgap;
    newgap.ID=currentid;
    newgap.first=lastexisting+currentid.dt;
    newgap.last=I->first-currentid.dt;
    TFXX_debug(debug, "gaps()", "new gap (not yet adjusted): " 
               << TFXX_value(newgap));

    if (!(firstofstream && newgap.isbreak()))
    { 
      stream[istream].gap.push_back(newgap); 
      TFXX_debug(debug, "gaps()", "pushed back gap of finite size "
                 << TFXX_value(newgap));
    }

    // check if this is the last entry for this stream
    bool lastofstream=false;
    TContiguouslist::const_iterator J=I;
    ++J;
    if (J==cl.end()) 
    {
      lastofstream=true; 
    }
    else
    {
      Gapid nextid(*J);
      if (nextid!=currentid) { lastofstream=true; }
    }
    if (lastofstream)
    {
      newgap.first=I->last+currentid.dt;
      newgap.last=latest;
      if (newgap.last>=newgap.first) 
      {
        stream[istream].gap.push_back(newgap); 
        TFXX_debug(debug, "gaps()", "pushed back gap of finite size "
                   << TFXX_value(newgap));
      }
    }
    lastexisting=I->last;;
  } // for (TContiguouslist::const_iterator I=cl.begin(); I!=cl.end(); ++I)
  TFXX_debug(debug, "gaps()", "finished...");
  return(stream);
} // Tvecofgaps gaps(const libtime::TAbsoluteTime& earliest,...

/* ----- END OF fngaps.cc ----- */
