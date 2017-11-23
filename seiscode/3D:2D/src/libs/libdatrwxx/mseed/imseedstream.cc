/*! \file imseedstream.cc
 * \brief provide mini-SEED data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 15/07/2004
 * 
 * provide mini-SEED data (implementation)
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
 *  - 15/07/2004   V1.0   Thomas Forbriger (thof)
 *  - 24/07/2006   V1.1   reading function was not comparing record start
 *                        time against expected record start time
 *  - 29/06/2007   V1.2   prefer to use our own functions
 *  - 23/11/2010   V1.3   introduced static members
 *  - 13/09/2011   V1.4   support format modifiers for ASCII dump
 *  - 08/05/2014   V1.5   add format modifier to tolerate time jitter
 *  - 22/07/2014   V1.6   thof: support new format modifier: estimateNframes
 *  - 05/07/2016   V1.7   thof: evaluate format modifiers to adjust
 *                        consistency checks
 *  - 12/07/2016   V1.8   thof: provide usec consistency check
 *  - 18/11/2016   V1.9   use debug flag in base class
 * 
 * ============================================================================
 */
#define DATRW_IMSEEDSTREAM_CC_VERSION \
  "DATRW_IMSEEDSTREAM_CC   V1.9"

#include <datrwxx/util.h>
#include <datrwxx/mseed.h>
#include <datrwxx/mseedread.h>
#include <aff/subarray.h>
#include <datrwxx/debug.h>
#include <vector>
#include <string>
#include <datrwxx/formatmodifier.h>
#include <datrwxx/mseed_keywords.h>

namespace datrw {

  const std::ios_base::openmode
    imseedstream::openmode=std::ios_base::in|std::ios_base::binary;

  //@{
  /*! \brief Format properties
   * \ingroup group_mseed
   */
  const bool mseed::isbinary=true;
  const char* const mseed::streamID="mseed";
  //@}

  /*======================================================================*/

  /*! \class imseedstream 
   *
   * \ingroup group_mseed
   * \paragraph mseed_imseedstream_internals Internals of imseedtream
   *
   * We must always be one record in advance. Data reading will be done by
   * concatenating several MiniSEED records. A time series will be
   * terminated after reading a noncontiguous MiniSEED record. We have to
   * store this record after reading and thus must always be one record 
   * ahead.
   *
   * Data read ahead is stored in imseedstream::Mrecord which is of type
   * mseed::Record.
   */

  /*----------------------------------------------------------------------*/

  imseedstream::imseedstream(std::istream& is,
                             const std::string& modifier,
                             const bool& debug):
    Tbase(is, true, true, true, debug), Mmodifier(modifier),
    Mdumpascii(false), Mchecks(true, true)
  { 
    datrw::Subformat subformat(Mmodifier);
    Mdumpascii=subformat.isset(mseed::key::dumpascii);
    subformat(mseed::key::ttolerance, "0.") >> this->Mttolerance;
    MestimateNframes=subformat.isset(mseed::key::estimateNframes);

    // note: flags have opposite meaning compared to subformat flags
    if (subformat.isset(mseed::key::nonfatal))
    {
      std::string checks=subformat.value(mseed::key::nonfatal);
      bool all=(checks.find(mseed::key::all)!=std::string::npos);
      Mchecks.nframes.fatal
        =!(all||(checks.find(mseed::key::nframes)!=std::string::npos));
      Mchecks.nsamples.fatal
        =!(all||(checks.find(mseed::key::nsamples)!=std::string::npos));
      Mchecks.data.fatal
        =!(all||(checks.find(mseed::key::data)!=std::string::npos));
      Mchecks.usec.fatal
        =!(all||(checks.find(mseed::key::usec)!=std::string::npos));
    }

    // note: flags have opposite meaning compared to subformat flags
    if (subformat.isset(mseed::key::skipcheck))
    {
      std::string checks=subformat.value(mseed::key::skipcheck);
      bool all=(checks.find(mseed::key::all)!=std::string::npos);
      Mchecks.nframes.docheck
        =!(all||(checks.find(mseed::key::nframes)!=std::string::npos));
      Mchecks.nsamples.docheck
        =!(all||(checks.find(mseed::key::nsamples)!=std::string::npos));
      Mchecks.data.docheck
        =!(all||(checks.find(mseed::key::data)!=std::string::npos));
      Mchecks.usec.docheck
        =!(all||(checks.find(mseed::key::usec)!=std::string::npos));
    }

    DATRW_assert_modifiers_are_recognized(subformat, 
                                          "imseedstream::imseedstream()");

    /* We must always be one record in advance. Data reading will be done by
     * concatenating several MiniSEED records. A time series will be
     * terminated after reading a noncontiguous MiniSEED record. We have to
     * store this record after reading and thus must always be one record 
     * ahead.
     */
    Mrecord.read(Mis, Mdumpascii, MestimateNframes, Mchecks);
    DATRW_debug(Mdebug, "Mrecord read", Mrecord.wid2.line());
  } // imseedstream::imseedstream(std::istream& is, const bool& debug)

  /*----------------------------------------------------------------------*/

  Tfseries imseedstream::fseries()
  {
    return(datrw::util::convert<Tiseries,Tfseries>(this->read()));
  } // Tfseries imseedstream::fseries()

  /*----------------------------------------------------------------------*/

  Tdseries imseedstream::dseries()
  {
    return(datrw::util::convert<Tiseries,Tdseries>(this->read()));
  } // Tdseries imseedstream::dseries()

  /*----------------------------------------------------------------------*/

  Tiseries imseedstream::iseries()
  {
    return(this->read());
  } // Tiseries imseedstream::dseries()

  /*----------------------------------------------------------------------*/

  void imseedstream::skipseries()
  { 
    this->read(true); 
  } // void imseedstream::skipseries()

  /*----------------------------------------------------------------------*/

  /*! \brief read next trace from file.
   *
   * All reading is controlled within this function...
   *
   * File reading always is one record ahead.
   * The record being read in advance is stored in member field
   * imseedstream::Mrecord.
   * This type of operation is necessary, since data data contiguity can only
   * be checked after the next record has been read. 
   * If this record is not contiguous, it must be stored for the next read
   * request, where it will provide the first samples of the next trace.
   *
   * Reading is done as follows:
   *  -# Data records are read from input as long as the next record read is
   *     contiguous to the previous one.
   *     They are intermediately stored in a vector.
   *  -# After having read all contiguous records, the total number of samples
   *     in the trace is established and data samples can be collected in a
   *     series container.
   */
  Tiseries imseedstream::read(const bool& skipdata)
  {
    DATRW_debug(Mdebug, "imseedstream::read", "entered reading function");

    // define vector of records, where input data are collected
    typedef std::vector<Tiseries> Tvecofrecorddata;
    Tvecofrecorddata vecofrecorddata;

    // abort is last record recently read from file is not valid
    // this indicates that the last reading operation arrived at the end of
    // file
    DATRW_assert(Mrecord.valid,
                   "invalid record; passed end of file?");

    /* The record present in this->Mrecord form the beginning of the next
     * trace. Its WID2 header defines the trace to be read. Only records
     * matching this header may be added to the current trace. The this as a
     * base.
     */
    sff::WID2 wid2line=Mrecord.wid2;
    libtime::TRelativeTime dt=libtime::double2time(wid2line.dt);

    /* Define a WID2 comparison instance to check whether WID headers of
     * subsequent record match the header of the total trace being assembled.
     */
    sff::WID2compare wid2areequal(sff::Fdate    
                                  | sff::Fstation
                                  | sff::Fchannel 
                                  | sff::Fauxid   
                                  | sff::Fdt      
                                  | sff::Fcalib   
                                  | sff::Fcalper  
                                  | sff::Finstype 
                                  | sff::Fhang     
                                  | sff::Fvang);  
    // set date tolerance
    wid2areequal.setdatetolerance(this->Mttolerance*1.e-6/wid2line.dt);
     
    /* collect data
     * ------------
     * Read record sequentially from file and check whether the recently read
     * record is part of the trace. Count the number of samples while doing
     * this.
     */
    DATRW_debug(Mdebug, "imseedstream::read", "collect data");

    sff::WID2 nextwid2=wid2line;

    int nsamples=0;
    bool contiguous=true;

    while(Mrecord.valid && Mis.good() && contiguous)
    {
      DATRW_debug(Mdebug, "imseedstream::read", 
                 "save data from record (" 
                 << Mrecord.data.size() << " samples)");
      
      Tiseries data=Mrecord.data;
      if (!skipdata) { vecofrecorddata.push_back(data.copyout()); }
      
      DATRW_debug(Mdebug, "imseedstream::read", "last data index " <<
                 data.last());
      
      /*
       * Remember value of last sample in previous record to be compared to
       * retrospect value derived from the succeeding record. A mismatch will
       * indicate non-contiguous data.
       */
      int xm1=data(data.last());
      nsamples += Mrecord.wid2.nsamples;
      nextwid2.date=wid2line.date+dt*nsamples;
      
      DATRW_debug(Mdebug, "imseedstream::read", "read next");
      
      Mrecord.read(Mis, Mdumpascii, MestimateNframes, Mchecks);

      DATRW_debug(Mdebug, "imseedstream::read", "compare next");
      DATRW_debug(Mdebug, "imseedstream::read", "expected wid2: " <<
                 nextwid2.line());
      DATRW_debug(Mdebug, "imseedstream::read", "    this wid2: " <<
                 Mrecord.wid2.line());
      
      if (Mrecord.valid)
      {
        if (! (wid2areequal(Mrecord.wid2, nextwid2) && (xm1 == Mrecord.xm1)))
        { 
          contiguous=false; 
          DATRW_debug(Mdebug, "imseedstream::read", "non-contiguous data");
        }
        else
        {
          DATRW_debug(Mdebug, "imseedstream::read", "data is still contiguous");
        }
      }
      else
      {
        // reading has met the end of file
        contiguous=false;
        this->setlast();
      }
    }
    wid2line.nsamples=nsamples;

    // extract data samples
    DATRW_debug(Mdebug, "imseedstream::read", "extract data");
    Tiseries series;
    if (!skipdata)
    {
      DATRW_debug(Mdebug, "imseedstream::read", "do not skip");
      Tvecofrecorddata::const_iterator I(vecofrecorddata.begin());
      DATRW_debug(Mdebug, "imseedstream::read", "create buffer");
      series=Tiseries(0,nsamples-1);
      nsamples=0;
      DATRW_debug(Mdebug, "imseedstream::read", "cycle vector");
      while (I != vecofrecorddata.end())
      {
        DATRW_debug(Mdebug, "imseedstream::read", "create dest");
        Tiseries dest=aff::subarray(series)(nsamples,nsamples+I->size()-1);
        DATRW_debug(Mdebug, "imseedstream::read", "copy to dest");
        dest.copyin(*I);
        DATRW_debug(Mdebug, "imseedstream::read", "step nsamples");
        nsamples += I->size();
        ++I;
      }
    }

    // set headers
    this->setwid2(wid2line);
    return (series);
  } // Tiseries imseedstream::read(const bool& skipdata)

} // namespace datrw

/* ----- END OF imseedstream.cc ----- */
