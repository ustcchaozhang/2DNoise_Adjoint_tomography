/*! \file mseed_record_read.cc
 * \brief reading function of class datrw::mseed::Record (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 23/06/2016
 * 
 * reading function of class datrw::mseed::Record (implementation)
 * 
 * Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.

 * ----
 *
 * REVISIONS and CHANGES 
 *  - 23/06/2016   V1.0   Thomas Forbriger
 *  - 05/07/2016   V1.1   pass check flags to MiniSEEDrecord
 * 
 * ============================================================================
 */
#define DATRW_MSEED_RECORD_READ_CC_VERSION \
  "DATRW_MSEED_RECORD_READ_CC   V1.1"

#include <datrwxx/mseed.h>
#include <datrwxx/mseedread.h>

namespace datrw {

  namespace mseed {

    //! read and decode a record to SFF
    void Record::read(std::istream& is, 
                      const bool& dumpascii,
                      const bool& estimateNframes, 
                      const ConsistencyChecks& checks)
    {
      datrw::mseed::MiniSEEDRecord seedrecord;
      seedrecord.checks(checks);
      seedrecord.estimateNframes(estimateNframes);
      seedrecord.debug().report_ascii_data_to_stdout=dumpascii;
      is >> seedrecord;
      this->wid2.date=seedrecord.date();
      this->wid2.station=seedrecord.station();
      this->wid2.channel=seedrecord.channel();
      this->wid2.auxid=seedrecord.network()+":"+seedrecord.location();
      this->wid2.nsamples=seedrecord.nsamples();
      this->wid2.dt=seedrecord.dt();
      this->data=seedrecord.data();
      this->valid=seedrecord.valid();
      this->xm1=seedrecord.xm1();
    } // void Record::read(std::istream& is, const bool& dumpascii)

  } // namespace mseed 

} // namespace datrw

/* ----- END OF mseed_record_read.cc ----- */
