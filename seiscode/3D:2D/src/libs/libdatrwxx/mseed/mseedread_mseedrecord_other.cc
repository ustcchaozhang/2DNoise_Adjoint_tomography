/*! \file mseedread_mseedrecord_other.cc
 * \brief various collected member functions of mseed::MiniSEEDRecord (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 23/06/2016
 * 
 * various collected member functions of mseed::MiniSEEDRecord (implementation)
 * 
 * Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
 * 
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
 *  - 12/07/2016   V1.1   thof: uniquely specify sign of correction
 * 
 * ============================================================================
 */
#define DATRW_MSEEDREAD_MSEEDRECORD_OTHER_CC_VERSION \
  "DATRW_MSEEDREAD_MSEEDRECORD_OTHER_CC   V1.1"

#include <datrwxx/mseedread.h>
#include<aff/subarray.h>

namespace datrw {

  namespace mseed {

    /*----------------------------------------------------------------------*/

    /*! \brief Return actual data samples stored in object.
     *
     * The container MiniSEEDRecord::Mdata can be larger than the actual
     * samples stored therein.
     * Extract as many samples as being indicated by to be read from file.
     *
     * \note
     *   The function returns an aff::Series object, which has reference
     *   semantics. 
     *   As a consequence sample values in the returned object will be altered
     *   upon reading more samples with the same MiniSEEDRecord object.
     */
    MiniSEEDRecord::Tseries MiniSEEDRecord::data() const
    {
      Tseries retval;
      if (this->valid())
      {
        retval=aff::subarray(Mdata)(Mrecordheader.nsamp-1);
      }
      return(retval);
    } // MiniSEEDRecord::Tseries MiniSEEDRecord::data() const

    /*----------------------------------------------------------------------*/

    libtime::TAbsoluteTime MiniSEEDRecord::date() const
    {
      libtime::TAbsoluteTime retval=convert(this->recordheader().stime);
      datrw::mseed::SEED::ActivityFlags aflags(this->recordheader().aflags);
      if (! (aflags.tcorrapp || (this->recordheader().tcorr == 0) ) )
      {
        libtime::TRelativeTime corr(0,0,0,0,0,100);
        //long int tcorr=this->recordheader().tcorr;
        int tcorr=this->recordheader().tcorr;
        if (tcorr >= 0)
        {
          retval += (corr * tcorr);
        }
        else
        {
          // result of multiplication of libtime::TAbsoluteTime with
          // any value (also negative) provides a positive result;
          // swap sign just to make things clear
          tcorr *= -1;
          retval -= (corr * tcorr);
        }
      }
      if (this->hasblockette1001())
      {
        int tcorr=this->blockette1001().iusec();
        if (tcorr != 0)
        {
          libtime::TRelativeTime corr(0,0,0,0,0,1);
          if (tcorr >= 0)
          {
            retval += (corr * tcorr);
          }
          else
          {
            // result of multiplication of libtime::TAbsoluteTime with
            // any value (also negative) provides a positive result;
            // swap sign just to make things clear
            tcorr *= -1;
            retval -= (corr * tcorr);
          }
        }
      }
      return(retval);
    } // libtime::TAbsoluteTime MiniSEEDRecord::date() const

    /*----------------------------------------------------------------------*/

    double MiniSEEDRecord::dt() const
    {
      return(samplinginterval(this->recordheader().srate,
                              this->recordheader().srmult));
    } // double MiniSEEDRecord::dt() const

  } // namespace mseed

} // namespace datrw

/* ----- END OF mseedread_mseedrecord_other.cc ----- */
