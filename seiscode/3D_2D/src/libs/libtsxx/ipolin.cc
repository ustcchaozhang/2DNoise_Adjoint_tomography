/*! \file ipolin.cc
 * \brief linear interpolation (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/07/2005
 * 
 * linear interpolation (implementation)
 * 
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
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
 * REVISIONS and CHANGES 
 *  - 13/07/2005   V1.0   Thomas Forbriger
 *  - 14/11/2017   V1.1   fix index value error in calculation of sample time
 * 
 * ============================================================================
 */
#define TF_IPOLIN_CC_VERSION \
  "TF_IPOLIN_CC   V1.1"

#include<iostream>
#include <tsxx/ipolin.h>
#include <tsxx/debug.h>

namespace ts {

  namespace ipo {

    /*! \brief initialize the linear interpolator
     */
    LinearInterpolator::LinearInterpolator(const Tconst_timeseries& ts,
                                           const bool& debug): 
      Tbase(ts, debug), 
      Mlast(sff::wid2lastsample(Mts.header)),
      Mdt(libtime::double2time(Mts.header.dt)),
      Madjustedfirst(Mts.header.date+(Mdt/2)),
      Minputwindow(Mts.header.date,Mlast)
      { 
        TSXX_debug(this->debug(), 
                   "LinearInterpolator::LinearInterpolatorperator(...):",
                   TSXX_value(ts.first()) << " " <<
                   TSXX_value(ts.last()) << " " 
                   << std::endl <<
                   "         " << TSXX_value(Mts.header.date.timestring())
                   << std::endl <<
                   "                   " << TSXX_value(Mlast.timestring())
                   << std::endl <<
                   "                     " << TSXX_value(Mdt.timestring())
                   << std::endl <<
                   "          " << TSXX_value(Madjustedfirst.timestring())
                  );
      } // LinearInterpolator::LinearInterpolator(...)

    /*----------------------------------------------------------------------*/

    /*! \brief calculate interpolated value
     *
     * \param t time for value to return
     * \return new sample value at time t
     */
    LinearInterpolator::Tvalue 
      LinearInterpolator::operator()(const libtime::TAbsoluteTime& t) const
    {
      using libtime::TAbsoluteTime;
      using libtime::TRelativeTime;
      const Tconst_series& s=this->Mts;
      const Theader& hd=this->Mts.header;
      //aff::Tsubscript ileft=s.f()+int(libtime::time2double(t-first)/hd.dt);
      /*
       * Notice that the division in libtime is defined to return the number
       * of samples that will come closest to the time interval. That means
       * that the operator round to the nearest integer and not to the lower
       * integer. For this reason we use the adjusted time of first sample.
       */
      aff::Tsubscript ileft=s.first()+(t-Madjustedfirst)/Mdt;
      TSXX_assert((Minputwindow.includes(t) && (ileft<=s.last())),
                  "requested sample lies outside time series");
      aff::Tsubscript iright= (ileft < s.last()) ? ileft+1 : ileft;
      double f=libtime::time2double(t-sff::wid2isample(hd, ileft-s.f()))/hd.dt;
      TSXX_debug(this->debug(), "LinearInterpolator::operator():",
           TSXX_value(ileft) << " " <<
           TSXX_value(iright) << " " <<
           TSXX_value(s.f()) << " " <<
           TSXX_value(f) << " " <<
           TSXX_value(hd.dt) << std::endl <<
           "                                " << TSXX_value(Mdt.timestring())
           << std::endl <<
           "                     " << TSXX_value(Madjustedfirst.timestring())
           << std::endl <<
           "  " << TSXX_value(sff::wid2isample(hd, ileft-s.f()).timestring())
           << std::endl <<
           "                                  " << TSXX_value(t.timestring())
           << std::endl <<
           " " << TSXX_value(sff::wid2isample(hd, iright-s.f()).timestring())
           << std::endl <<
           "                 " << TSXX_value((t-Madjustedfirst).timestring())
           << std::endl <<
           "                 " << TSXX_value((t-Madjustedfirst)/Mdt)
                );
      Tvalue retval=((1.-f)*s(ileft)+f*s(iright));
      return(retval);
    }

  } // namespace ipo

} // namespace ts

/* ----- END OF ipolin.cc ----- */
