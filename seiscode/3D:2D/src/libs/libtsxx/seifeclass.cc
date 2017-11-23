/*! \file seifeclass.cc
 * \brief provide all needed to use BasicFilter with seife code (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 20/05/2015
 * 
 * provide all needed to use BasicFilter with seife code (implementation)
 * 
 * Copyright (c) 2005, 2015 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 04/07/2005   V1.0   Thomas Forbriger
 *  - 11/07/2005   V1.1   
 *                        - support debug mode
 *                        - provide usage information
 *  - 18/12/2007   V1.2   
 *                        - support debugging
 *                        - replace comma delimiter by whitespace
 *  - 20/05/2015   V1.3
 *                        - take usage text from text file
 * 
 * ============================================================================
 */
#define TF_SEIFECLASS_CC_VERSION \
  "TF_SEIFECLASS_CC   V1.3"

#include<sstream>
#include<algorithm>
#include<tsxx/seifexx.h>
#include<tsxx/seifeclass.h>
#include<tsxx/seifeclass_usage_text.h>
#include<tfxx/misc.h>

namespace ts {

  namespace seife {

    /*! Butterworth lowpass (period t0, order o) */
    ts::filter::Ttimeseries LPB::operator()(const Ttimeseries& s,
                                            const bool& debug) const
      { lpb(s, s.header.dt, Mt0, Mo); return s; }

    /*! Butterworth highpass (period t0, order o) */
    ts::filter::Ttimeseries HPB::operator()(const Ttimeseries& s,
                                            const bool& debug) const
      { hpb(s, s.header.dt, Mt0, Mo); return s; }

    /*! 2nd order lowpass (period t0, damping h) */
    ts::filter::Ttimeseries LP2::operator()(const Ttimeseries& s,
                                            const bool& debug) const
      { lp2(s, s.header.dt, Mt0, Mh); return s; }

    /*! 2nd order highpass (period t0, damping h) */
    ts::filter::Ttimeseries HP2::operator()(const Ttimeseries& s,
                                            const bool& debug) const
      { hp2(s, s.header.dt, Mt0, Mh); return s; }

    /*! 2nd order bandpass (period t0, damping h) */
    ts::filter::Ttimeseries BP2::operator()(const Ttimeseries& s,
                                            const bool& debug) const
      { bp2(s, s.header.dt, Mt0, Mh); return s; }

    /*! 1st order lowpass (period t0) */
    ts::filter::Ttimeseries LP1::operator()(const Ttimeseries& s,
                                            const bool& debug) const
      { lp1(s, s.header.dt, Mt0); return s; }

    /*! 1st order highpass (period t0) */
    ts::filter::Ttimeseries HP1::operator()(const Ttimeseries& s,
                                            const bool& debug) const
      { hp1(s, s.header.dt, Mt0); return s; }

    /*! integration (time constant t0) */
    ts::filter::Ttimeseries INT::operator()(const Ttimeseries& s,
                                            const bool& debug) const
      { integrate(s, s.header.dt, Mt0); return s; }

    /*! 1st order highpass equalizer (former period t0s, new period t0) */
    ts::filter::Ttimeseries HE1::operator()(const Ttimeseries& s,
                                            const bool& debug) const
      { he1(s, s.header.dt, Mt0s, Mt0); return s; }

    /*! 1st order lowpass equalizer (former period t0s, new period t0) */
    ts::filter::Ttimeseries LE1::operator()(const Ttimeseries& s,
                                            const bool& debug) const
      { le1(s, s.header.dt, Mt0s, Mt0); return s; }

    /*! 2nd order highpass equalizer (former: period t0s and damping hs,
     *                               new: period t0 and damping h) 
     */
    ts::filter::Ttimeseries HE2::operator()(const Ttimeseries& s,
                                            const bool& debug) const
      { 
        if (debug) { ts::seife::debug_mode_on(); }
        he2(s, s.header.dt, Mt0s, Mhs, Mt0, Mh); 
        return s; 
      }

    /*! 2nd order lowpass equalizer (former: period t0s and damping hs,
     *                              new: period t0 and damping h) 
     */
    ts::filter::Ttimeseries LE2::operator()(const Ttimeseries& s,
                                            const bool& debug) const
      { le2(s, s.header.dt, Mt0s, Mhs, Mt0, Mh); return s; }

    /*! detide with synthetic tides interpolated over ni samples */
    ts::filter::Ttimeseries TID::operator()(const Ttimeseries& s,
                                            const bool& debug) const
      { tid(s, s.header.dt, Mni); return s; }

    /*! derivative (time constant t0) */
    ts::filter::Ttimeseries DIF::operator()(const Ttimeseries& s,
                                            const bool& debug) const
      { dif(s, s.header.dt, Mt0); return s; }

    /*! set baseline to first value */
    ts::filter::Ttimeseries FIRST::operator()(const Ttimeseries& s,
                                              const bool& debug) const
      { first(s); return s; }

    /*----------------------------------------------------------------------*/

    //! function to generate filter class
    ts::filter::Tfilterhandle make_seife_filter(std::string s,
                                                const bool& debug)
    {
      std::replace(s.begin(),s.end(),',',' ');
      typedef ts::filter::Tfilterhandle Tfh;
      TFXX_debug(debug, "make_seife_filter", "definition: " << s);
      Tfh fh(new ts::filter::Noop());
      std::string ID;
      std::istringstream is(s);
      is >> ID;
      TFXX_debug(debug, "make_seife_filter", "ID: " << ID);
      ts::seife::Tvalue t0, t0s, h, hs;
      int ni, o;
      if (ID=="lpb") {
        is >> t0 >> o;
        fh=Tfh(new LPB(t0, o));
      } else if (ID=="hpb") {
        is >> t0 >> o;
        fh=Tfh(new HPB(t0, o));
      } else if (ID=="lp2") {
        is >> t0 >> h;
        fh=Tfh(new LP2(t0, h));
      } else if (ID=="hp2") {
        is >> t0 >> h;
        fh=Tfh(new HP2(t0, h));
      } else if (ID=="bp2") {
        is >> t0 >> h;
        fh=Tfh(new BP2(t0, h));
      } else if (ID=="lp1") {
        is >> t0;
        fh=Tfh(new LP1(t0));
      } else if (ID=="hp1") {
        is >> t0;
        fh=Tfh(new HP1(t0));
      } else if (ID=="le1") {
        is >> t0s >> t0;
        fh=Tfh(new LE1(t0s, t0));
      } else if (ID=="he1") {
        is >> t0s >> t0;
        fh=Tfh(new HE1(t0s, t0));
      } else if (ID=="le2") {
        is >> t0s >> hs >> t0 >> h;
        fh=Tfh(new LE2(t0s, hs, t0, h));
      } else if (ID=="he2") {
        is >> t0s >> hs >> t0 >> h;
        TFXX_debug(debug, "make_seife_filter", 
                   "he2: " 
                   << "t0s=" << t0s
                   << "hs=" << hs
                   << "t0=" << t0
                   << "h=" << h);
        fh=Tfh(new HE2(t0s, hs, t0, h));
      } else if (ID=="int") {
        is >> t0;
        fh=Tfh(new INT(t0));
      } else if (ID=="dif") {
        is >> t0;
        fh=Tfh(new DIF(t0));
      } else if (ID=="tid") {
        is >> ni;
        fh=Tfh(new TID(ni));
      } else if (ID=="first") {
        fh=Tfh(new FIRST());
      } else if (ID=="DBG") {
        debug_mode_on(); 
      } else {
        TSXX_UnknownFilterAbort("ts::seife::make_seife_filter", ID);
      }
      return(fh);
    }

    //! print usage information
    void print_help(std::ostream& os)
    {
      os << TF_SEIFECLASS_CC_VERSION << std::endl;
      os << seifeclass_usage_text;
    }

  } // namespace seife

} // namespace ts

/* ----- END OF seifeclass.cc ----- */
