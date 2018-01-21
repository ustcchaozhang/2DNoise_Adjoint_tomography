/*! \file widXio.cc
 * \brief provide WID2 reading/writing with extended format (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/04/2010
 * 
 * provide WID2 reading/writing with extended format (implementation)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * libsffxx is free software; you can redistribute it and/or modify
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
 *  - 13/04/2010   V1.0   Thomas Forbriger
 *  - 03/05/2010   V1.1   
 *                        - provide debug output
 *                        - function nsignificantdigits() now safely takes
 *                          zeroes as input values
 * 
 * ============================================================================
 */
#define TF_WIDXIO_CC_VERSION \
  "TF_WIDXIO_CC   V1.1"

#include <sffxx.h>
#include <sstream>

namespace sff {

  namespace helper {

    //! return number of significant digits
    int nsignificantdigits(double v, const bool& debug=false)
    {
      const double epsilon=1.e-30;
      if (debug) 
      { 
        std::cerr << "DEBUG (nsignificantdigits): "
          << "entered function for value:" << v << std::endl; 
      }
      if (v<0) { v = -v; }
      int n=0;
      if (v<epsilon) 
      {
        n=2;
      }
      else
      {
        double basefactor=pow(10.,-floor(log10(v)));
        double v1,v2;
        do
        { 
          double factor=basefactor*pow(10.,n);
          v1=v*factor;
          v2=floor(v*factor);
          n++;
          if (debug) 
          { 
            std::cerr << "DEBUG (nsignificantdigits): "
              << "basefactor: " << basefactor << " "
              << "factor: " << factor << " "
              << "n: " << n << " "
              << "v1: " << v1 << " "
              << "v2: " << v2 << " "
              << std::endl; 
          }
        }
        while (v1 != v2);
      }
      return (n);
    } // int nsignificantdigits(const double& v)

  } // namespace helper

  /*----------------------------------------------------------------------*/

  /*! \brief write WID information in extended format.
   *
   * This function breaks the SFF format definition.
   * It writes the WID2 class data to a line with identifier WIDX rather than
   * WID2. The function is implemented within this library to provide
   * transparent reading of WID-data. Writing will be used outside of the
   * library only. 
   *
   * Reasons to implement this function are:
   * -# The GSE WID2 format does not use microseconds for the time of the
   *    first sample. The information present in sff::WID2 however is precise
   *    to the level of microseconds and is also used in this way when
   *    processing e.g. MiniSEED data. It is thus desirable to read and write
   *    this information with the full available content.
   * -# The GSE WID2 format restricts the sampling to be defined in terms of a
   *    sampling rate. This is inappropriate to extremely long period data
   *    with e.g. 1 Minute sampling interval (i.e. 60s). Round-off errors will
   *    lead to time residuals in these cases of more than a second within one
   *    day.
   *
   * \sa WIDXline(const std::string& line)
   */
  std::string WIDXline(const sff::WID2& wid2, const bool& debug)
  {
    if (debug) 
    { 
      std::cerr << "DEBUG (WIDXline): entered function" <<
                                      std::endl; 
    }
    std::ostringstream os;
    os << sff::WIDXID;
    os << " " << wid2.date.hierarchicalstring();
    os << " ";
    os.width(5);
    os.setf(std::ios_base::left, std::ios_base::adjustfield);
    os << wid2.station.substr(0,5);
    os << " ";
    os.width(3);
    os.setf(std::ios_base::left, std::ios_base::adjustfield);
    os << wid2.channel.substr(0,3);
    os << " ";
    os.width(4);
    os.setf(std::ios_base::left, std::ios_base::adjustfield);
    os << wid2.auxid.substr(0,4);
    os << " ";
    os.width(6);
    os.setf(std::ios_base::left, std::ios_base::adjustfield);
    os << wid2.instype.substr(0,6);
    os << " " << wid2.nsamples;
    if (debug) 
    { 
      std::cerr << "DEBUG (WIDXline): partial result:" <<
                                      os.str() << std::endl; 
    }
    double rate=1./wid2.dt;
    double value=wid2.dt;
    if (sff::helper::nsignificantdigits(rate) < 4)
    {
      os << " r ";
      value=rate;
    }
    else
    {
      os << " i ";
    }
    if (debug) 
    { 
      std::cerr << "DEBUG (WIDXline): partial result:" <<
                                      os.str() << std::endl; 
    }
    os.setf(std::ios_base::scientific, std::ios_base::floatfield);
    int ndigits=sff::helper::nsignificantdigits(value, debug);
    if (debug) 
    { 
      std::cerr << "DEBUG (WIDXline): ndigits:" <<
                                      ndigits << std::endl; 
    }
    os.precision(ndigits);
    os.width(ndigits+6);
    os << value;
    os << " ";
    os.precision(sff::helper::nsignificantdigits(wid2.calib, debug));
    os << wid2.calib;
    os << " ";
    os.precision(sff::helper::nsignificantdigits(wid2.calper, debug));
    os << wid2.calper;
    os << " ";
    os.precision(sff::helper::nsignificantdigits(wid2.hang, debug));
    os << wid2.hang;
    os << " ";
    os.precision(sff::helper::nsignificantdigits(wid2.vang, debug));
    os << wid2.vang;
    if (debug) 
    { 
      std::cerr << "DEBUG (WIDXline): final result:" <<
                                      os.str() << std::endl; 
    }
    return(os.str());
  } // std::string WIDXline(const sff::WID2& wid2)

  /*----------------------------------------------------------------------*/

  /*! \brief read WID information from extended format.
   *
   * \sa WIDXline(const sff::WID2& wid2)
   */
  sff::WID2 WIDXline(const std::string& line)
  {
    sff::WID2 retval;
    SFF_assert(line.substr(0,4) == WIDXID, 
               "wrong line ID");
    retval.date=libtime::TAbsoluteTime(line.substr(5,26));
    retval.station=line.substr(32,5);
    retval.channel=line.substr(38,3);
    retval.auxid=line.substr(42,4);
    retval.instype=line.substr(47,6);
    std::istringstream is(line.substr(54));
    is >> retval.nsamples;
    std::string type;
    is >> type;
    is >> retval.dt;
    if (type == "r") { retval.dt = 1./retval.dt; }
    is >> retval.calib;
    is >> retval.calper;
    is >> retval.hang;
    is >> retval.vang;
    return(retval);
  } // sff::WID2 WIDXline(const std::string& line)

} // namespace sff

/* ----- END OF widXio.cc ----- */
