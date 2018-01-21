/*! \file sffverbose.cc
 * \brief dump SFF headers in human readable from (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 26/06/2007
 * 
 * dump SFF headers in human readable from (implementation)
 * 
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 26/06/2007   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_SFFVERBOSE_CC_VERSION \
  "TF_SFFVERBOSE_CC   V1.0   "

#include <iomanip>
#include <sffxx.h>

namespace sff {
  
  namespace helper {
    // my output format
    struct MyFormat {
      int width;
      int precision;
      bool scientific;
      bool showpos;
      bool showpoint;
      MyFormat():
        width(15), precision(4), scientific(true), 
        showpos(true), showpoint(true)
        { }
      MyFormat operator()(const int& w, const int& p, const bool& s=true) const
      { 
        MyFormat retval=*this;
        retval.width=w;
        retval.precision=p;
        retval.scientific=s;
        return retval;
      }
    }; // struct MyFormat

    /*----------------------------------------------------------------------*/

    // use MyFormat as output manipulator
    std::ostream& operator<<(std::ostream& os, const MyFormat& format)
    {
      os << std::setw(format.width);
      os << std::setprecision(format.precision);
      if (format.scientific) { os << std::scientific; } 
      else { os << std::fixed; }
      if (format.showpos) { os << std::showpos; } 
      else { os << std::noshowpos; }
      if (format.showpoint) { os << std::showpoint; } 
      else { os << std::noshowpoint; }
      return os;
    } // std::ostream& operator<<(std::ostream& os, const MyFormat& format)

    /*----------------------------------------------------------------------*/

    // yes/no
    void yesno(std::ostream& os, const bool& f)
    {
      if (f) { os << "YES"; } else { os << "NO"; }
      os << std::endl;
    } // void yesno(std::ostream& os, const bool& f)

  } // namespace helper

  /*======================================================================*/

  void verbose(std::ostream& os, const WID2& wid2)
  {
    sff::helper::MyFormat format;
    sff::helper::MyFormat npformat;
    npformat.showpos=false;
    os << "contents of SFF WID2 line:" << std::endl;
    os << "  date and time of first sample: "
      << wid2.date.timestring() << std::endl;
    os << "  station code:    "
      << wid2.station << std::endl;
    os << "  channel code:    "
      << wid2.channel << std::endl;
    os << "  auxilliary code: "
      << wid2.auxid << std::endl;
    os << "  instrument code: "
      << wid2.instype << std::endl;
    os << "  number of samples: "
      << npformat(11,7,false) << wid2.nsamples << std::endl;
    os << "  sampling interval: "
      << npformat(11,7,false) << wid2.dt << " s" << std::endl;
    os << "  calibration factor: "
      << format(10,7,false) << wid2.calib << std::endl;
    os << "  calibration period: "
      << format(10,7,false) << wid2.calper << std::endl;
    os << "  horizontal orientation clockwise from north (hang): "
      << format(10,7,false) << wid2.hang << " deg" << std::endl;
    os << "  vertical orientation from vertical (vang): "
      << format(19,7,false) << wid2.vang << " deg" << std::endl;
  } // void verbose(std::ostream& os, const WID2& wid2)

  /*----------------------------------------------------------------------*/
  
  void verbose(std::ostream& os, const SRCE& srce)
  {
    sff::helper::MyFormat format;
    os << "contents of SFF SRCE line:" << std::endl;
    os << "  type of source (type): "
      << srce.type << std::endl;
    os << "  coordinate system (cs): ";
    switch (srce.cs) {
      case CS_cartesian: 
        os << "cartesian";
        break;
      case CS_spherical: 
        os << "spherical";
        break;
    }
    os << std::endl;
    switch (srce.cs) {
      case CS_cartesian: 
        os << "  X-coordinate (c1):" 
          << format(19,8) << srce.cx << " m" << std::endl;;
        os << "  Y-coordinate (c2):" 
          << format(19,8) << srce.cy << " m" << std::endl;;
        os << "  Z-coordinate (c3):" 
          << format(19,8) << srce.cz << " m" << std::endl;;
        break;
      case CS_spherical: 
        os << "spherical";
        os << "  latitude (c1): " 
          << format(19,8) << srce.cx << " deg" << std::endl;;
        os << "  longitude (c2):" 
          << format(19,8) << srce.cy << " deg" << std::endl;;
        os << "  height (c3):   " 
          << format(19,8) << srce.cz << " m" << std::endl;;
        break;
    }
    os << "  source date and time: "
      << srce.date.timestring() << std::endl;
  } // void verbose(std::ostream& os, const SRCE& srce)

  /*----------------------------------------------------------------------*/
  
  void verbose(std::ostream& os, const DAST& dast)
  {
    sff::helper::MyFormat format;
    os << "contents of SFF DAST line:" << std::endl;
    os << "  number of characters in encoded data set (nchar):"
      << format(8,8) << dast.nchar << std::endl;
    os << "  scaling factor (ampfac):" 
      << format(20,10) << dast.ampfac << std::endl;
    os << "  trace has FREE block (code):" << format(8,10);
    helper::yesno(os, dast.hasfree);
    os << "  trace has INFO line (code):" << format(9,10); 
    helper::yesno(os, dast.hasinfo);
    os << "  is last trace in file (code):" << format(7,10); 
    helper::yesno(os, dast.last);
  } // void verbose(std::ostream& os, const DAST& dast)

  /*----------------------------------------------------------------------*/
  
  void verbose(std::ostream& os, const INFO& info)
  {
    sff::helper::MyFormat format;
    os << "contents of SFF INFO line:" << std::endl;
    os << "  coordinate system (cs): ";
    switch (info.cs) {
      case CS_cartesian: 
        os << "cartesian";
        break;
      case CS_spherical: 
        os << "spherical";
        break;
    }
    os << std::endl;
    switch (info.cs) {
      case CS_cartesian: 
        os << "  X-coordinate (c1):" 
          << format(19,8) << info.cx << " m" << std::endl;;
        os << "  Y-coordinate (c2):" 
          << format(19,8) << info.cy << " m" << std::endl;;
        os << "  Z-coordinate (c3):" 
          << format(19,8) << info.cz << " m" << std::endl;;
        break;
      case CS_spherical: 
        os << "spherical";
        os << "  latitude (c1): " 
          << format(19,8) << info.cx << " deg" << std::endl;;
        os << "  longitude (c2):" 
          << format(19,8) << info.cy << " deg" << std::endl;;
        os << "  height (c3):   " 
          << format(19,8) << info.cz << " m" << std::endl;;
        break;
    }
    os << "  number of stacks (nstacks):" 
      << format(8,5) << info.nstacks << std::endl;
  } // void verbose(std::ostream& os, const INFO& info)

  /*----------------------------------------------------------------------*/
  
  void verbose(std::ostream& os, const FREE& free)
  {
    os << "contents of SFF FREE block:" << std::endl;
    sff::FREE::Tlines::const_iterator I=free.lines.begin();
    while (I != free.lines.end())
    {
      os << "  " << *I << std::endl;;
      ++I;
    }
  } // void verbose(std::ostream& os, const FREE& free)

  /*----------------------------------------------------------------------*/
  
  void verbose(std::ostream& os, const STAT& stat)
  {
    sff::helper::MyFormat format;
    os << "contents of SFF STAT line:" << std::endl;
    os << "  time stamp: " 
      << stat.timestamp << std::endl;
    os << "  file has FREE block (code):" << format(4,3);
    helper::yesno(os, stat.hasfree);
    os << "  file has SRCE line (code):" << format(5,3); 
    helper::yesno(os, stat.hassrce);
  } // void verbose(std::ostream& os, const STAT& stat)

  /*----------------------------------------------------------------------*/
  
  void verbose(std::ostream& os, const FileHeader& fh)
  {
    os << "contents of SFF file header:" << std::endl;
    verbose(os,fh.stat());
    if (fh.hassrce()) { verbose(os,fh.srce()); }
    else { os << "file header contains no SRCE line" << std::endl; }
    if (fh.hasfree()) { verbose(os,fh.free()); }
    else { os << "file header contains no FREE block" << std::endl; }
  } // void verbose(std::ostream& os, const FileHeader& fh)

  /*----------------------------------------------------------------------*/
  
  void verbose(std::ostream& os, const TraceHeader& th)
  {
    os << "contents of SFF trace header:" << std::endl;
    verbose(os,th.dast());
    verbose(os,th.wid2());
    if (th.hasinfo()) { verbose(os,th.info()); }
    else { os << "trace header contains no INFO line" << std::endl; }
    if (th.hasfree()) { verbose(os,th.free()); }
    else { os << "trace header contains no FREE block" << std::endl; }
  } // void verbose(std::ostream& os, const TraceHeader& th)

} // namespace sff

/* ----- END OF sffverbose.cc ----- */
