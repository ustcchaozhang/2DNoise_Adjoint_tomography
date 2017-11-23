/*! \file tsoftreader.cc
 * \brief module to read TSOFT data (implementation)
 * \ingroup group_tsoft
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/09/2009
 * 
 * module to read TSOFT data (implementation)
 * 
 * Copyright (c) 2009 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 16/09/2009   V1.0   Thomas Forbriger
 *  - 26/12/2012   V1.1   skip empty data lines
 * 
 * ============================================================================
 */
#define DATRW_TSOFTREADER_CC_VERSION \
  "DATRW_TSOFTREADER_CC   V1.1"

#include <iostream>
#include <sstream>
#include <datrwxx/error.h>
#include <datrwxx/tsoftreader.h>

using std::cout;
using std::endl;

namespace datrw {

  namespace tsoft {

    const char* TSOFTID="v01.0";

    /*======================================================================*/
    // struct Checklist

    Checklist::Checklist():
      fileid(false), timeformat(false), increment(false), channels(false),
      units(false), undetval(false), data(false)
    {} // Checklist::Checklist()

    /*----------------------------------------------------------------------*/

    void Checklist::report(std::ostream& os) const
    {
      this->reportitem(os, datrw::tsoft::tagfileid, this->fileid);
      this->reportitem(os, datrw::tsoft::tagtimeformat, this->timeformat);
      this->reportitem(os, datrw::tsoft::tagincrement, this->increment);
      this->reportitem(os, datrw::tsoft::tagchannels, this->channels);
      this->reportitem(os, datrw::tsoft::tagunits, this->units);
      this->reportitem(os, datrw::tsoft::tagundetval, this->undetval);
      this->reportitem(os, datrw::tsoft::tagdata, this->data);
    } // void Checklist::report(std::ostream& os) const

    /*----------------------------------------------------------------------*/

    void Checklist::reportitem(std::ostream& os,
                               const char* tag,
                               const bool& flag)
    {
      os << "tag " << tag << " is";
      if (!flag) { os << " NOT"; }
      os << " present" << std::endl;
    } // static void Checklist::reportitem

    /*----------------------------------------------------------------------*/

    bool Checklist::allchecked() const
    {
      return(fileid && timeformat && increment && channels && units 
             && undetval && data);
    } // bool Checklist::allchecked() const

    /*======================================================================*/
    // class TSOFTfile

    void TSOFTfile::read(std::istream& is, const bool& debug)
    {
      int count=0;
      bool indata=false;
      bool inchannels=false;
      bool inunits=false;
      std::string inputline; 
      inputline=getDOSline(is);
      if (debug) { std::cout << "<" << inputline << ">" << endl; }
      while (is.good())
      {
        datrw::tsoft::Line theline(inputline);
        int linelength=theline.theline().length();
        if ((!indata) && (linelength > 0)) 
        { Mfree.push_back(theline.theline()); }
        if (theline.hastag() && (linelength > 0))
        {
          // reset special block processing
          indata=false;
          inchannels=false;
          inunits=false;
          count=0;
          if (theline.thetag() == datrw::tsoft::tagdata) 
          { 
            indata=true; 
            Mchecklist.data=true;
            if (debug)
            { cout << "found data tag " << theline.thetag() << endl; }
          }
          else if (theline.thetag() == datrw::tsoft::tagchannels) 
          { 
            inchannels=true; 
            Mchecklist.channels=true;
            if (debug)
            { cout << "found channels tag " << theline.thetag() << endl; }
          }
          else if (theline.thetag() == datrw::tsoft::tagunits) 
          { 
            inunits=true; 
            Mchecklist.units=true;
            if (debug)
            { cout << "found units tag " << theline.thetag() << endl; }
          }
          else if (theline.thetag() == datrw::tsoft::tagfileid) 
          { 
            Mchecklist.fileid=true;
            DATRW_assert(theline.hascontent(),
                           "missing content in tagged line");
            DATRW_assert(theline.thecontent() == datrw::tsoft::TSOFTID,
                           "file ID is not TSOFT or not supported");
            if (debug)
            { cout << "found fileid tag " << theline.thetag() << endl; }
          }
          else if (theline.thetag() == datrw::tsoft::tagtimeformat) 
          { 
            Mchecklist.timeformat=true;
            DATRW_assert(theline.hascontent(),
                           "missing content in tagged line");
            DATRW_assert(theline.thecontent() == "DATETIME",
                           "time format is not supported");
            if (debug)
            { cout << "found timeformat tag " << theline.thetag() << endl; }
          }
          else if (theline.thetag() == datrw::tsoft::tagundetval) 
          { 
            Mchecklist.undetval=true;
            DATRW_assert(theline.hascontent(),
                           "missing content in tagged line");
            std::istringstream iss(theline.thecontent());
            iss >> Mundetval;
            if (debug)
            { cout << "found undetval tag " << theline.thetag() << endl; }
          }
          else if (theline.thetag() == datrw::tsoft::tagincrement) 
          { 
            Mchecklist.increment=true;
            DATRW_assert(theline.hascontent(),
                           "missing content in tagged line");
            // interval is provided in seconds
            std::istringstream iss(theline.thecontent());
            double dt;
            iss >> dt;
            Mincrement=libtime::double2time(dt);
            if (debug)
            { cout << "found increment tag " << theline.thetag() << endl; }
          }
        }
        else if (indata && (linelength>0))
        {
          if (debug)
          cout << "go for data line #" << count+1 << endl;
          Mdatacontainer.push_data(inputline, Mundetval, Mincrement,
                                   debug);
          ++count;
        }
        else if (inchannels)
        {
          if (debug)
          cout << "go for info for channel #" << count+1 << endl;
          Channelinfo& ci=Mdatacontainer[count].chinfo();
          if (debug)
          {
          cout << "have access to channelinfo for channel " << count+1 << endl;
          cout.flush();
          }
          ci.setchannelinfo(inputline);
          ++count;
          if (debug)
          cout << "read info for channel #" << count << endl;
        }
        else if (inunits)
        {
          Channelinfo& ci=Mdatacontainer[count].chinfo();
          ci.setunits(inputline);
          ++count;
          if (debug)
          cout << "read units for channel #" << count << endl;
        }
        // read next line
        inputline=getDOSline(is);
        if (debug) { std::cout << "<" << inputline << ">" << endl; }
      } // while (is.good())
      // file is read
      Mdatacontainer.flushchannels();
      // check if input is complete
      if (!Mchecklist.allchecked())
      {
        std::cerr << "mandatory fields in input file are missing!" 
          << std::endl;
        Mchecklist.report(std::cerr);
      }
      DATRW_assert(Mchecklist.allchecked(),
                     "mandatory fields are missed");
    } // void TSOFTfile::read(std::istream& is)

  } // namespace tsoft

} // namespace datrw

/* ----- END OF tsoftreader.cc ----- */
