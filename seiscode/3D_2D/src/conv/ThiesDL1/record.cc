/*! \file record.cc
 * \brief class to represent a data record (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 25/11/2008
 * 
 * class to represent a data record (implementation)
 * 
 * Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
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
 * 
 * REVISIONS and CHANGES 
 *  - 25/11/2008   V1.0   Thomas Forbriger (thof)
 *  - 22/12/2008   V1.1   
 *                        - tolerance modes log weird lines and set flags
 *                        - series is now filled upon reading data record
 *  - 20/03/2014 thof:    
 *                        modifications applied to class dl1::Record and
 *                        member function dl1::Record::put
 *                        - discard Mbetolerantagainstredundant
 *                        - accept duplicate time stamps as part of normal
 *                          operation (see \ref page_thiesdl1_duplicates)
 *                        - add sample values for duplicate time stamps
 *  - 24/03/2014 thof:
 *                        - avoid tsioxx modules
 *  - 31/03/2014 thof:
 *                        - report DataLine::gain in Record::header()
 *                        - report DL1 logger software version
 * 
 * ============================================================================
 */
#define DL_RECORD_CC_VERSION \
  "DL_RECORD_CC   2014-03-31"
#define DL_RECORD_CC_CVSID \
  "$Id$"

#include <aff/seriesoperators.h>
#include "record.h"
#include "logger.h"
#include "thiesdl1.h"
#include "version.h"

namespace dl1 {

  bool Record::Mbetolerantagainstwrongtime=false;

  const char* const precipitationID="WR1";

  void ExceptionRecordWindow::report() const {
    this->Exception::report();
    this->my_report();
  } // void ExceptionInconsistentLine::report() const

  /*----------------------------------------------------------------------*/

  void ExceptionRecordWindow::my_report() const {
    Logger(log_err)  << "  earliest in expected window: " <<
      this->Mearliest.timestring();
    Logger(log_err)  << "  latest in expected window: " <<
      this->Mlatest.timestring();
    Logger(log_err)  << "  DL1 data line: " << this->Mdataline;
  } // void ExceptionInconsistentLine::my_report() const

  /*======================================================================*/

  Record::Record(const libtime::TAbsoluteTime& earliest,
                 const libtime::TAbsoluteTime& latest):
    Mearliest(earliest), Mlatest(latest), 
    Mfoundunexpecteddatatime(false)
  {
    Mcreation=libtime::utc();
    Mdatalist.clear();
    Mlogmessages.clear();
    Mnsamples=(this->Mlatest-this->Mearliest)/dl1samplinginterval;
    Miseries=Tiseries(Mnsamples);
    Miseries=0;
    Mfilled=aff::Series<bool>(Mnsamples);
    Mfilled=false;
  } // Record::Record

  /*----------------------------------------------------------------------*/

#define DL1_rcassert( C, M, E, L, N ) \
  if (!(C)) { throw( dl1::ExceptionRecordWindow( M , __FILE__, \
                                                     __LINE__, #C, \
                                                 E, L, N)); }

  void Record::setinitial(const std::string& line)
  {
    DL1_rcassert(line.substr(0,6)=="Data :",
                 "invalid line identifier", 
                 this->Mearliest, this->Mlatest,
                 line);
    Minitial=line;
  } // void Record::setinitial(const std::string& line)

  /*----------------------------------------------------------------------*/

  void Record::setfinal(const std::string& line)
  {
    DL1_rcassert(line.substr(0,11)=="END OF DATA",
                 "invalid line identifier", 
                 this->Mearliest, this->Mlatest,
                 line);
    Mfinal=line;
  } // void Record::setfinal(const std::string& line)

  /*----------------------------------------------------------------------*/

  void Record::put(const DataLine& line)
  {
    std::ostringstream oss;
    // remember this line in any case
    Mdatalist.push_back(line);
    // check the time value against the expected time range 
    if (!Mbetolerantagainstwrongtime)
    {
      DL1_rcassert ((line.time()>=this->Mearliest)
          && (line.time()<=this->Mlatest),
          "sample does not fit in expected window",
           this->Mearliest, this->Mlatest,
           line.line());
    }
    else
    {
      if ((line.time()<this->Mearliest)
          || (line.time()>this->Mlatest))
      {
        Logger(log_err) << "sample does not fit in expected window";
        Logger(log_err)  << "  earliest in expected window: " <<
          this->Mearliest.timestring();
        Logger(log_err)  << "  latest in expected window: " <<
          this->Mlatest.timestring();
        Logger(log_err)  << "  DL1 data line: " << line.line();
        oss.clear();
        oss.str("");
        oss << "ERROR: data line with weird time: " << line.line();
        Mlogmessages.push_back(oss.str());
        Mfoundunexpecteddatatime=true;
      }
    }
    // evaluate value and fill my record of samples
    // this uses libtime nfit, which provides safe rounding
    int i=(line.time()-this->Mearliest)/dl1samplinginterval;
    if (!Record::Mbetolerantagainstwrongtime)
    {
      DL1_rcassert (i>=0 && i<Mnsamples,
          "sample index out of range",
           this->Mearliest, this->Mlatest, line.line());
    }
    if (i>=0 && i<Mnsamples)
    {
      if (Mfilled(i))
      {
        oss.clear();
        oss.str("");
        oss << "NOTICE: duplicate sample time (index " << i << "): " 
          << line.line();
        Logger(log_info) << oss.str();
        Mlogmessages.push_back(oss.str());
      }
      Mfilled(i)=true;
      Miseries(i) += line.counts();
    }
    else
    {
      oss.clear();
      oss.str("");
      oss << "ERROR: sample index " << i << " out of range: " << line.line();
      Logger(log_err) << oss.str();
      Mlogmessages.push_back(oss.str());
    }
  } // void Record::put(const DataLine& line)

  /*----------------------------------------------------------------------*/
  void Record::checkinitialandfinal() const
  {
    DL1_rcassert (Minitial.length()>0,
        "inital line is missing",
         this->Mearliest, this->Mlatest, std::string(""));
    DL1_rcassert (Mfinal.length()>0,
        "final line is missing",
         this->Mearliest, this->Mlatest, std::string(""));
  } // void Record::checkinitialandfinal() const

  /*----------------------------------------------------------------------*/

  /* latest in dictionary:
   * http://www.dict.cc/?s=latest+possible
   * "latest date" should be fine
   */
  Record::Tlinelist Record::header() const
  {  
    Tlinelist retval;
    this->checkinitialandfinal();
    retval.push_back(std::string("# earliest date: ")+this->Mearliest.timestring());
    retval.push_back(std::string("# latest date:   ")+this->Mlatest.timestring());
    retval.push_back(std::string("# creation date: ")+this->Mcreation.timestring());
    retval.push_back(std::string("# initial line:  ")+Minitial);
    retval.push_back(std::string("# final line:    ")+Mfinal);
    retval.push_back(std::string("# recorded by:   ")
                     +DL1_LOGGER_SOFTWARE_VERSION);
    std::ostringstream oss;
    oss <<                       "# gain:          " 
      << dl1::DataLine::gain << " mm/count";
    retval.push_back(oss.str());
    return(retval);
  } // Record::Tlinelist Record::header() const

  /*----------------------------------------------------------------------*/

  Record::Tlinelist Record::lines() const
  {  
    Tlinelist retval=this->header();
    retval.push_back(Minitial);
    Tdatalist::const_iterator I=Mdatalist.begin();
    while (I!=Mdatalist.end())
    {
      retval.push_back(I->line());
      ++I;
    }
    retval.push_back(Mfinal);
    return(retval);
  } // Record::Tlinelist Record::lines() const

  /*----------------------------------------------------------------------*/

  int Record::nsamples() const
  {
    return(Mnsamples);
  } // int Record::nsamples() const

  /*----------------------------------------------------------------------*/

  sff::WID2 Record::wid2line() const
  {
    sff::WID2 retval;
    this->checkinitialandfinal();
    retval.date=this->Mearliest+(dl1samplinginterval/2);
    retval.dt=libtime::time2double(dl1samplinginterval);
    retval.nsamples=Mnsamples;
    retval.station=Mfinal.substr(12,5);
    retval.instype=Mfinal.substr(21,5);
    retval.channel=precipitationID;
    return(retval);
  } // sff::WID2 Record::wid2line() const

  /*----------------------------------------------------------------------*/

  //! return data block of values
  Tdseries::Tcoc Record::dseries() const
  {
    Tdseries dseries(Mnsamples);
    dseries.copyin(Miseries);
    return(dseries);
  } // Tdseries::Tcoc Record::dseries() const

  /*----------------------------------------------------------------------*/

  //! return data block of counts
  Tiseries::Tcoc Record::iseries() const
  {
    return(Miseries);
  } // Tiseries::Tcoc Record::iseries() const

} // namespace dl1

/* ----- END OF record.cc ----- */
