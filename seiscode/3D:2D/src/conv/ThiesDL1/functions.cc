/*! \file functions.cc
 * \brief some functions to be used together with the DL1 data logger (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 24/03/2014
 * 
 * some functions to be used together with the DL1 data logger (implementation)
 * 
 * Copyright (c) 2008, 2014 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 27/11/2008   V1.0   Thomas Forbriger (thof)
 *  - 22/12/2008   V1.1   pass records log messages to data file
 *  - 25/11/2010   V1.2   g++ was not able to resolve template function split
 *  - 01/02/2014 thof:    use tsioxx/sfftimeseries.h instead of
 *                        tsxx/sffheaders.h; the latter has vanished
 *  - 24/03/2014 thof:    
 *                        - avoid tsioxx modules entirely
 *                        - use data output based in path patterns
 *                        - provide libdatrwxx output file types
 * 
 * ============================================================================
 */
#define DL_FUNCTIONS_CC_VERSION \
  "DL_FUNCTIONS_CC   2014-03-24"
#define DL_FUNCTIONS_CC_CVSID \
  "$Id$"

#include <iostream>
#include <fstream>
#include <tfxx/stringfunc.h>
#include <tfxx/misc.h>
#include <tfxx/filestatus.h>
#include <sffostream.h>
#include <datrwxx/thiesdl1.h>
#include "functions.h"
#include "logger.h"
#include "error.h"

namespace dl1 {

  const int normaltimeout=20;
  const int longtimeout=90;
  const int cmaxretries=3;

  const libtime::TRelativeTime oneday(1);
  const libtime::TRelativeTime twominutes(0,0,2);


  /*----------------------------------------------------------------------*/

  void initializeclock(DL1& dl1)
  {
    Logger() << "initialize DL1 clock at " << libtime::utc().timestring();
    std::ostringstream oss;
    dl1.setcurrenttime(oss);
    Logger() << listofstring(oss);
  } // void initializeclock()

  /*----------------------------------------------------------------------*/

  Tlistofstring listofstring(const std::ostringstream& oss,
                           const std::string& eol)
  {
    Tlistofstring los; 
    tfxx::string::gen_split(los, oss.str(), eol, true); 
    return(los); 
    /*
    std::string msg(oss.str());
    Tlistofstring v=::tfxx::string::split<std::list>(msg, eol, true);
    return(v);
    */
  } // Tlistofstring listofstring(const std::ostringstream& oss,
    //                          const std::string& eol)

  /*----------------------------------------------------------------------*/

  Tlistofstring status(DL1& dl1)
  {
    Tlistofstring retval;
    bool hot=true;
    int itry=0;
    while ((itry < cmaxretries) && hot)
    {
      ++itry;
      retval.clear();
      try{
        dl1.sendLL();
        while (hot)
        {
          std::string line=tfxx::string::trimws(dl1.getline(normaltimeout));
          if (line.size()>0)
          {
            if (line=="END")
            {
              hot=false;
            }
            else
            {
              retval.push_back(line);
            }
          }
        }
      }
      catch (TimeOut& E)
      {
        if (itry < cmaxretries)
        {
          Logger() << "Caught timeout ("
            << E.timeout() << "s) while reading logger status; "
            << "try again";
          hot=true;
        }
        else
        {
          DL1_timeout("Timeout while reading logger status", E.timeout());
        }
      }
    }
    return(retval);
  } // Tlistofstring status(DL1& dl1)

  /*----------------------------------------------------------------------*/

  libtime::TAbsoluteTime dateonly(const libtime::TAbsoluteTime& date)
  {
    libtime::TAbsoluteTime retval(date.year(), date.month(), date.day());
    return(retval);
  } // libtime::TAbsoluteTime dateonly(const libtime::TAbsoluteTime& date)

  /*----------------------------------------------------------------------*/

  void fillrecordfromlogger(DL1& dl1, Record& record)
  {
    try{
      std::string line;
      line=dl1.getline(longtimeout);
      while (line.substr(0,6)!="Data :") { line=dl1.getline(normaltimeout); }
      record.setinitial(line);
      line=dl1.getline(normaltimeout); 
      while (line.substr(0,11)!="END OF DATA") 
      { 
        record.put(DataLine(line));
        line=dl1.getline(normaltimeout); 
      }
      record.setfinal(line);
    }
    catch (TimeOut& E)
    {
      DL1_timeout("Timeout while filling record from logger", E.timeout());
    }
  }

  /*----------------------------------------------------------------------*/

  Record readrecordfordate(DL1& dl1, const libtime::TAbsoluteTime& date)
  {
    libtime::TAbsoluteTime first=dateonly(date);
    libtime::TAbsoluteTime last=first+oneday;
    Record retval(first, last);
    int itry=0;
    bool stilltrying=true;
    while ((itry<cmaxretries) && stilltrying)
    {
      ++itry;
      stilltrying=false;
      try {
        dl1.activate();
        sleep(2);
        retval=Record(first, last);
        dl1.sendts(first);
        fillrecordfromlogger(dl1, retval);
      }
      catch (ExceptionTimeOut &E)
      {
        E.report();
        if (itry < cmaxretries)
        {
          Logger(log_warning) << "caught a time out but try again";
          stilltrying=true;
        }
        else
        {
          throw;
        }
      }
    }
    return(retval);
  } // Record readrecordfordate(DL1& dl1, const libtime::TAbsoluteTime& date)

  /*----------------------------------------------------------------------*/

  Record readrecordsincedate(DL1& dl1, const libtime::TAbsoluteTime& date)
  {
    libtime::TAbsoluteTime first=date;
    libtime::TAbsoluteTime last=libtime::utc();
    Record retval(first, last);
    int itry=0;
    bool stilltrying=true;
    while ((itry<cmaxretries) && stilltrying)
    {
      ++itry;
      stilltrying=false;
      try {
        dl1.activate();
        sleep(2);
        retval=Record(first, last);
        dl1.sendds(first);
        fillrecordfromlogger(dl1, retval);
      }
      catch (ExceptionTimeOut &E)
      {
        E.report();
        if (itry < cmaxretries)
        {
          Logger(log_warning) << "caught a time out but try again";
          stilltrying=true;
        }
        else
        {
          throw;
        }
      }
    }
    return(retval);
  } // Record readrecordsincedate(DL1& dl1, const libtime::TAbsoluteTime& date)

  /*----------------------------------------------------------------------*/

  //! dump raw ASCII table to file
  void dumptofile(const std::string& filename, const Record& record,
                  const Tlistofstring& info)
  {
    std::ofstream ofs(filename.c_str());
    DL1_assert(ofs.good(), "could not open output file");
    /*
    Tlistofstring lines;
    lines=record.header();
    // ofs << "header from record:" << std::endl;
    Tlistofstring::const_iterator I=lines.begin();
    while (I!=lines.end()) { ofs << *I << std::endl; ++I; }
    */
    // ofs << "info block passed to function:" << std::endl;
    Tlistofstring::const_iterator I=info.begin();
    while (I!=info.end()) { ofs << "# " << *I << std::endl; ++I; }
    Tlistofstring lines=record.logmessages();
    // log messages
    I=lines.begin();
    while (I!=lines.end()) { ofs << "# " << *I << std::endl; ++I; }
    lines=record.lines();
    // ofs << "data lines supplied by record:" << std::endl;
    I=lines.begin();
    while (I!=lines.end()) { ofs << *I << std::endl; ++I; }
    DL1_assert(ofs.good(), "could not write to output file");
  } // void dumptofile

  /*----------------------------------------------------------------------*/

  void processday(DL1& dl1,
                  const std::string& datapath,
                  const std::string& datatypes,
                  const libtime::TAbsoluteTime& date)
  {
    // poll data
    Tlistofstring info=status(dl1);
    Record record=readrecordfordate(dl1, date);
    std::string dumpfilename=mkpathname(datapath, date,
                                        datrw::thiesdl1::streamID,
                                        false);
    dumptofile(dumpfilename, record, info);
    Tlistofstring types;
    tfxx::string::gen_split(types,
                            tfxx::string::patsubst(datatypes, ",", " "),
                            std::string(" "), true);
    for (Tlistofstring::const_iterator I=types.begin();
         I != types.end(); ++I)
    {
      std::string filename=mkpathname(datapath, date, *I, false);
      writedata(filename, *I, record, info);
    }
  }

}  // namespace dl1

/* ----- END OF functions.cc ----- */
