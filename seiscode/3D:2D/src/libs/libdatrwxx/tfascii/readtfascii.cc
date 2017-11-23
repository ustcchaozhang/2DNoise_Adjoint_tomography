/*! \file readtfascii.cc
 * \brief read data obtained in ASCII (any2ascii) from T. Forbriger 
 * \brief (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Daniel Armbruster
 * \date 05/10/2010
 * 
 * Purpose: read data obtained in ASCII (any2ascii) from T. Forbriger 
 *          (implementation)
 * ----
 * This file is part of libdatrwxx.
 *
 * libdatrwxx is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * libdatrwxx is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with libdatrwxx.  If not, see <http://www.gnu.org/licenses/>.
 * ----
 * 
 * Copyright (c) 2010 by Daniel Armbruster
 * 
 * REVISIONS and CHANGES 
 * 05/10/2010  V0.1  Daniel Armbruster
 * 
 * ============================================================================
 */
 
#define DATRW_READTFASCII_CC_VERSION \
  "DATRW_READTFASCII_CC   V1.0   "

#include<cstdlib>
#include<sstream>
#include<libtime++.h>
#include<datrwxx/readtfascii.h>
#include<datrwxx/error.h>

namespace datrw {

  namespace tfascii {

    namespace helper {

      /*---------------------------------------------------------------------*/
      void extractvalue(std::string& line, bool unit) 
      {
        if (unit)
        {
          std::string tmp = line.substr(0,line.rfind(" "));
          line = tmp.substr(tmp.rfind(" ")+1);  
        }
        else
        {
          line = line.substr(line.rfind(" ")+1);  
        }
      } // function extractvalue

      /*----------------------------------------------------------------------*/
      void extract(std::string& line)
      {
        line = line.substr(line.find(":")+1);
      } // function extract

    } // namespace helper

    /*----------------------------------------------------------------------*/
    const char* const streamname = "itfasciistream";

    /*----------------------------------------------------------------------*/
    FileHeader readfileheader(std::istream& is, const bool& verbose)
    {
      std::string line;
      bool hot = true;
      FileHeader fileheader;
      fileheader.hasfilefree = false;
      fileheader.hassrce = false;

      while (hot && is.good())
      {
        getline(is, line);
        // get SRCE line if available
        if (line == "contents of SFF SRCE line:")
        {
          fileheader.hassrce = true;
          if (verbose)
          {
            std::cout << "SFF SRCE line detected." << std::endl;
          }
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extract(line);
          fileheader.srce.type = line;    
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line);
          if (line == "cartesian")
          {
            fileheader.srce.cs = sff::CS_cartesian;
          } 
          else
          {
            fileheader.srce.cs = sff::CS_spherical; 
          }
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line, true);
          fileheader.srce.cx = std::atof(line.c_str());
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line, true);
          fileheader.srce.cy = std::atof(line.c_str());
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line, true);
          fileheader.srce.cz = std::atof(line.c_str());
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extract(line);
          std::istringstream iss(line);
          char c;
          int doy, day, month, year, hour, minute, secs, micsecs;
          iss >> doy;
          iss >> day >> c >> month >> c >> year;
          iss >> hour >> c >> minute >> c >> secs >> c >> micsecs;
          fileheader.srce.date = libtime::TAbsoluteTime(year, 
                                                        month,
                                                        day,
                                                        hour,
                                                        minute,
                                                        secs,
                                                        0,
                                                        micsecs);
          DATRW_assert(doy == fileheader.srce.date.doy(),
            "ERROR: inconsistent date values");
        }
        // get fileFREE if available
        if (line == "contents of SFF FREE block:")
        {
          fileheader.hasfilefree = true;
          if (verbose) 
          {
            std::cout << "file FREE block detected." << std::endl;
          }
          while (getline(is, line))
          {
            // break if stream reaches a empty line
            if (line.empty()) 
            { 
              hot = false;
              break; 
            }
            fileheader.filefree.append(line); 
          }
        } else 
        if (line == "file header contains no FREE block") 
        { hot = false; }
      }

      return fileheader;
    } // function readfileheader
      
    /*----------------------------------------------------------------------*/
    TraceHeader readtraceheader(std::istream& is, const bool& verbose)
    {
      std::string line;
      TraceHeader traceheader;
      traceheader.hastracefree = false;
      traceheader.hasinfo = false;
      traceheader.haswid2 = false;
      
      while (is.good())
      {
        getline(is, line);
        // get WID2 lines if available
        if (line == "contents of SFF WID2 line:")
        {
          traceheader.haswid2 = true;
          if (verbose)
          {
            std::cout << "SFF WID2 line detected." << std::endl;
          }
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extract(line);
          std::istringstream iss(line);
          char c;
          int doy, day, month, year, hour, minute, secs, micsecs;
          iss >> doy;
          iss >> day >> c >> month >> c >> year;
          iss >> hour >> c >> minute >> c >> secs >> c >> micsecs;
          traceheader.wid2.date = libtime::TAbsoluteTime(year, 
                                                        month,
                                                        day,
                                                        hour,
                                                        minute,
                                                        secs,
                                                        0,
                                                        micsecs);
          DATRW_assert(doy == traceheader.wid2.date.doy(),
            "ERROR: inconsistent date values");
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line);
          traceheader.wid2.station = line; 
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line);
          traceheader.wid2.channel = line; 
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line);
          traceheader.wid2.auxid = line; 
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line);
          traceheader.wid2.instype = line; 
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line);
          traceheader.wid2.nsamples = std::atoi(line.c_str()); 
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line, true);
          traceheader.wid2.dt = std::atof(line.c_str()); 
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line);
          traceheader.wid2.calib = std::atof(line.c_str()); 
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line);
          traceheader.wid2.calper = std::atof(line.c_str()); 
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line, true);
          traceheader.wid2.hang = std::atof(line.c_str()); 
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line, true);
          traceheader.wid2.vang = std::atof(line.c_str()); 
        }
        // get INFO lines if available
        if (line == "contents of SFF INFO line:")
        {
          traceheader.hasinfo = true;
          if (verbose)
          {
            std::cout << "SFF INFO line detected." << std::endl;
          }
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line);
          if (line == "cartesian")
          {
            traceheader.info.cs = sff::CS_cartesian;
          } 
          else
          {
            traceheader.info.cs = sff::CS_spherical;
          }
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line, true);
          traceheader.info.cx = std::atof(line.c_str());
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line, true);
          traceheader.info.cy = std::atof(line.c_str());
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line, true);
          traceheader.info.cz = std::atof(line.c_str());
          DATRW_assert(getline(is, line), "ERROR: reading file");
          helper::extractvalue(line);
          traceheader.info.nstacks = std::atoi(line.c_str());
        }
        // get traceFREE block if available
        if (line == "contents of SFF FREE block:")
        {
          traceheader.hastracefree = true;
          if (verbose)
          {
            std::cout << "trace FREE block detected." << std::endl;
          }
          while (getline(is, line))
          {
            // break if stream reaches a empty line
            if (line.empty()) { break; }
            traceheader.tracefree.append(line); 
          }
        }
        // break if stream reaches data
        if (line == "data:") { break; }
      }

      return traceheader;
    } // function readtraceheader

    /*----------------------------------------------------------------------*/
    void help(std::ostream& os)
    {
      using std::endl;
      os << endl;
      os << "TFASCII data reading functions" << endl;
      os << "------------------------------" << endl;
      os << endl;
      os << "This data reading module implements reading functions" << endl 
         << "for the output data of Thomas Forbriger's any2ascii.cc" << endl
         << "program. As the name of this program already says its" << endl
         << "output data are human readable ASCII files."<< endl
         << "If you wish to read an output file of any2ascii.cc just" << endl
         << "remember that files can be read that were generated " << endl
         << "without the [-tcol] option, because the reading module can" << endl
         << "not handle with an additional time column in the input" << endl
         << "data." << endl
         << endl
         << "IMPORTANT NOTE: The tfascii reading module just relies as" << endl
         << "in many cases on the input following the tfascii" << endl 
         << "format. The code does not contain any plausibility" << endl
         << "checks." << endl;
      os << endl;
    } // function help

  } // namespace tfascii

} // namespace datrw

/* ----- END OF readtfascii.cc ----- */
