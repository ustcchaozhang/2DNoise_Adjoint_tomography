/*! \file readbonjer.cc
 * \brief read data obtained in ASCII from K. Bonjer (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/03/2004
 * 
 * read data obtained in ASCII from K. Bonjer (implementation)
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 30/03/2004   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DATRW_READBONJER_CC_VERSION \
  "DATRW_READBONJER_CC   V1.0   "

#include<cstdlib>
#include<sstream>
#include<datrwxx/readbonjer.h>

namespace datrw {

  /*!
   * To provide a definition of the data format, we dump
   * the first lines of a data file example.
   *
   * B30815IB.001.NS.ACC.ASC:
   * \verbatim
********************************************************

ACHTUNG: Polaritaeten und Komponentenzuordung nur dem 
Konfigurationsfile  K2parcfg_xxxx.dat  entnehmen !!!

********************************************************

K2 Stn: IBA
Event Start Time: 11/30/2002 (334) 8:15:42.000
Samples per second: 200
Number of points: 24400
Filename: B30815IB.001.NS.ACC.ASC
Component: IB1N  NS
Sensitivity: 1.2540 V/g
Units: CM/S**2
 
 6.793764e-01
 6.789101e-01
 6.779776e-01
 ...
     \endverbatim
   */
  namespace bonjer {

/*----------------------------------------------------------------------*/

    //! function to read the file header
    header readheader(std::istream& is, const bool& verbose)
    {
      std::string line;
      bool hot=true;
      header retval;  
      int mandatory=0;
      while (hot && is.good()) {
        getline(is, line);
        if (line.substr(0,7)==std::string("K2 Stn:"))
        {
          retval.station=line.substr(8);
          mandatory++;
        } else
        if (line.substr(0,17)==std::string("Event Start Time:"))
        {
          std::istringstream iss(line.substr(18).c_str());
          char c;
          int day, month, year, hour, minute;
          double seconds;
          iss >> month >> c >> day >> c >> year;
          iss >> c >> hour >> c;
          iss >> hour >> c >> minute >> c >> seconds;
          retval.date=libtime::TAbsoluteTime(year,month,day,hour,minute);
          retval.date+=libtime::double2time(seconds);
          mandatory++;
        } else
        if (line.substr(0,19)==std::string("Samples per second:"))
        {
          retval.rate=std::atof(line.substr(20).c_str());
          mandatory++;
        } else
        if (line.substr(0,17)==std::string("Number of points:"))
        {
          retval.nsamples=std::atoi(line.substr(18).c_str());
          mandatory++;
        } else
        if (line.substr(0,9)==std::string("Filename:"))
        {
          retval.filename=line.substr(10);
          mandatory++;
        } else
        if (line.substr(0,10)==std::string("Component:"))
        {
          retval.component=line.substr(11);
          mandatory++;
        } else
        if (line.substr(0,12)==std::string("Sensitivity:"))
        {
          retval.sensitivity=line.substr(13);
          mandatory++;
        } else
        if (line.substr(0,6)==std::string("Units:"))
        {
          retval.units=line.substr(7);
          mandatory++;
        } 
        if (mandatory > 7) { hot=false; }
      }
      if (mandatory<8) throw;
      getline(is,line);
      if (verbose) 
      {
        std::cout << "File:        " << retval.filename << std::endl;
        std::cout << "Station:     " << retval.station << std::endl;
        std::cout << "Component:   " << retval.component << std::endl;
        std::cout << "First:       " << retval.date.timestring() << std::endl;
        std::cout << "Rate:        " << retval.rate << std::endl;
        std::cout << "Samples:     " << retval.nsamples << std::endl;
        std::cout << "Sensitivity: " << retval.sensitivity << std::endl;
        std::cout << "Units:       " << retval.units << std::endl;
      }
      return(retval);
    } // function readheader

/*----------------------------------------------------------------------*/

    //! function to read the file data
    Tdata readdata(std::istream& is, const header& hd)
    {
      Tdata retval(hd.nsamples);
      for (int i=0; i<hd.nsamples; ++i)
      {
        if (!is.good()) throw;
        is >> retval(i);
      }
      return(retval);
    } // function readdata

  } // namespace bonjer

} // namespace datrw

/* ----- END OF readbonjer.cc ----- */
