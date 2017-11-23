/*! \file seifeio.cc
 * \brief seife input/output (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 14/02/2011
 * 
 * seife input/output (implementation)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 14/02/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DATRW_SEIFEIO_CC_VERSION \
  "DATRW_SEIFEIO_CC   V1.0"

#include <sstream>
#include <iomanip>
#include <datrwxx/seifeio.h>
#include <datrwxx/error.h>
#include <datrwxx/debug.h>
#include <datrwxx/util.h>

namespace datrw {

  const char* const seife::seife_standard_format="(6(g12.5,1x))";
  const unsigned int seife::seife_standard_precision=5;
  const unsigned int seife::seife_standard_width=12;
  const unsigned int seife::seife_standard_columns=6;

  namespace seife {
    
    /*----------------------------------------------------------------------*/
    
    void ParameterLine::set(const std::string& line)
    {
      DATRW_debug(Mdebug, "ParameterLine::set",
                  "read values from line:\n" << line);
      std::istringstream iss(line.substr(0,10));
      DATRW_assert(iss.good(), "ERROR: iss is not good");
      DATRW_debug(Mdebug, "ParameterLine::set",
                  DATRW_value(line.substr(0,10)));
      int nsamples;
      iss >> nsamples;
      DATRW_assert(nsamples>0, "ERROR: number of samples is not positive");
      Mnsamples=static_cast<unsigned int>(nsamples);
      DATRW_debug(Mdebug, "ParameterLine::set",
                  DATRW_value(Mnsamples));
      //DATRW_assert(iss.good(), "ERROR: reading number of samples");
      iss.clear();
      iss.str(line.substr(10,20));
      DATRW_debug(Mdebug, "ParameterLine::set",
                  DATRW_value(iss.str()));
      iss >> Mformat;
      DATRW_debug(Mdebug, "ParameterLine::set",
                  DATRW_value(line.substr(10,20)) << "\n"
                  DATRW_value(Mformat));
      //DATRW_assert(iss.good(), "ERROR: reading Fortran data format");
      iss.clear();
      iss.str(line.substr(30,10));
      DATRW_debug(Mdebug, "ParameterLine::set",
                  DATRW_value(iss.str()));
      iss >> Mdt;
      DATRW_assert(Mdt>0., "ERROR: sampling interval is not positive");
      DATRW_debug(Mdebug, "ParameterLine::set",
                  DATRW_value(line.substr(30,10)) << "\n"
                  DATRW_value(Mdt));
      //DATRW_assert(iss.good(), "ERROR: reading sampling interval");
      double tmin, tsec;
      iss.clear();
      iss.str(line.substr(40,10));
      iss >> tmin;
      DATRW_debug(Mdebug, "ParameterLine::set",
                  DATRW_value(line.substr(40,10)) << "\n"
                  DATRW_value(tmin));
      //DATRW_assert(iss.good(), "ERROR: reading number of minutes");
      iss.clear();
      iss.str(line.substr(50,10));
      iss >> tsec;
      DATRW_debug(Mdebug, "ParameterLine::set",
                  DATRW_value(line.substr(50,10)) << "\n"
                  DATRW_value(tsec));
      //DATRW_assert(iss.good(), "ERROR: reading number of seconds");
      Mtime=libtime::double2time(tsec)+libtime::double2time(tmin*60.);
      DATRW_debug(Mdebug, "ParameterLine::set",
                  DATRW_value(Mtime.timestring()));
    } // void ParameterLine::set(const std::string& line)

    /*----------------------------------------------------------------------*/

    std::string ParameterLine::line() const
    {
      libtime::TRelativeTime
        ttmin(Mtime.days(),Mtime.hour(),Mtime.minute());
      libtime::TRelativeTime ttsec(Mtime);
      ttsec -= ttmin;
      double tmin, tsec;
      tmin=1440.*ttmin.days()+60.*ttmin.hour()+ttmin.minute();
      tsec=libtime::time2double(ttsec);
      std::ostringstream oss;
      oss << std::setw(10) << std::left << Mnsamples;
      oss << std::setw(20) << std::left << Mformat;
      oss << std::setw(10) << std::left << std::showpoint <<
        std::setprecision(3);
      if (datrw::util::ntrailingdigits(Mdt)>3)
      { oss << std::scientific; }
      else
      { oss << std::fixed; }
      oss << Mdt;
      oss << std::setw(10) << std::left << std::setprecision(1) 
        << std::fixed << tmin;
      oss << std::setw(10) << std::left << std::setprecision(6) << tsec;
      return(oss.str());
    } // void ParameterLine::set(const std::string& line)

    /*======================================================================*/
    // Header
    // ------
      
    void Header::read(std::istream& is)
    {
      std::string line;
      DATRW_assert(is.good(),
                   "ERROR (seife::Header::read): input stream is not good");
      getline(is, line);
      Mfree.append(line);
      DATRW_assert(is.good(),
                   "ERROR (seife::Header::read): input stream is not good");
      getline(is, line);
      while (line.substr(0,1) == "%")
      {
        Mfree.append(line.substr(1));
        DATRW_assert(is.good(),
                     "ERROR (seife::Header::read): input stream is not good");
        getline(is, line);
      }
      Mparameters.set(line); 
    } // void Header::read(std::istream& is)

    /*----------------------------------------------------------------------*/

    void Header::write(std::ostream& os) const
    {
      typedef ::sff::FREE::Tlines Tlines;
      const Tlines& lines=Mfree.lines;
      if (lines.empty())
      {
        os << DATRW_SEIFEIO_CC_VERSION << std::endl;
      }
      else
      {
        Tlines::const_iterator I=lines.begin();
        os << *I << std::endl;
        ++I;
        unsigned int nlines=0;
        while ((I != lines.end()) && (nlines<48))
        {
          os << "%" << *I << std::endl;
          ++I;
          ++nlines;
        }
      }
      os << Mparameters.line() << std::endl;
    } // void Header::write(std::ostream& os) const

    /*======================================================================*/

    void write_series(std::ostream& os, const Tseries::Tcoc& s)
    {
      unsigned int j=1;
      for (int i=s.f();i<=s.l();++i)
      {
        os << std::scientific;
        os << std::setprecision(seife_standard_precision);
        os << std::setw(seife_standard_width);
        os << s(i) << " ";
        ++j;
        if (j>seife_standard_columns)
        {
          j=1;
          os << std::endl;
        }
      }
      if (j != 1) { os << std::endl; }
    } // void write_series(std::ostream& os, const Tseries::Tcoc& s)

  } // namespace seife

} // namespace datrw

/* ----- END OF seifeio.cc ----- */
