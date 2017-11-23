/*! \file ovtaper.cc
 * \brief offset variable taper (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 26/01/2012
 * 
 * offset variable taper (implementation)
 * 
 * Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 26/01/2012   V1.1   Thomas Forbriger
 *  - 19/06/2016   V1.2   make offset variable taper robust in case of picks
 *                        outside seismogram range
 * 
 * ============================================================================
 */
#define TSXX_OVTAPER_CC_VERSION \
  "TSXX_OVTAPER_CC   V1.2"

#include <fstream>
#include <sstream>
#include <tsxx/tsxx.h>
#include <tsxx/debug.h>
#include <tsxx/ovtaper.h>
#include <tsxx/error.h>

namespace ts {

  namespace tapers {
    
    namespace ovtaper {
        
      //! read from file in refract taper file format
      void Picks::read(std::istream& is)
      {
        Mpicks.clear();
        std::string line;
        do {
          TSXX_assert(getline(is, line), 
                      "Picks::read: error while reading taper definition");
        } while (line.substr(0,9)!="taper set");

        TSXX_debug(Mdebug, "Picks::read",
                   "found data line:\n" << line);

        std::istringstream iss(line.substr(17,4));
        int n;
        iss >> n;
        TSXX_debug(Mdebug, "Picks::read",
                   "read " << n << " picks");

        // skip one line
        TSXX_assert(getline(is, line), 
                    "Picks::read: error while reading taper definition");
        for (int i=0; i<n; ++i)
        {
          Pick p;
          TSXX_assert(is >> p.x >> p.t,
                      "Picks::read: error while reading taper definition");
          Mpicks.push_back(p);
          TSXX_debug(Mdebug, "Picks::read",
                     "#" << i << " x=" << p.x << ", t=" << p.t);
        }

        Mpicks.sort();
      } // void Picks::read(std::istream& is)
        
      /*----------------------------------------------------------------------*/

      Pick Picks::pick(const double& offset) const
      {
        Pick p(offset);
        if (!Mpicks.empty())
        {
          Tlistofpick::const_iterator P=Mpicks.begin();
          if ((*P) > p)
          {
            p.t=P->t;
          }
          else
          {
            Tlistofpick::const_iterator Pp=P;
            while ((P!=Mpicks.end()) && (*P<p)) { Pp=P; P++; }
            if (P==Pp)
            {
              p.t=P->t;
            }
            else if (P==Mpicks.end())
            {
              p.t=Pp->t;
            }
            else 
            {
              p.t=Pp->t+(P->t-Pp->t)*(p.x-Pp->x)/(P->x-Pp->x);
            }
          }
        }
        return p;
      } // Pick Picks::pick(const double& offset) const
        
      /*----------------------------------------------------------------------*/

      double Picks::time(const double& offset) const
      {
        Pick p=this->pick(offset);
        return(p.t);
      } // double Picks::time(const double& offset) const

    } // namespace ovtaper

    /*======================================================================*/

    ts::tapers::FourPoint OffsetVariableTaper::taper(const double& offset,
                                                     const double& T0,
                                                     const double& T) const
    {
      TSXX_assert(Mvalid,
                  "OffsetVariableTaper::taper: "
                  "taper is undefined");
      // calculate times in units of the duration of the time series
      double t1=(Mt1.time(offset)-T0)/T;
      double t2=(Mt2.time(offset)-T0)/T;
      double t3=(Mt3.time(offset)-T0)/T;
      double t4=(Mt4.time(offset)-T0)/T;
      // keep values in bounds of time series
      t1 = t1 >=  0. ? t1 : 0.; 
      t2 = t2 >=  1.e-6 ? t2 : 1.e-6; 
      t3 = t3 <=  .99999 ? t3 : .99999; 
      t4 = t4 <=  1. ? t4 : 1.; 
      // initialize taper
      ts::tapers::FourPoint fpt(t1, t2, t3, t4);
      return(fpt);
    } // ts::tapers::FourPoint OffsetVariableTaper::taper(...)

    /*----------------------------------------------------------------------*/

    /*! read taper definition from input stream is
    */
    void OffsetVariableTaper::read(std::istream& is)
    {
      Mt1.read(is);
      Mt2.read(is);
      Mt3.read(is);
      Mt4.read(is);
      Mvalid=true;
    } // void OffsetVariableTaper::read(std::istream& is)

    /*----------------------------------------------------------------------*/

    /*! read taper definition from file
    */
    void OffsetVariableTaper::read(const std::string& filename)
    {
      std::ifstream is(filename.c_str());
      TSXX_assert(is.good(), 
                  "OffsetVariableTaper::read(): "
                  "error when opening taper file");
      this->read(is);
    } // void OffsetVariableTaper::read(const std::string& filename)

  } // namespace tapers

} // namespace ts

/* ----- END OF ovtaper.cc ----- */
