/*! \file datrw.cc
 * \brief abstract base class to read seismic data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/03/2004
 * 
 * abstract base class to read seismic data (implementation)
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
 *  - 17/09/2004   V1.1   make most fields private
 *  - 18/11/2016   V1.2   provide debug flag in base class
 *                        and produce debug output
 *  - 19/04/2017   V1.3   fix: do not reset Msrceset upon reading new trace
 *                        SRCE data is a file property not a trace property
 * 
 * ============================================================================
 */
#define DATRW_DATRW_CC_VERSION \
  "DATRW_DATRW_CC   V1.3"

#include <datrwxx/datread.h>
#include <datrwxx/debug.h>

namespace datrw {

  /*! constructor (must be called by derived class
   *
   * \param is C++ input stream
   * \param providesd \c true, if stream provides \c double type series
   * \param providesf \c true, if stream provides \c float type series
   * \param providesi \c true, if stream provides \c int type series
   * \param debug \c true, to select debug mode on a base class level
   */
  idatstream::idatstream(std::istream& is,
                 const bool& providesd,
                 const bool& providesf,
                 const bool& providesi,
                 const bool& debug)
  : Mis(is), Mdebug(debug), Mwid2set(false), Msrceset(false), Minfoset(false),
    Mtracefreeset(false), Mfilefreeset(false), Mlast(false),
    Mprovidesd(providesd), Mprovidesf(providesf), Mprovidesi(providesi)
  { 
    DATRW_debug(this->Mdebug,
                "idatstream::idatstream",
                "create new input stream");
    DATRW_assert(is.good(), "input stream is not good!");
  } // idatstream::idatstream

  /*----------------------------------------------------------------------*/

  bool idatstream::hasfree() const
  {
    if (Mwid2set) { return(Mtracefreeset); } else { return(Mfilefreeset); }
  }

  /*----------------------------------------------------------------------*/

  sff::FREE idatstream::free() const
  {
    if (Mwid2set) 
    {
      DATRW_debug(this->Mdebug, "idatstream::free",
                  DATRW_value(Mtracefree.lines.size()));
      return(Mtracefree); 
    } 
    else 
    {
      DATRW_debug(this->Mdebug, "idatstream::free",
                  DATRW_value(Mfilefree.lines.size()));
      return(Mfilefree); 
    }
  }

  /*----------------------------------------------------------------------*/

  void idatstream::setfilefree(const sff::FREE& free) 
  {
    Mfilefreeset=true;
    Mfilefree=free;
  }

  /*----------------------------------------------------------------------*/

  void idatstream::settracefree(const sff::FREE& free) 
  {
    Mtracefreeset=true;
    Mtracefree=free;
  }

  /*----------------------------------------------------------------------*/

  void idatstream::setwid2(const sff::WID2& wid2) 
  {
    Mwid2set=true;
    Mwid2=wid2;
  }

  /*----------------------------------------------------------------------*/

  void idatstream::setinfo(const sff::INFO& info) 
  {
    Minfoset=true;
    Minfo=info;
  }

  /*----------------------------------------------------------------------*/

  void idatstream::setsrce(const sff::SRCE& srce) 
  {
    Msrceset=true;
    Msrce=srce;
  }

  /*----------------------------------------------------------------------*/

  void idatstream::setlast() 
  {
    Mlast=true;
  }

  /*----------------------------------------------------------------------*/

  void idatstream::newtrace()
  {
    Mtracefreeset=false;
    Mwid2set=false;
    Minfoset=false;
  }
    
  /*----------------------------------------------------------------------*/

  void idatstream::help(std::ostream& os, const char* name)
  {
    os << "Class " << name << " provides no help text." << std::endl;
  }

  /* ====================================================================== */
  // input operators implemented here to provide debug output

  idatstream& operator>>(idatstream& is, sff::WID2& wid2)
  { 
    wid2=is.wid2(); 
    DATRW_debug(is.debug(), "operator>>(idatstream& is, sff::WID2& wid2)",
                DATRW_value(wid2.line().substr(0,50)));
    return(is); 
  } // idatstream& operator>>(idatstream& is, sff::WID2& wid2)

  /* ---------------------------------------------------------------------- */

  idatstream& operator>>(idatstream& is, sff::SRCE& srce)
  { 
    srce=is.srce();
    DATRW_debug(is.debug(), "operator>>(idatstream& is, sff::SRCE& srce)",
                DATRW_value(srce.line().substr(0,50)));
    return(is); 
  } // idatstream& operator>>(idatstream& is, sff::SRCE& srce)

  /* ---------------------------------------------------------------------- */

  idatstream& operator>>(idatstream& is, sff::INFO& info)
  { 
    info=is.info(); 
    DATRW_debug(is.debug(), "operator>>(idatstream& is, sff::INFO& info)",
                DATRW_value(info.line().substr(0,50)));
    return(is); 
  } // idatstream& operator>>(idatstream& is, sff::INFO& info)

  /* ---------------------------------------------------------------------- */

  idatstream& operator>>(idatstream& is, sff::FREE& free)
  { 
    free=is.free(); 
    DATRW_debug(is.debug(), "operator>>(idatstream& is, sff::FREE& free)",
                DATRW_value(free.lines.size()));
    return(is); 
  } // idatstream& operator>>(idatstream& is, sff::FREE& free)

  /* ---------------------------------------------------------------------- */

  idatstream& operator>>(idatstream& is, Tdseries& series)
  { 
    series=is.dseries(); 
    DATRW_debug(is.debug(), "operator>>(idatstream& is, Tdseries& series)",
                DATRW_value(series.f()) << ", " << DATRW_value(series.l()));
    return(is); 
  } // idatstream& operator>>(idatstream& is, Tdseries& series)

  /* ---------------------------------------------------------------------- */

  idatstream& operator>>(idatstream& is, Tfseries& series)
  { 
    series=is.fseries(); 
    DATRW_debug(is.debug(), "operator>>(idatstream& is, Tfseries& series)",
                DATRW_value(series.f()) << ", " << DATRW_value(series.l()));
    return(is); 
  } // idatstream& operator>>(idatstream& is, Tfseries& series)

  /* ---------------------------------------------------------------------- */

  idatstream& operator>>(idatstream& is, Tiseries& series)
  { 
    series=is.iseries();
    DATRW_debug(is.debug(), "operator>>(idatstream& is, Tiseries& series)",
                DATRW_value(series.f()) << ", " << DATRW_value(series.l()));
    return(is); 
  } // idatstream& operator>>(idatstream& is, Tiseries& series)

} // namespace datrw

/* ----- END OF datrw.cc ----- */
