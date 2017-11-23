/*! \file scaling.h
 * \brief contains definition of scaling structure (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 26/02/2008
 * 
 * contains definition of scaling structure (prototypes)
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
 * REVISIONS and CHANGES 
 *  - 26/02/2008   V1.0   Thomas Forbriger
 *  - 17/03/2015   V1.1   adjust libpgplotxx interface
 * 
 * ============================================================================
 */

// include guard
#ifndef STUPLO_SCALING_H_VERSION

#define STUPLO_SCALING_H_VERSION \
  "STUPLO_SCALING_H   V1.1   (17-03-2015)"

#include <vector>
#include <libtime++.h>
#include <tfxx/handle.h>
#include <pgplotxx/xpgplotxx.h>

namespace stuplo {

  //! struct to hold time axis data
  class PanelTime {
    public:
      PanelTime(): 
        Mtimerange(libtime::now(), libtime::now()),
        Mreferencetime(libtime::now()) { }
      //! return range for PGPLOT window
      pgplot::Trange frange() const;
      //! return x-value for first sample
      float fbegin() const;
      //! return x-value for last sample
      float fend() const;
      //! return time beginning of range
      libtime::TAbsoluteTime begin() const { return(Mtimerange.begin()); }
      //! return time end of range
      libtime::TAbsoluteTime end() const { return(Mtimerange.end()); }
      //! return reference
      libtime::TAbsoluteTime reference() const { return(Mreferencetime); }
      //! return time range
      libtime::TRange range() const { return(Mtimerange); }
      //! set time range
      void setrange(const libtime::TRange& range) 
      { Mtimerange=range; }
      //! set time range
      void setrange(const pgplot::Trange& range) ;
      //! set reference time 
      void setreference(const libtime::TAbsoluteTime& reference) 
      { Mreferencetime=reference; }
      //! return offset to reference
      libtime::TRelativeTime offset() const 
      { return(this->begin()-this->reference()); }
      //! return offset to reference
      libtime::TRelativeTime offset(const libtime::TAbsoluteTime& time) const 
      { return(time-this->reference()); }
      //! return offset to reference in seconds
      float foffset() const 
      { return(float(libtime::time2double(this->offset()))); }
      //! return offset to reference in seconds
      float foffset(const libtime::TAbsoluteTime& time) const 
      { return(float(libtime::time2double(this->offset(time)))); }
    private:
      //! range of time scale
      libtime::TRange Mtimerange;
      //! time to refer time scale to 
      libtime::TAbsoluteTime Mreferencetime;
  }; // struct PanelParameters

  /*----------------------------------------------------------------------*/

  //! struct to hold scaling values
  struct Scaling {
    Scaling(): 
      initialized(false), usecommontime(true),
      alwaysinittime(false) { }
    //! is false prior to first use by screen
    bool initialized;
    //! is true, if all panels use the same time scale
    bool usecommontime;
    //! is true, if time axis should be initialized always (repeat mode)
    bool alwaysinittime;
    //! define a common time scale 
    //! it depends on the screen whether this is used or not
    PanelTime commontime;
    //! define a time scale per panel
    //! it depends on the screen whether this is used or not
    typedef std::vector<PanelTime> TVectorPanelTime;
    TVectorPanelTime paneltime;
    //! define an ordinate range per panel
    typedef std::vector<pgplot::Trange> TVectorRange;
    TVectorRange range;
  }; // struct Scaling

  //! A handle on scaling values
  typedef tfxx::Handle<Scaling> THScaling;
  //! A const handle on scaling values
  typedef THScaling::Tcoc TCHScaling;

} // namespace stulo

#endif // STUPLO_SCALING_H_VERSION (includeguard)

/* ----- END OF scaling.h ----- */
