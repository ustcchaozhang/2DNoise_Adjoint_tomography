/*! \file windowpanel.h
 * \brief A window panel is a plotable pgplot panel (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 25/02/2008
 * 
 * A window panel is a plotable pgplot panel (prototypes)
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
 *  - 25/02/2008   V1.0   Thomas Forbriger
 *  - 17/03/2015   V1.1   adjust libpgplotxx interface
 * 
 * ============================================================================
 */

// include guard
#ifndef STUPLO_WINDOWPANEL_H_VERSION

#define STUPLO_WINDOWPANEL_H_VERSION \
  "STUPLO_WINDOWPANEL_H   V1.1   (17-03-2015)"

#include <string>
#include <pgplotxx/xpgplotxx.h>

#include "panel.h"
#include "scaling.h"

namespace stuplo {

  //! base class
  class WindowPanel {
    public:
      WindowPanel(const pgplot::Trect& tvp=pgplot::c_rect0101,
                  const pgplot::Trect& vp=pgplot::c_rect0101): 
        Mtvp(tvp), Mvp(vp) { }
      // needs a virtual destructor
      virtual ~WindowPanel() {}
      virtual void plot(pgplot::basic_device& dev) const =0;
      void settvp(const pgplot::Trect& tvp) { Mtvp=tvp; }
      void setvp(const pgplot::Trect& vp) { Mvp=vp; }
      pgplot::Trect vp() const { return Mvp; }
      pgplot::Trect tvp() const { return Mtvp; }
      //! return graph viewport relative to total view surface
      pgplot::Trect svp() const;
    private:
      //! total viewport to use relative to view surface
      pgplot::Trect Mtvp;
      //! viewport to use relative to my viewport on the view surface
      pgplot::Trect Mvp;
  }; // class WindowPanel

  /*======================================================================*/

  //! write text to a window panel
  class TextWindowPanel: public WindowPanel {
    public:
      typedef WindowPanel Tbase;
      TextWindowPanel(const pgplot::Trect& tvp=pgplot::c_rect0101,
                      const pgplot::Trect& vp=pgplot::c_rect0101,
                      const std::string text=""): 
        Tbase(tvp, vp), Mtext(text) { }
      // needs a virtual destructor
      virtual ~TextWindowPanel() {}
      virtual void plot(pgplot::basic_device& dev) const;
      std::string text() const { return Mtext; }
      void settext(const std::string& text) { Mtext=text; }
    private:
      //! text to be written
      std::string Mtext;
  }; // class TextWindowPanel

  /*======================================================================*/

  //! plot a time axis
  class TimeAxisWindowPanel: public WindowPanel {
    public:
      typedef WindowPanel Tbase;
      TimeAxisWindowPanel(const pgplot::Trect& tvp=pgplot::c_rect0101, 
                          const pgplot::Trect& vp=pgplot::c_rect0101, 
                          const pgplot::Trange tr=pgplot::c_range01,
                          const std::string& label="time"): 
        Tbase(tvp, vp), Mtimerange(tr), Mlabel(label) { }
      // needs a virtual destructor
      virtual ~TimeAxisWindowPanel() {}
      virtual void plot(pgplot::basic_device& dev) const;
      pgplot::Trange range() const { return Mtimerange; }
      void setrange(const pgplot::Trange& tr) { Mtimerange=tr; }
      void setrange(const PanelTime& pt);
      void setlabel(const std::string& label) { Mlabel=label; }
    private:
      //! time range always is defined in float (representing seconds)
      pgplot::Trange Mtimerange;
      //! label to print
      std::string Mlabel;
  }; // class TimeAxisWindowPanel

  /*======================================================================*/

  // plot chart stepped time series
  class ChartStepWindowPanel: public WindowPanel {
    public:
      typedef WindowPanel Tbase;
      //! vector needs a default constructor
      ChartStepWindowPanel():
        Tbase(pgplot::c_rect0101, pgplot::c_rect0101), Mpanelnumber(-1),
        Mscaling(new Scaling()) { }
      //! pass data later:
      ChartStepWindowPanel(const Panel& panel,
                           const int& panelnumber,
                           const THScaling& scaling,
                           const PGstyle& style,
                           const pgplot::Trect& tvp=pgplot::c_rect0101,
                           const pgplot::Trect& vp=pgplot::c_rect0101,
                           const bool& debug=false): 
        Tbase(tvp, vp), Mpanelnumber(panelnumber),
        Mscaling(scaling),
        Mpanel(panel),
        Mstyle(style),
        Mdebug(debug) { }
      // needs a virtual destructor
      virtual ~ChartStepWindowPanel() {}
      virtual void plot(pgplot::basic_device& dev) const;
    private:
      //! my index to scaling vector
      int Mpanelnumber;
      //! my handle to the scaling struct
      THScaling Mscaling;
      //! my panel data
      Panel Mpanel;
      //! PGPLOT style options
      stuplo::PGstyle Mstyle;
      //! debug option
      bool Mdebug;
  }; // class ChartStepWindowPanel

} // namespace stuplo

#endif // STUPLO_WINDOWPANEL_H_VERSION (includeguard)

/* ----- END OF windowpanel.h ----- */
