/*! \file screen.cc
 * \brief all stuff to handle a full plot screen (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/01/2008
 * 
 * all stuff to handle a full plot screen (implementation)
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
 *  - 28/01/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define STUPLO_SCREEN_CC_VERSION \
  "STUPLO_SCREEN_CC   V1.0   "

#include <libtime++.h>
#include <tfxx/error.h>
#include <tfxx/misc.h>

#include "screen.h"

namespace stuplo {

  ChartStepperScreen::ChartStepperScreen(const PanelVector& pv, 
                                         const THScaling& hs, 
                                         const PGstyle& st,
                                         const bool& debug): 
    Tbase(pv, hs, st), Mdebug(debug)
  {
    // set limits
    const float chartbottom=0.1;
    const float charttop=1.-0.05*st.tstitle;
    // gap between chart panel as fraction of total view surface height
    const float chartgap=0.003;
    TFXX_assert(charttop>chartbottom+0.1,
                "ERROR (ChartStepperScreen): "
                "title scaling factor is too large");
    TFXX_debug(Mdebug, "ChartStepperScreen", "initialize frame panels");
    // horizontal range of view surface to be used
    pgplot::Trange chartxrange(0.1,.999);
    Mgraphvp=pgplot::Trect(chartxrange, 
                           pgplot::Trange(chartbottom, charttop));
    // setup title window
    Mtitle.settvp(pgplot::Trect(pgplot::c_range01, 
                                pgplot::Trange(charttop, 1.)));
    Mtitle.setvp(pgplot::Trect(chartxrange,
                               pgplot::c_range01));
    Mtitle.settext(st.title);
    // setup time axis window
    Mtimeaxis.settvp(pgplot::Trect(pgplot::c_range01,
                                   pgplot::Trange(0.,chartbottom)));
    Mtimeaxis.setvp(pgplot::Trect(chartxrange, pgplot::Trange(0.95,1.)));
    // prepare chart panels
    TFXX_debug(Mdebug, "ChartStepperScreen", "initialize chart panels");
    float dy=(charttop-chartbottom)/pv.size();
    Mwindowpanelvector.clear();
    pgplot::Trect chartvp(chartxrange,pgplot::c_range01);
    for (unsigned int i=0; i<pv.size(); ++i)
    {
      pgplot::Trange thischartyrange(charttop-dy*(i+1)+chartgap,
                                    charttop-dy*(i)-chartgap);
      pgplot::Trect thischartvp(pgplot::c_range01, thischartyrange);
      ChartStepWindowPanel cswp(pv[i], i, hs, st, thischartvp, chartvp,
                                Mdebug);
      if (pv[i].dtl.size()<1)
      {
        std::cerr << "Warning (ChartStepperScreen): "
          << "panel #" << i << " contains no data" << std::endl;
      }
      Mwindowpanelvector.push_back(cswp);
    }
    // set up scaling
    TFXX_debug(Mdebug, "ChartStepperScreen", "set up scaling");
    if (Mscaling->initialized)
    {
      this->adjustscaling();
    }
    else
    {
      this->initscaling();
    }
    // set up time axis window
    Mtimeaxis.setrange(Mscaling->commontime);
    std::string reftimestring
      =tfxx::string::patsubst(Mscaling->
         commontime.reference().timestring().substr(4,19),
         "/", Mstyle.datesep);
    Mtimeaxis.setlabel("time after " + reftimestring + " UT");
    TFXX_debug(Mdebug, "ChartStepperScreen", "done");
  } // ChartStepperScreen::ChartStepperScreen(const PanelVector& pv, const
    //                       THScaling& hs, const PGstyle& st)

  /*----------------------------------------------------------------------*/

  void ChartStepperScreen::plot(pgplot::basic_device& dev) const
  {
    dev.page();
    TFXX_debug(Mdebug, "ChartStepperScreen::plot", 
               "plot time axis and title");
    this->Mtimeaxis.setrange(Mscaling->commontime.frange());
    this->Mtimeaxis.plot(dev);
    this->Mtitle.plot(dev);
    Twindowpanelvector::const_iterator I=Mwindowpanelvector.begin();
    while(I != Mwindowpanelvector.end())
    {
      TFXX_debug(Mdebug, "ChartStepperScreen::plot", 
                 "plot next panel");
      I->plot(dev);
      TFXX_debug(Mdebug, "ChartStepperScreen::plot", 
                 "panel is plotted");
      ++I;
    }
  } // void ChartStepperScreen::plot(const pgplot::basic_device& dev) const
    
  /*----------------------------------------------------------------------*/

  bool ChartStepperScreen::cursor(pgplot::device& dev, const char& curval,
                                  float& x, float&y) 
  {
    bool retval=false;
    float xref=x, yref=y;
    char ch;
    switch(curval) {
      // re-initialize the scaling values
      case 'i':
        this->initscaling();
        retval=true;
        break;
      // operate on time axis
      case 't':
        // set viewport overall to all graphs
        dev.svp(Mgraphvp);
        dev.swin(pgplot::Trect(Mscaling->commontime.frange(),
                               Mgraphvp.y));
        // call cursor function and wait for user's response
        TFXX_assert(dev.band(6, 0, xref, yref, x, y, ch)==1, 
                    "PGPLOT cursor input failed!");
        xref=x;
        yref=y;
        switch(ch) {
          // initialize time axis
          case 'i':
            this->inittime();
            break;
          // zoom time axis
          case 'z':
            TFXX_assert(dev.band(4, 1, xref, yref, x, y, ch)==1, 
                        "PGPLOT cursor input failed!");
            if (xref>x)
            {
              float h=x;
              x=xref;
              xref=h;
            }
            Mscaling->commontime.setrange(pgplot::Trange(xref,x));
            break;
        } // switch(ch)
        retval=true;
        break;
    } // switch(curval)
    return(retval);
  } // void ChartStepperScreen::cursor(pgplot::device& dev, 
    // const char& curval, float& x, float&y) 

  /*----------------------------------------------------------------------*/

  void ChartStepperScreen::inittime()
  {
    libtime::TRange range = this->Mpv.timerange();
    libtime::TAbsoluteTime reference = range.begin();
    libtime::TAbsoluteTime refday(reference.year(),
                                  reference.month(),
                                  reference.day());
    Mscaling->commontime.setrange(range);
    Mscaling->commontime.setreference(refday);
    Mscaling->usecommontime=true;
  } // void ChartStepperScreen::inittime()

  /*----------------------------------------------------------------------*/
  
  void ChartStepperScreen::initscaling()
  {
    this->inittime();
    // we do not use panelwise time scales
    this->Mscaling->paneltime.clear(); 
    // we need one ordinate range per panel
    this->Mtimeaxis.setrange(Mscaling->commontime.frange());
    this->Mscaling->initialized=true;
  } // void ChartStepperScreen::initscaling()

  /*----------------------------------------------------------------------*/

  void ChartStepperScreen::adjustscaling()
  {
    if (this->Mscaling->alwaysinittime) 
    {
      this->inittime(); 
    }
  } // void ChartStepperScreen::adjustscaling()

} // namespace stuplo

/* ----- END OF screen.cc ----- */
