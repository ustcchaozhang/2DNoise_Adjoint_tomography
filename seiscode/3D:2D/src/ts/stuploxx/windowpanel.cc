/*! \file windowpanel.cc
 * \brief A window panel is a plotable pgplot panel (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 25/02/2008
 * 
 * A window panel is a plotable pgplot panel (implementation)
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
 *  - 10/04/2008   V1.1   when chart stepping: replot old sample at new offset
 *  - 18/01/2011   V1.2   pgplot::Trange::total() was renamed
 *  - 17/03/2015   V1.3   adjust libpgplotxx interface
 * 
 * ============================================================================
 */
#define STUPLO_WINDOWPANEL_CC_VERSION \
  "STUPLO_WINDOWPANEL_CC   V1.1   (17-03-2015)"

#include <tfxx/error.h>
#include <tfxx/misc.h>
#include <aff/subarray.h>
#include <pgplotxx/affpgplot.h>
#include "globalsettings.h"
#include "windowpanel.h"
#include "functions.h"
#include "label.h"

namespace stuplo {

  /*======================================================================*/
  // DEBUG functions
  
  std::ostream& operator << (std::ostream& os, const pgplot::Trect& r)
  {
    os << "x.min=" << r.x.min << " "
       << "x.max=" << r.x.max << " "
       << "y.min=" << r.y.min << " "
       << "y.max=" << r.y.max << " " << std::endl;
    return(os);
  }

#define RECT( R ) std::cerr << "DEBUG (" << #R << ") " << R;

  /*======================================================================*/

  pgplot::Trect WindowPanel::svp() const
  {
    pgplot::Trect retval=Mtvp;
    retval *= Mvp;
    return(retval);
  } // pgplot::Trect WindowPanel::svp() const

  /*======================================================================*/

  void TextWindowPanel::plot(pgplot::basic_device& dev) const
  {
    // set viewport and window
    pgplot::Trect vp=this->svp();
    dev.svp(vp);
    dev.swin(pgplot::c_rect0101);
    // calculate character height
    //float ch=0.95*40.*vp.y.total();
    float ch=1.;
    dev.save();
    dev.sch(ch);
    float xbox[4], ybox[4];
    // first: determine unscaled size
    dev.qtxt(0., 0.0, 0., 0., Mtext.c_str(), xbox, ybox);
    /*
    std::cout << xbox[0] << " " << ybox[0] << std::endl;
    std::cout << xbox[1] << " " << ybox[1] << std::endl;
    std::cout << xbox[2] << " " << ybox[2] << std::endl;
    std::cout << xbox[3] << " " << ybox[3] << std::endl;
    */
    // second: determine scaling factor
    float dxh=xbox[3]-xbox[0];
    float dyv=ybox[1]-ybox[0];
    float xfactor=1./dxh;
    float yfactor=1./dyv;
    float factor = xfactor < yfactor ? xfactor : yfactor;
    // third: set scaling factor
    ch=factor;
    dev.sch(ch);
    // fourth: determine offset
    dev.qtxt(0., 0., 0., 0., Mtext.c_str(), xbox, ybox);
    /*
    std::cout << xbox[0] << " " << ybox[0] << std::endl;
    std::cout << xbox[1] << " " << ybox[1] << std::endl;
    std::cout << xbox[2] << " " << ybox[2] << std::endl;
    std::cout << xbox[3] << " " << ybox[3] << std::endl;
    */
    // fifth: correct for offset
    float xmin=-xbox[0];
    float ymin=-ybox[0];
    /*
    dev.qtxt(xmin, ymin, 0., 0., Mtext.c_str(), xbox, ybox);
    std::cout << xbox[0] << " " << ybox[0] << std::endl;
    std::cout << xbox[1] << " " << ybox[1] << std::endl;
    std::cout << xbox[2] << " " << ybox[2] << std::endl;
    std::cout << xbox[3] << " " << ybox[3] << std::endl;
    */
    // std::cerr << "DEBUG (ch): " << ch << std::endl;
    // pgplot::Tboxstyle boxstyle; boxstyle(dev);
    pgplot::Ttext text(xmin,ymin);
    text.print(dev, Mtext.c_str());
    dev.unsa();
  } // void TextWindowPanel::plot() const

  /*======================================================================*/

  void TimeAxisWindowPanel::setrange(const PanelTime& pt)
  {
    this->setrange(pt.frange());
    /*
    std::cerr << "DEBUG:" << pt.timerange.begin().timestring() << std::endl;
    std::cerr << "DEBUG:" << pt.timerange.end().timestring() << std::endl;
    std::cerr << "DEBUG:" << pt.referencetime.timestring() << std::endl;
    std::cerr << "DEBUG:" << first.timestring() << std::endl;
    std::cerr << "DEBUG:" << last.timestring() << std::endl;
    std::cerr << "DEBUG:" << range.min << " " << range.max << std::endl;
    */
  } // void TimeAxisWindowPanel::setrange(const PanelTime& pt)

  /*----------------------------------------------------------------------*/

  void TimeAxisWindowPanel::plot(pgplot::basic_device& dev) const
  {
    // RECT(this->tvp());
    // RECT(this->vp());
    pgplot::Trect vp=this->svp();
    pgplot::Trect win(Mtimerange,pgplot::c_range01);
    dev.save();
    dev.svp(vp);
    // RECT(vp);
    dev.swin(win);
    // RECT(win);
    pgplot::Tboxstyle boxstyle(true);
    // clear all
    {
      using namespace pgplot::boxflags;
      boxstyle.setmode(Fall, Fall, false);
      boxstyle.setmode(Fframe, Fbselect|Flselect|Frselect);
      boxstyle.setmode(Fxlabels, Fxselect);
      boxstyle.setmode(Fxticks, Fselect|Fticksinvert|Fticksminor|Fticksmajor);
      boxstyle.setmode(Fxtime, Fselect|Ftimesuper|Ftimemod24|Ftimeomitzeros);
      boxstyle(dev);
    }
    dev.mtxt("B", 2.8, 0.5, 0.5, Mlabel.c_str());
    dev.unsa();
  } // void TimeAxisWindowPanel::plot() const

  /*======================================================================*/

  void ChartStepWindowPanel::plot(pgplot::basic_device& dev) const
  {
    dev.save();
    TFXX_debug(Mdebug, "ChartStepWindowPanel::plot",
               "prepare viewport and window");
    // prepare viewport and world coordinate window
    pgplot::Trect vp=this->svp();
    pgplot::Trect win;
    if (Mpanel.ntraces()>0)
    {
      win=pgplot::Trect(Mscaling->commontime.frange(),
                       Mpanel.dtl.ordinaterange());
    }
    else
    {
      win=pgplot::Trect(Mscaling->commontime.frange(),
                       pgplot::c_range01);
    }
    dev.svp(vp);
    // set fraction of viewport height to be used for graph label
    float labelheight=Mstyle.graphlabelheight;
    if (!(labelheight>0))
    {
      // default: set label height to 0.025 of view surface height
      labelheight=0.030/vp.y.abs();
    }
    // reserve space for label
    pgplot::Trect adjustedwin=win;
    if (Mstyle.graphlabelreserve)
    {  
      adjustedwin.y.max += win.y.abs()*labelheight;
    }
    dev.swin(adjustedwin);
    // prepare and draw box
    pgplot::Tboxstyle boxstyle(true);
    {
      using namespace pgplot::boxflags;
      boxstyle.setmode(Fxtime, Fselect|Ftimesuper|Ftimemod24|Ftimeomitzeros);
      boxstyle.setmode(Fxlabels, Fselect, false);
      boxstyle.setmode(Fylabels, Fselect, false);
      boxstyle.setmode(Fyticks, Fselect, false);
    }
    boxstyle(dev);
    // print ordinate scale 
    dev.save();
    {
      using namespace pgplot::boxflags;
      boxstyle.setmode(Fxtime, Fselect|Ftimesuper|Ftimemod24|Ftimeomitzeros);
      boxstyle.setmode(Fxlabels, Fselect, false);
      boxstyle.setmode(Fxticks, Fselect, false);
      boxstyle.setmode(Fylabels, Fselect|Flabelvert);
      boxstyle.setmode(Fyticks, Fselect);
    }
    dev.sch(globalsettings.chy());
    boxstyle(dev);
    dev.unsa();
    Labelset labelset;
    // plot data
    // ---------
    if (Mpanel.ntraces()>0)
    {
      // iterator used to cycle through data trace list
      DataTraceList::const_iterator I=Mpanel.dtl.begin(); 
      // prepend annotations
      while (I != Mpanel.dtl.end())
      {
        GLstyle labstyle;
        labstyle.eraselabelbox=Mstyle.glstyle.eraselabelbox;
        labelset.add(Label(I->para.preannotation, labstyle,
                           pgplot::Tlinestyle()));
        ++I;
      } // while (I != Mpanel.dtl.end())

      I=Mpanel.dtl.begin(); 
      // prepare units label
      std::string ylabel=I->para.units;;
      while (I != Mpanel.dtl.end())
      {
        // calculate series index range
        // and extract time and sample series
        Tseries series=I->ts;
        Tseries time=I->time(Mscaling->commontime);

        // plot only if there is data in appropriate range
        tfxx::Range<int> trange(time.first(), time.last());
        tfxx::Range<int> srange(series.first(), series.last());
        tfxx::Range<int> commonrange=srange;
        commonrange.shrink(trange);
        if (commonrange.first()<commonrange.last())
        {
          series.setindexrange(commonrange.first(),commonrange.last());
          time.setindexrange(commonrange.first(),commonrange.last());

          // check label consistency
          TFXX_assert((ylabel == I->para.units),
                      "inconsistent units in panel");

          // do plot
          dev.save();
          I->para.graphls(dev);
          if (I->para.dochartstepping)
          {
            float offset=0;
            int ioffset=0;
            // chartstepping interval
            float yrange=win.y.abs();
            float dymax=I->para.chartsteppingwidth*yrange;
            float yavg=0.5*(win.y.max+win.y.min);
            float min=yavg-dymax*0.5;
            float max=yavg+dymax*0.5;
            float dystep=dymax*(1.-I->para.chartsteppinghystheresis);
            bool newsec=true;
            for (int i=series.first(); i<=series.last(); ++i)
            {
              float val=series(i);
              float yval=val-offset;
              float oyval=yval;
              if ((yval<min) || (yval>max)) 
              { 
                newsec=true; 
                ioffset=int((series(series.f())-yavg)/dystep);
                offset=ioffset*dystep;
                yval=val-offset;
              }
              if (newsec)
              {
                while(yval>max) 
                {
                  /*
                  std::cerr << "hu " << i << " " << yval << " " << offset << " " 
                    << dymax << " " << max << std::endl;
                    */
                  ++ioffset;
                  offset = ioffset*dystep;
                  yval = val-offset;
                }
                while(yval<min) 
                {
                  /*
                  std::cerr << "ha " << i << " " << yval << " " << offset << " " 
                    << dymax << " " << max << std::endl;
                    */
                  --ioffset;
                  offset = ioffset*dystep;
                  yval = val-offset;
                }
                if (i!=series.first()) { dev.draw(time(i), oyval); }
                dev.move(time(i), yval);
                newsec=false;
              }
              else
              {
                dev.draw(time(i), yval);
              }
            }
          }
          else
          {
            pgplot::pgaff::line(dev, time, series);
          }
          dev.unsa();
          
          // add label
          labelset.add(Label(I->label(), Mstyle.glstyle, I->para.graphls));
        }
        // if (commonrange.first()<commonrange.last())

        // next
        ++I;
      } // while (I != Mpanel.dtl.end())

      // append annotations
      I=Mpanel.dtl.begin(); 
      while (I != Mpanel.dtl.end())
      {
        GLstyle labstyle;
        labstyle.eraselabelbox=Mstyle.glstyle.eraselabelbox;
        labelset.add(Label(I->para.annotation, labstyle,
                           pgplot::Tlinestyle()));
        ++I;
      } // while (I != Mpanel.dtl.end())

      // plot units label
      dev.unsa();
      if (ylabel != std::string(Cstringnotspecified))
      {
        dev.mtxt("L", globalsettings.lyd(), 0.5, 0.5, ylabel.c_str());
      }
    } // if (Mpanel.ntraces()>0)
    else
    {
      pgplot::Tlinestyle warnls;
      warnls.setci(2);
      GLstyle glwarn;
      glwarn.colourlabel=true;
      glwarn.eraselabelbox=true;
      labelset.add(Label("no traces available", glwarn, warnls));
    } // if (Mpanel.ntraces()>0)

    // prepare label text
    // ------------------
    dev.save();
    const float border=0.003/vp.y.abs();
    // set easy to use world coordinates 
    dev.swin(pgplot::c_rect0101);
    // check actual character height
    pgplot::Tbbox tbbox;
    float labelch=1.;
    dev.sch(labelch);
    dev.qtxt(0., 0., 0., 0., "Mg", tbbox);
    float actualheight=tbbox.coor[1].y-tbbox.coor[0].y;
    // calculate appropriate character height and set position
    labelch *= (labelheight-3*border)/actualheight;
    dev.sch(labelch);
    dev.qtxt(0., 0., 0., 0., "Mg", tbbox);
    pgplot::Ttext labels(0.2*border,1.-labelheight+border-tbbox.coor[0].y);
    // draw labels
    labelset.draw(dev, labels);
    dev.unsa();
  } // void ChartStepWindowPanel::plot() const

} // namespace stuplo

/* ----- END OF windowpanel.cc ----- */
