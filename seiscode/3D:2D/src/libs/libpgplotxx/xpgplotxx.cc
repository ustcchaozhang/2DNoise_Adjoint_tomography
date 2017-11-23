/*! \file xpgplotxx.cc
 * \brief implementation of extension functions
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/01/2002
 * 
 * C++ extensions for PGPLOT (implementation)
 * 
 * Copyright '(c)' 2002 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 16/01/2002   V1.0   Thomas Forbriger
 *  - 13/02/2002   V1.1
 *                      - Tpanel::setvp now inline
 *  - 15/02/2002   V1.2   text printing facility
 *  - 21/02/2002   V1.3   correct tick mark plotting
 *  - 28/02/2008   V1.4   support time scales
 *  - 08/03/2008   V1.5   
 *                        - support rgb and hls in linestyle
 *                        - implemented space function in Ttext
 *  - 17/03/2015   V1.6   rename file to adopt naming convention in Seitosh
 * 
 * ============================================================================
 */
#define TF_XPGPLOTCPP_CC_VERSION \
  "TF_XPGPLOTCPP_CC   V1.6   (17-03-2015)"

#include <string>
#include <iostream>
#include <new>
#include <cmath>
#include <pgplotxx/xpgplotxx.h>

namespace pgplot {

  namespace helpers {

    //! limit a value to min and max
    template<class T, class T2>
      T limit(const T& v, const T2& min, const T2& max)
      {
        T retval = v > min ? v : min;
        retval = retval < max ? retval : max;
        return(retval);
      } // T limit(const T& v, const T2& min, const T2&, max)

  }; // namespace helpers 

  /*======================================================================*/
  /*! class Tlinestyle
   */
  void Tlinestyle::apply(basic_device& dev) const
  {
    if (Mflags&Fls) dev.sls(Mls); 
    if (Mflags&Flw) dev.slw(Mlw); 
    if (Mflags&Fci) 
    {
      dev.sci(Mci); 
      if (Mflags&Frgb) dev.scr(Mci, Mcol);
      if (Mflags&Fhls) dev.shls(Mci, Mcol);
    }
  }

  /*----------------------------------------------------------------------*/

  //! set hls values
  Tlinestyle& Tlinestyle::sethls(const Tcol& hls) 
  {
    Mflags &= (~Frgb);
    Mflags |= Fhls;
    Mcol.r = helpers::limit(hls.h, 0., 360.);
    Mcol.g = helpers::limit(hls.l, 0., 1.);
    Mcol.b = helpers::limit(hls.s, 0., 1.);
  }

  /*----------------------------------------------------------------------*/

  //! set rgb values
  Tlinestyle& Tlinestyle::setrgb(const Tcol& rgb) 
  {
    Mflags &= (~Fhls);
    Mflags |= Frgb;
    Mcol.r = helpers::limit(rgb.r, 0., 1.);
    Mcol.g = helpers::limit(rgb.g, 0., 1.);
    Mcol.b = helpers::limit(rgb.b, 0., 1.);
  }

  /*----------------------------------------------------------------------*/

  //! true if not equal
  bool Tlinestyle::operator!=(const Tlinestyle& ls) const
  {
    if (this->Mflags != ls.Mflags) return(true);
    if ((Mflags&Flw) && (this->Mlw != ls.Mlw)) return(true);
    if ((Mflags&Fls) && (this->Mls != ls.Mls)) return(true);
    if ((Mflags&Fci) && (this->Mci != ls.Mci)) return(true);
    if ((Mflags&Fci) && (Mflags&(Frgb|Fhls)) &&
        (this->Mcol.r != ls.Mcol.r)) return(true);
    if ((Mflags&Fci) && (Mflags&(Frgb|Fhls)) &&
        (this->Mcol.g != ls.Mcol.g)) return(true);
    if ((Mflags&Fci) && (Mflags&(Frgb|Fhls)) &&
        (this->Mcol.b != ls.Mcol.b)) return(true);
    return(false);
  } // bool Tlinestyle::operator!=(const Tlinestyle& ls) const

  /*======================================================================*/
  /*! class Ttiledpanels
   */

  void Ttiledpanels::setup(const Trect& svp, 
                      const Trect& win, const int& nx, const int& ny)
        {
          Mnx=(nx>0 ? nx : 1); 
          Mny=(ny>0 ? ny : 1); 

          float dx=(Mvp.x.max-Mvp.x.min)/Mnx;
          float dy=(Mvp.y.max-Mvp.y.min)/Mny;
          
          Mpanels=new Tpanel[Mny*Mnx];
          for (int i=0; i<Mnx; i++)
          { 
            for (int j=0; j<Mny; j++)
            {
              Tpanel& panel=Mpanels[i+j*Mnx];
              panel.setwin(win);
              panel.settvp(Trect(Mvp.x.min+i*dx,
                                 Mvp.x.min+(i+1)*dx,
                                 Mvp.y.min+j*dy,
                                 Mvp.y.min+(j+1)*dy));
              panel.setvp(svp);
            }
          }
        }

  /*----------------------------------------------------------------------*/

  Ttiledpanels::Ttiledpanels(const Ttiledpanels& panels):
    Mvp(panels.Mvp), Mnx(panels.Mnx), Mny(panels.Mny)
    {
      Mpanels=new Tpanel[Mny*Mnx];
      for (int i=0; i<Mnx; i++)
      { 
        for (int j=0; j<Mny; j++)
        { Mpanels[i+j*Mnx]=panels.Mpanels[i+j*Mnx]; }
      }
    }

  /*----------------------------------------------------------------------*/

  Ttiledpanels& Ttiledpanels::operator=(const Ttiledpanels& panels)
    {
      Mvp=panels.Mvp;
      Mnx=panels.Mnx;
      Mny=panels.Mny;
      delete[] Mpanels;
      Mpanels=new Tpanel[Mny*Mnx];
      for (int i=0; i<Mnx; i++)
      { 
        for (int j=0; j<Mny; j++)
        { Mpanels[i+j*Mnx]=panels.Mpanels[i+j*Mnx]; }
      }
      return(*this);
    }

  /*----------------------------------------------------------------------*/

  Ttiledpanels::~Ttiledpanels()
      { delete[] Mpanels; }

  /*----------------------------------------------------------------------*/

  Tpanel& Ttiledpanels::operator()(const int& ix, const int& iy)
      {
        int iix=(ix<Mnx ? ix : Mnx-1);
        int iiix=(iix>-1 ? iix : 0);
        int iiy=(iy<Mny ? iy : Mny-1);
        int iiiy=(iiy>-1 ? iiy : 0);
        return(Mpanels[iiix+iiiy*Mnx]);
      }

  /*======================================================================*/
  /* class Tboxstyle
   */
  
  // default constructor
  using namespace pgplot::boxflags;
  Tboxstyle::Tboxstyle(const bool& tbox):
      Mgridflags(Fhvselect), Mgridstyle(1, 4, 1),
      Mframeflags(Frltbselect), Mframestyle(3, 1, 1) ,
      Maxisflags(Fnone), Maxisstyle(1, 1, 1) ,
      Mxtickflags(Fselect|Fticksmajor|Fticksminor), 
      Mytickflags(Fselect|Fticksmajor|Fticksminor),
      Mxtick(0.), Mytick(0.), Mxticksub(0), Myticksub(0),
      Mticksstyle(1, 1, 1) ,
      Mxlabelflags(Fselect), 
      Mylabelflags(Fselect), Mlabelstyle(1, 1, 1),
      Mtbox(tbox),
      Mxtimeflags(Fnone), 
      Mytimeflags(Fnone) 
    { }
 
  /*----------------------------------------------------------------------*/

  //! general mode setting
  Tboxstyle& Tboxstyle::setmode(const int& elem, const int& modes, 
                     const bool& set)
  {
    if (elem&Fframe) {
      if (set) { Mframeflags=(Mframeflags|modes); }
      else { Mframeflags=(Mframeflags&(~modes)); }
    }
    if (elem&Faxis) {
      if (set) { Maxisflags=(Maxisflags|modes); }
      else { Maxisflags=(Maxisflags&(~modes)); }
    }
    if (elem&Fgrid) {
      if (set) { Mgridflags=(Mgridflags|modes); }
      else { Mgridflags=(Mgridflags&(~modes)); }
    }
    if (elem&Fxticks) {
      if (set) { Mxtickflags=(Mxtickflags|modes); }
      else { Mxtickflags=(Mxtickflags&(~modes)); }
    }
    if (elem&Fyticks) {
      if (set) { Mytickflags=(Mytickflags|modes); }
      else { Mytickflags=(Mytickflags&(~modes)); }
    }
    if (elem&Fxlabels) {
      if (set) { Mxlabelflags=(Mxlabelflags|modes); }
      else { Mxlabelflags=(Mxlabelflags&(~modes)); }
    } 
    if (elem&Fylabels) {
      if (set) { Mylabelflags=(Mylabelflags|modes); }
      else { Mylabelflags=(Mylabelflags&(~modes)); }
    } 
    if (elem&Fxtime) {
      if (set) { Mxtimeflags=(Mxtimeflags|modes); }
      else { Mxtimeflags=(Mxtimeflags&(~modes)); }
    } 
    if (elem&Fytime) {
      if (set) { Mytimeflags=(Mytimeflags|modes); }
      else { Mytimeflags=(Mytimeflags&(~modes)); }
    } 
    return(*this);
  } // Tboxstyle::setmode
 
  /*----------------------------------------------------------------------*/

  //! set linestyle
  Tboxstyle& Tboxstyle::setstyle(const int& elem, const Tlinestyle& style)
  {
    if (elem&Fframe) {
    } else if (elem&Faxis) {
      Maxisstyle=style;
    } else if (elem&Fgrid) {
      Mgridstyle=style;
    } else if (elem&(Fxticks|Fyticks)) {
      Mticksstyle=style;
    } else if (elem&(Fxlabels|Fylabels)) {
      Mlabelstyle=style;
    } 
    return(*this);
  }
 
  /*----------------------------------------------------------------------*/

  //! issue appropriate box or axis command
  void Tboxstyle::issuecommand(basic_device& dev,
                               const Tlinestyle& linestyle,
                               const char* xopt,
                               const char* yopt) const
  {
    if (Mtbox)
    {
      linestyle(dev).tbox(xopt,Mxtick,Mxticksub,
                          yopt,Mytick,Myticksub);
    }
    else
    {
      linestyle(dev).box(xopt,Mxtick,Mxticksub,
                         yopt,Mytick,Myticksub);
    }
  } // void Tboxstyle::issuecommand

  /*----------------------------------------------------------------------*/

  //! apply to basic device class
  void Tboxstyle::apply(basic_device& dev) const
  {
    // save plot settings
    dev.save();

    std::string xopt, yopt;

    // prepare time options as base for all further calls
    std::string txopt="", tyopt="";
    if (Mtbox)
    { 
      if ((Mxtimeflags&Fxyselect)|(Mytimeflags&Fxyselect))
      {
        if (Mxtimeflags& Fxyselect)
        {
          txopt.append("Z");
          if (Mxtimeflags& Ftimenoday ) txopt.append("Y");
          if (Mxtimeflags& Ftimemod24 ) txopt.append("X");
          if (Mxtimeflags& Ftimesuper ) txopt.append("H");
          if (Mxtimeflags& Ftimedegrees ) txopt.append("D");
          if (Mxtimeflags& Ftimeomitfirst ) txopt.append("F");
          if (Mxtimeflags& Ftimeomitzeros ) txopt.append("O");
        } // if (Mxtimeflags& Fxyselect)
        if (Mytimeflags& Fxyselect)
        {
          tyopt.append("Z");
          if (Mytimeflags& Ftimenoday ) tyopt.append("Y");
          if (Mytimeflags& Ftimemod24 ) tyopt.append("X");
          if (Mytimeflags& Ftimesuper ) tyopt.append("H");
          if (Mytimeflags& Ftimedegrees ) tyopt.append("D");
          if (Mytimeflags& Ftimeomitfirst ) tyopt.append("F");
          if (Mytimeflags& Ftimeomitzeros ) tyopt.append("O");
        } // if (Mytimeflags& Fxyselect)
      } // if ((Mxtimeflags&Fxyselect)|(Mytimeflags&Fxyselect))
    } // if (Mtbox)

    // frame
    if (Mframeflags) 
    {
      xopt=txopt; yopt=tyopt;
      if (Mframeflags& Flselect ) yopt.append("B");
      if (Mframeflags& Frselect ) yopt.append("C");
      if (Mframeflags& Fbselect ) xopt.append("B");
      if (Mframeflags& Ftselect ) xopt.append("C");

      this->issuecommand(dev, Mframestyle, xopt.c_str(), yopt.c_str());

      // ticks
      // PGPLOT will draw ticks only when frame is drawn
      // accumulate xopt and yopt
      if ((Mxtickflags&Fxyselect)|(Mytickflags&Fxyselect))
      {
        if (Mxtickflags& Fxyselect)
        {
          if (Mxtickflags& Fticksinvert ) xopt.append("I");
          if (Mxtickflags& Fticksextend ) xopt.append("P");
          if (Mxtickflags& Fticksmajor ) xopt.append("T");
          if (Mxtickflags& Fticksminor ) xopt.append("S");
        }
        if (Mytickflags& Fxyselect ) 
        {
          if (Mytickflags& Fticksinvert ) yopt.append("I");
          if (Mytickflags& Fticksextend ) yopt.append("P");
          if (Mytickflags& Fticksmajor ) yopt.append("T");
          if (Mytickflags& Fticksminor ) yopt.append("S");
        }

        // redraws frame in ticksstyle but now also plots tick marks
        this->issuecommand(dev, Mticksstyle, xopt.c_str(), yopt.c_str());
      }
    }

    // axis
    if (Maxisflags) 
    {
      xopt=txopt; yopt=tyopt;
      if (Maxisflags& Fxselect ) xopt.append("A");
      if (Maxisflags& Fyselect ) yopt.append("A");
      this->issuecommand(dev, Maxisstyle, xopt.c_str(), yopt.c_str());
    }

    // grid
    if (Mgridflags) 
    {
      xopt=txopt; yopt=tyopt;
      if (Mgridflags& Fvselect ) xopt.append("G");
      if (Mgridflags& Fhselect ) yopt.append("G");
      this->issuecommand(dev, Mgridstyle, xopt.c_str(), yopt.c_str());
    }

    // labels
    if ((Mxlabelflags&Fxyselect)|(Mylabelflags&Fxyselect))
    {
      xopt=txopt; yopt=tyopt;
      if (Mxlabelflags& Fxyselect)
      {
        if (Mxlabelflags& Flabelopp ) {
          xopt.append("M");
        } else {
          xopt.append("N");
        }
        if (Mxlabelflags& Flabelvert ) xopt.append("V");
        if (Mxlabelflags& Flabeldec ) {
          xopt.append("1");
        } else if (Mxlabelflags& Flabelexp ) {
          xopt.append("2");
        }
        if (Mxlabelflags& Flabellog ) xopt.append("L");
      }
      if (Mylabelflags& Fxyselect)
      {
        if (Mylabelflags& Flabelopp ) {
          yopt.append("M");
        } else {
          yopt.append("N");
        }
        if (Mylabelflags& Flabelvert ) yopt.append("V");
        if (Mylabelflags& Flabeldec ) {
          yopt.append("1");
        } else if (Mylabelflags& Flabelexp ) {
          yopt.append("2");
        }
        if (Mylabelflags& Flabellog ) yopt.append("L");
      }
      this->issuecommand(dev, Mlabelstyle, xopt.c_str(), yopt.c_str());
    }

    // restore plot settings
    dev.unsa();
  } // Tboxstyle::apply

  /*======================================================================*/
  /* class Ttext
   */
  Ttext& Ttext::print(basic_device& dev, const char* txt, 
               const bool& nl, const float& sep)
  {
    float xbox[4], ybox[4];
    // get bounding box
    dev.qtxt(Mxref, Myref, Mangle, Mjust, txt, xbox, ybox);
    // erase txet if requested
    if (Merase)
    {
      dev.save();
      dev.sfs(1);
      dev.sci(0);
      dev.poly(4, xbox, ybox);
      dev.unsa();
    }
    // store bounding box
    Mbbox=Tbbox(xbox, ybox);
    // print text
    dev.ptxt(Mxref, Myref, Mangle, Mjust, txt);

    this->advance(xbox, ybox, nl, sep);
    return(*this);
  } // Ttext::print

  /*----------------------------------------------------------------------*/

  void Ttext::advance(const float* xbox,
                      const float* ybox,
                      const bool& nl,
                      const float& sep)
  {
    // infer new refernce position from bounding box
    float dxh=xbox[3]-xbox[0];
    float dyh=ybox[3]-ybox[0];
    float ddh=sqrt(dxh*dxh+dyh*dyh);
    float dxv=xbox[1]-xbox[0];
    float dyv=ybox[1]-ybox[0];
    float ddv=sqrt(dxv*dxv+dyv*dyv);
    dxv+=sep*dxv/ddv;
    dyv+=sep*dyv/ddv;
    dxh+=sep*dxh/ddh;
    dyh+=sep*dyh/ddh;
    if (nl)
    {
      // start at new line position
      if (Minline)
      {
        // we were already within the line -> take prepare reference
        Mxref=Mxnl;
        Myref=Mynl;
      }
      else
      {
        // we did start from the reference position -> shift both
        Mxref=Mxref-dxv;
        Myref=Myref-dyv;
      }
      Mxnl=Mxref-dxv;
      Mynl=Myref-dyv;
      Minline=false;
    }
    else
    {
      // stay in line
      if (!Minline)
      {
        // we did start from newline position -> remember new newline
        Mxnl=Mxref-dxv;
        Mynl=Myref-dyv;
      }
      // find new reference position
      // this will only work with Mjust=0. and Mjust=1.
      Mxref=Mxref+(1.-2.*Mjust)*dxh;
      Myref=Myref+(1.-2.*Mjust)*dyh;
      Minline=true;
    }
  } // Ttext::advance

  /*----------------------------------------------------------------------*/

  Ttext& Ttext::space(basic_device& dev)
  {
    const char* spacechar="t";
    float xbox[4], ybox[4];
    // get bounding box
    dev.qtxt(Mxref, Myref, Mangle, Mjust, spacechar, xbox, ybox);
    // erase txet if requested
    if (Merase)
    {
      dev.save();
      dev.sfs(1);
      dev.sci(0);
      dev.poly(4, xbox, ybox);
      dev.unsa();
    }
    this->advance(xbox, ybox, false, 0.);
    return(*this);
  } // Ttext::space

  /*======================================================================*/
  /*
   * constants
   * =========
   */
  //! range form 0 to 1
  Trange c_range01(0.,1.);
  //! range form 0.1 to 0.9
  Trange c_range19(0.1,0.9);
  //! rect form 0,0 to 1,1
  Trect c_rect0101(c_range01, c_range01);
  //! rect form 0.1,0.1 to 0.9,0.9
  Trect c_rect1919(c_range19, c_range19);
  
} // namespace pgplot

/* ----- END OF xpgplotxx.cc ----- */
