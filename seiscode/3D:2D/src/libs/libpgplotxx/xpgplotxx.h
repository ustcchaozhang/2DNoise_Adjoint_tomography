/*! \file xpgplotxx.h
 * \brief C++ extensions to PGPLOT
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/01/2002
 * 
 * C++ extensions to PGPLOT (prototypes)
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
 *  - 12/02/2002   V1.1   changed to new iterator style
 *  - 13/02/2002   V1.2
 *                      - added panel shrinking facilities
 *                      - exchanged constructor arguments of Tpanel!
 *  - 15/02/2002   V1.3   text printing facility
 *  - 26/02/2002   V1.4   Tpanel returns settings
 *  - 28/02/2008   V1.5   support time scales
 *  - 08/03/2008   V1.6   support rgb and hls setting in linestyle
 *  - 17/03/2015   V1.7   rename file to adopt naming convention in Seitosh
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_XPGPLOTCPP_H_VERSION

#define TF_XPGPLOTCPP_H_VERSION \
  "TF_XPGPLOTCPP_H   V1.7   (17-03-2015)"

#include<pgplotxx/pgplotxx.h>

namespace pgplot {

/*! \brief Interface provided through xpgplotxx.h
 *
 * \defgroup xpgplotcpp_h Interface provided through xpgplotxx.h
 */
/*@{*/

  /*! \brief line style attributes
   *
   * This class may store and apply line style plotting attributes
   *
   * \note
   *   The class supports colour setting by rgb or hls values. These settings
   *   will only take effect, if a colour index was explicitely selected.
   */
  class Tlinestyle {
    public:
      //! flags indicating the setting used
      enum Eflags {
        Fnone=0, Fls=1, Flw=2, Fci=4, Frgb=8, Fhls=16
      };
      //! default constructor
      Tlinestyle():
        Mls(1), Mci(1), Mlw(1), Mflags(Fnone) { }
      //! linewidth constructor
      Tlinestyle(const int& lw):
        Mls(1), Mci(1), Mlw(lw), Mflags(Flw) { }
      //! linewidth and linestyle constructor
      Tlinestyle(const int& lw, const int& ls):
        Mls(ls), Mci(1), Mlw(lw), Mflags(Flw|Fls) { }
      //! linewidth and linestyle and color index constructor
      Tlinestyle(const int& lw, const int& ls, const int& ci):
        Mls(ls), Mci(ci), Mlw(lw), Mflags(Flw|Fls|Fci) { }
      //! \name style setting
      //@{
      //! set linewidth
      Tlinestyle& setlw(const int& lw)
      { Mflags=(Mflags | Flw); Mlw=lw; return(*this); }
      //! set linestyle
      Tlinestyle& setls(const int& ls)
      { Mflags=(Mflags | Fls); Mls=ls; return(*this); }
      //! set color index
      Tlinestyle& setci(const int& ci)
      { Mflags=(Mflags | Fci); Mci=ci; return(*this); }
      //! set color representation with hls values
      Tlinestyle& sethls(const Tcol& hls);
      //! set color representation with rgb values
      Tlinestyle& setrgb(const Tcol& rgb);
      //! discard line width setting
      Tlinestyle& clw() { Mflags &= (!Flw); return(*this); }
      //! discard line style setting
      Tlinestyle& cls() { Mflags &= (!Fls); return(*this); }
      //! discard colour index setting
      Tlinestyle& cci() { Mflags &= (!Fci); return(*this); }
      //! discard colour representation setting
      Tlinestyle& ccr() { Mflags &= (!(Frgb|Fhls)); return(*this); }
      //@}
      
      //! compare
      bool operator!=(const Tlinestyle& ls) const;
        
      //! apply linestyle to device context
      template<class T>
      T& operator()(T& dev) const
      { apply(dev); return(dev); }
    private:
      //! apply to basic device class
      void apply(basic_device& dev) const;

      //! line style
      int Mls;
      //! color index
      int Mci;
      //! line width
      int Mlw;
      //! flags
      int Mflags;
      //! rgb or hls colour setting
      Tcol Mcol;
  }; // class Tlinestyle

  //! compare for euqality
  inline bool operator==(const Tlinestyle& a, const Tlinestyle& b) 
  { return(!(a!=b)); }

  /*----------------------------------------------------------------------*/

  //! forward declaration
  class Ttiledpanels;

  /*! \brief panel area
   *
   * This class may store a panle view area
   *
   * \param T any pgplot device class
   */
  class Tpanel { 
    //! allow class Ttiled panels to call the default constructor
    friend class Ttiledpanels;
    public:
      /*! constructor
       *
       * \param win world coordinates
       * \param vp viewport relative to \c tvp
       * \param tvp erase-area on vire surface
       */
      Tpanel(const Trect& win, 
             const Trect& tvp=Trect(0.,1.,0.,1.),
             const Trect& vp=Trect(0.1,0.9,0.1,0.9)):
        Mvp(tvp), Mtvp(tvp), Mwin(win) 
        { setvp(vp); }
      /*! constructor
       *
       * \param ref reference panel. Take all values relative to the total
       *            viewport of this reference panel.
       * \param win world coordinates
       * \param vp viewport relative to \c tvp
       * \param tvp erase-area on vire surface
       */
      Tpanel(const Tpanel& ref,
             const Trect& win, 
             const Trect& tvp=Trect(0.,1.,0.,1.),
             const Trect& vp=Trect(0.1,0.9,0.1,0.9)):
        Mvp(ref.Mtvp), Mtvp(ref.Mtvp), Mwin(win) 
        { Mtvp.shrinkf(tvp); Mvp=Mtvp; setvp(vp); }
      //! set viewport relative to total viewport
      Tpanel& setvp(const Trect& vp)
      { Mvp=Mtvp; Mvp.shrinkf(vp); return(*this); }
      //! set total viewport relative to device surface
      Tpanel& settvp(const Trect& tvp)
      { Mtvp=tvp; return(*this); }
      //! set world coordinates
      Tpanel& setwin(const Trect& win)
      { Mwin=win; return(*this); }
      //! return viewport
      Trect getvp() const { return(Mvp); }
      //! return total viewport
      Trect gettvp() const { return(Mtvp); }
      //! return world coordinate range
      Trect getwin() const { return(Mwin); }
      //! use in device context
      template <class T>
      T& operator()(T& dev) const 
      {
        dev.svp(Mvp);
        dev.swin(Mwin);
        return(dev);
      }
      //! erase panel area
      template <class T>
      T& erase(T& dev) const
      {
        dev.svp(Mtvp);
        int fs=dev.qfs();
        int ci=dev.qci();
        dev.sfs(1);
        dev.sci(0);
        dev.rect(Mwin);
        dev.sfs(fs);
        dev.sci(ci);
        return(this->operator()(dev));
      }
    protected:
      //! default constructor
      Tpanel():
        Mvp(Trect(0.,1.,0.,1.)), Mtvp(Trect(0.,1.,0.,1.)),
        Mwin(Trect(0.,1.,0.,1.))
        { }
    // private:
      //! view port for graph
      Trect Mvp;
      //! total viewport including labels
      Trect Mtvp;
      //! world coordinates
      Trect Mwin;
  }; // class Tpanel

  /*----------------------------------------------------------------------*/

  /*! \brief multipanel area
   *
   * This class may hold tiled panels
   *
   * \param T any pgplot device class
   */
  class Ttiledpanels {
    public:
      /*! constructor
       *
       * \param vp total view surface area to be filled
       * \param svp viewport of each panel relative to its subarea
       * \param win world coordinates for each panel
       * \param nx number of x-subpanels
       * \param ny number of y-subpanels
       */
      Ttiledpanels(const Tpanel& panel, const Trect& svp, const Trect& win,
                   const int& nx, const int& ny):
        Mvp(panel.Mtvp), Mnx(nx), Mny(ny), Mpanels(0) 
        { this->setup(svp, win, nx, ny); }
      /*! constructor
       *
       * \param vp total view surface area to be filled
       * \param svp viewport of each panel relative to its subarea
       * \param win world coordinates for each panel
       * \param nx number of x-subpanels
       * \param ny number of y-subpanels
       */
      Ttiledpanels(const Trect& vp, const Trect& svp, const Trect& win,
                  const int& nx, const int& ny):
        Mvp(vp), Mnx(nx), Mny(ny), Mpanels(0) 
        { this->setup(svp, win, nx, ny); }
      /*! copy constructor
       *
       * Since we use a heap allocation for \c Mpanels we must provide a copy
       * constructor allocating heap for the copy - or - we must forbid copy
       * operations.
       */
      Ttiledpanels(const Ttiledpanels& panels);
      //! destructor
      ~Ttiledpanels();
      //! return number of x-subpanels
      int nxsub() const { return(Mnx); }
      //! return number of x-subpanels
      int nysub() const { return(Mny); }
      //! return subpanel
      Tpanel& operator()(const int& ix, const int& iy);
      /*! copy operator
       *
       * Since we use a heap allocation for \c Mpanels we must provide a copy
       * constructor allocating heap for the copy - or - we must forbid copy
       * operations.
       */
      Ttiledpanels& operator=(const Ttiledpanels& panels);
    private:
      //! function used by constructors
      void setup(const Trect& svp, const Trect& win,
                 const int& nx, const int& ny);
      //! the total view surface area
      Trect Mvp;
      //! number of subpanels
      int Mnx, Mny;
      //! array of subpanels
      Tpanel* Mpanels;
  }; // class Ttiledpanels

  /*----------------------------------------------------------------------*/

  //! namespace containing flags for Tboxstyle
  namespace boxflags {
    /*! style elements
     *
     * Use these flags to select for which elements you want to set or clear
     * style flags.
     */
    enum Eelem { Fnone=0,
      //! frame
      Fframe=1,
      //! axis
      Faxis=2,
      //! grid
      Fgrid=4,
      //! x-ticks
      Fxticks=8,
      //! y-ticks
      Fyticks=16,
      //! both
      Fticks=Fxticks|Fyticks,
      //! x-labels
      Fxlabels=32,
      //! y-labels
      Fylabels=64,
      //! both
      Flabels=Fxlabels|Fylabels,
      //! x-time scale
      Fxtime=128,
      //! y-time scale
      Fytime=256
    }; // enum Eelem

    /*! box style mode setting flags
     *
     * Use these flags to set or clear style options
     * (ticks, labels; grid (hv); axis (xy); frame (lrtb)).
     */
    enum Emodes { 
      //! setting selection flags
      //@{
      //! xselect, hselect, lselect (x, horizontal, left)
      Fselect=1, Fxselect=1, Fhselect=1, Flselect=1,
      //! yselect, vselect, rselect (y, vertical, right)
      Fyselect=2, Fvselect=2, Frselect=2,
      //! tselect (top)
      Ftselect=4,
      //! bselect (bottom)
      Fbselect=8,
      //! Frltbselect (right, left, top, and bottom)
      Frltbselect=Flselect|Frselect|Ftselect|Fbselect,
      //! Fxyselect, Fhvselect (x and y, left and right)
      Fxyselect=Fxselect|Fyselect, Fhvselect=Fxyselect,
      //@}

      //! ticks options
      //@{
      //! invert (tick marks)
      Fticksinvert=16,
      //! extend (tick marks)
      Fticksextend=32,
      //! major (tick marks)
      Fticksmajor=64,
      //! minor (tick marks)
      Fticksminor=128,
      //@}

      //!\name numeric labels options
      //@{
      //! opposite (labels)
      Flabelopp=16,
      //! vertical (labels)
      Flabelvert=32,
      //! force decimal (labels)
      Flabeldec=64,
      //! force exponential (labels)
      Flabelexp=128,
      //! logarithmic scale (labels)
      Flabellog=256,
      //@}

      //!\name time scale options
      //@{
      //! do not include day field (time scale)
      Ftimenoday=16,
      //! hour modulus 24h (time scale)
      Ftimemod24=32,
      //! use superscript symbols (time scale)
      Ftimesuper=64,
      //! degrees, minutes, and seconds (time scale)
      Ftimedegrees=128,
      //! omit first label (time scale)
      Ftimeomitfirst=256,
      //! omit leading zeros (time scale)
      Ftimeomitzeros=512,
      //@}

      //! all
      Fall=~Fnone
    }; // enum Emodes
  } // namespace boxflags

  /*! \brief box style
   *
   * This class holds a box style
   *
   * a lot of functionality may be added in the future :-)
   *
   * The style includes
   * - separate linestyles for
   *   - frame
   *   - axes (both have the same style)
   *   - ticks
   *   - labels (both have the same style)
   *   - grid
   * - for x and y tick marks separately:
   *   - select or selected; if selected
   *     - extended
   *     - inverted
   *     - major tick marks
   *     - minor tick marks
   *   - non-standard timemark intervals if requested
   * - elements may be switched on/off individually:
   *   - axes (with tick marks)
   *     - x-axis
   *     - y-axis
   *   - labels
   *     - x-labels 
   *     - y-labels
   *     - for both may be selected independently:
   *       - opposite or standard location
   *       - vertical or standard character orientation
   *       - decimal values
   *       - exponential values
   *       - logarithmic scale
   *   - grid lines
   *     - vertical
   *     - horizontal
   *   - frame
   *     - left
   *     - right
   *     - top
   *     - bottom
   * - if the box is selected to be a time scale box (by constructor)
   *   further operations may be applied to x and scales independently:
   *   - switch time scale on/of
   *   - select day field
   *   - present hour value modulo 24h
   *   - use degree, seconds, and minutes symbols
   *   - omit first label
   *   - omit zeros
   */
  class Tboxstyle {
    public:
      /*! default constructor is not very interesting
       *
       * \param tbox supports TBOX elements if true (this is the only place to
       *             activate time scales)
       */
      Tboxstyle(const bool& tbox=false);
      //! \name modification functions
      //@{
      /*! general mode setting
       *
       * \param elem selects elements like Fframe, Fxticks, Fylabels, etc.
       * \param modes selects flags to apply to the elements like 
       *              Fxselect, Flabelopp, Ftickminor, etc.
       * \param set flags will be set if true (cleared otherwise)
       */
      Tboxstyle& setmode(const int& elem, const int& modes, 
                         const bool& set=true);
      //! set linestyle
      Tboxstyle& setstyle(const int& elem, const Tlinestyle& style);
      //! set x-ticks interval
      Tboxstyle& setxtickint(const int& minor=0, const float& major=0.)
      { Mxtick=major; Mxticksub=minor; return(*this); }
      //! set y-ticks interval
      Tboxstyle& setytickint(const int& minor=0, const float& major=0.)
      { Mytick=major; Myticksub=minor; return(*this); }
      //@}
        
      //! apply boxstyle to device context
      template<class T>
      T& operator()(T& dev) const
      { apply(dev); return(dev); }
    private:
      //! apply to basic device class
      void apply(basic_device& dev) const;
      //! issue appropriate box command
      void issuecommand(basic_device& dev,
                        const Tlinestyle& linestyle,
                        const char* xopt,
                        const char* yopt) const;

      //! \name grid configuration
      //@{
      //! has grid
      int Mgridflags;
      //! grid linestyle
      Tlinestyle Mgridstyle;
      //@}
      
      //! \name frame configuration
      //@{
      //! has frame
      int Mframeflags;
      //! frame linestyle
      Tlinestyle Mframestyle;
      //@}
      
      //! \name axis configuration
      //@{
      //! has axis
      int Maxisflags;
      //! axis linestyle
      Tlinestyle Maxisstyle;
      //@}
      
      //! \name tick marks configuration
      //@{
      //! tick options
      int Mxtickflags, Mytickflags;
      //! major tick interval
      float Mxtick, Mytick;
      //! tick subintervals
      int Mxticksub, Myticksub;
      //! ticks linestyle
      Tlinestyle Mticksstyle;
      //@}
      
      //! \name numeric label configuration
      //@{
      //! label options
      int Mxlabelflags, Mylabelflags;
      //! labels linestyle
      Tlinestyle Mlabelstyle;
      //@}
      
      //! \name time box configuration
      //@{
      //! is a TBOX
      bool Mtbox;
      //! time scale options
      int Mxtimeflags, Mytimeflags;
      //@}
  }; // class Tboxstyle

  /*----------------------------------------------------------------------*/

  /*! \brief text plotting tool.
   *
   * This class ist used to plot multiline text.
   *
   * By calling the constructor, you set the initial reference point for the
   * text to be plotted. The member function print can then be called
   * repeatedly. Upon each call to print you may decide whether to issue a
   * carriage return after plotting the text or not.
   */
  class Ttext {
    public:
      //! \brief default constructor.
      Ttext(): Mxref(0.), Myref(0.), Mangle(0.), Mjust(0.),
               Mxnl(0.), Mynl(0.), Minline(false) { }
       /*! \brief setup initial text position and orientation.
        *
        * See pgptxt-documentation for details.
        *
        * \param x x-position in world coordinates
        * \param y y-position in world coordinates
        * \param angle text angle
        * \param just text justification
        */
      Ttext(const float& x, const float& y, const float& angle=0.,
            const float& just=0.): 
        Mxref(x), Myref(y), Mangle(angle), Mjust(just), Mxnl(0.), 
        Mynl(0.), Minline(false), Merase(false) { }
      /*! \brief print text.
       *
       * \param dev device to write to
       * \param txt text to write
       * \param nl carriage return after string if true
       * \param sep separation to next string in same line
       */
      Ttext& print(basic_device& dev, const char* txt, 
                   const bool& nl=false, const float& sep=0.025);
      /*! \brief return bounding box of previously printed text.
       *
       * \return bounding box of previously printed text
       */
      Tbbox bbox() const { return(Mbbox); }
      //! erase text box prior to plotting text
      void erase() { Merase=true; }
      //! do not erase text box prior to plotting text
      void noerase() { Merase=false; }
      //! advance one character 
      Ttext& space(basic_device& dev);
    private:
      //! advance to new reference position
      void advance(const float* xbox,
                   const float* ybox,
                   const bool& nl,
                   const float& sep);
      float Mxref, Myref; //!< text reference position
      float Mangle, Mjust; //!< parameters to pgptxt
      float Mxnl, Mynl; //!< text reference position for carriage return
      bool Minline; //!< true if text was written previously with no nl
      Tbbox Mbbox; //!< place to store bounding box of print function
      bool Merase; //!< erase box prior to plotting text
  }; // class Ttext

  //@}
  
  //*======================================================================*/
  /* \name constants
   *
   */
  //! range form 0 to 1
  extern Trange c_range01;
  //! range form 0.1 to 0.9
  extern Trange c_range19;
  //! rect form 0,0 to 1,1
  extern Trect c_rect0101;
  //! rect form 0.1,0.1 to 0.9,0.9
  extern Trect c_rect1919;

  /*======================================================================*/
  /* \name extension functions
   *
   * Functions extending the PGPLOT functionality
   */
  //@{

  /*! \brief find range in series
   *
   * Find the range of the values in a series. The series may be any series
   * class the provides
   *
   * -# \c BrowserT the type of an appropriate browser
   *
   * The browser must provide
   * -# \c hot() true if pointing to valid element
   */
  template<class S>
  Trange find_series_range(const S& series)
  {
    typename S::BrowserT iter=series;
    float value(*iter);
    Trange result(value,value);
    while (iter.hot()) 
    {
      value=(*iter);
      result.min=(result.min > value ? value : result.min);
      result.max=(result.max < value ? value : result.max);
      ++iter;
    }
    return(result);
  }

  /*! \brief draw an open polygon from a dense series
   *
   * This implements \c cpgline functionality.
   *
   * The following is required for class SX and SY:
   *
   * -# \c BrowserT the type of an appropriate browser
   *
   * The browser must provide
   * -# \c hot() true if pointing to valid element
   *
   * \param dev device to be plotted on
   * \param x series of x-coordinates
   * \param y corresponding series of y-coordinates
   *
   * \param SX series class for x
   * \param SY series class for y
   * \param T pgplot device class
   *
   * \return returns a reference to the device context
   */
  template<class T, class SX, class SY>
  T& series_line(T& dev, const SX& x, const SY& y)
  {
    typename SX::BrowserT ix=x;
    typename SY::BrowserT iy=y;
    float fx=float(*ix);
    float fy=float(*iy);
    dev.move(fx, fy);
    while ((ix.hot()) && (iy.hot()))
    { fx=float(*ix); fy=float(*iy); dev.draw(fx, fy); ++ix; ++iy; }
    return(dev);
  }

  /*! \brief error bar directions
   *
   */
  enum Eerrdir {
    //! +x (x to x+e)
    Fdirright=1,
    //! +y (y to y+e)
    Fdirtop=2,
    //! -x (x to x-e)
    Fdirleft=3,
    //! -y (y to y-e)
    Fdirbottom=4,
    //! +/-x (x-e to x+e)
    Fdirhor=5,
    //! +/-y (y-e to y+e)
    Fdirver=6
  };

  /*! \brief draw error bars from series
   *
   * This implements \c cpgerrb functionality.
   *
   * The following is required for class SX, SY and SE:
   *
   * -# \c BrowserT the type of an appropriate browser
   *
   * The browser must provide
   * -# \c hot() true if pointing to valid element
   *
   * \param dev device to be plotted on
   * \param x series of x-coordinates
   * \param y corresponding series of y-coordinates
   * \param e corresponding series of error bars
   * \param t length of terminals to be drawn
   *
   * \param SX series class for x
   * \param SY series class for y
   * \param SE series class for e
   * \param T pgplot device class
   *
   * \return returns a reference to the device context
   */
  template<class T, class SX, class SY, class SE>
  T& series_errb(T& dev, const SX& x, const SY& y, const SE& e, 
                 const Eerrdir& dir=Fdirver, const float& t=1.)
  {
    typename SX::BrowserT ix=x;
    typename SY::BrowserT iy=y;
    typename SE::BrowserT ie=e;
    float fx=float(*ix);
    float fy=float(*iy);
    float fe=float(*ie);
    int idir=int(dir);
    while ((ix.hot()) && (iy.hot()) && (ie.hot()))
    { fx=float(*ix); fy=float(*iy); fe=float(*ie);
      dev.err1(idir, fx, fy, fe, t); ++ix; ++iy; ++ie; }
    return(dev);
  }
  
/*@}*/

} // namespace pgplot

#endif // TF_XPGPLOTCPP_H_VERSION (includeguard)

/* ----- END OF xpgplotxx.h ----- */
