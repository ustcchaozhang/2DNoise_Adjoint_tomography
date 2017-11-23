/*! \file basicdevice.h
 * \brief PGPLOT basic device class (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/03/2015
 * 
 * PGPLOT basic device class (prototypes)
 * 
 * Copyright (c) 2015 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 17/03/2015   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_PGPLOTBASICDEVICE_H_VERSION

#define TF_PGPLOTBASICDEVICE_H_VERSION \
  "TF_BASICDEVICE_H   V1.0   (17-03-2015)"

#include <pgplotxx/structs.h>

namespace pgplot {

#include<cpgplot.h>

/*! \brief Interface provided through basicdevice.h
 *
 * \defgroup basicdevice_h Interface provided through basicdevice.h
 */
/*@{*/
 
/*! \brief pgplot base class
 *
 * basic_device is the interface to cpgplot. It allows to hide functions as
 * being declared protected. They may be made public in special derived
 * classes.
 *
 * Some cpgplot-functions are dropped. These are:
 * - cpgbeg       is not needed (we use cpgopen)
 * - cpgopen      is used via devopen and the constructor
 * - cpgclos      is used via the destructor
 * - cpgend       is not allowed as termination is
 *                always done per device instance
 * - pgslct       the output device is selected implicitely by each member
 *                function
 *
 * \note
 * One pgplot device per class instance will be opened.
 * Pass this instance by reference to subroutines.
 * No copy constructor or copy operator will be provided.
 */ 
class basic_device {
  public:
    //! constructor opens PGPLOT device
    basic_device(const char* devName) { this->devopen(devName); }
    //! destructor closes PGPLOT device
    ~basic_device() { this->select(); cpgupdt(); cpgclos(); }

    // -----------------------------------------------------------------
    
    /*! \name interface to PGPLOT plot functions
     *
     * These member functions provide a 1:1 interface to the PGPLOT functions
     * offered by cpgplot.h
     */
    //@{
    //! plot arrow
    basic_device& arro(const float& x1, const float& y1, 
                       const float& x2, const float& y2)
      { this->select(); cpgarro(x1, y1, x2, y2); return(*this); } 
    //! ask before clearing page? (Logical version)
    basic_device& ask(const Logical& flag)
      { this->select(); cpgask(flag); return(*this); } 
    //! ask before clearing page? (bool version)
    basic_device& ask(const bool& flag=true)
      { this->select(); cpgask(flag); return(*this); }
    //! plot arbitrary axis
    basic_device& axis(const char *opt, const float& x1, const float& y1,
                       const float& x2, const float& y2, const float& v1,
                       const float& v2, const float& step,
                       const int& nsub, const float& dmajl,
                       const float& dmajr, const float& fmin,
                       const float& disp, const float& orient)
      { 
        this->select(); 
        cpgaxis(opt, x1, y1, x2, y2, v1, v2, step, nsub, 
                dmajl, dmajr, fmin, disp, orient);
        return(*this);
      }
    //! plot arbitrary axis
    basic_device& axis(const char *opt, const Trect& line, 
                       const Trange& val, const float& step=0.,
                       const int& nsub=0, const float& dmajl=0.01,
                       const float& dmajr=0.01, const float& fmin=0.3,
                       const float& disp=0.05, const float& orient=0.)
      { 
        return(this->axis(opt, line.x.min, line.y.min, line.x.max, line.y.max,
                          val.min, val.max, step, nsub, dmajl, dmajr, fmin, 
                          disp, orient));
      }
    //! start buffering
    basic_device& bbuf(void)
      { this->select(); cpgbbuf(); return(*this); }
    //! plot binning
    basic_device& bin(const int& nbin, const float *x, const float *data, 
                      const Logical& center)
      { this->select(); cpgbin(nbin, x, data, center); return(*this); }
    //! plot box around graph
    basic_device& box(const char *xopt, float xtick, int nxsub, 
                      const char *yopt, float ytick, int nysub)
      { 
        this->select(); 
        cpgbox(xopt, xtick, nxsub, yopt, ytick, nysub);
        return(*this);
      }
    /*
    void circ(float xcent, float ycent, float radius)
      { this->select(); }
    void conb(const float *a, int idim, int jdim, int i1, int i2, int j1, int
              j2, const float *c, int nc, const float *tr, float blank)
      { this->select(); }
    void conf(const float *a, int idim, int jdim, int i1, int i2, int j1, int
              j2, float c1, float c2, const float *tr)
      { this->select(); }
    void conl(const float *a, int idim, int jdim, int i1, int i2, int j1, int
              j2, float c, const float *tr, const char *label, int intval, int
              minint)
      { this->select(); }
    void cons(const float *a, int idim, int jdim, int i1, int i2, int j1, int
              j2, const float *c, int nc, const float *tr)
      { this->select(); }
    void cont(const float *a, int idim, int jdim, int i1, int i2, int j1, int
              j2, const float *c, int nc, const float *tr)
      { this->select(); }
    void ctab(const float *l, const float *r, const float *g, const float *b,
              int nc, float contra, float bright)
      { this->select(); }
    */
    //! draw line to position
    basic_device& draw(const float& x, const float& y)
      { this->select(); cpgdraw(x, y); return(*this); }
    //! draw line to position
    basic_device& draw(const Tcoor& c) { return(this->draw(c.x, c.y)); }
    //! stop buffering
    basic_device& ebuf(void)
      { this->select(); cpgebuf(); return(*this); } 
    //! define PGPLOT window and axis
    basic_device& env(const float &xmin, const float &xmax,
                      const float &ymin, const float &ymax, 
                      const int &just=0, const int &axis=0)
      {
        this->select();
        cpgenv(xmin, xmax, ymin, ymax, just, axis);
        return(*this);
      }
    //! define PGPLOT window and axis
    basic_device& env(const Trect rect,
                      const int &just=0, const int &axis=0)
      {
        return(this->env(rect.x.min, rect.x.max, rect.y.min, rect.y.max, 
                         just, axis));
      }
    //! erase total panel
    basic_device& eras(void)
      { this->select(); cpgeras(); return(*this); }
    //! plot single error bar
    basic_device& err1(const int& dir, const float& x, 
                       const float& y, const float& e,
                       const float& t)
      { this->select(); cpgerr1(dir, x, y, e ,t); return(*this); }
    /*
    void errb(int dir, int n, const float *x, const float *y, const float *e,
              float t)
      { this->select(); }
    void errx(int n, const float *x1, const float *x2, const float *y, float t)
      { this->select(); }
    void erry(int n, const float *x, const float *y1, const float *y2, float t)
      { this->select(); }
    void etxt(void)
      { this->select(); }
    */
    //! plot gray scale image
    basic_device& gray(const float *a, int idim, int jdim,
                       int i1, int i2, int j1, int j2, 
                       float fg, float bg, const float *tr)
      { 
        this->select(); 
        cpggray(a, idim, jdim, i1, i2, j1, j2, fg, bg, tr);
        return(*this);
      }
    /*
    void hi2d(const float *data, int nxv, int nyv, int ix1, int ix2, int iy1,
              int iy2, const float *x, int ioff, float bias, Logical center,
              float *ylims)
      { this->select(); }
    void hist(int n, const float *data, float datmin, float datmax, int nbin,
              int pgflag)
      { this->select(); }
    void iden(void)
      { this->select(); }
    void imag(const float *a, int idim, int jdim, int i1, int i2, int j1, int
              j2, float a1, float a2, const float *tr)
      { this->select(); }
    */
    //! write labels
    basic_device& lab(const char *xlbl, const char *ylbl, const char *toplbl)
      { this->select(); cpglab(xlbl, ylbl, toplbl); return(*this); }
    /*
    void len(int units, const char *string, float *xl, float *yl)
      { this->select(); }
    */
    //! draw an open polygon
    basic_device& line(const int& n, const float *xpts, const float *ypts)
      { this->select(); cpgline(n, xpts, ypts); return(*this); }
    //! move plot position
    basic_device& move(const float& x, const float& y)
      { this->select(); cpgmove(x, y); return(*this); }
    //! move plot position
    basic_device& move(const Tcoor& c) { return(this->move(c.x, c.y)); }
    //! plot text outside viewport (for labels)
    basic_device& mtxt(const char *side, const float& disp, const float& coord, 
                       const float& fjust, const char *text)
      { 
        this->select(); 
        cpgmtxt(side, disp, coord, fjust, text); 
        return(*this); 
      }
    /*
    void numb(int mm, int pp, int form, char *string, int *string_length)
      { this->select(); }
    */
    //! advance to next output page
    basic_device& page()
      { this->select(); cpgpage(); return(*this); }
    //! switch to a different panel on the view surface
    basic_device& panl(int nxc, int nyc)
      { this->select(); cpgpanl(nxc, nyc); return(*this); }
    /*
    void pap(float width, float aspect)
      { this->select(); }
    void pixl(const int *ia, int idim, int jdim, int i1, int i2, int j1, int
              j2, float x1, float x2, float y1, float y2)
      { this->select(); }
    void pnts(int n, const float *x, const float *y, const int *symbol, int ns)
      { this->select(); }
    */
    //! draw polygonial line
    basic_device& poly(int n, const float *xpts, const float *ypts)
      { this->select(); cpgpoly(n, xpts, ypts); return(*this); }
    //! draw a sequence of marker symbols
    basic_device& pt(int n, const float *xpts, const float *ypts, int symbol)
      { this->select(); cpgpt(n, xpts, ypts, symbol); return(*this); }
    //! draw one marker symbol
    basic_device& pt1(float xpt, float ypt, int symbol)
      { this->select(); cpgpt1(xpt, ypt, symbol); return(*this); }
    //! write text at arbitraty position
    basic_device& ptxt(const float& x, const float& y, 
                       const float& angle, const float& fjust,
                       const char *text)
      { this->select(); cpgptxt(x, y, angle, fjust, text); return(*this); }
    //@}

    // -----------------------------------------------------------------
    
    /*! \name interface to PGPLOT query functions
     *
     * Functions to inquire various plot settings
     */
    //@{
    /*
    void qah(int *fs, float *angle, float *barb)
      { this->select(); }
    void qcf(int *font)
      { this->select(); }
    void qch(float *size)
      { this->select(); }
    void qci(int *ci)
      { this->select(); }
    void qcir(int *icilo, int *icihi)
      { this->select(); }
    void qclp(int *state)
      { this->select(); }
    void qcol(int *ci1, int *ci2)
      { this->select(); }
    */
    //! inquire color representation
    basic_device& qcr(const int& ci, float *cr, float *cg, float *cb)
      { this->select(); cpgqcr(ci, cr, cg, cb); return(*this); }
    /*
    void qcs(int units, float *xch, float *ych)
      { this->select(); }
    void qdt(int n, char *type, int *type_length, char *descr, int
             *descr_length, int *inter)
      { this->select(); }
    void qfs(int *fs)
      { this->select(); }
    void qhs(float *angle, float *sepn, float *phase)
      { this->select(); }
    void qid(int *id)
      { this->select(); }
    void qinf(const char *item, char *value, int *value_length)
      { this->select(); }
    void qitf(int *itf)
      { this->select(); }
    void qls(int *ls)
      { this->select(); }
    void qlw(int *lw)
      { this->select(); }
    void qndt(int *n)
      { this->select(); }
    void qpos(float *x, float *y)
      { this->select(); }
    void qtbg(int *tbci)
      { this->select(); }
    */
    //! get bounding box of text
    basic_device& qtxt(const float& x, const float& y, const float& angle,
                       const float& fjust, const char *text,
                       float *xbox, float *ybox)
      {
        this->select();
        cpgqtxt(x, y, angle, fjust, text, xbox, ybox);
        return(*this);
      }
    //! get bounding box of text
    basic_device& qtxt(const float& x, const float& y, const float& angle,
                       const float& fjust, const char *text, Tbbox& b)
      {
        float xbox[4], ybox[4];
        this->qtxt(x, y, angle, fjust, text, xbox, ybox);
        b=Tbbox(xbox, ybox);
        return(*this);
      }
    /*
    void qvp(int units, float *x1, float *x2, float *y1, float *y2)
      { this->select(); }
    void qvsz(int units, float *x1, float *x2, float *y1, float *y2)
      { this->select(); }
    */
    //! inquire window boundary coordinates
    basic_device& qwin(float *x1, float *x2, float *y1, float *y2)
      { 
        this->select(); 
        cpgqwin(x1, x2, y1, y2); 
        return(*this);
      }
    //! inquire window boundary coordinates
    basic_device& qwin(float &x1, float &x2, float &y1, float &y2)
      { return(this->qwin(&x1, &x2, &y1, &y2)); }
    //! inquire window boundary coordinates
    basic_device& qwin(Trect &win)
      { 
        float x1,x2,y1,y2;
        this->qwin(x1, x2, y1, y2); 
        win=Trect(Trange(x1,x2),Trange(y1,y2));
        return(*this); 
      }
    //@}
      
    // -----------------------------------------------------------------
    
    /*! \name interface to PGPLOT plot functions
     */
    //@{
    //! plot rectangle (cf. poly)
    basic_device& rect(const float& x1, const float& x2,
                       const float& y1, const float& y2)
      { this->select(); cpgrect(x1, x2, y1, y2); return(*this); }
    //! plot rectangle (cf. poly)
    basic_device& rect(const Trect& rec)
      {
        this->select(); 
        cpgrect(rec.x.min, rec.x.max, rec.y.min, rec.y.max); 
        return(*this);
      }
    //@}

    // -----------------------------------------------------------------
    
    /*! \name interface to PGPLOT set functions
     *
     * Functions used to set various plot properties
     */
    //@{
    //! set arrow head style
    basic_device& sah(const int& fs, const float& angle, const float& barb)
      { this->select(); cpgsah(fs, angle, barb); return(*this); }
    //! save device settings
    basic_device& save(void)
      { this->select(); cpgsave(); return(*this); }
    //! restore device settings
    basic_device& unsa(void)
      { this->select(); cpgunsa(); return(*this); }
    //! set character font
    basic_device& scf(const int& font)
      { this->select(); cpgscf(font); return(*this); }
    //! set character height
    basic_device& sch(const float& size)
      { this->select(); cpgsch(size); return(*this); }
    //! set color index
    basic_device& sci(const int& ci)
      { this->select(); cpgsci(ci); return(*this); }
    //! set color index range
    basic_device& scir(const int& icilo, const int& icihi)
      { this->select(); cpgscir(icilo, icihi); return(*this); }
    //! set clipping mode
    basic_device& sclp(const int& state)
      { this->select(); cpgsclp(state); return(*this); }
    //! set color representation
    basic_device& scr(const int& ci, const float& cr, 
                      const float& cg, const float& cb)
      { this->select(); cpgscr(ci, cr, cg, cb); return(*this); }
    //! set color representation by rgb values
    basic_device& scr(const int& ci, const Tcol& rgb)
      { return(this->scr(ci, rgb.r, rgb.g, rgb.b)); }
    /*
    void scrl(float dx, float dy)
      { this->select(); }
    void scrn(int ci, const char *name, int *ier)
      { this->select(); }
    */
    //! set fill style
    basic_device& sfs(const int& fs)
      { this->select(); cpgsfs(fs); return(*this); }
    //! set color in the HLS system
    basic_device& shls(const int& ci, const float& ch, const float& cl,
                       const float& cs)
      { this->select(); cpgshls(ci, ch, cl, cs); return(*this); }
    //! set color representation by hls values
    basic_device& shls(const int& ci, const Tcol& hls)
      { return(this->shls(ci, hls.h, hls.l, hls.s)); }
    //! set hatching style
    basic_device& shs(const float& angle, const float& sepn,
                      const float& phase)
      { this->select(); cpgshs(angle, sepn, phase); return(*this); }
    //! set image transfer function
    basic_device& sitf(const int& itf)
      { this->select(); cpgsitf(itf); return(*this); }
    //! set line style
    basic_device& sls(const int& ls)
      { this->select(); cpgsls(ls); return(*this); }
    //! set line width
    basic_device& slw(const int& lw)
      { this->select(); cpgslw(lw); return(*this); }
    //! set background color index
    basic_device& stbg(const int& tbci)
      { this->select(); cpgstbg(tbci); return(*this); }
    //! subdivide view surface into panels
    basic_device& subp(int nxsub, int nysub)
      { this->select(); cpgsubp(nxsub, nysub); return(*this); }
    //! set viewport (view area)
    basic_device& svp(const float& xleft, const float& xright,
                      const float& ybot, const float& ytop)
      { 
        this->select();
        cpgsvp(xleft, xright, ybot, ytop); 
        return(*this);
      }
    //! set viewport (view area)
    basic_device& svp(const Trect& vp)
      { return(this->svp(vp.x.min, vp.x.max,vp.y.min,vp.y.max)); }
    //! set window (world coordinate range) 
    basic_device& swin(const float& x1, const float& x2,
                       const float& y1, const float& y2)
      { 
        this->select();
        cpgswin(x1, x2, y1, y2);
        return(*this);
      }
    //! set window (world coordinate range) 
    basic_device& swin(const Trect& win)
      { return(this->swin(win.x.min, win.x.max,win.y.min,win.y.max)); }
    //@}
   
    // -----------------------------------------------------------------
      
    /*! \name interface to PGPLOT plot functions
     */
    //@{
    //! plot tbox around graph
    basic_device& tbox(const char *xopt, float xtick, int nxsub, 
                       const char *yopt, float ytick, int nysub)
      { 
        this->select(); 
        cpgtbox(xopt, xtick, nxsub, yopt, ytick, nysub);
        return(*this);
      }

    /*
    void text(float x, float y, const char *text)
      { this->select(); }
    void tick(float x1, float y1, float x2, float y2, float v, float tikl,
              float tikr, float disp, float orient, const char *str)
      { this->select(); }
    */
    //! update plot (in buffering mode)
    basic_device& updt(void)
      { this->select(); cpgupdt(); return(*this); }
    /*
    void vect(const float *a, const float *b, int idim, int jdim, int i1, int
              i2, int j1, int j2, float c, int nc, const float *tr, float
              blank)
      { this->select(); }
    */
    //! set viewport (view area - here: in inches)
    basic_device& vsiz(const float& xleft, const float& xright,
                       const float& ybot, const float& ytop)
      { this->select(); cpgvsiz(xleft, xright, ybot, ytop); return(*this); }
    //! set viewport (view area - here: in inches)
    basic_device& vsiz(const Trect& vp)
      { return(this->vsiz(vp.x.min, vp.x.max, vp.y.min, vp.y.max)); }
    //! select standard viewport
    basic_device& vstd(void)
      { this->select(); cpgvstd(); return(*this); }
    //! annotate an image plot with a wedge
    basic_device& wedg(const char *side, 
                       const float& disp, const float& width, 
                       const float& fg, const float& bg,
                       const char *label)
      { 
        this->select();
        cpgwedg(side, disp, width, fg, bg, label);
        return(*this);
      } 
    //! annotate an image plot with a wedge
    basic_device& wedg(const char *side, 
                       const float& disp, const float& width, 
                       const Trange& range,
                       const char *label)
      { return(this->wedg(side, disp, width, range.min, range.max, label)); } 
    //! annotate an image plot with a wedge
    basic_device& gwedg(const Trange& range,
                        const char *label="value",
                        const float& disp=0.3,
                        const float& width=3.0,
                        const char *side="RG")
      { return(this->wedg(side, disp, width, range.min, range.max, label)); } 
    //! set world coordinates and adjust viewport to same aspect ratio
    basic_device& wnad(const float& x1, const float& x2, 
              const float& y1, const float& y2)
      { this->select(); cpgwnad(x1, x2, y1, y2); return(*this); } 
    //! set world coordinates and adjust viewport to same aspect ratio
    basic_device& wnad(const Trect& win)
      { return(this->wnad(win.x.min, win.x.max, win.y.min, win.y.max)); } 
    //@}

    // -----------------------------------------------------------------
    
    /*! \name interface to device-independent PGPLOT functions
     *
     * These are class specific member functions
     */
    //@{
    //! find smallest "round" number greater than x
    static float rnd(const float& x, int *nsub)
      { return(cpgrnd(x, nsub)); }
    //! tell about available devices
    static void ldev(void)
      { cpgldev(); }
    //! found suitable range
    static void rnge(const float& x1, const float& x2, float *xlo, float *xhi)
      { cpgrnge(x1, x2, xlo, xhi); }
    //! found suitable range
    static Trange rnge(const Trange& xin)
      { 
        Trange result;
        cpgrnge(xin.min, xin.max, &result.min, &result.max); 
        return(result);
      }
    //@}
   
  protected:
    /*! \name interface to PGPLOT cursor routines
     * 
     * Here we go with cpgplot functions that are only valid together
     * with special devices. They are dclared protected and may be made public
     * by a derived class. They are protected in the base class since not all
     * devices are interactive.
     */
    //@{
    int band(const int& mode, const int& posn, 
             const float& xref, const float& yref, float *x, float *y,
             char *ch_scalar)
      {
        this->select(); 
        return(cpgband(mode, posn, xref, yref, x, y, ch_scalar));
      }
    int curs(float *x, float *y, char *ch_scalar)
      {
        this->select(); 
        return(cpgcurs(x, y, ch_scalar)); 
      }
    basic_device& lcur(int maxpt, int *npt, float *x, float *y)
      { this->select(); cpglcur(maxpt, npt, x, y); return(*this); }
    basic_device& ncur(int maxpt, int *npt, float *x, float *y, int symbol)
      { this->select(); cpgncur(maxpt, npt, x, y, symbol); return(*this); }
    basic_device& olin(int maxpt, int *npt, float *x, float *y, int symbol)
      { this->select(); cpgolin(maxpt, npt, x, y, symbol); return(*this); }
    //@}
  private:
    void select() { cpgslct(MdevID); }
    void devopen(const char *devname) { MdevID=cpgopen(devname); }
    int MdevID;
}; // class basic_device

/*@}*/

} // namespace pgplot

#endif // TF_PGPLOTBASICDEVICE_H_VERSION (includeguard)

/* ----- END OF basicdevice.h ----- */
