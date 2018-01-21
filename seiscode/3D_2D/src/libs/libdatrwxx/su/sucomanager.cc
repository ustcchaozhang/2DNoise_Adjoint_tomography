/*! \file sucomanager.cc
 * \brief manage coordinate scaling (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/12/2010
 * 
 * manage coordinate scaling (implementation)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 06/12/2010   V1.0   Thomas Forbriger
 *  - 14/01/2011   V1.1   extended modifications:
 *                        - resolved problem with coordinate values close to
 *                          zero which lead to a logarithm approaching
 *                          infinity upon testing the number of significant
 *                          digits
 *                        - resolved a problem with unreasonable large scale
 *                          values in case of small round off errors in
 *                          floating point coordinate values
 *                        - apply proper rounding to coordinate values, such
 *                          that 0.0999999 will be stored as 0.1
 *                        - check actual round-off error rather than scale
 *                          and issue a warning in cases where round-off error
 *                          exceed 0.1%
 *  - 29/03/2011   V1.2   - search for the smallest possible power larger or
 *                          equal a desired value
 *  - 22/01/2012   V1.3   
 *                        - handle control parameters
 *                        - pass control parameters to Coordinates and ScalCoo
 *                        - add conversion functions
 *  - 24/01/2012   V1.4   
 *                        - static cast of value to type float in function
 *                          ScalCoo::set(const double&) caused terrible
 *                          roundoff resulting in awfully many trailing
 *                          digits. I see no reason for this cast and remove
 *                          it.
 *                        - reworked ScalCoo::smallestpower(const short&)
 *  - 26/01/2012   V1.5   bug fix: ScalCoo::set(const double&):
 *                        initial scale value must equal one
 *                        see ticket:168
 *  - 08/07/2016   V1.6   make correct use of new DATRW_report_assert
 *  - 01/12/2016   V1.7   provide improved error message and warning to user
 * 
 * ============================================================================
 */
#define DATRW_SUCOMANAGER_CC_VERSION \
  "DATRW_SUCOMANAGER_CC   V1.7"

#include <cmath>
#include <climits>
#include <datrwxx/suformat.h>
#include <datrwxx/sucomanager.h>
#include <datrwxx/error.h>
#include <datrwxx/debug.h>
#include <datrwxx/util.h>

namespace datrw {

  namespace su {

  /*======================================================================*/

    /*! some local helpers
     * \ingroup group_su
     */
    namespace helper {

      /*! \brief output format modifier for debug output
       * \ingroup group_su
       */
      class MyOutputFormat {
        public:
          void set_output_format(std::ostream& os) const
          {
            os.width(14);
            os.precision(12);
          }
      }; // class MyOutputFormat {

      /*----------------------------------------------------------------------*/

      /*! \brief output operator to apply output format modifier
       */
      std::ostream& operator<<(std::ostream& os, const MyOutputFormat& f)
      {
        f.set_output_format(os);
        return(os);
      } // std::ostream& operator<<(std::ostream& os, const MyOutputFormat& f)


    } // namespace helper
      
    /*======================================================================*/
    // constants to support coordinate scaling

    double ScalCoo::effectivezero=datrw::su::subformat::def::thresholdzero;

    /*----------------------------------------------------------------------*/

    /*! set from header fields
     *
     * checked 29.3.2011
     */
    void ScalCoo::set(const short& sin, const int& c)
    { 
      // make a copy to allow modification
      short s=sin;
      fixscalevalue(s, Mcontrol.bestrict);
      this->scale=s;
      this->coo=c; 
    } // void ScalCoo::set(const short& s, const int& c)

    /*----------------------------------------------------------------------*/

    /*! set from coordinate value in meters
     *
     * this functions takes the coordinate value as a floating point value in
     * meters; it has to find the appropriate scaling to represent this value
     * with appropriate precision in an integer variable
     */
    void ScalCoo::set(const double& value)
    {
      const bool debug=false;
      this->scale=1;
      this->coo=static_cast<int>(nearbyint(value));
      double v=(value>=0 ? value : -value);
      if (v<ScalCoo::effectivezero)
      {
        this->scale=1;
        this->coo=0;
      }
      else
      {
        DATRW_debug(debug, "ScalCoo::set(const double& v)",
                    "v: " << helper::MyOutputFormat() << v << "\n"
                    "log10(v): " << std::log10(v) << "\n"
                    "floor(log10(v)): " << std::floor(std::log10(v)));
        int nlead=1+static_cast<int>(std::floor(std::log10(v)));
        int ntrail=::datrw::util::ntrailingdigits(v);
        int nsig=::datrw::util::nsignificantdigits(v, debug);
        int nzero=ntrail-nsig;
        DATRW_debug(debug, "ScalCoo::set(const double& v)",
                    "nlead: " << nlead << 
                    " ntrail: " << ntrail << 
                    " nsig: " << nsig);
        nsig = nsig>static_cast<int>(Mcontrol.coodigits)
          ? Mcontrol.coodigits : nsig;
        ntrail=nsig+nzero;
        DATRW_debug(debug, "ScalCoo::set(const double& v)",
                    "nlead: " << nlead << 
                    " ntrail: " << ntrail << 
                    " nsig: " << nsig);
        if (ntrail>0)
        {
          DATRW_debug(debug, "ScalCoo::set(const double& value)",
                      "ntrail>0");
          int s=std::pow(10,ntrail);
          s = s > 10000 ? 10000 : s;
          DATRW_assert(s<100000,
                       "ERROR (ScalCoo::set): scale too large");
          this->scale=-s;
          this->coo=static_cast<int>(nearbyint(value*s));
          // check truncation error
          double truevalue=value*s;
          double roundedvalue=nearbyint(truevalue);
          double truncationerror=fabs(1.-(truevalue/roundedvalue));
          DATRW_debug(debug, "ScalCoo::set(const double& value)",
                      "truncation error:"
                      << " truevalue: " << truevalue
                      << " roundedvalue: " << roundedvalue
                      << " truncationerror: " << truncationerror);
          DATRW_report_assert(truncationerror<0.001,
                              "WARNING (ScalCoo::set) " 
                              "truncation error > 0.1%\n"
                              "coordinate value " 
                              << helper::MyOutputFormat() << value
                              << " will be truncated!"
                              << "\n*"
                              << " scaled value: " << truevalue
                              << " rounded scaled value: "
                              << roundedvalue);
        }
        else if (nlead>nsig)
        {
          DATRW_debug(debug, "ScalCoo::set(const double& value)",
                      "nlead>nsig");
          int s=std::pow(10,nlead-nsig);
          s = s > 10000 ? 10000 : s;
          DATRW_assert(s<100000,
                       "ERROR (ScalCoo::set): scale too large");
          this->scale=s;
          this->coo=static_cast<int>(nearbyint(value/s));
        }
      } // if (v<ScalCoo::effectivezero), else clause
      // adjust scale
      this->scaletopower(this->smallestpower(Mcontrol.scalco));
      DATRW_debug(debug, "ScalCoo::set(const double& value)",
                  "value on exit:" 
                  << " value: " << value
                  << " this->coo: " << this->coo
                  << " this->scale: " << this->scale);
    } // void ScalCoo::set(const double& v)

    /*----------------------------------------------------------------------*/

    /*! return decimal power of scaling factor
     *
     * tested 29.3.2011
     */
    int ScalCoo::power() const
    {
      return(datrw::su::scaletopower(this->scale));
    } // int ScalCoo::power() const

    /*----------------------------------------------------------------------*/

    //! scale to given scaling factor as defined by decimal power
    void ScalCoo::scaletopower(const int& p)
    {
      int pd=this->power()-p;
      int vnew, vcmp;
      if (pd<0)
      {
        int fac=std::pow(10,-pd);
        vnew=static_cast<int>(std::floor(this->coo/fac));
        vcmp=static_cast<int>(std::floor(vnew*fac));
      }
      else
      {
        int fac=std::pow(10,pd);
        vnew=std::floor(this->coo*fac);
        vcmp=static_cast<int>(std::floor(vnew/fac));
      }
      DATRW_report_assert(vcmp==this->coo,
                          "WARNING ScalCoo::scaletopower will truncate "
                          "coordinate value\n"
                          "value was " 
                          << helper::MyOutputFormat() << this->coo <<
                          " value will be " << vcmp);
      this->coo = vnew;
      this->scale=powertoscale(p);
      DATRW_assert(this-scale != 0,
                   "ERROR (ScalCoo::scaletopower): illegal scale value!");
    } // void ScalCoo::scaletopower(const int& p)

    /*----------------------------------------------------------------------*/

    /*! \brief adjust scale to optimal representation of significant digits
     *
     * This function removes trailing zeroes from the ScalCoo::coo value, by
     * chosing an appropriate scale value. 
     * Upon return from this function the scale power is as large as possible.
     * Any increase in the power of the scale value will result in truncation
     * of the coordinate value.
     */
    void ScalCoo::adjustscale()
    {
      int p=this->power();
      int v1, v2;
      do {
        v1=this->coo;
        v2=std::floor(v1/10)*10;
        if ((v1==v2) && (p<4))
        {
          ++p;
          this->coo /= 10;
        }
      } while ((v1==v2) && (p<4));
      this->scale=powertoscale(p);
      DATRW_assert(this-scale != 0,
                   "ERROR (ScalCoo::scaletopower): illegal scale value!");
    } // void ScalCoo::adjustscale()

    /*----------------------------------------------------------------------*/

    /*! \brief return smallest power possible without truncation
     */
    int ScalCoo::smallestpower(const short& desiredscale) const
    {
      // limiting power values
      int desired=datrw::su::scaletopower(desiredscale);
      int lp=desired >= -4 ? desired : -4;
      lp = lp <= 4 ? lp : 4;
      // initial power
      int p=this->power();
      /*
       * code modification; 24.1.2012
       * tests were previously done detecting overflow
       *
      // value and compare value
      ScalCoo v1(Mcontrol), v2(Mcontrol);
      v1.coo=this->coo;
      v2.coo=std::floor(v1.coo*10)/10;
      do {
        if ((v1.coo==v2.coo) && (p>lp))
        {
          --p;
          v1.coo *= 10;
          v2.coo=std::floor(v1.coo*10)/10;
        }
      } while ((v1.coo==v2.coo) && (p>lp));
      *
      * I prefer to use numerical comparison
      * increasing the scale power will decrease the coo value and vice versa;
      * start with the optimal power as being found in this->scale
      * decrease the power until 
      *   either a further decrease would result in coo not fitting into a
      *     variable of type short
      *   or the desired scale (scale power) value is obtained
      */
      int c1=this->coo;
      int c2;
      bool hot=true;
      do {
        if (hot && (p>lp))
        {
          --p;
          c1 *= 10;
        }
        c2=c1*10; 
        hot = ((c2 <= SHRT_MAX) && (c2 >= SHRT_MIN));
      } while (hot && (p>lp));
      return(p);
    } // int ScalCoo::smallestpower()
      
    /*----------------------------------------------------------------------*/

    //! return coordinate value
    double ScalCoo::value() const
    {
      double c=static_cast<double>(this->coo);
      double retval = c*scalefactor(this->scale,
                                    Mcontrol.bestrict);
      return(retval);
    } // double ScalCoo::value() const

    /*======================================================================*/

    //! read values from SU header
    void Coordinates::getvaluesfrom(const TraceHeaderStruct& h)
    {
      bool debug=false;
      DATRW_debug(debug, "Coordinates::getvaluesfrom:",
                  "values in header on entry:"
                  << " scalco: " << h.scalco
                  << " scalel: " << h.scalel
                  << " sx: " << h.sx
                  << " sy: " << h.sy
                  << " gx: " << h.gx
                  << " gy: " << h.gy
                  << " sdepth: " << h.sdepth
                  << " gelev: " << h.gelev);
      this->sx.set(h.scalco, h.sx);
      this->sy.set(h.scalco, h.sy);
      this->gx.set(h.scalco, h.gx);
      this->gy.set(h.scalco, h.gy);
      this->sdepth.set(h.scalel, h.sdepth);
      this->gelev.set(h.scalel, h.gelev);
    } // Coordinates::getvaluesfrom(const TraceHeaderStruct& h)

    /*----------------------------------------------------------------------*/

    //! set values in SU header
    void Coordinates::setvaluesin(TraceHeaderStruct& h) 
    {
      bool debug=false;
      this->equalizescaling();
      h.scalco=this->sx.scale;
      h.sx=this->sx.coo;
      h.sy=this->sy.coo;
      h.gx=this->gx.coo;
      h.gy=this->gy.coo;
      h.scalel=this->sdepth.scale;
      h.sdepth=this->sdepth.coo;
      h.gelev=this->gelev.coo;
      DATRW_debug(debug, "Coordinates::setvaluesin:",
                  "values in header on finish:"
                  << " scalco: " << h.scalco
                  << " scalel: " << h.scalel
                  << " sx: " << h.sx
                  << " sy: " << h.sy
                  << " gx: " << h.gx
                  << " gy: " << h.gy
                  << " sdepth: " << h.sdepth
                  << " gelev: " << h.gelev);
    } // Coordinates::setvaluesin(TraceHeaderStruct& h) const

    /*----------------------------------------------------------------------*/

    //! equalize scaling
    void Coordinates::equalizescaling()
    {
      bool debug=false;
      // adjust horizontal coordinates
      DATRW_debug(debug, "Coordinates::equalizescaling",
                  "scales on entry"
                  << " gelev.scale " << this->gelev.scale
                  << " sdepth.scale " << this->sdepth.scale);
      /* is not required when search for the smallest possible power
      this->sx.adjustscale();
      this->sy.adjustscale();
      this->gx.adjustscale();
      this->gy.adjustscale();
      */
      // search for smallest possible power
      int psx=this->sx.smallestpower(Mcontrol.scalco);
      int psy=this->sy.smallestpower(Mcontrol.scalco);
      int pgx=this->gx.smallestpower(Mcontrol.scalco);
      int pgy=this->gy.smallestpower(Mcontrol.scalco);
      int pnew = psx > psy ? psx : psy;
      pnew = pnew > pgx ? pnew : pgx;
      pnew = pnew > pgy ? pnew : pgy;
      // check against largest possible power
      /* is not necessary
      pnew = pnew < sx.power() ? pnew : sx.power();
      pnew = pnew < sy.power() ? pnew : sy.power();
      pnew = pnew < gx.power() ? pnew : gx.power();
      pnew = pnew < gy.power() ? pnew : gy.power();
      */
      this->sx.scaletopower(pnew);
      this->sy.scaletopower(pnew);
      this->gx.scaletopower(pnew);
      this->gy.scaletopower(pnew);
      DATRW_assert(((this->sx.scale==this->sy.scale) &&
                    (this->sy.scale==this->gx.scale) &&
                    (this->gx.scale==this->gy.scale)),
                   "ERROR: inconsistent coordinate scaling");
      // adjust vertical coordinates
      /* is not required when scaling with the smallest possible power
      this->sdepth.adjustscale();
      this->gelev.adjustscale();
      DATRW_debug(debug, "Coordinates::equalizescaling",
                  "scales after adjust"
                  << " gelev.scale " << this->gelev.scale
                  << " sdepth.scale " << this->sdepth.scale);
      */
      int psdepth=this->sdepth.smallestpower(Mcontrol.scalco);
      int pgelev=this->gelev.smallestpower(Mcontrol.scalco);
      pnew = psdepth > pgelev ? psdepth : pgelev;
      /* is not necessary
      pnew = pnew < sdepth.power() ? pnew : sdepth.power();
      pnew = pnew < gelev.power() ? pnew : gelev.power();
      */
      this->sdepth.scaletopower(pnew);
      this->gelev.scaletopower(pnew);
      DATRW_assert((this->sdepth.scale==this->gelev.scale),
                   "ERROR: inconsistent coordinate scaling");
    } // void Coordinates::equalizescaling()

    /*======================================================================*/

    /*! \brief convert a decimal power to a SeismicUn*x scale value
     */
    short powertoscale(const int& p)
    {
      int pp = p < 0 ? -p : p;
      short s=static_cast<short>(std::pow(10,pp));
      return(p<0 ? -s: s);
    } // short powertoscale(const int& p)

    /*----------------------------------------------------------------------*/

    /*! \brief convert a SeismicUn*x scale value to a decimal power
     */
    int scaletopower(short s, const bool& strict)
    {
      fixscalevalue(s, strict);
      int sabs=s > 0 ? s : -s;
      int retval=std::log10(sabs);
      if (s < 0) { retval *= -1; }
      return(retval);
    } // int scaletopower(short s, const bool& strict=true)

    /*----------------------------------------------------------------------*/

    /*! \brief fix a SeismicUn*x scale value 
     */
    void fixscalevalue(short& s, const bool& strict)
    {
      /*
       * a scale value of 0 is not defined for the trace header in the SEGY
       * standard; since Geode data acquisitions produce SEGY data with scalel
       * set to 0, we have to deal with this value
       */
      DATRW_report_assert(s != 0,
                          "WARNING (ScalCoo::set): incorrect scale\n"
                          "The value for the scalco and scalel field\n"
                          "violates the SEG-Y trace header definition.\n"
                          "value: " << s << "; will be set to 1");
      DATRW_assert(!((strict) && (s==0)), 
                   "Violation of SeismicUn*x format definition!");
      // make a copy to allow modification
      if (s==0) { s=1; }
      int sabs=s > 0 ? s : -s;
      if ((sabs < 10) && (s != 1))
      {
        /*
         * some code out there produces SU data with scale values understood
         * as exponents of ten; SEGY defines the scale value to be taken as
         * numerator or denominator of the scaling factor with values -10000,
         * -1000, -100, -10, 1, 10, 100, 1000, 10000 only; here we allow for
         * the non-standard setting indicated by the absolute value of scale
         * being larger than 1 and smaller than 10
         */
        DATRW_report_assert(((sabs < 10) && (s != 1)),
                            "WARNING (ScalCoo::set): non-standard scale\n"
                            "The value for the scalco and scalel field\n"
                            "violates the SEG-Y trace header definition.\n"
                            "value: " << s);
        DATRW_assert(!strict,
                     "Violation of SeismicUn*x format definition!");
        /*
         * 10 to a power larger than 4 cannot be represented by a short int
         */
        DATRW_assert((sabs<5),
                      "ERROR (ScalCoo::set): "
                      "scale value does not fit in short int field");
        s= s > 0 ? std::pow(10,sabs) : -std::pow(10,sabs);
      }
    } // void fixscalevalue(short& s, const bool& strict=true)

    /*----------------------------------------------------------------------*/

    /*! \brief convert scale value to a factor to be applied
     */
    double scalefactor(short s, const bool& strict)
    {
      fixscalevalue(s, strict);
      double sv=static_cast<double>(s);
      return(s>0 ? sv : -1./sv);
    } // double scalefactor(short s, const bool& strict=true)

  } // namespace su

} // namespace datrw

/* ----- END OF sucomanager.cc ----- */
