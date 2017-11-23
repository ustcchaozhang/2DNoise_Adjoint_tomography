/*! \file tapers.cc
 * \brief provide signal tapers (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/09/2007
 * 
 * provide signal tapers (implementation)
 * 
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 06/09/2007   V1.0   Thomas Forbriger
 *  - 17/09/2007   V1.1   added PSD norm
 *  - 27/04/2009   V1.2   added Cosine taper
 *  - 26/01/2012   V1.3   added FourPoint taper
 * 
 * ============================================================================
 */
#define TF_TAPERS_CC_VERSION \
  "TF_TAPERS_CC   V1.3"

//#include <iostream>
#include <tsxx/tapers.h>
#include <tsxx/tsxx.h>
#include <tsxx/error.h>
//#include <tsxx/debug.h>
#include <cmath>

//using std::cout;
//using std::endl;

namespace ts {

  //! \brief Provides signal tapers.
  namespace tapers {

    void Hanning::init(const int& f, const int& l) const
    {
      Mf=f;
      Mfac=3.1415926535897931/(l-f);
    } // void Hanning::init(const int& f, const int& l) const

    double Hanning::value(const int& i) const
    {
       double s=std::sin((i-Mf)*Mfac);
       return (s*s);
    } // double Hanning::value(const int& i) const

    /*! PSD scaling factor
     *
     * Scaling factor for Hanning taper
     * --------------------------------
     *
     * Tapering stationary noise with a Hanning taper reduces total signal
     * energy an thus the signal power obtained through an FFT. In his
     * tutorial Agnew recommends to scale each sample \f$s_k\f$ of the series
     * by \f$w_k/W\f$, where \f$w_k\f$ is the k-th sample of the taper and 
     *
     * \f[
     *   W^2 = (sum_0^{N-1} w_k ^2) /N
     * \f]
     *
     * is a measure for the loss in total signal power due to application of the
     * taper. This applies only for staionary noise.
     * 
     * From Walter's Matlab
     * scripts I took:
     *
     * If data is the time series vector and y is the Hanning taper of
     * appropriate length, Walter calculates
     *   
     * \f[
     *   w = sqrt(sum(y.*y)/ndata);
     * \f]
     *
     * and
     *   
     * \f[
     *   y=y/w;
     * \f]
     *
     * where ndata is the length of data and y.
     *
     * For a Hanning taper:
     *
     * \f[
     *   w_k = sin^2(k*pi/(N-1))
     * \f]
     *
     * Thus
     *
     * \f[
     *   W^2 = (sum_0^{N-1} sin^4(k*pi/(N-1))) /N
     * \f]
     *
     * From Gradshteyn and Ryzhik (eq. 1.321 3) I find
     *
     * \f[
     *   sin^4(k*pi/(N-1)))
     *
     *     = (1/8) * ( cos(4 * k*pi/(N-1))
     *                 - 4 * cos(2 * k*pi/(N-1))
     *                 + 3 ).
     * \f]
     *
     * Within the sum the contribution of both cos-terms will vanish, since both
     * are averaged over one and two periods, respectively. Thus
     *
     * \f[
     *   W^2 = (sum_0^{N-1} 3/8) /N = 3/8.
     * \f]
     *
     * Since foutra is not scaling the taper but scaling the power spectrum, we
     * have to apply the factor 8/3 to the result of power spectrum calculation.
     *
     * This factor 8/3=2.66667 was tested against the value for \f$W^2\f$, when
     * explicitely derived from a Hanning taper time series by the above
     * formula.
     *
     * 13/9/2007, thof
     */
    double Hanning::psdnorm() const
    {
       const double retval=std::sqrt(3./8.);
       return (retval);
    } // double Hanning::psdnorm() const

    /*======================================================================*/

    Cosine::Cosine(const double& f): Mfrac(f)
    {
      TSXX_assert(((0<=Mfrac) && (Mfrac<=1.)),
        "Cosine taper fraction is outside meaningful range");
    } // Cosine::Cosine(const double& f)

    void Cosine::init(const int& f, const int& l) const
    {
      Mf=f;
      Ml=l;
      Msf=int(0.5*Mfrac*double(Ml-Mf));
      Mfac=0.5*3.1415926535897931/double(Msf);
    } // void Cosine::init(const int& f, const int& l) const

    double Cosine::value(const int& i) const
    {
       double s=1.;
       if (i<(Mf+Msf)) { s=std::sin((i-Mf)*Mfac); }
       else if (i>(Ml-Msf)) { s=std::sin((Ml-i)*Mfac); }
       return (s*s);
    } // double Cosine::value(const int& i) const

    double Cosine::psdnorm() const
    {
       const double retval=1.;
       TSXX_abort("Cosine::psdnorm() is not yet defined");
       /*
        *
        * NOTICE:
        * The transform of the cosine taper is available in fouriertrafo.tex
        * in work/pjt/doz/mess
        *
        */
       return (retval);
    } // double Cosine::psdnorm() const

    /*======================================================================*/

    FourPoint::FourPoint(const double& t1,
                         const double& t2,
                         const double& t3,
                         const double& t4)
      : Mt1(t1), Mt2(t2), Mt3(t3), Mt4(t4)
    {
      TSXX_assert(((0<=Mt1) && (Mt1<=Mt2) && (Mt2<=Mt3) && (Mt3<=Mt4) &&
                   (Mt4<=1.)),
        "FourPoint taper times are not in increasing order");
    } // FourPoint::FourPoint(...)

    /*----------------------------------------------------------------------*/

    void FourPoint::init(const int& f, const int& l) const
    {
      /*
      TSXX_debug(true, "FourPoint::init",
                 TSXX_value(f) << " " <<
                 TSXX_value(l) << " " <<
                 TSXX_value(Mt1) << " " <<
                 TSXX_value(Mt2) << " " <<
                 TSXX_value(Mt3) << " " <<
                 TSXX_value(Mt4));
                 */
      double F=static_cast<double>(f);
      double L=static_cast<double>(l);
      double T=L-F;
      Mti1=F+T*Mt1;
      Mti2=F+T*Mt2;
      Mti3=F+T*Mt3;
      Mti4=F+T*Mt4;
      Mfac1=M_PI/(Mti2-Mti1);
      Mfac2=M_PI/(Mti4-Mti3);
      /*
      TSXX_debug(true, "FourPoint::init",
                 TSXX_value(Mti1) << " " <<
                 TSXX_value(Mti2) << " " <<
                 TSXX_value(Mti3) << " " <<
                 TSXX_value(Mti4) << " " <<
                 TSXX_value(Mfac1) << " " <<
                 TSXX_value(Mfac2));
                 */
    } // void FourPoint::init(const int& f, const int& l) const

    /*----------------------------------------------------------------------*/

    double FourPoint::value(const int& i) const
    {
       //cout << i << " ";
       double retval=0.;
       double t=static_cast<double>(i);
       if (t<Mti1)
       {
         //cout << "t<Mti1" << " ";
         retval=0.;
       }
       else if (t<=Mti2)
       {
         //cout << "t<=Mti2" << " ";
         retval=0.5*(1.-cos(Mfac1*(t-Mti1)));
       }
       else if (t<Mti3)
       {
         //cout << "t<Mti3" << " ";
         retval=1.;
       }
       else if (t<=Mti4)
       {
         //cout << "t<=Mti4" << " ";
         retval=0.5*(1.+cos(Mfac2*(t-Mti3)));
       }
       else
       {
         //cout << "else" << " ";
         retval=0.;
       }
       //cout << "retval=" << retval << endl;
       return (retval);
    } // double FourPoint::value(const int& i) const

    /*----------------------------------------------------------------------*/

    double FourPoint::psdnorm() const
    {
       const double retval=1.;
       TSXX_abort("FourPoint::psdnorm() is not yet defined");
       /*!
        * \todo
        * FourPoint::psdnorm() must be implemented
        */
       return (retval);
    } // double FourPoint::psdnorm() const

  } // namespace tapers

} // namespace ts

/* ----- END OF tapers.cc ----- */
