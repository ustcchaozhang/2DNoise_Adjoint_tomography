/*! \file filter.cc
 * \brief some time series filter classes (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/03/2016
 * 
 * some time series filter classes (implementation)
 * 
 * Copyright (c) 2005, 2015, 2016 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 05/07/2005   V1.0   Thomas Forbriger
 *  - 11/07/2005   V1.1   
 *                        - provide usage information
 *                        - correction in rev code
 *                        - correction in fbl code
 *  - 02/08/2005   V1.2   - RemoveTrend had a problem, resolved by explict
 *                          calculation of inverse matrix
 *  - 18/12/2007   V1.3    
 *                        - support debugging
 *                        - replace comma delimiter by whitespace
 *  - 06/05/2009   V1.4   
 *                        - added delay filter
 *  - 19/02/2011   V1.5   
 *                        - added rectifier
 *  - 25/05/2011   V1.6 
 *                        - added normalizer
 *  - 14/09/2011   V1.7 
 *                        - added set by index
 *  - 25/10/2012   V1.8 
 *                        - added cumsum
 *  - 20/05/2015   V1.9  
 *                        - take usage text from text file
 *  - 31/07/2015   V1.10 
 *                        - add filter Powerof
 *  - 19/03/2016   V1.11 
 *                        - add filter GaussianNoise
 * 
 * ============================================================================
 */
#define TF_FILTER_CC_VERSION \
  "TF_FILTER_CC   V1.11"

#include <cmath>
#include <sstream>
#include <algorithm>
#include <tsxx/random.h>
#include <tsxx/filter.h>
#include <tsxx/filter_usage_text.h>
#include <aff/functions/avg.h>
#include <aff/functions/absmax.h>
#include <aff/seriesoperators.h>
#include <aff/subarray.h>
#include <tsxx/error.h>
#include <tsxx/debug.h>

namespace ts {

  namespace filter {

    typedef aff::Tsubscript Tindex;
    typedef aff::Tsize Tsize;
    typedef Ttimeseries::Tvalue Tvalue;
    typedef Ttimeseries::Tseries Tseries;

    //! remove trend
    Ttimeseries RemoveTrend::operator()(const Ttimeseries& s,
                                        const bool& debug) const
      { 
        /*
         * The trend function (index l) is
         *   
         *   f(l)= a + b * l
         *
         * We minimize
         *
         *   \sum_l ( x(l) - a - b * l )**2
         *
         * where x(l) is the time series.
         *
         * The least squares condition is satisfied for a and b that satisfy
         *
         *   a * N       + b * sum_l l    = sum_l x(l)
         *   a * sum_l l + b * sum_l l**2 = sum_l l * x(l)
         *
         * N = number of samples
         *
         * in case the summation starts at index 1:
         *
         * sum_l l = 0.5 * N * ( N + 1 )
         * sum_l l**2 = N * ( N + 1 ) * ( 2 * N + 1 ) / 6
         */
        // shift first index to 1 (to make values given above applicable)
        Tseries x=s;
        x.shift(1-x.f());
        Tsize n= (Mn<1) ? x.size() : Mn;
        n= (n < x.size()) ? n : x.size();
        /* 
        double m11=n;
        double m12=0.5*n*(n+1);
        double m21=m12;
        double m22=n*(n+1)*(2*n+1)/6.;
        double det=m11*m22-m12*m21;
        */
        // calculate inverse
        double dn=n;
        double w11=2.*(2.*dn+1.)/(dn*(dn-1));
        double w12=-6./(dn*(dn-1.));
        double w21=w12;
        double w22=12./(dn*(dn+1.)*(dn-1.));

        double L1=0, L2=0;
        for (Tindex l=x.f(); l<=Tindex(n); ++l)
        {
          L1 += x(l);
          L2 += double(l)*x(l);
        }
        /*
        double a=(L1*m22-L2*m12)/det;
        double b=(-L1*m21+L2*m11)/det;
        */
        // apply inverse
        double a = L1 * w11 + L2 * w12;
        double b = L1 * w21 + L2 * w22;
        for (Tindex l=x.f(); l<=x.l(); ++l)
        { x(l) -= (a+double(l)*b); }
        return s; 
      }

    /*----------------------------------------------------------------------*/

    //! set values selected by index
    Ttimeseries SetByIndex::operator()(const Ttimeseries& s,
                                       const bool& debug) const
      {  
        Tseries x=s;
        int ifirst=x.f() > Mn1? x.f() : Mn1;
        int ilast=x.l() < Mn2? x.l() : Mn2;
        TSXX_assert(ifirst <= ilast,
                    "ERROR (SetByIndex): illegal sample index range");
        for (Tindex l=ifirst; l<=ilast; ++l)
        { x(l) = Mv; }
        return s; 
      }

    /*----------------------------------------------------------------------*/

    //! square root of signal
    Ttimeseries SquareRoot::operator()(const Ttimeseries& s,
                                       const bool& debug) const
      {  
        Tseries x=s;
        for (Tindex l=x.f(); l<=x.l(); ++l)
        { x(l) = std::sqrt(x(l)); }
        return s; 
      }

    /*----------------------------------------------------------------------*/

    //! square signal
    Ttimeseries Square::operator()(const Ttimeseries& s,
                                   const bool& debug) const
      {  
        Tseries x=s;
        for (Tindex l=x.f(); l<=x.l(); ++l)
        { x(l) = x(l)*x(l); }
        return s; 
      }

    /*----------------------------------------------------------------------*/

    //! take each sample to the power of a given exponent
    Ttimeseries Powerof::operator()(const Ttimeseries& s,
                                    const bool& debug) const
      {  
        Tseries x=s;
        for (Tindex l=x.f(); l<=x.l(); ++l)
        { x(l) = std::pow(x(l),Mv); }
        return s; 
      }

    /*----------------------------------------------------------------------*/

    //! add Gaussian noise
    Ttimeseries GaussianNoise::operator()(const Ttimeseries& s,
                                          const bool& debug) const
      {  
        Tseries x=s;
        Tseries n=ts::rnd::dugauss(x.size())*Ma;
        x += n;
        return s; 
      }

    /*----------------------------------------------------------------------*/

    //! rectification
    Ttimeseries Rectifier::operator()(const Ttimeseries& s,
                                      const bool& debug) const
      {  
        Tseries x=s;
        for (Tindex l=x.f(); l<=x.l(); ++l)
        { x(l) = x(l)<0 ? -x(l) : x(l); }
        return s; 
      }

    /*----------------------------------------------------------------------*/

    //! cumulative sum
    Ttimeseries CumSum::operator()(const Ttimeseries& s,
                                   const bool& debug) const
      {  
        Tseries x=s;
        if (x.size()>1) 
        {
          for (Tindex l=x.f()+1; l<=x.l(); ++l)
          { x(l) += x(l-1); }
        }
        return s; 
      }

    /*----------------------------------------------------------------------*/

    //! remove average
    Ttimeseries RemoveAverage::operator()(const Ttimeseries& s,
                                          const bool& debug) const
      {  
        Tsize n= (Mn<1) ? s.size() : Mn;
        n= (n < s.size()) ? n : s.size();
        Tindex f=s.f();
        Tindex l=s.f()+n-1;
        s -= aff::func::avg<Tseries>(aff::subarray<Tseries>(s)(f,l));
        return s; 
      }

    /*----------------------------------------------------------------------*/

    /*! \brief hanning taper 
     *
     * \note
     *   This taper is normalized:
     *   \f[
     *     H_k = \frac{1}{\sqrt{2}} (1-\cos(2\pi k /N))
     *   \f]
     */
    Ttimeseries HanningTaper::operator()(const Ttimeseries& s,
                                         const bool& debug) const
      { 
        double p=double(2.*M_PI/s.size());
        for (Tindex i=s.f(); i<=s.l(); ++i)
        { s(i) *= M_SQRT1_2*(1.-cos(p*(i-s.f()))); }
        return s; 
      }

    /*----------------------------------------------------------------------*/

    //! scale sample values
    Ttimeseries Scale::operator()(const Ttimeseries& s,
                                  const bool& debug) const
      { s *= Mv; return s; }

    /*----------------------------------------------------------------------*/

    //! add offset to sample values
    Ttimeseries Add::operator()(const Ttimeseries& s,
                                const bool& debug) const
      { s += Mv; return s; }

    /*----------------------------------------------------------------------*/

    //! force signal to baseline
    Ttimeseries ForceToBase::operator()(const Ttimeseries& s,
                                        const bool& debug) const
      { 
        Tindex s1=s.f();
        int n1= (Mn1 < 1) ? 1 : Mn1;
        Tindex e1=s1+n1-1;
        Tindex m1=(s1+e1)/2;
        int n2= (Mn2 < 1) ? n1 : Mn2;
        Tindex e2= (Mne < 1) ? s.l() : Mne;
        Tindex s2= e2-n2+1;
        Tindex m2=(s2+e2)/2;
        /* DEBUG
        std::cerr << "fbl: " << " n1 " << n1 << " n2 " << n2 
          << " s1 " << s1 << " e1 " << e1 << " m1 " << m1
          << " s2 " << s2 << " e2 " << e2 << " m2 " << m2 << std::endl;
        */
        Tvalue avg1=aff::func::avg<Tseries>(aff::subarray<Tseries>(s)(s1,e1));
        Tvalue avg2=aff::func::avg<Tseries>(aff::subarray<Tseries>(s)(s2,e2));
        double a=(avg2-avg1)/double(m2-m1);
        for (Tindex i=s.f(); i<=s.l(); ++i)
        { s(i) -= (a*double(i-m1)+avg1); }
        return s; 
      }

    /*----------------------------------------------------------------------*/

    //! reverse time series
    Ttimeseries Reverse::operator()(const Ttimeseries& s,
                                    const bool& debug) const
      { 
        // std::cerr << "entering reverse" << std::endl;
        Tindex k=s.f();
        Tindex l=s.l();
        Tindex ek=(k+l)/2+1;
        Tindex el=(k+l)/2-1;
        while ((k<ek) && (l>el))
        {
          Tvalue vk=s(k);
          Tvalue vl=s(l);
          // std::cerr << " # k" << k << " l" << l;
          // std::cerr << " vk" << vk << " vl" << vl;
          s(k)=vl;
          s(l)=vk;
          ++k;
          --l;
        }
        return s; 
      }

    /*----------------------------------------------------------------------*/

    /*! normalize
     */
    Ttimeseries Normalize::operator()(const Ttimeseries& s,
                                      const bool& debug) const
      { 
        Ttimeseries::Tseries series=s;
        double a=aff::func::absmax(s);
        double fac=Mv/a;
        if (a > 1.e-20)
        {
          series *= fac;
        }
        else
        { 
          series=0;
        }
        return s; 
      }

    /*----------------------------------------------------------------------*/

    /*! delays time series
     */
    Ttimeseries Delay::operator()(const Ttimeseries& s,
                                    const bool& debug) const
      { 
        Ttimeseries::Theader header=s.header;
        Ttimeseries::Tseries series=s;
        Ttimeseries src(series.copyout(), header);
        double dishift=-(this->Mv)/header.dt;
        int iashift=int(dishift);
        int ibshift=iashift;
        if (dishift < 0) { --iashift; } else { ++ibshift; }
        double ibfac=dishift-double(iashift);
        double iafac=double(ibshift)-dishift;
        /*
        std::cout << "Mv " << Mv << std::endl;
        std::cout << "dt " << header.dt << std::endl;
        std::cout << "dishift " << dishift << std::endl;
        std::cout << "iashift " << iashift << std::endl;
        std::cout << "ibshift " << ibshift << std::endl;
        std::cout << "iafac " << iafac << std::endl;
        std::cout << "ibfac " << ibfac << std::endl;
        std::cout << "fac " << ibfac+iafac << std::endl;
        std::cout.flush();
        */
        Ttimeseries::Tvalue s1,s2;
        s1=src(src.f());
        s2=s1;
        for (int i=s.f(); i<=s.l(); ++i)
        {
          if (((i+iashift) >= s.f()) && (i+iashift) <= s.l()) 
          { s1=src(i+iashift); }
          if (((i+ibshift) >= s.f()) && (i+ibshift) <= s.l())
          { s2=src(i+ibshift); }
          s(i)=s1*iafac+s2*ibfac;
        }
        return s; 
      }

    /*----------------------------------------------------------------------*/

    /*! remove first
     */
    Ttimeseries RemoveFirst::operator()(const Ttimeseries& s,
                                        const bool& debug) const
      { 
        Tindex f=s.f();
        RemoveFirst::Mf=s(f);
        s -= RemoveFirst::Mf;
        return s; 
      }

    //! definition of static member data is required
    double RemoveFirst::Mf=0;

    /*----------------------------------------------------------------------*/

    /*! restore first
     */
    Ttimeseries RestoreFirst::operator()(const Ttimeseries& s,
                                         const bool& debug) const
      { 
        s += RemoveFirst::Mf;
        RemoveFirst::Mf=0.;
        return s; 
      }

    /*----------------------------------------------------------------------*/

    //! function to generate filter class
    Tfilterhandle make_filter(std::string s,
                              const bool& debug)
    {
      std::replace(s.begin(),s.end(),',',' ');
      TSXX_debug(debug, "make_filter", "process " + s );
      typedef Tfilterhandle Tfh;
      Tfh fh(new Noop());
      std::string ID;
      std::istringstream is(s);
      is >> ID;
      TSXX_debug(debug, "make_filter", "  filter ID is " + ID );
      Tvalue v;
      int n, n2, n3;
      if (ID=="tre") {
        is >> n;
        fh=Tfh(new RemoveTrend(n));
      } else if (ID=="iset") {
        is >> n >> n2 >> v;
        fh=Tfh(new SetByIndex(n,n2,v));
      } else if (ID=="avg") {
        is >> n;
        fh=Tfh(new RemoveAverage(n));
      } else if (ID=="han") {
        fh=Tfh(new HanningTaper());
      } else if (ID=="fac") {
        is >> v;
        fh=Tfh(new Scale(v));
      } else if (ID=="add") {
        is >> v;
        TSXX_debug(debug, "make_filter", "  filter is: Add(" << v << ")" );
        fh=Tfh(new Add(v));
      } else if (ID=="fbl") {
        is >> n >> n2 >> n3;
        fh=Tfh(new ForceToBase(n, n2, n3));
      } else if (ID=="pow") {
        is >> v;
        fh=Tfh(new Powerof(v));
      } else if (ID=="rev") {
        fh=Tfh(new Reverse());
      } else if (ID=="sqr") {
        fh=Tfh(new Square());
      } else if (ID=="sqt") {
        fh=Tfh(new SquareRoot());
      } else if (ID=="rec") {
        fh=Tfh(new Rectifier());
      } else if (ID=="cus") {
        fh=Tfh(new CumSum());
      } else if (ID=="del") {
        is >> v;
        fh=Tfh(new Delay(v));
      } else if (ID=="noi") {
        is >> v;
        fh=Tfh(new GaussianNoise(v));
      } else if (ID=="nrm") {
        is >> v;
        fh=Tfh(new Normalize(v));
      } else if (ID=="lof") {
        fh=Tfh(new RemoveFirst());
      } else if (ID=="rsf") {
        fh=Tfh(new RestoreFirst());
      } else {
        TSXX_debug(debug, "make_filter", "  filter ID " + ID + " is unknown" );
        TSXX_UnknownFilterAbort("ts::filter::make_filter", ID.c_str());
      }
      return(fh);
    }

    //! print usage information
    void print_help(std::ostream& os)
    {
      os << TF_FILTER_CC_VERSION << std::endl;
      os << filter_usage_text;
    }

  } // namespace filter

} // namespace ts

/* ----- END OF filter.cc ----- */
