/*! \file tapers.h
 * \brief provide signal tapers (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/09/2007
 * 
 * provide signal tapers (prototypes)
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

// include guard
#ifndef TF_TAPERS_H_VERSION

#define TF_TAPERS_H_VERSION \
  "TF_TAPERS_H   V1.3"

namespace ts {

  namespace tapers {

    /*! \brief Taper abstract base class.
     *
     * Use a taper by creating an instance of a derived class, passing the
     * appropriate parameters to the constructor.
     *
     * Then pass any container to function apply.
     */
    class Taper {
      public:
        virtual ~Taper() { }
        //! apply taper to series container c.
        template<class C>
          void apply(C c) const;
        //! apply taper to series container c for PSD calculation.
        template<class C>
          void psdapply(C c) const;
        /*! \brief return normalization for PSD calculation.
         * The taper coefficients \f$w_k\f$ have to be devided by the return
         * value \f$W\f$.
         * \return
         *   \f$ W=\sqrt{\frac{\sum\limits_{k=1}^{N} w_k^2}{N}} \f$
         * \sa ts::tapers::Taper::psdapply
         */
        virtual double psdnorm() const =0;
      private:
        /*! function to set index range
         * required by template function apply();
         * will be called prior to value to set ranges.
         */
        virtual void init(const int& f, const int& l) const =0;
        /*! returns taper value for sample index i.
         * Private virtual function such that this function can only be
         * accessed through the template function apply(). 
         * This way we ensure that the appropriate index range was set first.
         */
        virtual double value(const int& i) const =0;
    }; // class Taper

    /*----------------------------------------------------------------------*/

    /*! \brief Provides a Cosine taper
     *
     * \param f taper fraction (0<=f<=0.5)
     *          for f=0.5 the taper effectively is a Hanning taper
     */
    class Cosine: public Taper {
      public:
        Cosine(const double& f=0.1);
        ~Cosine() { }
      private:
        void init(const int& f, const int& l) const;
        double value(const int& i) const;
        double psdnorm() const;
        mutable int Mf,Ml; //<! index of first and last sample
        mutable int Msf; //<! sample index fraction
        mutable double Mfac; //<! sine function argument scaling
        double Mfrac; //<! taper fraction
    }; // class Hanning

    /*----------------------------------------------------------------------*/

    /*! \brief Provides a Hanning taper (no parameters):
     */
    class Hanning: public Taper {
      public:
        ~Hanning() { }
      private:
        void init(const int& f, const int& l) const;
        double value(const int& i) const;
        double psdnorm() const;
        mutable int Mf; //<! first index
        mutable double Mfac; //<! sine function argument scaling
    }; // class Hanning

    /*----------------------------------------------------------------------*/

    /*! \brief Provides a 4-point taper
     *
     * Provides a taper being defined by four times specified in units of the
     * duration of the entire time series.
     * The first sample is at t=0 and the last sample is at t=1.
     *
     * \param t1 taper equals zero for t<t1.
     * \param t2 taper increases with sin**2(x) 
     *           with x=0 at t=t1 and x=pi/2 at t=t2
     * \param t3 taper is one for t2 < t < t3
     * \param t4 taper decreases with cos**2(x)
     *           with x=0 at t=t3 and x=pi/2 at t=t4;
     *           taper equals zero for t>t4.
     *
     * Default parameters define a 10% sine**2,osine**2 taper
     */
    class FourPoint: public Taper {
      public:
        FourPoint(const double& t1=0.,
                  const double& t2=0.1,
                  const double& t3=0.9,
                  const double& t4=1.);
        ~FourPoint() { }
      private:
        void init(const int& f, const int& l) const;
        double value(const int& i) const;
        double psdnorm() const;
        //! times in units of time series duration relative to first sample
        double Mt1, Mt2, Mt3, Mt4;
        //! times in units of sampling interval relative to sample index zero
        mutable double Mti1, Mti2, Mti3, Mti4;
        mutable double Mfac1, Mfac2;
    }; // class FourPoint

    /*----------------------------------------------------------------------*/

    //! inline function for template argument
    template<class C>
      void Taper::apply(C c) const
      {
        this->init(c.f(), c.l());
        for (int i=c.f(); i<=c.l(); ++i)
        { c(i) *= this->value(i); }
      } // template<class C> void Taper::apply(C c)

    //! inline function for template argument
    template<class C>
      void Taper::psdapply(C c) const
      {
        this->init(c.f(), c.l());
        for (int i=c.f(); i<=c.l(); ++i)
        { c(i) *= this->value(i)/this->psdnorm(); }
      } // template<class C> void Taper::psdapply(C c)
    
  } // namespace tapers

} // namespace ts

#endif // TF_TAPERS_H_VERSION (includeguard)

/* ----- END OF tapers.h ----- */
