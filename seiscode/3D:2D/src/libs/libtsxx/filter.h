/*! \file filter.h
 * \brief some time series filter classes (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/03/2016
 * 
 * some time series filter classes (prototypes)
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
 *  - 18/12/2007   V1.1   
 *                        - support debugging
 *                        - replace comma delimiter by whitespace
 *  - 06/05/2009   V1.2   
 *                        - added delay filter
 *  - 19/02/2011   V1.3   
 *                        - added rectifier
 *  - 25/05/2011   V1.4 
 *                        - added normalizer
 *  - 14/09/2011   V1.5 
 *                        - added set by index
 *  - 25/10/2012   V1.6 
 *                        - added cumsum
 *  - 31/07/2015   V1.7  
 *                        - add filter Powerof
 *  - 19/03/2016   V1.8  
 *                        - add filter GaussianNoise
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_FILTER_H_VERSION

#define TF_FILTER_H_VERSION \
  "TF_FILTER_H   V1.8"

#include<tsxx/filterbase.h>

namespace ts {

  /*! \brief time series filters
   *
   * \defgroup filter Time series filters
   */

  /*! \brief time series filters
   *
   * \ingroup filter
   */
  namespace filter {

    /*! \brief set sample values selected by index
     *
     * \ingroup filter
     *
     * Set all samples from index \p n1 to index \p n2 to value \p v.
     *
     * \param n1 first sample to set
     * \param n2 last sample to set
     * \param v  value to set
     */
    class SetByIndex: public ts::filter::BasicFilter {
      public:
        SetByIndex(const int& n1, const int& n2, const double& v): 
          Mn1(n1), Mn2(n2), Mv(v) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        int Mn1, Mn2;
        double Mv;
    }; // class SetByIndex

    /*----------------------------------------------------------------------*/

    /*! \brief remove trend
     *
     * \ingroup filter
     *
     * We calculate the linear trend over the first \p n samples by a linear
     * regression.
     * This trend is removed from the whole series.
     *
     * \param n number of samples to calculate average from
     *          (will be set to n=number of samples, if n=0 is passed)
     */
    class RemoveTrend: public ts::filter::BasicFilter {
      public:
        RemoveTrend(const int& n): Mn(n) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        int Mn;
    }; // class RemoveTrend

    /*----------------------------------------------------------------------*/

    /*! \brief remove average
     *
     * \ingroup filter
     *
     * Calculate average over first \p n samples and remove this value from
     * the whole time series.
     *
     * \param n number of samples to calculate average from
     *          (will be set to n=number of samples, if n=0 is passed)
     */
    class RemoveAverage: public ts::filter::BasicFilter {
      public:
        RemoveAverage(const int& n): Mn(n) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        int Mn;
    }; // class RemoveAverage

    /*----------------------------------------------------------------------*/

    /*! \brief hanning taper.
     *
     * \ingroup filter
     *
     * \note 
     *   There is a Hanning taper available in the taper collection too.
     *   \sa ts::tapers::Hanning
     */
    class HanningTaper: public ts::filter::BasicFilter {
      public:
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
    }; // class HanningTaper

    /*----------------------------------------------------------------------*/

    /*! scale with factor
     * \ingroup filter
     */
    class Scale: public ts::filter::BasicFilter {
      public:
        Scale(const double& v): Mv(v) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mv;
    }; // class Scale

    /*----------------------------------------------------------------------*/

    /*! add a constant
     * \ingroup filter
     */
    class Add: public ts::filter::BasicFilter {
      public:
        Add(const double& v): Mv(v) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mv;
    }; // class Add

    /*----------------------------------------------------------------------*/

    /*! \brief force signal to a baseline.
     * \ingroup filter
     *
     * The filter removes a linear trend from the time series. 
     * After this operation the average in the index ranges [i1,i2] and
     * [i3,i4] will vanish.
     * The index range limits are
     * - i1 = index of first sample
     * - i2 = i1 + \p n1 - 1
     * - i3 = i4 - \p n2 + 1
     * - i4 = \p ne
     * 
     * \param n1 define size of first range 
     *           (will be set to n1=1 if n1=0 is passed)
     * \param n2 define size of second range
     *           (will be set to n2=n1 if n2=0 is passed)
     * \param ne define end of second range
     *           (will be set to n2=index of last sample, if n2=0 is passed)
     */
    class ForceToBase: public ts::filter::BasicFilter {
      public:
        ForceToBase(const int& n1, const int& n2, const int& ne): 
          Mn1(n1), Mn2(n2), Mne(ne) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        int Mn1, Mn2, Mne;
    }; // class ForceToBase

    /*----------------------------------------------------------------------*/

    /*! reverse time series
     * \ingroup filter
     */
    class Reverse: public ts::filter::BasicFilter {
      public:
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
    }; // class Reverse

    /*----------------------------------------------------------------------*/

    /*! normalize time series to given maximum amplitude
     * \ingroup filter
     */
    class Normalize: public ts::filter::BasicFilter {
      public:
        Normalize(const double& v): Mv(v) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mv;
    }; // class Normalize

    /*----------------------------------------------------------------------*/

    /*! delay time series by linear interpolation
     * \ingroup filter
     */
    class Delay: public ts::filter::BasicFilter {
      public:
        Delay(const double& v): Mv(v) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mv;
    }; // class Delay

    /*----------------------------------------------------------------------*/

    /*! take square of signal
     * \ingroup filter
     */
    class Square: public ts::filter::BasicFilter {
      public:
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
    }; // class Square

    /*----------------------------------------------------------------------*/

    /*! take square root of signal
     * \ingroup filter
     */
    class SquareRoot: public ts::filter::BasicFilter {
      public:
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
    }; // class SquareRoot

    /*----------------------------------------------------------------------*/

    /*! rectify signal (take absolute value)
     * \ingroup filter
     */
    class Rectifier: public ts::filter::BasicFilter {
      public:
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
    }; // class Rectifier

    /*----------------------------------------------------------------------*/

    /*! calculate cumulative sum
     * \ingroup filter
     */
    class CumSum: public ts::filter::BasicFilter {
      public:
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
    }; // class CumSum

    /*----------------------------------------------------------------------*/

    /*! take each sample to the power of a given exponent
     * \ingroup filter
     */
    class Powerof: public ts::filter::BasicFilter {
      public:
        Powerof(const double& v): Mv(v) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mv;
    }; // class Powerof

    /*----------------------------------------------------------------------*/

    /*! \brief Add random Gaussian noise.
     * \ingroup filter
     *
     * \param a rms amplitude of noise to be added
     */
    class GaussianNoise: public ts::filter::BasicFilter {
      public:
        GaussianNoise(const double& a): Ma(a) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Ma;
    }; // class GaussianNoise

    /*----------------------------------------------------------------------*/

    /*! \brief remove value of first sample from series
     *
     * \ingroup filter
     *
     * Subtract the value of the first sample from the entire series and store
     * this value for later reference. This is useful to effectively load
     * filters operators with the value of the first sample.
     */
    class RemoveFirst: public ts::filter::BasicFilter {
      friend class RestoreFirst;
      public:
        RemoveFirst() { RemoveFirst::Mf=0.; }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        static double Mf;
    }; // class RemoveFirst

    /*----------------------------------------------------------------------*/

    /*! \brief restore value of first sample to series
     *
     * \ingroup filter
     *
     * The value of the first sample previously subtracted by RemoveFirst will
     * now be restored again. The stored value of the first sample is reset to
     * zero after the operation.
     */
    class RestoreFirst: public ts::filter::BasicFilter {
      public:
        RestoreFirst() { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
    }; // class RestoreFirst

    /*----------------------------------------------------------------------*/

    /*! create a filter
     * \ingroup filter
     */
    ts::filter::Tfilterhandle make_filter(std::string s,
                                          const bool& debug=false);

    /*! print information on available filters
     * \ingroup filter
     */
    void print_help(std::ostream& os);

  } // namespace filter

} // namespace ts

#endif // TF_FILTER_H_VERSION (includeguard)

/* ----- END OF filter.h ----- */
