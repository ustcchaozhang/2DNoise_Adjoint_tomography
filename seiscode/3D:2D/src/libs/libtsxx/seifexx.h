/*! \file seifexx.h
 * \brief make seife functions available for C++ (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 14/01/2005
 * 
 * make seife functions available for C++ (prototypes)
 * 
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 14/01/2005   V1.0   Thomas Forbriger
 *  - 11/07/2005   V1.1   support debug mode
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_SEIFEXX_H_VERSION

#define TF_SEIFEXX_H_VERSION \
  "TF_SEIFEXX_H   V1.1"

#include<aff/series.h>

namespace ts {

  /*! \brief interface to seife C-functions
   *
   * \defgroup seife Interface to seife C-functions
   */

  /*! \brief interface to seife C-functions
   *
   * \ingroup seife
   */
  namespace seife {

    //! seife functions use double precision
    typedef double Tvalue;
    //! series to be passed to seife functions
    typedef aff::Series<Tvalue> Tseries;

    /*! switch on debug mode */
    void debug_mode_on();
    /*! Butterworth lowpass (period t0, order o) */
    void lpb(const Tseries& s, double dt, double t0, int o);
    /*! Butterworth highpass (period t0, order o) */
    void hpb(const Tseries& s, double dt, double t0, int o);
    /*! 2nd order lowpass (period t0, damping h) */
    void lp2(const Tseries& s, double dt, double t0, double h);
    /*! 2nd order highpass (period t0, damping h) */
    void hp2(const Tseries& s, double dt, double t0, double h);
    /*! 2nd order bandpass (period t0, damping h) */
    void bp2(const Tseries& s, double dt, double t0, double h);
    /*! 1st order lowpass (period t0) */
    void lp1(const Tseries& s, double dt, double t0);
    /*! 1st order highpass (period t0) */
    void hp1(const Tseries& s, double dt, double t0);
    /*! integration (time constant t0) */
    void integrate(const Tseries& s, double dt, double t0);
    /*! 1st order highpass equalizer (former period t0s, new period t0) */
    void he1(const Tseries& s, double dt, double t0s, double t0);
    /*! 1st order lowpass equalizer (former period t0s, new period t0) */
    void le1(const Tseries& s, double dt, double t0s, double t0);
    /*! 2nd order highpass equalizer (former: period t0s and damping hs,
     *                               new: period t0 and damping h) 
     */
    void he2(const Tseries& s, double dt, 
                   double t0s, double hs, double t0, double h);
    /*! 2nd order lowpass equalizer (former: period t0s and damping hs,
     *                              new: period t0 and damping h) 
     */
    void le2(const Tseries& s, double dt, 
                   double t0s, double hs, double t0, double h);
    /*! detide with synthetic tides interpolated over ni samples */
    void tid(const Tseries& s, double dt, int ni);
    /*! derivative (time constant t0) */
    void dif(const Tseries& s, double dt, double t0);
    /*! set baseline to first value */
    void first(const Tseries& s);

  } // namespace seife

} // namespace ts

#endif // TF_SEIFEXX_H_VERSION (includeguard)

/* ----- END OF seifexx.h ----- */
