/*! \file seifexx.cc
 * \brief make seife functions available for C++ (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 14/01/2005
 * 
 * make seife functions available for C++ (implementation)
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
 * The function calls provided here are very simple. They could easily be
 * inlined. However, we do not (which introduces some runtime overhead), since
 * we like to hide cseife.h from the user of seifexx.h
 * 
 * REVISIONS and CHANGES 
 *  - 14/01/2005   V1.0   Thomas Forbriger
 *  - 11/07/2005   V1.1   support debug mode
 * 
 * ============================================================================
 */
#define TF_SEIFEXX_CC_VERSION \
  "TF_SEIFEXX_CC   V1.1"

#include <tsxx/seifexx.h>
extern "C" { 
#include <cseife.h> 
} // extern "C"

namespace ts {

  namespace seife {

    /*! switch on debug mode */
    void debug_mode_on() { seife_debug_mode_on(); }

    /*! Butterworth lowpass (period t0, order o) */
    void lpb(const Tseries& s, double dt, double t0, int o)
    { seife_lpb(s.pointer(), s.size(), dt, t0, o); }

    /*! Butterworth highpass (period t0, order o) */
    void hpb(const Tseries& s, double dt, double t0, int o)
    { seife_hpb(s.pointer(), s.size(), dt, t0, o); }

    /*! 2nd order lowpass (period t0, damping h) */
    void lp2(const Tseries& s, double dt, double t0, double h)
    { seife_lp2(s.pointer(), s.size(), dt, t0, h); }

    /*! 2nd order highpass (period t0, damping h) */
    void hp2(const Tseries& s, double dt, double t0, double h)
    { seife_hp2(s.pointer(), s.size(), dt, t0, h); }

    /*! 2nd order bandpass (period t0, damping h) */
    void bp2(const Tseries& s, double dt, double t0, double h)
    { seife_bp2(s.pointer(), s.size(), dt, t0, h); }

    /*! 1st order lowpass (period t0) */
    void lp1(const Tseries& s, double dt, double t0)
    { seife_lp1(s.pointer(), s.size(), dt, t0); }

    /*! 1st order highpass (period t0) */
    void hp1(const Tseries& s, double dt, double t0)
    { seife_hp1(s.pointer(), s.size(), dt, t0); }

    /*! integration (time constant t0) */
    void integrate(const Tseries& s, double dt, double t0)
    { seife_int(s.pointer(), s.size(), dt, t0); }

    /*! 1st order highpass equalizer (former period t0s, new period t0) */
    void he1(const Tseries& s, double dt, double t0s, double t0)
    { seife_he1(s.pointer(), s.size(), dt, t0s, t0); }

    /*! 1st order lowpass equalizer (former period t0s, new period t0) */
    void le1(const Tseries& s, double dt, double t0s, double t0)
    { seife_le1(s.pointer(), s.size(), dt, t0s, t0); }

    /*! 2nd order highpass equalizer (former: period t0s and damping hs,
     *                               new: period t0 and damping h) 
     */
    void he2(const Tseries& s, double dt, 
                   double t0s, double hs, double t0, double h)
    { seife_he2(s.pointer(), s.size(), dt, t0s, hs, t0, h); }

    /*! 2nd order lowpass equalizer (former: period t0s and damping hs,
     *                              new: period t0 and damping h) 
     */
    void le2(const Tseries& s, double dt, 
                   double t0s, double hs, double t0, double h)
    { seife_le2(s.pointer(), s.size(), dt, t0s, hs, t0, h); }

    /*! detide with synthetic tides interpolated over ni samples */
    void tid(const Tseries& s, double dt, int ni)
    { seife_tid(s.pointer(), s.size(), dt, ni); }

    /*! derivative (time constant t0) */
    void dif(const Tseries& s, double dt, double t0)
    { seife_dif(s.pointer(), s.size(), dt, t0); }

    /*! set baseline to first value */
    void first(const Tseries& s)
    { seife_first(s.pointer(), s.size()); }

  } // namespace seife

} // namespace ts

/* ----- END OF seifexx.cc ----- */
