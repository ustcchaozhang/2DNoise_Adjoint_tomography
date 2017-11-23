/*! \file ipolin.h
 * \brief linear interpolation (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/07/2005
 * 
 * linear interpolation (prototypes)
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
 *  - 13/07/2005   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_IPOLIN_H_VERSION

#define TF_IPOLIN_H_VERSION \
  "TF_IPOLIN_H   V1.0   "

#include<tsxx/ipo.h>

namespace ts {

  namespace ipo {

    /*! \brief very simple linear interpolator
     *
     * locates position of new sample and uses linear interpolation between
     * neighbouring samples to calculate new value.
     */
    class LinearInterpolator: public Interpolator {
      public:
        typedef Interpolator Tbase;
        using Tbase::Ttimeseries;
        using Tbase::Tconst_timeseries;
        using Tbase::Tconst_series;
        using Tbase::Theader;
        using Tbase::Tvalue;
	  LinearInterpolator(const Tconst_timeseries& ts,
                           const bool& debug=false); 
        ~LinearInterpolator() { }
        Tvalue operator()(const libtime::TAbsoluteTime& t) const;
      private:
        libtime::TAbsoluteTime Mlast;
        libtime::TRelativeTime Mdt;
        libtime::TAbsoluteTime Madjustedfirst;
        libtime::TRange Minputwindow;
    }; // class LinearInterpolator

  } // namespace ipo

} // namespace ts

#endif // TF_IPOLIN_H_VERSION (includeguard)

/* ----- END OF ipolin.h ----- */
