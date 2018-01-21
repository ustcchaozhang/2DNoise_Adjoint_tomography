/*! \file inputoperators.h
 * \brief input operators for SFF TimeSeries traces and files for libdatrwxx
 *        streams (prototypes).
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 18/07/2005
 * \date 30/01/2014
 * 
 * input operators for SFF TimeSeries traces and files for libdatrwxx streams
 * (prototypes)
 * 
 * Copyright (c) 2005-2007, 2012, 2014 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 30/01/2014   V1.0   Thomas Forbriger (thof):
 *                        copied from sffheaders.h
 *  - 22/11/2016   V1.1   use explicit assignment in input operator
 * 
 * ============================================================================
 */

// include guard
#ifndef TSIO_INPUTOPERATORS_H_VERSION

#define TSIO_INPUTOPERATORS_H_VERSION \
  "TF_INPUTOPERATORS_H   2016/11/22"

#include<tsioxx/sfftsfile.h>
#include<datrwxx/datread.h>

namespace ts {

  namespace sff {

    /*! \brief libdatrwxx input operators
     * \defgroup group_inputoperators libdatrwxx input operators
     *
     * This module is presented through inputoperators.h
     *
     * @{
     */

    // libdatrwxx input operators 

    datrw::idatstream& operator>>(datrw::idatstream& is, FileHeader& fh);
    datrw::idatstream& operator>>(datrw::idatstream& is, TraceHeader& th);

    /*----------------------------------------------------------------------*/

    template<class C>
    datrw::idatstream& operator>>(datrw::idatstream& is, 
                                    SFFTimeSeries<C>& s)
    { 
      typename SFFTimeSeries<C>::Tseries series;
      is >> series >> s.header;
      s=series;
      return(is); }

    /*!
     * @{
     */

  } // namespace sff
} // namespace ts

#endif // TSIO_INPUTOPERATORS_H_VERSION (includeguard)

/* ----- END OF inputoperators.h ----- */
