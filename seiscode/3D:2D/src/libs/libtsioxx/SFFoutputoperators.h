/*! \file SFFoutputoperators.h
 * \brief deprecated output operators to libsffxx output stream (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 18/07/2005
 * \date 30/01/2014
 * 
 * deprecated output operators to libsffxx output stream (prototypes)
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
 * 
 * ============================================================================
 */

// include guard
#ifndef TSIO_SFFOUTPUTOPERATORS_H_VERSION

#define TSIO_SFFOUTPUTOPERATORS_H_VERSION \
  "TF_SFFOUTPUTOPERATORS_H   2014/01/30"

#warning Use of this module is deprecated.
#warning Better use outputoperators.h for libdatrwxx streams.

#include<tsioxx/sfftsfile.h>
#include<sffostream.h>

namespace ts {

  namespace sff {

    /*! \brief libsffxx output operators
     * \defgroup group_SFFoutputoperators libsffxx output operators
     *
     * This module is presented through SFFoutputoperators.h
     *
     * \deprecated
     *   This interface should no longer be used.
     *   The interface provided by outputoperators.h for output to libdatrwxx
     *   file streams is much more flexible.
     *
     * @{
     */

    /*======================================================================*/
    // sff::SFFostream output operators 

    template<class C>
    ::sff::SFFostream<C>& operator<<(::sff::SFFostream<C>& os, 
                                     const FileHeader& fh)
    { 
      if (fh.hasfree()) { os << fh.free(); }
      if (fh.hassrce()) { os << fh.srce(); }
      return os;
    }

    /*----------------------------------------------------------------------*/

    template<class C>
    ::sff::SFFostream<C>& operator<<(::sff::SFFostream<C>& os, 
                                     const TraceHeader& th)
    { 
      os << th.wid2();
      if (th.hasfree()) { os << th.free(); }
      if (th.hasinfo()) { os << th.info(); }
      return os;
    }

    /*----------------------------------------------------------------------*/

    /*! \note
     * The compiler cannot distinguish between this definition and the
     * next one.
     */
    template<class C>
    ::sff::SFFostream<C>& operator<<(::sff::SFFostream<C>& os, 
                       const typename SFFTimeSeries<C>::Tconsttimeseries& s)
    { 
      typedef typename SFFTimeSeries<C>::Tconsttimeseries::Tseries Tseries;
      return(os << Tseries(s) << s.header); 
    }

    /*----------------------------------------------------------------------*/

    template<class C>
    ::sff::SFFostream<C>& operator<<(::sff::SFFostream<C>& os, 
                                   const SFFTimeSeries<C>& s)
    { 
      typedef typename SFFTimeSeries<C>::Tcoc Tcoc;
      return(os << Tcoc(s) << s.header); 
    }

    /*----------------------------------------------------------------------*/

    template<class C>
    ::sff::SFFostream<C>& operator<<(::sff::SFFostream<C>& os, 
                                   const TraceVector<C>& tv)
    { 
      typedef TraceVector<C> Ttracevector;
      for(typename Ttracevector::const_iterator i=tv.begin();
          i != tv.end(); ++i)
      { os << *i; }
      return os;
    }

    /*----------------------------------------------------------------------*/

    template<class C>
    ::sff::SFFostream<C>& operator<<(::sff::SFFostream<C>& os, 
                                     const File<C>& f)
    { return(os << f.fileheader << TraceVector<C>(f)); }

    /*!
     * @}
     */

  } // namespace sff
} // namespace ts

#endif // TSIO_SFFOUTPUTOPERATORS_H_VERSION (includeguard)

/* ----- END OF SFFoutputoperators.h ----- */
