/*! \file tsoftsffcontainer.h
 * \brief a container to hold SFF data for one file (prototypes)
 * \ingroup group_tsoft
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 11/11/2009
 * 
 * a container to hold SFF data for one file (prototypes)
 * 
 * Copyright (c) 2009 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 11/11/2009   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_TSOFTSFFCONTAINER_H_VERSION

#define DATRW_TSOFTSFFCONTAINER_H_VERSION \
  "DATRW_TSOFTSFFCONTAINER_H   V1.0   "

#include<iostream>
#include<sffxx.h>
#include<aff/series.h>
#include<datrwxx/datread.h>
#include<datrwxx/tsoftconfig.h>

namespace datrw {

  namespace tsoft {

    /*! \brief contents for one trace of data
     * \ingroup group_tsoft
     */
    struct Trace {
      public:
        //! \brief header
        ::sff::WID2 Mwid2;
        //! \brief free comment lines
        ::sff::FREE Mfree;
        //! \brief samples
        datrw::Tdseries Mseries;
    }; // struct Trace

    /*----------------------------------------------------------------------*/

    //! \brief collection of traces
    typedef aff::Series<datrw::tsoft::Trace> Ttraceseries;

    /*----------------------------------------------------------------------*/

    /*! \brief contents of a complete TSOFT file
     * \ingroup group_tsoft
     */
    struct File {
      //! \brief free comment lines
      ::sff::FREE Mfree;
      //! \brief all traces in file
      Ttraceseries Mtraces;
    }; // struct File

    /*----------------------------------------------------------------------*/

    /*! \brief function to read a complete TSOFT file
     * \ingroup group_tsoft
     */
    File readfile(std::istream& is, const ReaderConfig& rc);

  } // namespace tsoft

} // namespace datrw

#endif // DATRW_TSOFTSFFCONTAINER_H_VERSION (includeguard)

/* ----- END OF tsoftsffcontainer.h ----- */
