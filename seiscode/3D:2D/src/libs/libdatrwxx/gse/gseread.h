/*! \file gseread.h
 * \brief raw GSE reading module (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/09/2007
 * 
 * raw GSE reading module (prototypes)
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
 * 
 * REVISIONS and CHANGES 
 *  - 19/09/2007   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_GSEREAD_H_VERSION

#define DATRW_GSEREAD_H_VERSION \
  "DATRW_GSEREAD_H   V1.0   "

#include<istream>
#include<aff/series.h>
#include<gsexx.h>
#include<sffxx.h>

namespace datrw {

  /*! \brief all the stuff to read raw GSE data.
   * \defgroup group_gse Reading module for: raw GSE data
   */

  /*! \brief all the stuff to read raw GSE data
   *
   * \ingroup group_gse
   */
  namespace gse {

    /*! \brief default return type for GSE waveforms
     * \ingroup group_gse
     */
    typedef GSE2::waveform::intT Tivalue;
    typedef aff::Series<Tivalue> Tiseries;

    /*! \brief print info about GSE reading
     * \ingroup group_gse
     */
    void help(std::ostream& os);
      
    /*! \brief find next WID2 line
     * \ingroup group_gse
     */
    sff::WID2 next_wid2(std::istream& is);

    /*! \brief read samples from file
     * \ingroup group_gse
     */
    Tiseries read_gse_data(std::istream& is, const int& nsamples);

  } // namespace gse

} // namespace datrw

#endif // DATRW_GSEREAD_H_VERSION (includeguard)

/* ----- END OF gseread.h ----- */
