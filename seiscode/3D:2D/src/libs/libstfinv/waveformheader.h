/*! \file waveformheader.h
 * \brief definition of waveform header to be used in C and C++ (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/05/2011
 * 
 * definition of waveform header to be used in C and C++ (prototypes)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 06/05/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef STFINV_WAVEFORMHEADER_H_VERSION

#define STFINV_WAVEFORMHEADER_H_VERSION \
  "STFINV_WAVEFORMHEADER_H   V1.0"

#ifdef __cplusplus
extern "C" {
#endif

  /*! \brief Value type of samples.
   * \ingroup cinterface
   *
   * All references to time series samples in user workspace are based on this
   * type.
   */
  typedef float Tvalue;

/*----------------------------------------------------------------------*/

  /*! \brief Temporal sampling for time series data.
   * \ingroup cinterface
   */
  struct CWaveformHeader {
    /*! \brief Sampling interval in seconds.
     */
    double dt;
    /*! \brief Number of samples in time series array.
     */
    unsigned int n;
  }; // struct CWaveformHeader

  /*----------------------------------------------------------------------*/

  /*! \brief Structure to hold header information for a triple of waveforms.
   * \ingroup cinterface
   *
   * Each triple of waveforms reference by e.g. struct CWaveformTriple are
   * associated to a receiver.
   * The coordinates of the receiver as well as of the associated shot
   * location are stored here.
   * Further temporal sampling is defined by this->sampling.
   */
  struct CTripleHeader {
    /*! \name Source coordinates in meters.
     *
     * - \c sx: x-coordinate
     * - \c sy: y-coordinate
     * - \c sz: z-coordinate
     */
    //@{
    double sx, sy, sz;
    //@}
    /*! \name Receiver coordinates in meters.
     *
     * - \c rx: x-coordinate
     * - \c ry: y-coordinate
     * - \c rz: z-coordinate
     */
    //@{
    double rx, ry, rz;
    //@}
    /*! \brief Temporal sampling of all three waveforms.
     */
    struct CWaveformHeader sampling;
  }; // struct CTripleHeader

#ifdef __cplusplus
} // extern "C"
#endif


#endif // STFINV_WAVEFORMHEADER_H_VERSION (includeguard)

/* ----- END OF waveformheader.h ----- */
