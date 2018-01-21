/*! \file stfinv.h
 * \brief C API to library (prototypes)
 * 
 * \ingroup cinterface
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/05/2011
 * 
 * C API to library (prototypes)
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
 *  - 28/06/2011   V1.1   requires extern declaration for C++ since binary
 *                        code is compiled by C++ compiler, not C compiler
 *  - 30/09/2011   V1.2   implemented handling of additional time series pairs
 *  - 14/10/2015   V1.3   new end-user usage functions
 * 
 * ============================================================================
 */

// include guard
#ifndef STFINV_STFINV__H_VERSION

#define STFINV_STFINV__H_VERSION \
  "STFINV_STFINV__H   V1.3"

#include <stfinv/waveformheader.h>

#ifdef __cplusplus
extern "C" {
#endif

/*! \brief A struct to store the time series for a waveform triple.
 * \ingroup cinterface
 *
 * This struct provides references to the users workspace, where the time
 * series for on receiver are stored.
 */
struct CWaveformTriple {
  /*! \brief Temporal and spatial sampling parameters.
   * The header is expected to be the same for all three time series.
   * In particular the number of samples must be appropriate for the memory
   * allocated for all three time series arrays.
   */
  struct CTripleHeader header;
  /*! \brief Time series of recorded data.
   * This field actually is a pointer (C array) to the workspace where the
   * users stores the samples of recorded data for this specific receiver.
   * The size of the array is expected to be
   * this->header::sampling::n.
   */
  Tvalue* data;
  /*! \brief Time series of synthetic data.
   * This field actually is a pointer (C array) to the workspace where the
   * users stores the samples of the current set of synthetic data for this
   * specific receiver.
   * The size of the array is expected to be
   * this->header::sampling::n.
   * The signal is understood as the impulse response of the subsurface.
   */
  Tvalue* synthetics;
  /*! \brief Time series of convolved synthetic data.
   * This field actually is a pointer (C array) to the workspace where the
   * user wishes to receive the samples of the current set of synthetic data
   * for this specific receiver as a result of convolution with the source
   * time function.
   * The size of the array is expected to be
   * this->header::sampling::n.
   * This will contain the synthetic data (this->synthetics) convolved with
   * the obtained source correction filter as a result of a call to the library
   * functions.
   */
  Tvalue* convolvedsynthetics;
}; // struct CWaveformTriple

/*----------------------------------------------------------------------*/

/*! \brief A struct to store the time series for a pair of waveforms.
 * \ingroup cinterface
 *
 * This struct provides references to the users workspace, where the time
 * series for on receiver are stored.
 */
struct CWaveformPair {
  /*! \brief Temporal sampling parameters.
   * The header is expected to be the same for both time series.
   * In particular the number of samples must be appropriate for the memory
   * allocated for both time series arrays.
   */
  struct CWaveformHeader sampling;
  /*! \brief Time series of synthetic data.
   * This field actually is a pointer (C array) to the workspace where the
   * users stores the samples of a time series of synthetic data.
   * The size of the array is expected to be
   * this->header::sampling::n.
   */
  Tvalue* synthetics;
  /*! \brief Time series of convolved synthetic data.
   * This field actually is a pointer (C array) to the workspace where the
   * user wishes to receive the samples of the time series of synthetic data
   * as a result of convolution with the source correction filter.
   * The size of the array is expected to be
   * this->header::sampling::n.
   * This will contain the synthetic data (this->synthetics) convolved with
   * the obtained source correction filter as a result of a call to the library
   * functions.
   */
  Tvalue* convolvedsynthetics;
}; // struct CWaveformPair

/*----------------------------------------------------------------------*/

/*! \brief A struct to store a single waveform.
 *
 * \ingroup cinterface
 *
 * This will be used to pass the source correction filter.
 */
struct CWaveform {
  /*! \brief Temporal sampling.
   */
  struct CWaveformHeader sampling;
  /*! \brief Time series of waveform.
   * This field actually is a pointer (C array) to the workspace where the
   * user wishes to receive the samples of the obtained source correction
   * filter.
   * The size of the array is expected to be
   * this->sampling::n.
   */
  Tvalue* series;
}; // struct CWaveform

/*----------------------------------------------------------------------*/

/*! \brief Array of waveform triples.
 * \ingroup cinterface
 *
 * This is used to pass data for a complete profile.
 * A profile consists of CTriples::n receivers.
 * For each receiver this struct holds a reference to a struct CWaveformTriple
 * which itself provides a reference to the users workspace for time series.
 */
struct CTriples {
  /*! \brief Number of triples (i.e. receivers) in the array.
   */
  int n;
  /*! \brief Pointer to array of waveform triples.
   * This actually is a C array for elements of type struct CWaveformTriple.
   * The size ot the array is expected to be identical with the number of
   * receivers to be used (i.e. CTriples::n).
   */
  struct CWaveformTriple* triples;
}; // struct CTriples

/*----------------------------------------------------------------------*/

/*! \brief Array of waveform pairs.
 * \ingroup cinterface
 *
 * This is used to pass data for a set of synthetic time series, which should
 * be convolved with the new source correction filter on the fly.
 * A collection of time series consists of CPairs::n waveform pairs.
 * For each waveform pair this struct holds a reference to a struct
 * CWaveformPair which itself provides a reference to the users workspace for
 * time series.
 */
struct CPairs {
  /*! \brief Number of pairs in the array.
   */
  int n;
  /*! \brief Pointer to array of waveform pairs.
   * This actually is a C array for elements of type struct CWaveformPair.
   * The size ot the array is expected to be identical with the number of
   * receivers to be used (i.e. CPairs::n).
   */
  struct CWaveformPair* pairs;
}; // struct CPairs

/*======================================================================*/
// function interface to libstfinv

/*! \brief Initialize the engine.
 *
 * \ingroup cinterface
 *
 * \param triples
 *   This function expects a struct CTriples containing references to the users
 *   workspace for recorded time series as well as synthetic time series.
 *   These will be used as input.
 *   As a third set a reference to a workspace for synthetic time series
 *   convolved with the source correction filter is expected. 
 *   The latter will be used as output.
 * \param stf
 *   The struct CWaveform presents a reference to the users work space for the
 *   source correction filter time series.
 *   It will be used to present the result of the processing to the user.
 * \param parameters
 *   Parameters to select one of the engines as well as to control the engines
 *   are passed in a character sequence.
 *   See also \ref page_eu_subsec_parameters
 */
void initstfinvengine(struct CTriples triples,
                      struct CWaveform stf,
                      char* parameters);

/*! \brief Initialize the engine and pass additional time series to be
 * convolved on the fly.
 *
 * The purpose of this function in comparion to initstfinvengine() is
 * explained in the comments to parameter \p pairs.
 * \ingroup cinterface
 *
 * \param triples
 *   This function expects a struct CTriples containing references to the users
 *   workspace for recorded time series as well as synthetic time series.
 *   These will be used as input.
 *   As a third set a reference to a workspace for synthetic time series
 *   convolved with the source correction filter is expected. 
 *   The latter will be used as output.
 * \param stf
 *   The struct CWaveform presents a reference to the users work space for the
 *   source correction filter time series.
 *   It will be used to present the result of the processing to the user.
 * \param pairs
 *   The struct CPairs presents a reference to the users work space for
 *   additional synthetic time series.
 *   These time series will not be used to determine the optimal source
 *   correction filter filter, but will be convolved with the obtained source
 *   time function on the fly.
 *   This is useful in particular with forward modelling code which uses a
 *   band limited source time function for the initial synthetics already.
 *   This source time function can be passed through this argument and will
 *   then be convolved with the optimized source correction filter,
 *   such that the
 *   result of the convolution is appropriate to obtain synthetics which
 *   provide a reduced misift with respect to the data.
 * \param parameters
 *   Parameters to select one of the engines as well as to control the engines
 *   are passed in a character sequence.
 *   See also \ref page_eu_subsec_parameters
 */
void initstfinvenginewithpairs(struct CTriples triples,
                               struct CWaveform stf,
                               struct CPairs pairs,
                               char* parameters);

/*! \brief Run the engine.
 *
 * \ingroup cinterface
 */
void runstfinvengine();

/*! \brief Free the engine.
 *
 * \ingroup cinterface
 */
void freestfinvengine();

/*! \brief List procedures (engines) on stdout
 *
 * \ingroup cinterface
 */
void printengines();

/*! \brief Print usage summary to stdout
 *
 * \ingroup cinterface
 */
void printhelp();

/*! \brief Print detailed description for engine "id" to stdout
 *
 * \param id character string selecting a specific engine
 *
 * \ingroup cinterface
 */
void printusage(char* id);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // STFINV_STFINV__H_VERSION (includeguard)

/* ----- END OF stfinv.h ----- */
