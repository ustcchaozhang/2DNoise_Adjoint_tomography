/*! \file stfinv.cc
 * \brief C API to library (implementation)
 * 
 * ----------------------------------------------------------------------------
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
 * \author Thomas Forbriger
 * \date 06/05/2011
 * 
 * C API to library (implementation)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 06/05/2011   V1.0   Thomas Forbriger
 *  - 30/09/2011   V1.1   implemented handling of additional time series pairs
 *  - 05/10/2011   V1.2   correction: initstfinvenginewithpairs was named
 *                        incorrectly initstfinvenginepairs
 *  - 14/10/2015   V1.3   new end-user usage functions
 * 
 * ============================================================================
 */
#define STFINV_STFINV_C_VERSION \
  "STFINV_STFINV_C   V1.3"

#include <stfinv/stfinv.h>
#include <stfinv/error.h>
#include <stfinv/stfinvany.h>

/*! \brief Internals of C API.
 *
 * The C language API uses some internal data structures to keep engines.
 *
 * \defgroup capiinternal Internals of C API
 * \sa cinterface
 */

namespace stfinv {

  /*! \brief Namespace for internal data structures of the C API.
   * \ingroup capiinternal
   */
  namespace capi {

    /*! \brief Pointer to engine to work with.
     * \ingroup capiinternal
     */
    stfinv::STFEngine* Pengine=0;

  } // namespace capi

} // namespace stfinv

/*======================================================================*/

void initstfinvenginewithpairs(struct CTriples triples,
                               struct CWaveform stf,
                               struct CPairs pairs,
                               char* parameters)
{
  // remove existing engine if has previously been initialized
  freestfinvengine();

  // convert parameter string
  std::string cxxparameters(parameters);

  // convert pointer to source correction filter array
  stfinv::Waveform cxxstf;
  cxxstf.sampling=stf.sampling;
  aff::LinearShape shape(0, cxxstf.sampling.n-1, 0);
  cxxstf.series
    =stfinv::Tseries(shape, 
                     aff::SharedHeap<Tvalue>(stf.series,
                                             cxxstf.sampling.n));

  // convert pointer triples
  // notice: it is not required to check consistency here; all header fields
  // are transferred to the C++ containers; the C++ engine will check
  // consistency during its initialization
  STFINV_assert(triples.n>0, "no triples provided");
  stfinv::Tvectoroftriples cxxtriples(triples.n);
  for (int i=0; i<triples.n; ++i)
  {
    CWaveformTriple ctriple=triples.triples[i];
    cxxtriples[i].header=ctriple.header;
    const int& n=cxxtriples[i].header.sampling.n;
    shape=aff::LinearShape(0, n-1, 0);
    cxxtriples[i].data
      =stfinv::Tseries::Tcoc(shape, 
                       aff::SharedHeap<Tvalue>::Tcoc(ctriple.data, n));
    cxxtriples[i].synthetics
      =stfinv::Tseries::Tcoc(shape, 
                       aff::SharedHeap<Tvalue>::Tcoc(ctriple.synthetics, n));
    cxxtriples[i].convolvedsynthetics
      =stfinv::Tseries(shape, 
                       aff::SharedHeap<Tvalue>(ctriple.convolvedsynthetics, n));
  }

  // convert pointer pairs
  // notice: it is not required to check consistency here; all header fields
  // are transferred to the C++ containers; the C++ engine will check
  // consistency during its initialization
  stfinv::Tvectorofpairs cxxpairs(pairs.n);
  for (int i=0; i<pairs.n; ++i)
  {
    CWaveformPair cpair=pairs.pairs[i];
    cxxpairs[i].sampling=cpair.sampling;
    const int& n=cxxpairs[i].sampling.n;
    shape=aff::LinearShape(0, n-1, 0);
    cxxpairs[i].synthetics
      =stfinv::Tseries::Tcoc(shape, 
                       aff::SharedHeap<Tvalue>::Tcoc(cpair.synthetics, n));
    cxxpairs[i].convolvedsynthetics
      =stfinv::Tseries(shape, 
                       aff::SharedHeap<Tvalue>(cpair.convolvedsynthetics, n));
  }

  // create engine
  stfinv::capi::Pengine=new stfinv::STFEngine(cxxtriples, cxxstf,
                                              cxxpairs,
                                              cxxparameters);
} // void initstfinvenginewithpairs

/*----------------------------------------------------------------------*/

void initstfinvengine(struct CTriples triples,
                      struct CWaveform stf,
                      char* parameters)
{
  struct CPairs pairs;
  pairs.n=0;
  initstfinvenginewithpairs(triples, stf, pairs, parameters);
} // void initstfinvengine

/*----------------------------------------------------------------------*/

void runstfinvengine()
{
  STFINV_assert(stfinv::capi::Pengine != 0,
                "runstfinvengine(): engine is not initialized!");
  stfinv::capi::Pengine->run();
} // void runstfinvengine()

/*----------------------------------------------------------------------*/

void freestfinvengine()
{
  if (stfinv::capi::Pengine != 0) { delete stfinv::capi::Pengine; }
} // void freestfinvengine()

/*----------------------------------------------------------------------*/

void printengines()
{
  stfinv::engines();
} // void printengines()

/*----------------------------------------------------------------------*/

void printhelp()
{
  stfinv::help();
} // void printhelp()

/*----------------------------------------------------------------------*/

void printusage(char* id)
{
  std::string selectid(id);
  stfinv::usage(selectid);
} // void printhelp(age(char* id)

/* ----- END OF stfinv.cc ----- */
