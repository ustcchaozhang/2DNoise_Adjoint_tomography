/*! \file stfinvfdleastsquares.cc
 * \brief least squares in the frequency domain (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/05/2011
 * 
 * least squares in the frequency domain (implementation)
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
 *  - 04/10/2011   V1.1   renamed engine
 *  - 14/10/2015   V1.2   new end-user usage functions
 * 
 * ============================================================================
 */
#define STFINV_STFINVFDLEASTSQUARES_CC_VERSION \
  "STFINV_STFINVFDLEASTSQUARES_CC   V1.2"

#include <iostream>
#include <aff/functions/sqrsum.h>
#include <stfinv/stfinvfdleastsquares.h>
#include <stfinv/stfinvfdleastsquares_summary_usage.h>
#include <stfinv/stfinvfdleastsquares_description_usage.h>
#include <stfinv/debug.h>
#include <stfinv/tools.h>

namespace stfinv {

  const char* const STFEngineFDLeastSquares::ID="fdlsq";

  const char* const STFEngineFDLeastSquares::description
    ="least squares in the frequency domain";

  /*----------------------------------------------------------------------*/

  void STFEngineFDLeastSquares::help(std::ostream& os) const
  {
    STFEngineFDLeastSquares::classhelp(os);
  } // void STFEngineFDLeastSquares::help(std::ostream& os) const

  /*----------------------------------------------------------------------*/

  void STFEngineFDLeastSquares::usage(std::ostream& os) const
  {
    STFEngineFDLeastSquares::classusage(os);
  } // void STFEngineFDLeastSquares::usage(std::ostream& os) const

  /*----------------------------------------------------------------------*/

  const char* STFEngineFDLeastSquares::name() const
  {
    return("STFEngineFDLeastSquares");
  } //  const char const* STFEngineFDLeastSquares::name() const

  /*----------------------------------------------------------------------*/

  void STFEngineFDLeastSquares::initialize() 
  {
    // scale energy
    std::istringstream is (this->parameter("waterlevel","1.e-3")); 
    is >> Mwaterlevel;
    STFINV_debug(Mdebug&1, "STFEngineFDLeastSquares::initialize()",
                 "Mwaterlevel=" << Mwaterlevel);
    STFINV_assert(Mwaterlevel > 0,
            "ERROR: parameter for option \"waterlevel\" not larger than 0");
  } // void STFEngineFDLeastSquares::initialize()

  /*----------------------------------------------------------------------*/

  void STFEngineFDLeastSquares::classhelp(std::ostream& os)
  {
    os << stfinvfdleastsquares_summary_usage;
    os << std::endl;
    stfinv::tools::report_engine_ID<STFEngineFDLeastSquares>(os);
  } // void STFEngineFDLeastSquares::classhelp(std::ostream& os)

  /*----------------------------------------------------------------------*/

  void STFEngineFDLeastSquares::classusage(std::ostream& os)
  {
    os << stfinvfdleastsquares_description_usage;
    os << std::endl;
    Tbase::classusage(os);
    os << std::endl;
    stfinv::tools::report_engine_ID<STFEngineFDLeastSquares>(os);
  } // void STFEngineFDLeastSquares::classusage(std::ostream& os)

  /*----------------------------------------------------------------------*/

  void STFEngineFDLeastSquares::exec() 
  {
    // read signals and calculate FFT
    this->fftinput();

    // calculate waterlevel
    double waterlevel=0.;
    for (unsigned int i=0; i<this->nreceivers(); ++i)
    {
      waterlevel += this->weight(i)*this->weight(i)
        *aff::func::sqrsum(this->synthetic(i));
    } // for (unsigned int i=0; i<this->nreceivers(); ++i)
    waterlevel *= Mwaterlevel;

    STFINV_debug(Mdebug&2, "STFEngineFDLeastSquares::exec()",
                 "waterlevel=" << waterlevel);

    STFINV_debug(Mdebug&2, "STFEngineFDLeastSquares::exec()",
                 "nfreq=" << this->nfreq());
    // cycle through frequencies
    for (unsigned int i=0; i<this->nfreq(); ++i)
    {
      STFINV_debug(Mdebug&4, "STFEngineFDLeastSquares::exec()",
                   "frequency=" << i);
      TAspectrum syntheticref=this->syntheticcoeff(i);
      TAspectrum recordingref=this->recordingcoeff(i);
      STFINV_debug(Mdebug&4, "STFEngineFDLeastSquares::exec()",
                   "syntheticref.first(0)=" << syntheticref.first(0) << " " <<
                   "syntheticref.last(0)=" << syntheticref.last(0) << " "
                   "syntheticref.first(1)=" << syntheticref.first(1) << " " <<
                   "syntheticref.last(1)=" << syntheticref.last(1) << " ");
      TAspectrum::Tvalue numerator, denominator;
      denominator=waterlevel;
      numerator=0.;
      for (unsigned int j=0; j<this->nreceivers(); ++j)
      {
        numerator += this->weight(j)*this->weight(j)
          *conj(syntheticref(j))*recordingref(j);
        denominator += this->weight(j)*this->weight(j)
          *conj(syntheticref(j))*syntheticref(j);
      }
      this->stfcoeff(i)=numerator/denominator;
    } // for (unsigned int i=0; i<this->nfreq(); ++i)

    // provide results to user
    this->fftoutput();
  } // void STFEngineFDLeastSquares::exec()

} // namespace stfinv

/* ----- END OF stfinvfdleastsquares.cc ----- */
