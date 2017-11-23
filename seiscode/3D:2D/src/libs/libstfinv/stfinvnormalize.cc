/*! \file stfinvnormalize.cc
 * \brief a Fourier domain engine which finds a normalizing source wavelet (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/05/2011
 * 
 * a Fourier domain engine which finds a normalizing source wavelet (implementation)
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
 *  - 08/05/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define STFINV_STFINVNORMALIZE_CC_VERSION \
  "STFINV_STFINVNORMALIZE_CC   V1.0"

#include <stfinv/stfinvnormalize.h>
#include <aff/functions/sqrsum.h>
#include <stfinv/debug.h>
#include <cmath>

namespace stfinv {

  const char* const STFEngineNormalize::ID="fdnorm";

  const char* const STFEngineNormalize::description
    ="Fourier domain normalization";

  /*----------------------------------------------------------------------*/

  void STFEngineNormalize::help(std::ostream& os) const
  {
    STFEngineNormalize::classhelp(os);
  } // void STFEngineNormalize::help(std::ostream& os) const

  /*----------------------------------------------------------------------*/

  const char* STFEngineNormalize::name() const
  {
    return("STFEngineNormalize");
  } //  const char const* STFEngineNormalize::name() const

  /*----------------------------------------------------------------------*/

  void STFEngineNormalize::initialize() 
  {
    // scale energy
    std::istringstream is (this->parameter("waterlevel","1.e-3")); 
    is >> Mwaterlevel;
    STFINV_debug(Mdebug&1, "STFEngineNormalize::initialize()",
                 "Mwaterlevel=" << Mwaterlevel);
    STFINV_assert(Mwaterlevel > 0,
            "ERROR: parameter for option \"waterlevel\" not larger than 0");
  } // void STFEngineNormalize::initialize()

  /*----------------------------------------------------------------------*/

  void STFEngineNormalize::exec() 
  {
    // read signals and calculate FFT
    this->fftinput();

    // cycle through frequencies
    // calculate weighted energy of all synthetic traces
    Tseries syntheticenergy(0,this->nfreq()-1);
    syntheticenergy=0.;
    double totalsyntheticenergy=0.;
    for (unsigned int i=0; i<this->nfreq(); ++i)
    {
      TAspectrum syntheticref=this->syntheticcoeff(i);
      for (unsigned int j=0; j<this->nreceivers(); ++j)
      {
        syntheticenergy(i) += this->weight(j)*this->weight(j)
          *std::norm(syntheticref(j));
      }
      totalsyntheticenergy += syntheticenergy(i);
    }

    // calculate waterlevel
    double waterlevel=Mwaterlevel*totalsyntheticenergy/this->nfreq();

    double weightsum=aff::func::sqrsum(this->weights());

    for (unsigned int i=0; i<this->nfreq(); ++i)
    {
      double recordingenergy=0.;
      TAspectrum recordingref=this->recordingcoeff(i);
      TAspectrum syntheticref=this->syntheticcoeff(i);
      for (unsigned int j=0; j<this->nreceivers(); ++j)
      {
        recordingenergy += this->weight(j)*this->weight(j)
          *std::norm(recordingref(j));
      }

      double modulus
        =std::sqrt(recordingenergy/(syntheticenergy(i)+waterlevel));

      double phase = 0.;
      for (unsigned int j=0; j<this->nreceivers(); ++j)
      {
        phase += this->weight(j)*
          (std::arg(recordingref(j))-std::arg(syntheticref(j)));
      }
      phase /= weightsum;

      this->stfcoeff(i)=std::polar(modulus, phase);
    } // for (unsigned int i=0; i<this->nfreq(); ++i)

    // provide results to user
    this->fftoutput();
  } // void STFEngineNormalize::exec()

  /*----------------------------------------------------------------------*/

  void STFEngineNormalize::classhelp(std::ostream& os)
  {
    os << "class STFEngineNormalize (" 
      << STFEngineNormalize::ID << ")\n";
    os << STFEngineNormalize::description << "\n" << std::endl;
    STFINV_illegal;
    os << "DESCRIBE HERE\n"
      << "A waterlevel as a fraction of the signal energy of the\n"
      << "input synthetics is applied. If per receiver scaling is\n"
      << "selected, the receivers will be weighted in the deconvolution.\n";
    os << "Options and parameters:\n"
      << "waterlevel=l  waterlevel to be applied for regularization."
      << std::endl;
    Tbase::classhelp(os);
  } // void STFEngineNormalize::classhelp(std::ostream& os)

} // namespace stfinv

/* ----- END OF stfinvnormalize.cc ----- */
