/*! \file stfinvidentity.cc
 * \brief just find a scaling factor (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/05/2011
 * 
 * just find a scaling factor (implementation)
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
 *  - 07/05/2011   V1.0   Thomas Forbriger (thof)
 *  - 21/02/2014   V1.1   implemented scaling to average weighted energy
 *  - 14/10/2015   V1.2   new end-user usage functions
 *  - 19/10/2015   V1.3   weight factor is defined in terms amplitude; apply
 *                        square of weight to signal energy
 * 
 * ============================================================================
 */
#define STFINV_STFINVIDENTITY_CC_VERSION \
  "STFINV_STFINVIDENTITY_CC   V1.3"

#include <stfinv/stfinvidentity.h>
#include <stfinv/stfinvidentity_summary_usage.h>
#include <stfinv/stfinvidentity_description_usage.h>
#include <aff/functions/sqrsum.h>
#include <aff/seriesoperators.h>
#include <cmath>
#include <stfinv/tools.h>

namespace stfinv {

  const char* const STFEngineIdentity::ID="ident";

  const char* const STFEngineIdentity::description
    ="scale with amplitude factor";

  /*----------------------------------------------------------------------*/

  void STFEngineIdentity::help(std::ostream& os) const
  {
    STFEngineIdentity::classhelp(os);
  } // void STFEngineIdentity::help(std::ostream& os) const

  /*----------------------------------------------------------------------*/

  void STFEngineIdentity::usage(std::ostream& os) const
  {
    STFEngineIdentity::classusage(os);
  } // void STFEngineIdentity::usage(std::ostream& os) const

  /*----------------------------------------------------------------------*/

  const char* STFEngineIdentity::name() const
  {
    return("STFEngineIdentity");
  } //  const char const* STFEngineIdentity::name() const

  /*----------------------------------------------------------------------*/

  void STFEngineIdentity::initialize() 
  {
    // scale energy
    Mscaleenergy=(this->parameter("scaleenergy","false")=="true");
  } // void STFEngineIdentity::initialize()

  /*----------------------------------------------------------------------*/

  void STFEngineIdentity::exec() 
  {
    // effective amplitude factor
    double fac=1.;

    // scale to reproduce average energy if requested
    if (Mscaleenergy)
    {
      double recording_sqrsum=0.;;
      double synthetic_sqrsum=0.;;
      for (unsigned int i=0; i<this->nreceivers(); ++i)
      {
        synthetic_sqrsum 
          += aff::func::sqrsum(this->synthetic(i)) 
             * this->weight(i) * this->weight(i);
        recording_sqrsum 
          += aff::func::sqrsum(this->recording(i)) 
             * this->weight(i) * this->weight(i);
        fac = std::sqrt(recording_sqrsum/synthetic_sqrsum);
      }
    } // if (Mscaleenergy)

    Tseries stf=this->stf();
    stf=0.;
    stf(0)=fac/this->dt();
    for (unsigned int i=0; i<this->nreceivers(); ++i)
    {
      Tseries::Tcoc synthetic=this->synthetic(i);
      Tseries convolvedsynthetic=this->convolvedsynthetic(i);
      convolvedsynthetic.copyin(synthetic);
      if (Mscaleenergy) { convolvedsynthetic *= fac; }
    }
    for (unsigned int i=0; i<this->npairs(); ++i)
    {
      Tseries::Tcoc series=this->series(i);
      Tseries convolvedseries=this->convolvedseries(i);
      convolvedseries.copyin(series);
      if (Mscaleenergy) { convolvedseries *= fac; }
    }
  } // void STFEngineIdentity::exec()

  /*----------------------------------------------------------------------*/

  void STFEngineIdentity::classhelp(std::ostream& os)
  {
    os << stfinvidentity_summary_usage;
    os << std::endl;
    stfinv::tools::report_engine_ID<STFEngineIdentity>(os);
  } // void STFEngineIdentity::classhelp(std::ostream& os)

  /*----------------------------------------------------------------------*/

  void STFEngineIdentity::classusage(std::ostream& os)
  {
    os << stfinvidentity_description_usage;
    os << std::endl;
    Tbase::classusage(os);
    os << std::endl;
    stfinv::tools::report_engine_ID<STFEngineIdentity>(os);
  } // void STFEngineIdentity::classusage(std::ostream& os)

} // namespace stfinv

/* ----- END OF stfinvidentity.cc ----- */
