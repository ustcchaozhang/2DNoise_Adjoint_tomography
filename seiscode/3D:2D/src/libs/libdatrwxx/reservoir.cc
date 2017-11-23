/*! \file reservoir.cc
 * \brief series reservoir (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 20/02/2010
 * 
 * series reservoir (implementation)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 20/02/2010   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DATRW_RESERVOIR_CC_VERSION \
  "DATRW_RESERVOIR_CC   V1.0"

#include <datrwxx/reservoir.h>
#include <datrwxx/error.h>

namespace datrw {

  namespace util {

    seriesreservoir::seriesreservoir()
    {
      Mvaluetype=Fnone;
    } // seriesreservoir::seriesreservoir()

    seriesreservoir::seriesreservoir(const Tdseries::Tcoc& series)
    {
      Mvaluetype=Fdouble;
      Mdseries=series.copyout();
    } // seriesreservoir::seriesreservoir(const Tdseries& series)

    seriesreservoir::seriesreservoir(const Tfseries::Tcoc& series)
    {
      Mvaluetype=Ffloat;
      Mfseries=series.copyout();
    } // seriesreservoir::seriesreservoir(const Tfseries& series)

    seriesreservoir::seriesreservoir(const Tiseries::Tcoc& series)
    {
      Mvaluetype=Finteger;
      Miseries=series.copyout();
    } // seriesreservoir::seriesreservoir(const Tiseries& series)

    Tdseries::Tcoc seriesreservoir::dseries() const 
    { 
      DATRW_assert((Mvaluetype == Fdouble), "wrong data type");
      return Mdseries; 
    } // Tdseries seriesreservoir::dseries() const

    Tfseries::Tcoc seriesreservoir::fseries() const 
    { 
      DATRW_assert((Mvaluetype == Ffloat), "wrong data type");
      return Mfseries; 
    } // Tfseries seriesreservoir::fseries() const

    Tiseries::Tcoc seriesreservoir::iseries() const 
    { 
      DATRW_assert((Mvaluetype == Finteger), "wrong data type");
      return Miseries; 
    } // Tiseries seriesreservoir::iseries() const

  } // namespace util

} // namespace datrw

/* ----- END OF reservoir.cc ----- */
