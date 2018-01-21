/*! \file reservoir.h
 * \brief series reservoir (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 20/02/2010
 * 
 * series reservoir (prototypes)
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
 *  - 07/06/2011   V1.1   promise constness of series samples
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_RESERVOIR_H_VERSION

#define DATRW_RESERVOIR_H_VERSION \
  "DATRW_RESERVOIR_H   V1.1"

#include<datrwxx/datwrite.h>

namespace datrw {

  namespace util {

    /*! a flag to distingtuish the three types of sample data
     *
     */
    enum Evaluetype {
      Fnone,    //! empty reservoir
      Fdouble,  //! double precision floating point
      Ffloat,   //! single precision floating point 
      Finteger  //! integer 
    }; // enum Evaluetype

    /*----------------------------------------------------------------------*/

    /*! class to store series data intermediately.
     *
     * Since SFF data requires a flag to be set which indicates whether another
     * trace will follow the current trace or whether the current trace is the
     * last one in the data file, the actual writing process has to be
     * postponed until the next trace is submitted or until the file is going
     * to be closed.
     * This class is a small utility that helps to save an intermediate copy
     * of the data until it is actually written.
     */
    class seriesreservoir {
      public:
        seriesreservoir();
        seriesreservoir(const Tdseries::Tcoc& series);
        seriesreservoir(const Tfseries::Tcoc& series);
        seriesreservoir(const Tiseries::Tcoc& series);
        Evaluetype valuetype() const { return Mvaluetype; }
        Tdseries::Tcoc dseries() const;
        Tfseries::Tcoc fseries() const;
        Tiseries::Tcoc iseries() const;
      private:
        Evaluetype Mvaluetype;
        Tdseries::Tcoc Mdseries;
        Tfseries::Tcoc Mfseries;
        Tiseries::Tcoc Miseries;
    }; // class seriesreservoir

  } // namespace util

} // namespace datrw

#endif // DATRW_RESERVOIR_H_VERSION (includeguard)

/* ----- END OF reservoir.h ----- */
