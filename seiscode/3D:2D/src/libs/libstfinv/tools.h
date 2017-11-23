/*! \file tools.h
 * \brief tools and utilities (prototypes)
 * 
 * \ingroup group_tools
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/05/2011
 * 
 * tools and utilities (prototypes)
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
 *  - 28/05/2011   V1.0   Thomas Forbriger
 *  - 15/10/2015   V1.1   new report function (report_engine)
 * 
 * ============================================================================
 */

// include guard
#ifndef STFINV_TOOLS_H_VERSION

#define STFINV_TOOLS_H_VERSION \
  "STFINV_TOOLS_H   V1.1"

#include<iostream>

namespace stfinv {

  namespace tools {

    /*! \brief function to compare doubles
     * \ingroup group_tools
     * \param a a double value
     * \param b a double value
     * \param eps relative residual allowed for \c a and \c b
     * \return true if relative residual between \c a and \c b is smaller than
     *         \p eps
     */
    bool sameineps(const double &a, const double& b, const double& eps=1.e-8);

/* ---------------------------------------------------------------------- */

    /*! \brief report engine identifier
     * \ingroup group_tools
     * \param C class to report ID and oneline description
     * \param os output stream to send output to
     */
    template<class C>
      void report_engine(std::ostream& os)
      {
        os << "  ID: ";
        os.width(10); 
        os.setf(std::ios_base::left);
        os << C::ID;
        os.width(0);
        os << " (" << C::description << ")" << std::endl;
      } // void report_engine(std::ostream& os)

/* ---------------------------------------------------------------------- */

    /*! \brief report engine identifier with heading
     * \ingroup group_tools
     * \param C class to report ID and oneline description
     * \param os output stream to send output to
     */
    template<class C>
      void report_engine_ID(std::ostream& os)
      {
        os << "Identifier to selected this procedure:" << std::endl;
        report_engine<C>(os);
      } // void report_engine_ID(std::ostream& os)
    
  } // namespace tools

} // namespace stfinv

#endif // STFINV_TOOLS_H_VERSION (includeguard)

/* ----- END OF tools.h ----- */
