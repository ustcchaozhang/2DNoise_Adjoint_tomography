/*! \file dump_series.h
 * \brief factored out Series dump function (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/12/2002
 * 
 * factored out Series dump function (prototypes)
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
 * \ingroup group_helpers
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 19/12/2002   V1.0   Thomas Forbriger
 *  - 28/12/2002   V1.1   (thof)
 *                        - function now takes ConstSeries class reference
 *                          arguments
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_DUMP_SERIES_H_VERSION

#define AFF_DUMP_SERIES_H_VERSION \
  "AFF_DUMP_SERIES_H   V1.1"

#include<iostream>
#include<aff/series.h>

namespace aff {

using std::endl;

/*! \brief Dump series contents 
 *
 * \ingroup group_helpers
 */
template<typename T>
void dump(const aff::ConstSeries<T>& series, 
          std::ostream& os=std::cout)
{
  os << "dump of a Series object:" << endl;
  os << "  size: " << series.size() << endl;
  os << "  index range: [" << series.f() << ":" << series.l() << "]" << endl;
  os << "  elements:" << endl;
  int i=0;
  for (aff::Tsubscript k=series.f(); k<=series.l(); k++)
  {
    if (!i) { os << "   "; }
    os.width(8);
    os << series(k) << " ";
    if (++i>7) { i=0; os << endl; }
  }
  os << endl;
}

} // namespace aff

#endif // AFF_DUMP_SERIES_H_VERSION (includeguard)

/* ----- END OF dump_series.h ----- */
