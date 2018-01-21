/*! \file dump_sharedheap.h
 * \brief factored out SharedHeap dump function (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/12/2002
 * 
 * factored out SharedHeap dump function (prototypes)
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
 *                        - functions now take ConstSharedHeap class reference
 *                          arguments
 *  - 03/01/2003   V1.2   (thof)
 *                        - distinguish more clearly between size and values
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_DUMP_SHAREDHEAP_H_VERSION

#define AFF_DUMP_SHAREDHEAP_H_VERSION \
  "AFF_DUMP_SHAREDHEAP_H   V1.2"

#include<iostream>
#include<aff/lib/sharedheap.h>

namespace aff {

using std::endl;

/*! \brief Dump heap layout 
 *
 * \ingroup group_helpers
 */
template<typename T>
void dump_layout(const aff::ConstSharedHeap<T>& sharedheap, 
          std::ostream& os=std::cout)
{
  os << "  SharedHeap object size: "
   << sharedheap.size() << " elements" << endl;
}

/*----------------------------------------------------------------------*/

/*! \brief Dump heap contents 
 *
 * \ingroup group_helpers
 */
template<typename T>
void dump(const aff::ConstSharedHeap<T>& sharedheap, 
          std::ostream& os=std::cout)
{
  os << "dump of SharedHeap object:" << endl;
  dump_layout(sharedheap, os);
  os << "  element values:" << endl;
  int i=0;
  for (aff::Tsize k=0; k<sharedheap.size(); k++)
  {
    if (!i) { os << "   "; }
    os.width(8);
    os << sharedheap[k] << " ";
    if (++i>7) { i=0; os << endl; }
  }
  os << endl;
}

} // namespace aff

#endif // AFF_DUMP_SHAREDHEAP_H_VERSION (includeguard)

/* ----- END OF dump_sharedheap.h ----- */
