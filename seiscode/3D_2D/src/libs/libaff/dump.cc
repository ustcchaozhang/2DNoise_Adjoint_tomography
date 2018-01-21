/*! \file dump.cc
 * \brief debug helpers (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/12/2002
 * 
 * debug helpers (implementation)
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
 * \sa group_helpers
 *
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 12/12/2002   V1.0   Thomas Forbriger
 *  - 18/12/2002   V1.1   (thof)
 *                        - stated definitions
 *  - 29/12/2002   V1.2   (thof)
 *                        - include only those headers we need
 * 
 * ============================================================================
 */
#define AFF_DUMP_CC_VERSION \
  "AFF_DUMP_CC   V1.1"

#include <aff/lib/dump_strided.h>
#include <aff/lib/error.h>

namespace aff {

//! dump shape
void dump(const Strided& shape, std::ostream& os)
{
  os << "dump of a Strided object:" << endl;
  os << "  index ranges: ";
  for (Tsize i=0; i<Strided::Mmax_dimen; i++)
  {
    os << "[" << shape.first(i) << ":" << shape.last(i) << "]";
  }
  os << endl;
  os << "  strides: ";
  for (Tsize i=0; i<Strided::Mmax_dimen; i++)
  {
    if (i>0) os << ", ";
    os << shape.stride(i);
  }
  os << endl;
  os << "  total number of mapped elements: "
    << shape.size() << endl;
  os << "  total address range in memory: ["
    << shape.first_offset() << ":" << shape.last_offset() << "] = "
    << shape.memory_size() << " positions" << endl;
}

/*----------------------------------------------------------------------*/

namespace {

  /*! \brief recursive usage tests all offset functions
   *
   * \ingroup group_helpers
   */
  void dump_map_helper(const Strided& shape, const Tdim& i, const Tdim& j,
                  Strided::TIndexVec& index, std::ostream& os)
  {
    if (i>1)
    {
      for (index[i]=shape.first(i); index[i]<=shape.last(i); index[i]++)
      {
        os << "    dimension "; 
        os << i << ": [" << index[i] << "]";
        dump_map_helper(shape, i-1, j, index, os);
      }
    }
    else if (i>0)
    {
      if (i<j) os << endl;
      os.width(5); os << " ";
      for (int k=shape.first(1); k<=shape.last(1); k++)
      {
        os.width(6); os << k;
      }
      os << endl;
      for (index[0]=shape.first(0); index[0]<=shape.last(0); index[0]++)
      {
        os.width(5); os << index[0];
        for (index[1]=shape.first(1); index[1]<=shape.last(1); index[1]++)
        {
          os.width(5); 
          if (j==0)
          { os << shape.offset(index[0]); }
          else if (j==1)
          { os << shape.offset(index[0], index[1]); }
          else if (j==2)
          { os << shape.offset(index[0], index[1], index[2]); }
          else if (j==3)
          { os << shape.offset(index[0], index[1], index[2], index[3]); }
          else 
          { os << shape.offset(index); }
          os << "#";
        }
        os << endl;
      }
    }
    else 
    {
      for (index[0]=shape.first(0); index[0]<=shape.last(0); index[0]++)
      {
        os.width(5); os << index[0];
        os.width(5); 
        if (j==0)
        { os << shape.offset(index[0]); }
        else if (j==1)
        { os << shape.offset(index[0], index[1]); }
        else if (j==2)
        { os << shape.offset(index[0], index[1], index[2]); }
        else if (j==3)
        { os << shape.offset(index[0], index[1], index[2], index[3]); }
        else 
        { os << shape.offset(index); }
        os << "#" << endl;
      }
    }
  }
}

//! dump index mapping of shape
void dump_map(const Strided& shape, const Tdim& i, std::ostream& os)
{
  AFF_assert((i<Strided::Mmax_dimen),
             "ERROR (dump_map): illegal dimensionality");
  dump(shape);
  os << "  index mapping for " << i+1 << " dimensional access:" << endl;
  Strided::TIndexVec index(shape.first());
  dump_map_helper(shape, i, i, index, os); 
}

} // namespace aff

/* ----- END OF dump.cc ----- */
