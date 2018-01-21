/*! \file dump_array.h
 * \brief factored out Array dump function (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/12/2002
 * 
 * factored out Array dump function (prototypes)
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
 *                        - functions now take ConstArray class reference
 *                          arguments
 *  - 03/01/2003   V1.2   (thof)
 *                        - value dump looked like dump of SharedHeap
 *                          introduced section title
 *  - 21/10/2016   V1.3   (thof)
 *                        - provide separate function to dump array values
 *                          only
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_DUMP_ARRAY_H_VERSION

#define AFF_DUMP_ARRAY_H_VERSION \
  "AFF_DUMP_ARRAY_H   V1.3"

#include<iostream>
#include<aff/array.h>
#include<aff/lib/dump_sharedheap.h>

namespace aff {

using std::endl;

namespace {

  /*! \brief recursive usage tests all access functions
   *
   * \ingroup group_helpers
   */
  template<class T>
    void dump_array_helper(const aff::ConstArray<T>& array, 
                           const Tdim& i, const Tdim& j,
                           Strided::TIndexVec& index, 
                           std::ostream& os)
  {
    if (i>1)
    {
      for (index[i]=array.f(i); index[i]<=array.l(i); index[i]++)
      {
        os << "    dimension "; 
        os << i << ": [" << index[i] << "]";
        dump_array_helper(array, i-1, j, index, os);
      }
    }
    else if (i>0)
    {
      if (i<j) os << endl;
      os.width(5); os << " ";
      for (int k=array.f(1); k<=array.l(1); k++)
      {
        os.width(6); os << k;
      }
      os << endl;
      for (index[0]=array.f(0); index[0]<=array.l(0); index[0]++)
      {
        os.width(5); os << index[0];
        for (index[1]=array.f(1); index[1]<=array.l(1); index[1]++)
        {
          os.width(5); 
          if (j==0)
          { os << array(index[0]); }
          else if (j==1)
          { os << array(index[0], index[1]); }
          else if (j==2)
          { os << array(index[0], index[1], index[2]); }
          else if (j==3)
          { os << array(index[0], index[1], index[2], index[3]); }
          else 
          { os << array(index); }
          os << "#";
        }
        os << endl;
      }
    }
    else 
    {
      for (index[0]=array.f(0); index[0]<=array.l(0); index[0]++)
      {
        os.width(5); os << index[0];
        os.width(5); 
        if (j==0)
        { os << array(index[0]); }
        else if (j==1)
        { os << array(index[0], index[1]); }
        else if (j==2)
        { os << array(index[0], index[1], index[2]); }
        else if (j==3)
        { os << array(index[0], index[1], index[2], index[3]); }
        else 
        { os << array(index); }
        os << "#" << endl;
      }
    }
  }
}

/*----------------------------------------------------------------------*/

/*! \brief Dump array values only
 *
 * \ingroup group_helpers
 */
template<class T>
void dump_array_values(const ConstArray<T>& array, 
                       const Tdim& i=(Strided::Mmax_dimen-1),
                       std::ostream& os=std::cout)
{
  AFF_assert((i<Strided::Mmax_dimen),
             "ERROR (dump_map): illegal dimensionality");
  Strided::TIndexVec index(array.first());
  dump_array_helper(array, i, i, index, os); 
}

/*----------------------------------------------------------------------*/

/*! \brief Dump array values
 *
 * \ingroup group_helpers
 */
template<class T>
void dump_array(const ConstArray<T>& array, 
                const Tdim& i=(Strided::Mmax_dimen-1),
                std::ostream& os=std::cout)
{
  AFF_assert((i<Strided::Mmax_dimen),
             "ERROR (dump_map): illegal dimensionality");
  dump(array);
  os << "Array elements through " << i+1 << " dimensional access:" << endl;
  dump_array_values(array, i, os);
}

/*----------------------------------------------------------------------*/

/*! \brief Dump array shape
 *
 * \ingroup group_helpers
 */
template<class T>
void dump(const ConstArray<T>& array, std::ostream& os=std::cout)
{
  os << "dump of a Array object layout:" << endl;
  os << "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^" << endl;
  dump(array.shape());
  dump_layout(array.representation());
}

} // namespace aff

#endif // AFF_DUMP_ARRAY_H_VERSION (includeguard)

/* ----- END OF dump_array.h ----- */
