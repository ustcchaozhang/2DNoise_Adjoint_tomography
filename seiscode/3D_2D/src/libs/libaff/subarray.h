/*! \file subarray.h
 * \brief external class to create subarrays (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/12/2002
 * 
 * external class to create subarrays (prototypes)
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
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 19/12/2002   V1.0   Thomas Forbriger
 *  - 29/12/2002   V1.1   (thof)
 *                        - reworked class
 *                        - removed default constructor and bracket operator
 *                          of container object
 *                        - introduced aff::subarray
 *  - 04/01/2003   V1.2   (thof)
 *                        - now provides a Tcontainer typedef (see
 *                          aff::deepcopy)
 *  - 09/01/2003   V1.3   (thof)
 *                        - now throws exception in case of too many
 *                          concatenated bracket operators
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_SUBARRAY_H_VERSION

#define AFF_SUBARRAY_H_VERSION \
  "AFF_SUBARRAY_H   V1.3"

#include<aff/array.h>

namespace aff {
namespace util {

  /*! \brief Create subarrays
   *
   * This class helps you to create a subarray from an existing array.
   * Use this through the function aff::subarray.
   *
   * Examples can be found in tests/helpertest.cc
   *
   * \todo
   * The class may also work for aff::Series objects, if their shape offers
   * appropriate functionality. This is left to a future revision.
   *
   * \todo 
   * The constructor copys the shape twice. That's inefficient.
   */
  template<class C>
    class Subarray
    {
      public:
        //! Type of array to be handled
        typedef C Tarray;
        //! Type of array to be handled
        typedef C Tcontainer;
        //! Type of const element
        typedef typename Tarray::Tconst_value Tconst_value;
        //! Type of reference to const element
        typedef typename Tarray::Tconst_reference Tconst_reference;
        //! Type of pointer to const element
        typedef typename Tarray::Tconst_pointer Tconst_pointer;
        //! Type of shape to be used
        typedef typename Tarray::Tshape Tshape;
        //! Type of reference to be used
        typedef typename Tarray::Trepresentation Trepresentation;
        //! Constructor takes a reference to an array
        Subarray(const Tarray& array)
        {
          // notice: a const Array does not return its representation
          // thus we create a local (non-const) copy
          // that's truely inefficient (copying the shape twice)
          Tarray copy=array;
          Mshape=copy.shape();
          Mrepresentation=copy.representation();
          Mdim=0;
        }
        //! use full range of this dimension
        Subarray& operator()() 
        { ++Mdim; return(*this); }
        //! set last index of this dimension
        Subarray& operator()(const Tsubscript& last) 
        { 
          this->check_dim();
          Mshape.shrink(Mdim, last);
          ++Mdim;
          return(*this); 
        }
        //! set first and last index of this dimension
        Subarray& operator()(const Tsubscript& first,
                             const Tsubscript& last) 
        { 
          this->check_dim();
          Mshape.shrink(Mdim, first, last);
          ++Mdim;
          return(*this); 
        }
        //! provide value assignment to a subarray
        Subarray& operator=(Tconst_reference value)
        {
          typename Tshape::Tstepper st(Mshape);
          for(st.tofirst(); st.valid(); st.incr())
          { Mrepresentation[st.current()]=value; } 
          return(*this);
        }
        //! return an array
        Tarray array() const
        { return(Tarray(Mshape,Mrepresentation)); }
        //! convert to array
        operator Tarray() const
        { return(Tarray(Mshape,Mrepresentation)); }
      private:
        //! check dimensions
        void check_dim() const
        {
          AFF_assert((Mdim<Tshape::Mmax_dimen),
                     "ERROR (Subarray): you use too many dimension-brackets!");
        }
        //! Shape to process
        Tshape Mshape;
        //! Representation to use
        Trepresentation Mrepresentation;
        //! Dimension to specify
        Tdim Mdim;
    }; // class Subarray

} // namespace util

/*----------------------------------------------------------------------*/

/*! \brief Wrapper function to select correct type
 *
 * Returns an appropriate aff::util::Subarray object for a given container
 * class. 
 *
 * \sa tests/helpertest.cc
 * \sa aff::util::Subarray
 */
template<class C>
inline
aff::util::Subarray<C> subarray(const C& c) 
{ return(aff::util::Subarray<C>(c)); }

} // namespace aff

#endif // AFF_SUBARRAY_H_VERSION (includeguard)

/* ----- END OF subarray.h ----- */
