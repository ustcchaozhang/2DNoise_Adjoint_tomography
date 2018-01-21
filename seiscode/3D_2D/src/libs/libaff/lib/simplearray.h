/*! \file simplearray.h
 * \brief a simple rigid array
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 08/12/2002
 * 
 * a simple rigid array
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
 *  - 08/12/2002   V1.0   copied from libcontxx
 *  - 11/12/2002   V1.1   introduced extra inner product for mixed types
 *  - 15/12/2002   V1.2   (thof)
 *                        - no need to place this into namespace prebuilt
 *  - 10/12/2007   V1.3   (thof)
 *                        - checked initializer
 *  - 14/05/2011   V1.4   (thof)
 *                        - reworked documentation
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_SIMPLEARRAY_H_VERSION

#define AFF_SIMPLEARRAY_H_VERSION \
  "AFF_SIMPLEARRAY_H   V1.4"

#include<aff/lib/types.h>
#include<aff/lib/rawarfun.h>
 
namespace aff {

using namespace aff::util;

  /*! \brief A very basic rigid array class (with deep inline copy).
   *
   * This class is intensively used by the array classes, in particular by the
   * Shape category.
   * Shape categories need to hold a set of index ranges (e.g.) in an array
   * with array dimension depending on the dimensionality of the array.
   * The arrays used for this purpose should be rigid, i.e. their dimension is
   * defined during compile time and they usually perform no heap allocation
   * on their own (an thus may also be used with stack memory).
   * aff::util::SimpleRigidArray serves for this purpose.
   *
   * \anchor anchor_simplerigigarray
   * \note
   * To pass index values to a aff::Array (during construction or access),
   * we pass a SimpleRigidArray of appropriate dimension \c N. To fill this
   * array we would either need to provide specializations with the
   * appropriate number of constructor arguments or fill each element after
   * the other. 
   * Both approaches range from inconvenient to impossible.
   *
   * For examples to use SimpleRigidArray see tests/simplearraytest.cc
   *
   * \note
   * If you use the default constructor, the values are uninitialized. There
   * is no hidden mechanism to set the values to 0 or other meaningful value!
   *
   * \param T element type
   * \param N dimension of the array
   *
   * Functions to manipulate SimpleRigidArray objects are provided in
   * aff::util::Inline and aff::util::Inline2 which are presented in
   * lib/rawarfun.h
   *
   * \sa aff::util::Inline, aff::util::Inline2
   *
   * \todo
   * Rework this documentation in particular with respect to scoping
   */
  template<class T, Tsize N>
    class SimpleRigidArray {
      public:
        //! element type
        typedef T Telement;
        /*! element type with different name to be consistent with other
         * container classes
         */
        typedef T Tvalue;

        /*-----------------------------------------------------------------*/

        /*! \name Constructors
         *
         * \note 
         * The order of declaration matters in template expansion!
         */
        //@{
        //! construct without(!) initializing
        SimpleRigidArray() { }
        //! copy with deep inline copy
        template<class TT>
        SimpleRigidArray(const SimpleRigidArray<TT, N>& array)
        { 
          aff::util::Inline2<TT, T, N>::copy(array.pointer(), Marray); 
        }
        //! set constructor
        explicit SimpleRigidArray(const T& value) 
        { 
          aff::util::Inline<T, N>::set(Marray, value); 
        }
        //@}

        /*-----------------------------------------------------------------*/

        /*! \name Operators
         *
         * \note 
         * The order of declaration matters in template expansion!
         */
        //@{
        //! copy a value to all positions
        template<class TT>
        SimpleRigidArray& operator=(const TT& value)
        {
          aff::util::Inline2<T, TT, N>::set(Marray, value); 
          return(*this); 
        }
        //! copy with deep inline copy
        template<class TT>
        SimpleRigidArray& operator=(const SimpleRigidArray<TT, N>& array)
        { 
          aff::util::Inline2<TT, T, N>::copy(array.pointer(), Marray); 
          return(*this); 
        }
        //@}

        /*-----------------------------------------------------------------*/
        
        /*! \name Access
         */
        //@{
        //! access
        T& operator[](const Tsubscript& i) { return Marray[i]; }
        //! const access
        const T& operator[](const Tsubscript& i) const { return Marray[i]; }
        //! pointer access
        T* pointer() { return Marray; }
        //! const pointer access
        const T* pointer() const { return Marray; }
        //! size of array
        Tsize size() const { return N; }
        //@}

        /*-----------------------------------------------------------------*/

      private:
        T Marray[N];
    }; // Simple Array

  /*======================================================================*/
  // inlined operations
   
  //! Product of all elements
  template<typename T, Tsize N>
    inline T inline_product(const SimpleRigidArray<T, N>& array)
    { return Inline<T, N>::product(array.pointer()); }

  //! Sum of all elements
  template<typename T, Tsize N>
    inline T inline_sum(const SimpleRigidArray<T, N>& array)
    { return Inline<T, N>::sum(array.pointer()); }

  //! Returns true if any of A is smaller than corresponding B
  template<typename T, Tsize N>
    inline bool inline_anysmaller(const SimpleRigidArray<T, N>& A,
                                  const SimpleRigidArray<T, N>& B)
    { return Inline<T, N>::anysmaller(A.pointer(), B.pointer()); }

  //! Returns true if any of A is larger than corresponding B
  template<typename T, Tsize N>
    inline bool inline_anylarger(const SimpleRigidArray<T, N>& A,
                                 const SimpleRigidArray<T, N>& B)
    { return Inline<T, N>::anylarger(A.pointer(), B.pointer()); }

  //! Returns true if any of A is larger than corresponding B
  template<typename T1, typename T2, Tsize N>
    inline bool inline_anylarger(const SimpleRigidArray<T1, N>& A,
                                 const SimpleRigidArray<T2, N>& B)
    { return Inline2<T1, T2, N>::anylarger(A.pointer(), B.pointer()); }

  //! Returns inner product
  template<typename T, Tsize N>
    inline T inline_innerproduct(const SimpleRigidArray<T, N>& A,
                                 const SimpleRigidArray<T, N>& B)
    { return Inline<T, N>::innerproduct(A.pointer(), B.pointer()); }

  //! Returns inner product
  template<typename T1, typename T2, Tsize N>
    inline T1 inline_innerproduct(const SimpleRigidArray<T1, N>& A,
                                  const SimpleRigidArray<T2, N>& B)
    { return Inline2<T1, T2, N>::innerproduct(A.pointer(), B.pointer()); }

  //! Returns strided product
  template<typename T, Tsize N>
    inline T inline_strideproduct(const SimpleRigidArray<T, N>& A,
                                  const SimpleRigidArray<T, N>& B)
    { return Inline<T, N>::strideproduct(A.pointer(), B.pointer()); }

} // namespace aff

#endif // AFF_SIMPLEARRAY_H_VERSION (includeguard)

/* ----- END OF simplearray.h ----- */
