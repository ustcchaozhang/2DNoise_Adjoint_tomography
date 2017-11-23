/*! \file rawarfun.h
 * \brief raw array function templates
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 08/12/2002
 * 
 * raw array function templates
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
 * they are unrolled and inlined loops.
 *
 * \note
 * Normally you will not include this header directly. It is used internally
 * by libaff and included through array.h and binarray.h
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 08/12/2002   V1.0   copied from libcontxx
 *  - 11/12/2002   V1.1   introduced mixed type operatrions
 *  - 15/12/2002   V1.2   (thof)
 *                        - no need to place this in namespace prebuilt
 *  - 14/05/2011   V1.3   (thof)
 *                        - removed reference to libcontxx in documentation
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_RAWARFUN_H_VERSION

#define AFF_RAWARFUN_H_VERSION \
  "AFF_RAWARFUN_H   V1.3"

#include<aff/lib/types.h>
 
namespace aff {

namespace util {

  //! Recursive functions to inline raw array operations.
  template<typename T, int I>
    class Inline {
      public:
        //! copy all values from source to target
        static inline void copy(const T* source, T* target)
        { 
          target[I-1]=source[I-1]; 
          Inline<T,I-1>::copy(source, target); 
        }
        /*! \brief calculate product of all elements
         *
         * \param array \f$ A_l \f$
         * \return \f$ \prod\limits_{l=0}^{I-1}A_l \f$
         */
        static inline T product(const T* array)
        { 
          return(array[I-1]*Inline<T,I-1>::product(array)); 
        }
        /*! \brief calculate sum of all elements
         *
         * \param array \f$ A_l \f$
         * \return \f$ \sum\limits_{l=0}^{I-1}A_l \f$
         */
        static inline T sum(const T* array)
        { 
          return(array[I-1]+Inline<T,I-1>::sum(array));
        }
        //! set all elements to value
        static inline void set(T* array, const T& value)
        { 
          array[I-1]=value; Inline<T,I-1>::set(array, value);
        }
        //! true if ony of A is smaller than corresponding B
        static inline bool anysmaller(const T* A, const T* B)
        {
          return (((*A) < (*B)) 
                  || Inline<T,I-1>::anysmaller(&(A[1]), &(B[1])));
        }
        //! true if ony of A is larger than corresponding B
        static inline bool anylarger(const T* A, const T* B)
        {
          return (((*A) > (*B)) 
                  || Inline<T,I-1>::anylarger(&(A[1]), &(B[1])));
        }
        /*! \brief calculate inner product
         *
         * \param A \f$ A_l \f$
         * \param B \f$ B_l \f$
         * \return \f$ \sum\limits_{l=0}^{I-1}A_l\,B_l \f$
         */
        static inline T innerproduct(const T* A, const T* B)
        {
          return(A[I-1]*B[I-1]+Inline<T, I-1>::innerproduct(A,B));
        }
        /*! \brief calculate stride product
         *
         * \param A \f$ A_l \f$
         * \param B \f$ B_l \f$
         * \return
         *   \f$
         *     A_{0}+\sum\limits_{l=1}^{I-1}
         *        \left(A_l\,\prod\limits_{k=0}^{l-1}B_k\right)
         *     =A_{0}+B_{0}\;(A_{1}+B_{1}\;(A_{2}+\ldots))
         *   \f$
         *
         * This is used to calculate offset values for the strided
         * shape.
         * \sa aff::Strided
         */
        static inline T strideproduct(const T* A, const T* B)
        {
          return((*A)+(*B)*Inline<T, I-1>::strideproduct(&(A[1]),&(B[1])));
        }
    }; // class Inline
  
  //! Partial specialization to stop recursion.
  template<typename T>
    class Inline<T,1> {
      public:
        static inline void copy(const T* source, T* target) 
        { target[0]=source[0]; }
        static inline T product(const T* array) 
        { return (array[0]); }
        static inline T sum(const T* array) 
        { return (array[0]); }
        static inline void set(T* array, const T& value) 
        { array[0]=value; }
        static inline bool anysmaller(const T* A, const T* B)
        { return (((*A) < (*B))); }
        static inline bool anylarger(const T* A, const T* B)
        { return (((*A) > (*B))); }
        static inline T innerproduct(const T* A, const T* B)
        { return(A[0]*B[0]); }
        static inline T strideproduct(const T* A, const T* B)
        { return(A[0]+B[0]); }
    }; // class Inline<T, 1>

  /*----------------------------------------------------------------------*/

  //! Recursive functions to inline raw array operations.
  template<typename T1, typename T2, int I>
    class Inline2 {
      public:
        //! copy all values from source to target
        static inline void copy(const T1* source, T2* target)
        { 
          target[I-1]=source[I-1]; 
          Inline2<T1, T2, I-1>::copy(source, target); 
        }
        //! set all elements to value
        static inline void set(T1* array, const T2& value)
        { 
          array[I-1]=value; Inline2<T1, T2 ,I-1>::set(array, value);
        }
        //! true if ony of A is smaller than corresponding B
        static inline bool anysmaller(const T1* A, const T2* B)
        {
          return (((*A) < (*B)) 
                  || Inline2<T1, T2, I-1>::anysmaller(&(A[1]), &(B[1])));
        }
        //! true if ony of A is larger than corresponding B
        static inline bool anylarger(const T1* A, const T2* B)
        {
          return (((*A) > (*B)) 
                  || Inline2<T1, T2,I-1>::anylarger(&(A[1]), &(B[1])));
        }
        /*! \brief calculate inner product
         *
         * \param A \f$ A_l \f$
         * \param B \f$ B_l \f$
         * \return \f$ \sum\limits_{l=0}^{I-1}A_l\,B_l \f$
         */
        static inline T1 innerproduct(const T1* A, const T2* B)
        {
          return(A[I-1]*B[I-1]+Inline2<T1, T2, I-1>::innerproduct(A,B));
        }
        /*! \brief calculate stride product
         *
         * \param A \f$ A_l \f$
         * \param B \f$ B_l \f$
         * \return
         *   \f$
         *     A_{0}+\sum\limits_{l=1}^{I-1}
         *        \left(A_l\,\prod\limits_{k=0}^{l-1}B_k\right)
         *     =A_{0}+B_{0}\;(A_{1}+B_{1}\;(A_{2}+\ldots))
         *   \f$
         *
         * This is used to calculate offset values for the strided
         * shape.
         * \sa aff::Strided
         */
        static inline T1 strideproduct(const T1* A, const T2* B)
        {
          return((*A)+(*B)*Inline2<T1, T2, I-1>::strideproduct(&(A[1]),&(B[1])));
        }
    }; // class Inline2
  
  //! Partial specialization to stop recursion.
  template<typename T1, typename T2>
    class Inline2<T1, T2, 1> {
      public:
        static inline void copy(const T1* source, T2* target) 
        { target[0]=source[0]; }
        static inline void set(T1* array, const T2& value) 
        { array[0]=value; }
        static inline bool anysmaller(const T1* A, const T2* B)
        { return (((*A) < (*B))); }
        static inline bool anylarger(const T1* A, const T2* B)
        { return (((*A) > (*B))); }
        static inline T1 innerproduct(const T1* A, const T2* B)
        { return(A[0]*B[0]); }
        static inline T1 strideproduct(const T1* A, const T2* B)
        { return(A[0]+B[0]); }
    }; // class Inline2<T1, T2, 1>

} // namespace util

} // namespace aff

#endif // AFF_RAWARFUN_H_VERSION (includeguard)

/* ----- END OF rawarfun.h ----- */
