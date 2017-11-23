/*! \file checkedcast.h
 * \brief size checked pointer cast (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 03/01/2003
 * 
 * size checked pointer cast (prototypes)
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
 * \sa aff::util::SizeCheckedCast
 * 
 * Copyright (c) 2003 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 03/01/2003   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_CHECKEDCAST_H_VERSION

#define AFF_CHECKEDCAST_H_VERSION \
  "AFF_CHECKEDCAST_H   V1.0"

namespace aff {
namespace util {

  /*! \brief utility for compile-time checked cast
   *
   * Cast a pointer from T* to TT* and check for same size of values of type T
   * and TT and for const-correctness of the conversion. Although
   * reinterpret_cast should check for conts-correctness, we implement a check
   * here too (just for safety - I'm so paranoid, you know).
   *
   * \sa aff::util::SizeCheckedCast::ConstCheck
   * \sa aff::util::SizeCheckedCast::SizeCheck
   *
   * When exchaning data with Fortran code we have to pass pointers to arrays.
   * The f2c.h uses a structure (e.g.) for values of type doublereal that have
   * the same memory layout like complex<double> (first a double for the real
   * part than a double for the imaginary part) but there is no trivial
   * conversion between pointers of these types. Hence we have to use
   * reinterpret_cast, which is totally unchecked. With this class we
   * implement a cast that checks for matching type-size and const-correctness
   * at compile-time. The involved code-overhead is easily optimized away by
   * the compiler.
   *
   * There could be solutions to this problem that involve less code. This
   * however is a solution that produces well understandable error messages
   * with g++.
   *
   * Invoking static functions of subclasses in the cast-function may seem
   * unnecessary. Indeed, there is a solution of that problem, that relies on
   * typedefs to be resolved, which works well with g++. Since I don't know
   * about the template instatiation characteristics of other compilers, I
   * prefer to force the compiler to instantiate all involved template
   * specializations to check whether they support the cast.
   *
   * \sa aff::FortranArray
   * \sa tests/f77test.cc
   *
   * \param T the input pointer is of type T*
   * \param TT the output pointer is of type TT*
   */
  template<class T, class TT>
    class SizeCheckedCast {

      public:
        //! return a type-casted (and type-checked) pointer
        static TT* cast(T* p) {
          return(reinterpret_cast<TT*>(
                   SizeCheck<sizeof(T),sizeof(TT)>::have_same_size(
                     ConstCheck<T,TT>::respects_constness(p)))); }

      private:
        /*! \brief checks for const-correctness of the cast
         *
         * Only those specializations of this class offer a function
         * respects_constness, that represent a const-correct conversion.
         *
         * The general template if for two non-const types - which is a
         * const-correct conversion
         *
         * \param Tfrom source type (non-const)
         * \param Tto destination type (non-const)
         *
         * \sa aff::util::SizeCheckedCast::ConstCheck<const Tfrom, const Tto>
         * \sa aff::util::SizeCheckedCast::ConstCheck<Tfrom, const Tto>
         * \sa aff::util::SizeCheckedCast::ConstCheck<const Tfrom, Tto>
         * \sa aff::util::SizeCheckedCast
         */
        template<class Tfrom, class Tto> class ConstCheck {
          public: static T* respects_constness(T* v) { return v; }
        }; // class ConstCheck

        /*! \brief specialization for const-checked cast
         *
         * \param Tfrom source type (non-const)
         * \param Tto destination type (const)
         *
         * This conversion is const-correct.
         * \sa aff::util::SizeCheckedCast::ConstCheck
         * \sa aff::util::SizeCheckedCast
         */
        template<class Tfrom, class Tto> 
          class ConstCheck<Tfrom, const Tto> {
          public: static T* respects_constness(T* v) { return v; }
        }; // class ConstCheck (specialization)

        /*! \brief specialization for const-checked cast
         *
         * \param Tfrom source type (const)
         * \param Tto destination type (const)
         *
         * This conversion is const-correct.
         * \sa aff::util::SizeCheckedCast::ConstCheck
         * \sa aff::util::SizeCheckedCast
         */
        template<class Tfrom, class Tto> 
          class ConstCheck<const Tfrom, const Tto> {
          public: static T* respects_constness(T* v) { return v; }
        }; // class ConstCheck (specialization)

        /*! \brief specialization for const-checked cast
         *
         * \param Tfrom source type (const)
         * \param Tto destination type (non-const)
         *
         * This conversion is \b not const-correct.
         * \sa aff::util::SizeCheckedCast::ConstCheck
         * \sa aff::util::SizeCheckedCast
         */
        template<class Tfrom, class Tto> 
          class ConstCheck<const Tfrom, Tto> 
          { }; // class ConstCheck (specialization)

        /*----------------------------------------------------------------*/

        /*! \brief checks for matching type-size
         *
         * The general template (non-matching sizes) does not offer 
         * a function have_same_size. This function is only offered by the
         * specialization that is invoked by conversions for types of same
         * size.
         *
         * \param N size of source type
         * \param M size of destination type (N!=M)
         *
         * \sa aff::util::SizeCheckedCast::SizeCheck<N,N>
         * \sa aff::util::SizeCheckedCast
         */ 
        template<int N, int M> class SizeCheck 
        { }; // class SizeCheck (specialization)

        /*! \brief specialization for type-size checked cast
         *
         * This specialization is invoked if both types have the same size 
         * \p N. It thus offers the function have_same_size.
         *
         * \sa aff::util::SizeCheckedCast::SizeCheck
         * \sa aff::util::SizeCheckedCast
         */
        template<int N> class SizeCheck<N,N> { 
          public: static T* have_same_size(T* v) { return v; }
        }; // class SizeCheck

    }; // class SizeCheckedCast

} // namespace util
} // namespace aff

#endif // AFF_CHECKEDCAST_H_VERSION (includeguard)

/* ----- END OF checkedcast.h ----- */
